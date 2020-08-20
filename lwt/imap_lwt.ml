(* The MIT License (MIT)

   Copyright (c) 2015-2018 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Lwt.Infix
open Imap

type error =
  | Incorrect_tag of string * string
  | Decode_error of string * int
  | Unexpected_cont
  | Bad_greeting
  | Auth_error of string
  | Server_error of string

exception Error of error

let () =
  Printexc.register_printer (function
    | Error (Decode_error (s, pos)) ->
        Some
          (Printf.sprintf "Parsing error:\n%s\n%s^\n" s (String.make pos ' '))
    | _ -> None)

type t = {
  sock: Lwt_ssl.socket;
  ic: Lwt_io.input_channel;
  oc: Lwt_io.output_channel;
  mutable debug: bool;
  mutable tag: int;
  mutable stop_poll: (unit -> unit) option;
}

let create_connection sock =
  let ic = Lwt_ssl.in_channel_of_descr sock in
  let oc = Lwt_ssl.out_channel_of_descr sock in
  {
    sock;
    ic;
    oc;
    debug = Sys.getenv_opt "IMAPDEBUG" <> None;
    tag = 0;
    stop_poll = None;
  }

let tag { tag; _ } = Printf.sprintf "%04d" tag

module L : sig
  type t = string

  val is_literal : t -> int option
end = struct
  type t = string

  type state = Begin | Int of int | Cr of int | Lf of int

  let is_literal s =
    let rec loop state i =
      if i >= String.length s then None
      else
        match (state, s.[i]) with
        | Begin, '{' -> loop (Int 0) (i + 1)
        | Int n, ('0' .. '9' as c) ->
            loop (Int ((10 * n) + Char.code c - Char.code '0')) (i + 1)
        | Int n, '}' -> loop (Cr n) (i + 1)
        | Cr n, '\r' -> loop (Lf n) (i + 1)
        | Lf n, '\n' ->
            assert (i + 1 = String.length s);
            Some n
        | _ -> loop Begin i
    in
    loop Begin 0
end

let parse { ic; _ } =
  let buf = Buffer.create 17 in
  let rec loop () =
    Lwt_io.read_line ic >>= fun s ->
    let s = s ^ "\r\n" in
    Buffer.add_string buf s;
    match L.is_literal s with
    | Some n ->
        let b = Bytes.create n in
        Lwt_io.read_into_exactly ic b 0 n >>= fun () ->
        Buffer.add_bytes buf b;
        loop ()
    | None -> Lwt.return (Buffer.contents buf)
  in
  loop () >>= fun s ->
  match Parser.response { Parser.s; p = 0 } with
  | Ok x -> Lwt.return x
  | Error (s, pos) -> Lwt.fail (Error (Decode_error (s, pos)))

let rec send imap r process res =
  match r with
  | [] -> Lwt.return res
  | Encoder.Wait :: r ->
      let rec loop res =
        parse imap >>= function
        | Response.Cont _ -> send imap r process res
        | Untagged u -> loop (process res u)
        | Tagged _ -> Lwt.fail (Failure "not expected")
      in
      Lwt_io.flush imap.oc >>= fun () -> loop res
  | Crlf :: r ->
      Lwt_io.write imap.oc "\r\n" >>= fun () -> send imap r process res
  | Raw s :: r -> Lwt_io.write imap.oc s >>= fun () -> send imap r process res

let send imap r process res =
  let r = r [] in
  (* Printf.eprintf "%s\n%!" (Sexplib.Sexp.to_string_hum (Encoder.sexp_of_s r)); *)
  send imap r process res >>= fun res ->
  Lwt_io.flush imap.oc >>= fun () -> Lwt.return res

let wrap_process f res = function
  | Response.Untagged.State (NO (_, s) | BAD (_, s)) ->
      raise (Error (Server_error s))
  | u -> f res u

let run imap { format; u = E (v, process, finish) } =
  let process = wrap_process process in
  let tag = tag imap in
  let r = Encoder.((raw tag ++ format) & crlf) in
  let rec loop res =
    parse imap >>= function
    | Response.Cont _ -> Lwt.fail_with "unexpected"
    | Untagged u -> loop (process res u)
    | Tagged (_, (NO (_code, s) | BAD (_code, s))) ->
        Lwt.fail (Error (Server_error s))
    | Tagged (_, OK _) ->
        imap.tag <- imap.tag + 1;
        Lwt.return res
  in
  send imap r process v >>= loop >|= finish

let () = Ssl.init ()

let connect ~host ~port ~username ~password =
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  let addr = Lwt_unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
  Lwt_unix.connect sock addr >>= fun () ->
  Lwt_ssl.ssl_connect sock ctx >>= fun sock ->
  let imap = create_connection sock in
  parse imap >>= function
  | Response.Untagged _ -> run imap (login username password) >|= fun () -> imap
  | Tagged _ | Cont _ -> Lwt.fail_with "unexpected response"

let disconnect imap =
  run imap logout >>= fun () -> Lwt_ssl.ssl_shutdown imap.sock
