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

type acc =
  {
    host: string;
    port: int;
    username: string;
    password: string;
  }

type mb =
  {
    account: acc;
    mutable mailbox: string;
  }

let waiters = Lwt_condition.create ()

let conns : Core.t list ref = ref []

let max_conns = ref 5

let wrap t =
  Lwt.on_success t (fun _ -> Lwt_condition.signal waiters ());
  t

let rec use ({account = {host; port; username; password}; mailbox} as state) f =
  match List.find_opt (fun imap -> Core.state imap = Core.SELECTED mailbox) !conns with
  | None ->
      begin match List.find_opt (fun imap -> Core.state imap = Core.AUTHENTICATED) !conns with
      | None ->
          if List.length !conns < !max_conns then begin
            incr max_conns;
            Printf.eprintf "[Starting new connection to %s]\n%!" host;
            Core.connect ~host ~port ~username ~password >>= fun x ->
            conns := x :: !conns;
            Core.select x mailbox >>= fun () ->
            wrap (f x)
          end else
            Lwt_condition.wait waiters >>= fun () -> use state f
      | Some x ->
          Printf.eprintf "[Reusing new connection to %s]\n%!" host;
          Core.select x mailbox >>= fun () ->
          wrap (f x)
      end
  | Some x ->
      Printf.eprintf "[Reusing new connection to %s]\n%!" host;
      wrap (f x)

let uid_search mb query =
  use mb (fun imap -> Core.uid_search imap query)

let uid_fetch mb uids attrs push =
  use mb (fun imap -> Core.uid_fetch imap uids attrs push)

let append mb ?flags ?internaldate data =
  use mb (fun imap -> Core.append imap mb.mailbox ?flags ?internaldate data)