(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

open Sexplib.Std
open Imap_uint

type flag =
  [ `Answered
  | `Flagged
  | `Deleted
  | `Seen
  | `Draft
  | `Recent
  | `Keyword of string
  | `Extension of string ] with sexp

type flag_perm =
  [ flag
  | `All ] with sexp

type day_month_year =
  int * int * int

type search_key =
  [ `ALL
  | `ANSWERED
  | `BCC of string
  | `BEFORE of day_month_year
  | `BODY of string
  | `CC of string
  | `DELETED
  | `FLAGGED
  | `FROM of string
  | `KEYWORD of string
  | `NEW
  | `OLD
  | `ON of day_month_year
  | `RECENT
  | `SEEN
  | `SINCE of day_month_year
  | `SUBJECT of string
  | `TEXT of string
  | `TO of string
  | `UNANSWERED
  | `UNDELETED
  | `UNFLAGGED
  | `UNKEYWORD of string
  | `UNSEEN
  | `DRAFT
  | `HEADER of string * string
  | `LARGER of int
  | `NOT of search_key
  | `OR of search_key * search_key
  | `SENTBEFORE of day_month_year
  | `SENTON of day_month_year
  | `SENTSINCE of day_month_year
  | `SMALLER of int
  | `UID of Uid_set.t
  | `UNDRAFT
  | `INSET of Seq_set.t
  | `AND of search_key * search_key
  | `MODSEQ of (flag * [`Shared | `Priv | `All]) option * Modseq.t
  | `X_GM_RAW of string
  | `X_GM_MSGID of Gmsgid.t
  | `X_GM_THRID of Gthrid.t
  | `X_GM_LABELS of string ]

type section_msgtext =
  [ `HEADER
  | `HEADER_FIELDS of string list
  | `HEADER_FIELDS_NOT of string list
  | `TEXT
  | `ALL ] with sexp

type section_spec =
  [ section_msgtext
  | `MIME
  | `PART of int * section_spec ] with sexp

type section =
  [ section_msgtext
  | `PART of int * section_spec ] with sexp

type fetch_att_section =
  [ section
  | `PARTIAL of section * int * int ] with sexp

type fetch_att =
  [ `ENVELOPE
  | `INTERNALDATE
  | `RFC822_HEADER
  | `RFC822_TEXT
  | `RFC822_SIZE
  | `RFC822
  | `BODY
  | `BODYSECTION of fetch_att_section
  | `BODYPEEK of fetch_att_section
  | `BODYSTRUCTURE
  | `UID
  | `FLAGS
  | `MODSEQ
  | `X_GM_MSGID
  | `X_GM_THRID
  | `X_GM_LABELS ] with sexp

type date_time = {
  dt_day : int;
  dt_month : int;
  dt_year : int;
  dt_hour : int;
  dt_min : int;
  dt_sec : int;
  dt_zone : int
} with sexp

type msg_att_section =
  [ section
  | `PARTIAL of section * int ] with sexp

type msg_att_static =
  [ `ENVELOPE of Imap_envelope.t
  | `INTERNALDATE of date_time
  | `RFC822 of string
  | `RFC822_HEADER of string
  | `RFC822_TEXT of string
  | `RFC822_SIZE of int
  | `BODY of Imap_body.t
  | `BODYSTRUCTURE of Imap_body.t
  | `BODYSECTION of msg_att_section * string
  | `UID of Uid.t
  | `X_GM_MSGID of Gmsgid.t
  | `X_GM_THRID of Gthrid.t ] with sexp

type msg_att_dynamic =
  [ `FLAGS of flag list
  | `MODSEQ of Modseq.t
  | `X_GM_LABELS of string list ] with sexp
  
type msg_att =
  [ msg_att_static | msg_att_dynamic ] with sexp

type store_att =
  [ `FLAGS of flag list
  | `FLAGS_SILENT of flag list
  | `X_GM_LABELS of string list
  | `X_GM_LABELS_SILENT of string list ] with sexp

type status_att =
  [ `MESSAGES
  | `RECENT
  | `UIDNEXT
  | `UIDVALIDITY
  | `UNSEEN
  | `HIGHESTMODSEQ ] with sexp

type status_info =
  [ `MESSAGES of int
  | `RECENT of int
  | `UIDNEXT of Uid.t
  | `UIDVALIDITY of Uid.t
  | `UNSEEN of int
  | `HIGHESTMODSEQ of Modseq.t ] with sexp

type mailbox_data_status = {
  st_mailbox : string;
  st_info_list : status_info list
} with sexp

type sflag =
  [ `Noselect
  | `Marked
  | `Unmarked ] with sexp

type oflag =
  [ `Noinferiors
  | `Extension of string ] with sexp

type mbx_list_flags = {
  mbf_sflag : sflag option;
  mbf_oflags : oflag list
} with sexp

type mailbox_list = {
  mb_flag : mbx_list_flags;
  mb_delimiter : char;
  mb_name : string
} with sexp

type capability =
  [ `AUTH_TYPE of string
  | `NAME of string ] with sexp

type namespace = {
  ns_prefix : string;
  ns_delimiter : char;
  ns_extensions : (string * string list) list
} with sexp

type response_info = {
  rsp_alert : string;
  rsp_parse : string;
  rsp_badcharset : string list;
  rsp_trycreate : bool;
  rsp_mailbox_list : mailbox_list list;
  (* rsp_mailbox_lsub : mailbox_list list; *)
  rsp_search_results : Uint32.t list;
  rsp_search_results_modseq : Modseq.t;
  rsp_status : mailbox_data_status;
  rsp_expunged : Seq.t list;
  rsp_fetch_list : (Uint32.t * msg_att list) list;
  rsp_appenduid : Uid.t * Uid.t;
  rsp_copyuid : Uid.t * Uid_set.t * Uid_set.t;
  rsp_compressionactive : bool;
  rsp_id : (string * string) list;
  rsp_modified : Uint32_set.t;
  rsp_namespace : namespace list * namespace list * namespace list;
  rsp_enabled : capability list;
  rsp_other : string * string
}

type selection_info = {
  sel_perm_flags : flag_perm list;
  sel_perm : [ `READ_ONLY | `READ_WRITE ];
  sel_uidnext : Uid.t;
  sel_uidvalidity : Uid.t;
  sel_first_unseen : Seq.t;
  sel_flags : flag list;
  sel_exists : int option;
  sel_recent : int option;
  sel_uidnotsticky : bool;
  sel_highestmodseq : Modseq.t
}

type capability_info =
  capability list
