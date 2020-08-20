(lang dune 2.0)

(name ocaml-imap)

(version 1.2.0)

(source
 (github roddyyaga/ocaml-imap))

(license MIT)

(generate_opam_files true)

(package
 (name imap)
 (synopsis "OCaml IMAP library")
 (depends
  (ocaml
   (>= 4.02.3))
  (base64
   (>= 2.0.0))
  uutf
  re))

(package
 (name imap-unix)
 (depends imap unix ssl))

(package
 (name imap-lwt)
 (depends imap lwt lwt_ssl))
