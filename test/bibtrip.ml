(*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers. All rights reserved.
   Copyright (c) 2026 The bibtex programmers. All rights reserved.
  ---------------------------------------------------------------------------*)
open Bytesrw

let exec = Filename.basename Sys.executable_name

let dump_bib bib =
  List.iter
    (function
      | Bib.Raw.Comment s -> Fmt.pr "Comment:\n  %s\n" s
      | Bib.Raw.String kv ->
          Fmt.pr "String:\n";
          Bib.Raw.Kv.to_list kv
          |> List.iter (fun (k, v) ->
              let parts = Bib.Raw.text v in
              Format.printf "  %s = %s\n%!" k parts)
      | Bib.Raw.Entry { type'; citation_key; tags } ->
          Fmt.pr "Entry(%s,%s):\n" type' citation_key;
          Bib.Raw.Kv.to_list tags
          |> List.iter (fun (k, v) ->
              let parts = Bib.Raw.text v in
              Format.printf "  %s = %s\n%!" k parts)
      | Bib.Raw.Preamble txt -> Fmt.pr "Preamble:\n  %s\n" (Bib.Raw.text txt))
    bib

let trip filename dump =
  let with_input fn =
    match filename with
    | "-" -> Bytes.Reader.of_in_channel In_channel.stdin |> fn
    | f ->
        In_channel.with_open_bin f @@ fun ic ->
        fn (Bytes.Reader.of_in_channel ic)
  in
  with_input @@ fun reader ->
  let bib = Bib.decode ~filename reader in
  if dump then dump_bib bib
  else
    let writer = Bytes.Writer.of_out_channel Out_channel.stdout in
    Bib.encode bib writer

let main () =
  let usage =
    Printf.sprintf
      "Usage: %s [OPTION]... [INFILE]\n\
      \ Recode Bibtex from stdin to stdout.\n\
       Options:"
      exec
  in
  let cmd = ref `Trip in
  let inf = ref "-" in
  let dump = ref false in
  let set_inf f =
    if !inf <> "-" then raise (Arg.Bad "only one file can be specified")
    else inf := f
  in
  let options =
    [
      ("-dump", Arg.Set dump, "dump bibtex file.");
      ("--dump", Arg.Set dump, "dump bibtex file.");
    ]
  in
  Arg.parse (Arg.align options) set_inf usage;
  match !cmd with `Trip -> trip !inf !dump

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers
   Copyright (c) 2026 The bibtex programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
