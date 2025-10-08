open Bytesrw

let trip filename =
  In_channel.with_open_bin filename @@ fun ic ->
  let reader = Bytes.Reader.of_in_channel ic in
  let bib = Bib.decode ~filename reader in
  Fmt.pr "===== Encoded %s =====\n%!" filename;
  Bib.encode bib (Bytes.Writer.of_out_channel Out_channel.stdout) 

let inproceedings_authors () =
  let filename = "test.bib" in
  In_channel.with_open_bin filename @@ fun ic ->
  let reader = Bytes.Reader.of_in_channel ic in
  let bib = Bib.decode ~filename reader in
  Fmt.pr "===== inproceedings_authors %s =====\n%!" filename;
  Bib.Raw.fold_entries ~type':"inproceedings" (fun ~citation_key:_ kv () ->
    match Bib.Raw.Kv.find "author" kv with
    | None -> () 
    | Some authors -> Fmt.pr "%s\n%!" (Bib.Raw.text authors)
  ) bib () 

  
let () =
  trip "test.bib";
  trip "test2.bib";
  inproceedings_authors ()
  

