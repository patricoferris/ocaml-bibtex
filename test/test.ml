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

let test_concat () =
  let filename = "concat.bib" in
  In_channel.with_open_bin filename @@ fun ic ->
  let reader = Bytes.Reader.of_in_channel ic in
  let bib = Bib.decode ~filename reader in
  Fmt.pr "===== Concat %s =====\n%!" filename;
  Bib.Raw.fold_entries (fun ~citation_key kv () ->
    Fmt.pr "Entry: %s\n%!" citation_key;
    List.iter (fun (k, v) ->
      let parts = Bib.Raw.parts v in
      let parts_info = List.map (Bib.Raw.part ~delimiter:true) parts in
      Fmt.pr "  %s = %s\n%!" k (String.concat " # " parts_info)
    ) (Bib.Raw.Kv.to_list kv)
  ) bib ();
  Fmt.pr "===== Concat roundtrip %s =====\n%!" filename;
  Bib.encode bib (Bytes.Writer.of_out_channel Out_channel.stdout)

let () =
  trip "test.bib";
  trip "test2.bib";
  inproceedings_authors ();
  test_concat ()
  

