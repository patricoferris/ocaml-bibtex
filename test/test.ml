open Bytesrw

let pp_heading ppf s =
  let pad_len = 80 - String.length s - 2 in
  let left_len = pad_len / 2 in
  let is_even = pad_len mod 2 = 0 in
  let left = String.make (left_len - 1) '=' in  
  let right = if is_even then left else String.make left_len '=' in
  Fmt.pf ppf "%s %s %s\n%!" left s right

let trip filename =
  In_channel.with_open_bin filename @@ fun ic ->
  let reader = Bytes.Reader.of_in_channel ic in
  let bib = Bib.decode ~filename reader in
  Fmt.pr "%a" pp_heading (Fmt.str "encoded %s" filename);
  Bib.encode bib (Bytes.Writer.of_out_channel Out_channel.stdout) 

let inproceedings_authors () =
  let filename = "test.bib" in
  In_channel.with_open_bin filename @@ fun ic ->
  let reader = Bytes.Reader.of_in_channel ic in
  let bib = Bib.decode ~filename reader in
  Fmt.pr "%a" pp_heading (Fmt.str "inproceedings_authors %s" filename);
  Bib.Raw.fold_entries ~type':"inproceedings" (fun ~citation_key:_ kv () ->
    match Bib.Raw.Kv.find "author" kv with
    | None -> ()
    | Some authors -> Fmt.pr "%s\n%!" (Bib.Raw.text authors)
  ) bib ()


let print_entries_and_encode ?(testname="test") filename =
  In_channel.with_open_bin filename @@ fun ic ->
  let reader = Bytes.Reader.of_in_channel ic in
  let bib = Bib.decode ~filename reader in
  Fmt.pr "%a" pp_heading (Fmt.str "%s %s" testname filename);
  Bib.Raw.fold_entries (fun ~citation_key kv () ->
    Fmt.pr "Entry: %s\n%!" citation_key;
    List.iter (fun (k, v) ->
      let parts = Bib.Raw.text v in
      Fmt.pr "  %s = %s\n%!" k parts 
    ) (Bib.Raw.Kv.to_list kv)
  ) bib ();
  Fmt.pr "%a" pp_heading (Fmt.str "concat roundtrip %s" filename);
  Bib.encode bib (Bytes.Writer.of_out_channel Out_channel.stdout)

let test_concat_and_vars () =
  print_entries_and_encode ~testname:"concat" "concat.bib" 

let () =
  trip "test.bib";
  trip "test2.bib";
  inproceedings_authors ();
  test_concat_and_vars ()
  

