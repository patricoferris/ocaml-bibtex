open Bytesrw

let trip filename =
  In_channel.with_open_bin filename @@ fun ic ->
  let reader = Bytes.Reader.of_in_channel ic in
  let bib = Bib.decode ~filename reader in
  Fmt.pr "===== Encoded %s =====\n%!" filename;
  Bib.encode bib (Bytes.Writer.of_out_channel Out_channel.stdout) 

let () =
  trip "test.bib";
  trip "test2.bib"
  

