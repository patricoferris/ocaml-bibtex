(*---------------------------------------------------------------------------
   Large parts of this codec are taken from the jsont codec.

   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)
open Bytesrw
open Astring

(* Base Bibtex types *)
module Raw = struct
  module Kv = struct
    type 'a t = (string * 'a) list

    let empty = []

    let add k v t = (k, v) :: t 

    let find = List.assoc_opt

    let find_and_remove k assoc =
      let rec loop acc = function
        | [] -> raise Not_found
        | (k', v) :: rest when k = k' -> v, List.rev acc @ rest
        | x :: xs -> loop (x :: acc) xs
      in
      loop [] assoc

    let to_list t = List.rev t
  end

  type text = { text : string; delimiter : [ `Curly | `Dquote ] option }

  let text t = t.text
  let delimiter t = t.delimiter

  type entry =
    | String of text Kv.t
    | Preamble of string
    | Comment of string
    | Entry of { type' : string; citation_key : string; tags : text Kv.t }

  let pp_kv fmt k =
    let pp_txt fmt { text; delimiter }= match delimiter with
      | None -> Fmt.string fmt text
      | Some `Curly -> Fmt.pf fmt "{%s}" text 
      | Some `Dquote -> Fmt.quote Fmt.string fmt text
    in
    Fmt.pf fmt "%a" Fmt.(list ~sep:(Fmt.any ",@\n") (fun ppf (k, v) -> Fmt.pf ppf "%s=%a" k pp_txt v)) (Kv.to_list k) 

  let pp_entry fmt = function
    | String kv -> Fmt.pf fmt "@[<v 2>@string{@;%a@.}@]" pp_kv kv
    | Preamble kv -> Fmt.pf fmt "@preamble{%s}" kv
    | Comment kv -> Fmt.pf fmt "@comment{%a}" Fmt.string kv
    | Entry { type'; citation_key; tags } ->
        Fmt.pf fmt "@%s{%s,@[<hov 2>  %a@.}@]" type' citation_key pp_kv tags

  type t = entry list

  let pp fmt = Fmt.list ~sep:(Fmt.any "\n") pp_entry fmt

  let fold_entries ?type' fn t acc =
    let rec loop acc = function
      | [] -> acc
      | String _ :: es | Preamble _ :: es | Comment _ :: es -> loop acc es
      | Entry { type' = type2; citation_key; tags } :: es ->
          let skip = Option.map (fun t -> not (t = type2)) type' |> Option.value ~default:false in
          if skip then loop acc es
          else
            let acc2 = fn ~citation_key tags acc in
            loop acc2 es
    in
    loop acc t
end

(* Parsed entries defined by LaTeX *)

module Name = struct 
  type t = { first : string; last : string; suffix : string option }

  let v ?suffix ~first ~last () = { first; last; suffix }

  let first t = t.first
  let last t = t.last
  let suffix t = t.suffix

  let of_string s =
    match String.cuts ~sep:"," s with
    | [ fullname ] -> (
      match String.cuts ~sep:" " (String.trim fullname) |> List.filter (fun s -> not (String.equal s "")) with
      | [ first; last ] -> { first; last; suffix = None }
      | _ -> Fmt.invalid_arg "Expected <firstname> <lastname> but got: %s" s
    )
    | [ last; first ] -> { first = String.trim first; last = String.trim last; suffix = None }
    | [ last; suffix; first ] -> { first = String.trim first; last = String.trim last; suffix = Some (String.trim suffix) }
    | _ -> Fmt.invalid_arg "Too many commas in name: %s" s

  let display t = match t.suffix with
    | None -> t.first ^ " " ^ t.last
    | Some suffix ->  t.first ^ " " ^ t.last ^ " " ^ suffix

  let to_bibtex t = match t.suffix with
    | None -> Fmt.str "%s, %s" t.last t.first
    | Some suffix -> Fmt.str "%s, %s, %s" t.last suffix t.first
end

let names s =
  String.cuts ~sep:" and " s
   |> List.filter (fun s -> not (String.is_empty s))
   |> List.map String.trim
   |> List.map Name.of_string

let names_to_bibtex =
  let rec loop acc = function
    | [] -> acc
    | [ name ] -> acc ^ Name.to_bibtex name
    | name :: names ->
      let acc = acc ^ Name.to_bibtex name ^ " and " in
      loop acc names
  in
  loop ""


let kv_add_text ?(delimiter=Some `Curly) ~key ~value =
  Raw.Kv.add key { Raw.text = value; delimiter } 

module Article = struct
  type t = {
    author : Name.t list;
    title : string;
    journal : string;
    year : int;
    volume : int option;
    number : int option;
    pages : int option;
    month : string option;
    note : string option;
  }

  let v ?volume ?number ?pages ?month ?note ~author ~journal ~year title =
    { author; title; journal; year; volume; number; pages; month; note }

  let author t = t.author
  let title t = t.title
  let year t = t.year
  let journal t = t.journal
  let volume t = t.volume
  let number t = t.number
  let pages t = t.pages
  let month t = t.month
  let note t = t.note

  let to_kv t : Raw.text Raw.Kv.t =
    Raw.Kv.empty
    |> kv_add_text ~key:"author" ~value:(names_to_bibtex t.author)
    |> kv_add_text ~key:"title" ~value:t.title
    |> kv_add_text ~delimiter:None ~key:"year" ~value:(string_of_int t.year)
    |> kv_add_text ~key:"journal" ~value:t.journal

end

module Inproceedings = struct
  type t = {
    author : Name.t list;
    title : string;
    booktitle : string;
    year : int;
  }
  
  let v ~author ~booktitle ~year title = { author; title; booktitle; year }

  let author t = t.author
  let title t = t.title
  let year t = t.year
  let booktitle t = t.booktitle

  let to_kv t : Raw.text Raw.Kv.t =
    Raw.Kv.empty
    |> kv_add_text ~key:"author" ~value:(names_to_bibtex t.author)
    |> kv_add_text ~key:"title" ~value:t.title
    |> kv_add_text ~delimiter:None ~key:"year" ~value:(string_of_int t.year)
    |> kv_add_text ~key:"booktitle" ~value:t.booktitle
end

type 'a with_extra_tags = 'a * Raw.text Raw.Kv.t

type entry =
  | Article of Article.t with_extra_tags
  | Inproceedings of Inproceedings.t with_extra_tags
  | Other of string with_extra_tags

type t = (string * entry) list

let article ?(extra=Raw.Kv.empty) a = Article (a, extra)
let inproceedings ?(extra=Raw.Kv.empty) a = Inproceedings (a, extra)

let err_lookup field e = 
  Failure (Fmt.str "No field %s in %a" field Raw.pp_kv e)

let find_exn k (e : Raw.text Raw.Kv.t) =
  try Raw.Kv.find_and_remove k e |> fun (f, s) -> Raw.text f, s
  with Not_found -> 
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (err_lookup k e) bt

let of_raw : Raw.t -> t = fun es ->
  let convert (e : Raw.entry) : (string * entry) option = match e with
    | String _ | Preamble _ | Comment _ -> None
    | Entry e ->
      let citation_key = e.citation_key in
      match e.type' with
      | "article" ->
        let s_author, acc = find_exn "author" e.tags in
        let author = names s_author in
        let title, acc = find_exn "title" acc in 
        let journal, acc = find_exn "journal" acc in 
        let year, acc = find_exn "year" acc in 
        let v = Article.v ~author ~year:(int_of_string year) ~journal title in
        Some (citation_key, Article (v, acc))
      | "inproceedings" ->
        let s_author, acc = find_exn "author" e.tags in
        let author = names s_author in
        let title, acc = find_exn "title" acc in 
        let year, acc = find_exn "year" acc in 
        let v = Inproceedings.v ~author ~year:(int_of_string year) ~booktitle:"" title in
        Some (citation_key, Inproceedings (v, acc))
      | type' -> Some (citation_key, Other (type', e.tags)) 
    in
    List.filter_map convert es

let to_raw : t -> Raw.t = fun ts ->
  let convert ~citation_key (e : entry) : Raw.entry = match e with 
    | Article (article, e) ->
      let tags = Article.to_kv article @ e in
      Entry { type' = "article"; citation_key; tags } 
    | Inproceedings (proc, e) ->
      let tags = Inproceedings.to_kv proc @ e in
      Entry { type' = "inproceedings"; citation_key; tags } 
    | Other (type', tags) ->
      Entry { type'; citation_key; tags } 
  in
  List.map (fun (citation_key, e) -> convert ~citation_key e) ts

(* Common fields across entries *)
let type' = function
  | Article _ -> "article" 
  | Inproceedings _ -> "inproceedings"
  | Other (s, _) -> s

let extra = function
  | Article (_, e) -> e
  | Inproceedings (_, e) -> e
  | Other (_, e) -> e

let author = function
  | Article (a, _) -> Article.author a
  | Inproceedings (a, _) -> Inproceedings.author a
  | v ->
    Raw.Kv.find "author" (extra v) 
    |> Option.map Raw.text
    |> Option.map names
    |> Option.value ~default:[] 

let title = function
  | Article (a, _) -> Some (Article.title a)
  | Inproceedings (a, _) -> Some (Inproceedings.title a)
  | v -> 
    Raw.Kv.find "title" (extra v) 
    |> Option.map Raw.text

let year  = function
  | Article (a, _) -> Some (Article.year a)
  | Inproceedings (a, _) -> Some (Inproceedings.year a)
  | v ->
    Raw.Kv.find "title" (extra v)
    |> Option.map Raw.text
    |> fun v -> Option.bind v int_of_string_opt

let journal = function
  | Article (a, _) -> Some (Article.journal a)
  | v -> Raw.Kv.find "journal" (extra v) |> Option.map Raw.text

let doi e = extra e |> Raw.Kv.find "doi" |> Option.map Raw.text
let abstract e = extra e |> Raw.Kv.find "abstract" |> Option.map Raw.text
let url e = extra e |> Raw.Kv.find "url" |> Option.map Raw.text

(* Parser *)
module Textloc = struct
  type line_pos = int
  type byte_pos = int

  type t = {
    filename : string;
    first_byte : byte_pos;
    first_line : line_pos * byte_pos;
    last_byte : byte_pos;
    last_line : line_pos * byte_pos;
  }

  let make ~filename ~first_byte ~last_byte ~first_line ~last_line =
    { filename; first_byte; last_byte; first_line; last_line }

  let none = make ~filename:"-" ~first_byte:(-1) ~last_byte:0 ~first_line:(1,0) ~last_line:(1,0)

  let pp_path = Fmt.string
  let pf = Fmt.pf

  let is_none l = l.first_byte < 0
  let pp ppf l = match is_none l with
  | true -> pf ppf "File \"%a\"" pp_path l.filename
  | false ->
      let pp_lines ppf l = match fst l.first_line = fst l.last_line with
      | true -> pf ppf "line %d" (fst l.first_line)
      | false -> pf ppf "lines %d-%d" (fst l.first_line) (fst l.last_line)
      in
      (* "characters" represent positions (insertion points) not columns *)
      let pos_s = l.first_byte - snd l.first_line in
      let pos_e = l.last_byte - snd l.last_line + 1 in
      if pos_s = 0 && pos_e = 0
      then pf ppf "File \"%a\", %a" pp_path l.filename pp_lines l
      else pf ppf "File \"%a\", %a, characters %d-%d"
          pp_path l.filename pp_lines l pos_s pos_e
end

type error = Textloc.t * string
exception Error of error

module Error = struct
  type t = error 
  let msg meta msg = raise_notrace (Error (meta, msg))
  let msgf meta fmt = Format.kasprintf (fun m -> msg meta m) fmt

  let () =
    Printexc.register_printer (function 
      | Error (txtloc, msg) ->
        Some (Fmt.str "%a: %s" Textloc.pp txtloc msg)
      | _ -> None
    ) 
end

open Raw

type decoder = {
    filename : string;
    reader : Bytes.Reader.t; (* The source of bytes. *)
    mutable i : Stdlib.Bytes.t; (* Current input slice. *)
    mutable i_max : int; (* Maximum byte index in [i]. *)
    mutable i_next : int; (* Next byte index to read in [i]. *)
    mutable byte_count : int; (* Global byte count. *)
    mutable line : int; (* Current line number. *)
    mutable line_start : int; (* Current line global byte position. *)
    mutable c : int; (* Next character *)
    mutable d_count : int; (* Delimiter Counter *)
    token : Buffer.t;
}

let[@inline] is_digit u = 0x0030 (* 0 *) <= u && u <= 0x0039 (* 9 *)
let[@inline] is_text u = 
  (0x0061 (* a *) <= u && u <= 0x007A (* z *)) ||
  (0x0041 (* A *) <= u && u <= 0x005A (* Z *))
let[@inline] is_underscore u = Int.equal u 95 

let make_decoder ?(filename = "-") reader =
  let token = Buffer.create 255 in
  let i = Stdlib.Bytes.create 1 in
  { filename; reader;
    c = 0;
    i_max = 0; i_next = 1 (* triggers an initial refill *); i;
    byte_count = 0; line = 1; line_start = 0; token; d_count = 0 }

let sot = 0x1A0000  (* start of text U+10FFFF + 1 *)
let eot = 0x1A0001  (*   end of text U+10FFFF + 2 *)

(* Decoder positions *)
let[@inline] get_line_pos d = d.line, d.line_start
let get_last_byte d =
  if d.c <= 0x7F then d.byte_count - 1 else
  if d.c = sot || d.c = eot then d.byte_count else failwith "Impossible last byte" 

(* Errors *)

let err_to_here ~first_byte ~first_line d fmt =
  let last_byte = get_last_byte d and last_line = get_line_pos d in
  Error.msgf (Textloc.make ~first_byte ~first_line ~last_byte ~last_line ~filename:d.filename) fmt

let err_unclosed_string ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Unclosed string" 

let err_illegal_ctrl_char ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Illegal control character '%#x'" d.c 

let err_malformed_key ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Malformed key, got a '%c'" (Stdlib.Char.unsafe_chr d.c) 

let err_malformed_kv ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Malformed set of key-values" (Stdlib.Char.unsafe_chr d.c) 

let err_unexpected_character ~first_byte ~first_line e d =
  err_to_here ~first_byte ~first_line d 
    "Unexpected character, expected '%c' but got '%c'" (Stdlib.Char.unsafe_chr e) (Stdlib.Char.unsafe_chr d.c) 

(* Decode next character in d.u *)
let[@inline] is_eod d = d.i_max = - 1 (* Only happens on Slice.eod *)
let[@inline] available d = d.i_max - d.i_next + 1
let[@inline] set_slice d slice =
  d.i <- Bytes.Slice.bytes slice;
  d.i_next <- Bytes.Slice.first slice;
  d.i_max <- d.i_next + Bytes.Slice.length slice - 1

let rec nextc d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let a = available d in
  if a <= 0 then
    (if is_eod d
     then d.c <- eot
     else (set_slice d (Bytes.Reader.read d.reader); nextc d))
  else
  let b = Bytes.get d.i d.i_next in
  d.c <- match b with
  | '\x00' .. '\x09' | '\x0B' | '\x0E' .. '\x7F' as u -> (* ASCII fast path *)
      d.i_next <- d.i_next + 1; d.byte_count <- d.byte_count + 1;
      Stdlib.Char.code u
  | '\x0D' (* CR *) ->
      d.i_next <- d.i_next + 1; d.byte_count <- d.byte_count + 1;
      d.line_start <- d.byte_count; d.line <- d.line + 1;
      0x000D
  | '\x0A' (* LF *) ->
      d.i_next <- d.i_next + 1; d.byte_count <- d.byte_count + 1;
      d.line_start <- d.byte_count;
      if d.c <> 0x000D then d.line <- d.line + 1;
      0x000A
  | c ->
    err_to_here ~first_line ~first_byte d "Unsupported encoding, please use ASCII with LaTeX. Got %i." (Stdlib.Char.code c)

let[@inline] token_clear d = Buffer.clear d.token
let[@inline] token_pop d = let t = Buffer.contents d.token in (token_clear d; t)
let[@inline] token_add d u = Buffer.add_char d.token (Stdlib.Char.unsafe_chr u)
let[@inline] accept d = token_add d d.c; nextc d

(* Whitespace *)
let[@inline] is_ws u =
  if u > 0x20 then false else match Stdlib.Char.unsafe_chr u with
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let[@inline] read_ws d =
  while is_ws d.c do
    nextc d
  done

let opening_delimiter = function
  | 0x0022 -> 0x0022
  | 0x007D -> 0x007B
  | _ -> failwith "Unknown closing delimiter" 

let consume_delimited_word ?(delimiter=0x0022 (*DQUOTE*)) d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let opener = opening_delimiter delimiter in
  let rec loop d = match d.c with
    | u when u = delimiter ->
      if d.d_count = 0 then 
        let v = token_pop d in
        nextc d; v
      else (accept d; d.d_count <- d.d_count - 1; loop d)
    | u when u = eot -> err_unclosed_string ~first_byte ~first_line d
    | u when is_ws u -> accept d; loop d
    | u when 0x0000 <= u && u <= 0x001F ->
      err_illegal_ctrl_char ~first_byte ~first_line d
    | v ->
      if v = opener then d.d_count <- d.d_count + 1; 
      accept d; loop d
  in
  loop d

let consume_word d =
  (* let first_byte = get_last_byte d and first_line = get_line_pos d in *)
  let rec loop d =
    if is_text d.c || is_digit d.c then (accept d; loop d)
  in
  loop d;
  token_pop d

let consume_kv d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let rec key_loop d = match d.c with
    | 0x003D (* = *) -> let v = token_pop d in nextc d; v
    | v when is_ws v -> (
      let v = token_pop d in
      read_ws d;
      match d.c with
        | 0x003D -> nextc d; v
        | _ -> err_malformed_key ~first_byte ~first_line d
    )
    | v when is_text v || is_digit v || is_underscore v ->
      token_add d v; nextc d; key_loop d
    | _ -> err_malformed_key ~first_byte ~first_line d  
  in
  let value d = match d.c with
    | 0x007B (* { *) -> nextc d; { text = consume_delimited_word ~delimiter:0x007D d; delimiter = Some `Curly }
    | 0x0022 (* DQUOTE *) -> nextc d; { text = consume_delimited_word ~delimiter:0x0022 d; delimiter = Some `Dquote }
    | _ -> { text = consume_word d; delimiter = None }
  in
  read_ws d;
  let key = key_loop d in
  read_ws d;
  let value = value d in
  (key, value)

let consume_all_kvs d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let rec loop acc d =
    read_ws d;
    let k, v = consume_kv d in
    let acc = Kv.add k v acc in 
    read_ws d;
    match d.c with
    | 0x002C (* COMMA *) -> nextc d; loop acc d
    | 0x007D (* } *) -> nextc d; acc
    | _ ->
      err_malformed_kv ~first_byte ~first_line d
  in
  loop Kv.empty d

let readc c d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  if d.c = c then nextc d
  else err_unexpected_character ~first_byte ~first_line c d

let read_left_curly = readc 0x007B
let read_comma = readc 0x002c

let consume_preamble _ = Preamble "TODO"

let consume_string d = 
  read_left_curly d;
  let kv = consume_all_kvs d in
  String kv

let consume_comment d =
  read_left_curly d;
  let cmt = consume_delimited_word ~delimiter:0x007D d in
  Comment cmt

let is_ascii i = 0 <= i && i <= 255
let is_comma = Int.equal 0x002c  

let consume_citation_key d = 
  let rec loop d =
    if is_ascii d.c && not (is_comma d.c) then (accept d; loop d)
  in
  loop d;
  token_pop d

let consume_entry ~type' d = 
  read_left_curly d;
  read_ws d;
  let citation_key = consume_citation_key d in
  read_ws d;
  read_comma d;
  let tags = consume_all_kvs d in
  Entry { type'; citation_key; tags }

let consume_entry d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  match (read_ws d; d.c) with
  | 0x0040 (* @ *) -> (
    nextc d;
    match consume_word d |> Stdlib.String.lowercase_ascii with
    | "premable" -> consume_preamble d
    | "string" -> consume_string d
    | "comment" -> consume_comment d
    | type' -> consume_entry ~type' d
  )
  | _ -> err_unexpected_character ~first_byte ~first_line 0x0040 d 

let decode ?filename r =
  let d = make_decoder ?filename r in
  nextc d;
  let rec loop acc d =
    let entry = consume_entry d in
    read_ws d;
    match d.c with
    | v when v = eot -> List.rev (entry :: acc)  
    | _ -> loop (entry :: acc) d
  in
  loop [] d

type encoder = {
  writer : Bytes.Writer.t; (* Destination of bytes. *)
  o : Bytes.t; (* Buffer for slices. *)
  o_max : int; (* Max index in [o]. *)
  mutable o_next : int; (* Next writable index in [o]. *)
}

let make_encoder ?buf writer =
  let o = match buf with
  | Some buf -> buf
  | None -> Bytes.create (Bytes.Writer.slice_length writer)
  in
  let len = Bytes.length o in
  let o_max = len - 1 and o_next = 0 in
  { writer; o; o_max; o_next  }

let flush e =
  Bytes.Writer.write e.writer (Bytes.Slice.make e.o ~first:0 ~length:e.o_next);
  e.o_next <- 0

let[@inline] rem_len e = e.o_max - e.o_next + 1

let write_eot ~eod e = flush e; if eod then Bytes.Writer.write_eod e.writer
let write_char e c =
  if e.o_next > e.o_max then flush e;
  Stdlib.Bytes.set e.o e.o_next c; e.o_next <- e.o_next + 1

let rec write_substring e s first length =
  if length = 0 then () else
  let len = Int.min (rem_len e) length in
  if len = 0 then (flush e; write_substring e s first length) else
  begin
    Bytes.blit_string s first e.o e.o_next len;
    e.o_next <- e.o_next + len;
    write_substring e s (first + len) (length - len)
  end

let write_bytes e s = write_substring e s 0 (String.length s)

let write_indent e ~nest =
  for _ = 1 to nest do write_char e ' '; write_char e ' ' done

let write_text e = function
  | { text; delimiter = None } -> write_bytes e text
  | { text; delimiter = Some `Curly } ->
      write_bytes e "{";
      write_bytes e text;
      write_bytes e "}"
  | { text; delimiter = Some `Dquote } ->
      write_bytes e "\"";
      write_bytes e text;
      write_bytes e "\""

let write_kv e kv =
  let idx = ref 1 in
  let kv = Kv.to_list kv in
  let vs = List.length kv in
  List.iter (fun (k, v) ->
    write_indent e ~nest:1;
    write_bytes e k;
    write_bytes e "=";
    write_text e v;
    (if not (Int.equal !idx vs) then begin
      write_bytes e ",\n"
    end else write_bytes e "\n");
    incr idx
  ) kv 

let write_entry e = function
  | String kv ->
      write_bytes e "@string{\n";
      write_kv e kv;
      write_bytes e "}\n"
  | Comment cmt -> 
      write_bytes e "@comment{";
      write_bytes e cmt;
      write_bytes e "}\n"
  | Preamble p -> 
      write_bytes e "@preamble{";
      write_bytes e p;
      write_bytes e "}\n"
  | Entry { type'; citation_key; tags } ->
      write_bytes e "@";
      write_bytes e type';
      write_bytes e "{";
      write_bytes e citation_key;
      write_bytes e ",\n";
      write_kv e tags;
      write_bytes e "}\n"

let encode ?buf v w =
  let e = make_encoder ?buf w in
  List.iter (write_entry e) v;
  write_eot ~eod:true e 

let of_string ?(filename="of_string") s =
  decode ~filename (Bytes.Reader.of_string s)
  |> of_raw
