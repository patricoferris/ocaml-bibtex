(*---------------------------------------------------------------------------
   Large parts of this codec are taken from the jsont codec.

   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)
open Bytesrw

(* Base Bibtex types *)
module Raw = struct
  module Kv : sig
    type 'a t

    val empty : 'a t

    val add : string -> 'a -> 'a t -> 'a t

    val to_list : 'a t -> (string * 'a) list
  end = struct
    type 'a t = (string * 'a) list

    let empty = []

    let add k v t = (k, v) :: t 

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
end

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
  err_to_here ~first_byte ~first_line d "Malformed key, got a '%c'" (Char.unsafe_chr d.c) 

let err_malformed_kv ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Malformed set of key-values" (Char.unsafe_chr d.c) 

let err_unexpected_character ~first_byte ~first_line e d =
  err_to_here ~first_byte ~first_line d "Unexpected character, expected '%c' but got '%c'" (Char.unsafe_chr e) (Char.unsafe_chr d.c) 

(* Decode next character in d.u *)
let[@inline] is_eod d = d.i_max = - 1 (* Only happens on Slice.eod *)
let[@inline] available d = d.i_max - d.i_next + 1
let[@inline] set_slice d slice =
  d.i <- Bytes.Slice.bytes slice;
  d.i_next <- Bytes.Slice.first slice;
  d.i_max <- d.i_next + Bytes.Slice.length slice - 1

let rec nextc d =
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
      Char.code u
  | '\x0D' (* CR *) ->
      d.i_next <- d.i_next + 1; d.byte_count <- d.byte_count + 1;
      d.line_start <- d.byte_count; d.line <- d.line + 1;
      0x000D
  | '\x0A' (* LF *) ->
      d.i_next <- d.i_next + 1; d.byte_count <- d.byte_count + 1;
      d.line_start <- d.byte_count;
      if d.c <> 0x000D then d.line <- d.line + 1;
      0x000A
  | _ -> failwith "Unsupported encoding, please use ASCII"

let[@inline] token_clear d = Buffer.clear d.token
let[@inline] token_pop d = let t = Buffer.contents d.token in (token_clear d; t)
let[@inline] token_add d u = Buffer.add_char d.token (Char.unsafe_chr u)
let[@inline] accept d = token_add d d.c; nextc d

(* Whitespace *)
let[@inline] is_ws u =
  if u > 0x20 then false else match Char.unsafe_chr u with
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
    | v when is_text v || is_digit v ->
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
let read_comma = readc 0x002C

let consume_preamble _ = Preamble "TODO"

let consume_string d = 
  read_left_curly d;
  let kv = consume_all_kvs d in
  String kv

let consume_comment d =
  read_left_curly d;
  let cmt = consume_delimited_word ~delimiter:0x007D d in
  Comment cmt

let consume_citation_key = consume_word

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
    match consume_word d |> String.lowercase_ascii with
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
