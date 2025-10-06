(** {1 A Bibtex Codec}

    {! Bib} is a Bibtex codec. *)

open Bytesrw

(** {2 Decoding}
   
    The standard decoder consumes bytes from a {! Bytes.Reader.t}
    and constructs a {! Raw.t} representation of the Bibtex file.
    This performs no validation of any of the entries or tags in
    the Bibtex file, but is meant more as a means to get the data
    into OCaml.

    Use {! decode} to read your Bibtex data and {! encode} to write
    it. For example, here is a little program to format a Bibtex file
    from a string.

    {@ocaml[
     open Bytesrw

     let format s =
       let reader = Bytes.Reader.of_string s in
       let writer = Bytes.Writer.of_out_channel Out_channel.stdout in
       Bib.encode (Bib.decode reader) writer
    ]}

    You can then use it with something like:

    {@ocaml[
      # format "@string{  hello=world }";;
      @string{
        hello=world
      }
      - : unit = ()
    ]}

*)

module Raw : sig
  module Kv : sig
    type 'a t

    val to_list : 'a t -> (string * 'a) list
  end

  type text
  (** Raw strings from the Bibtex file *)

  val text : text -> string
  (** The string value of the entry *)

  val delimiter : text -> [ `Curly | `Dquote ] option
  (** The delimiter for the text (e.g. [hello={world}] is [Some `Dquote]). *)

  type entry =
    | String of text Kv.t
    | Preamble of string
    | Comment of string
    | Entry of { type' : string; citation_key : string; tags : text Kv.t }
  
  type t = entry list
  (** A representation of a Bibtex file *)

  val pp : t Fmt.t
  (** A pretty printer *)
end
(** The [Raw] module is for the initial parsing of the Bibtex
    file. It will be as close as possible to what is directly in the
    file. No abbreviations have been expanded, no validation of fields
    or narrowing of types. *)

val decode : ?filename:string -> Bytes.Reader.t -> Raw.t
(*( [decode ?filename r] parses a Bibtex file into a list of {! Raw.entry}s *)

(** {2 Encoding} *)

val encode : ?buf:Bytes.t -> Raw.t -> Bytes.Writer.t -> unit
(** [encode raw w] encodes [raw] onto [w]. *)

(** {2 Errors} *)
module Textloc : sig
  type t
  (** Text locations. *)

  val pp : t Fmt.t
  (** A printer for text locations. *)


  val none : t
  (** The [none] text location. *)
end

module Error : sig
  type t = Textloc.t * string 
end

exception Error of Error.t
(** Parsing errors for {! Bib} which combine a text-location ({!Textloc.t})
    alongside a useful error message. During a {! decode} exceptions might
    be raised.

    For example:
    
    {@ocaml[

      # format {|@string{ key="unclosed }|};;
      Exception: File "-", line 1, characters 14-25: Unclosed string
    ]} *)

