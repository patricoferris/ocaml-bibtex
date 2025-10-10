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
    (** [to_list kv] converts [kv] to an association list. *)

    val find : string -> 'a t -> 'a option
    (** [find key kv] looks for [key] in [kv]. *)
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

  val fold_entries : ?type':string -> (citation_key:string -> text Kv.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  (** [fold ?type' fn t] folds over the entries of [t] using [fn]. You can supply
      an optional [type'] to filter by (e.g. [~type':"article"]). *)

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

(** {2 Parsed Bibtex}

    There are a series of standardised types of Bibtex entries. You can
    convert {! Raw.t}s into them using {! of_raw}.
 *)

(** Names, for people *)
module Name : sig
  type t
  (** An author's name *)

  val v : ?suffix:string -> first:string -> last:string -> unit -> t
  (** A constructor for names, see the example in {! of_string}
      for more information *)

  val display : t -> string
  (** [display t] puts the parts of name in order *)

  val to_bibtex : t -> string
  (** [to_bibtex t] puts the author name back into the bibtex format *)

  val first : t -> string
  (** The first name of the author. Note that this may include middle names and initials.
      For example: ["Smith, Alice M."] will have the [first] of ["Alice M."]. *)

  val last : t -> string
  (** [last name] will be the last name of the author *)

  val suffix : t -> string option
  (** An optional suffix of a name, like ["King, Jr., Martin Luther"]. *)

  val of_string : string -> t
  (** Parses a Bibtex name, may raise [Invalid_argument _].

   For example:

   {@ocaml[
     # Bib.Name.of_string "Ada Lovelace" |> Bib.Name.display;;
     - : string = "Ada Lovelace"
     # Bib.Name.of_string "King, Jr., Martin Luther" |> Bib.Name.display;;
     - : string = "Martin Luther King Jr."
     # Bib.Name.of_string "Turing, Alan M." |> Bib.Name.first;;
     - : string = "Alan M."
   ]}
   *)
end

val names : string -> Name.t list
(** [names s] parses an [author] field into a list of names.

  {@ocaml[
     # Bib.names "Ada Lovelace and  Turing, Alan M."
       |> List.map Bib.Name.display
     - : string list = ["Ada Lovelace"; "Alan M. Turing"]
  ]}
 *)

(** Articles are texts from journals or magazines. *)
module Article : sig
  type t
  (** An article *)

  val v :
    ?volume:int ->
    ?number:int ->
    ?pages:int ->
    ?month:string ->
    ?note:string ->
    author:Name.t list ->
    journal:string ->
    year:int ->
    string ->
    t
  (** Make a new article entry *)

  (** Required fields *)

  val author : t -> Name.t list
  val title : t -> string
  val year : t -> int
  val journal : t -> string

  (** Optional fields *)

  val volume : t -> int option
  val number : t -> int option
  val pages : t -> int option
  val month : t -> string option
  val note : t -> string option
end

(** An {! Inproceedings.t} is an article in a conference's proceedings. *)
module Inproceedings : sig
  type t

  val v :
    author:Name.t list ->
    booktitle:string ->
    year:int ->
    string ->
    t
  (** Make a new Inproceedings entry *)

  val author : t -> Name.t list
  val title : t -> string
  val year : t -> int
  val booktitle : t -> string
end

type 'a with_extra_tags = ('a * Raw.text Raw.Kv.t)
(** ['a with_extra_tags] will decode as many of the fields as possible
    and stash the rest in the second value of the pair *)

type entry =
  | Article of Article.t with_extra_tags
  | Inproceedings of Inproceedings.t with_extra_tags
  | Other of string with_extra_tags
(** A bibtex entry. In [Other (s, tags)], [s] is the name of the entry type *)

type t = (string * entry) list
(** A {! Bib.t} is a list of database entries that associate citation
    keys with a full {! entry}. *)

val article : ?extra:Raw.text Raw.Kv.t -> Article.t -> entry
(** Create a new article entry *)

val inproceedings : ?extra:Raw.text Raw.Kv.t -> Inproceedings.t -> entry
(** Create a new inproceedings entry *)

val of_raw : Raw.t -> t
(** [of_raw raw] converts a set of raw entries (most likely from {! decode}).

    For example:

    {@ocaml[
     # let bib_of_string s =
       let reader = Bytes.Reader.of_string s in
       Bib.of_raw (Bib.decode reader);;
     val bib_of_string : string -> Bib.t = <fun>
     # bib_of_string "@article{smith2020paper, author={Alice Smith}, title={Paper}, year=2020, journal={Some Journal}}";;
     - : Bib.t = [("smith2020paper", Bib.Article (<abstr>, <abstr>))]
    ]}

    Note that for any unimplemented converts from raw to parsed, they will become [Other]s.
    The common functions below will still work as expected.

    {@ocaml[
      # let v = bib_of_string "@misc{ocaml-bib, author={Bib Maintainers}}";;
      val v : Bib.t = [("ocaml-bib", Bib.Other ("misc", <abstr>))]
      # List.map (fun (_, e) -> Bib.author e |> List.map Bib.Name.display) v;;
      - : string list list = [["Bib Maintainers"]]
    ]}
*)

val to_raw : t -> Raw.t
(** [to_raw t] allows a user to convert well-formed bibtex entries into the raw format
    suitable for encoding. This can be used to programmatically construct Bibtex entries
    in OCaml and serialise them.

    {@ocaml[
      # let v = [
        "some-paper", 
        Bib.article (
          Bib.Article.v 
            ~author:[ Bib.Name.v ~first:"Ada" ~last:"Lovelace" () ] 
            ~journal:"Journal"
            ~year:1234 "Title"
        )];;
      val v : (string * Bib.entry) list =
        [("some-paper", Bib.Article (<abstr>, <abstr>))]
      # Bib.encode (Bib.to_raw v) (Bytes.Writer.of_out_channel Out_channel.stdout);; 
      @article{some-paper,
        author={Lovelace, Ada},
        title={Title},
        year=1234,
        journal={Journal}
      }
      - : unit = ()
    ]}
*)

(** {3 Common fields}

   Quite a few parsed Bibtex entries share the same fields. To make it easier
   to extract fields you wish to manipulate, we provide some projections from
   {! entry}s. *)

val type' : entry -> string
(** Convert the entry into its type, for example ["inproceedings"]. *)

val author : entry -> Name.t list
val title : entry -> string option
val year : entry -> int option
val doi : entry -> string option
val abstract : entry -> string option
val journal : entry -> string option
val url : entry -> string option

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

