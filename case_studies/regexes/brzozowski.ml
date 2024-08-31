open Base
open Base_quickcheck
open Regex_signature

(** Representation of regexes & the Brzozowski derivative in OCaml
    - Adapted from {{: https://www.youtube.com/watch?v=QaMU0wMMczU} Harry Goldstein's video} on DFAs & regexes    
*)

(* Suppress unused value compiler warnings *)
[@@@ocaml.warning "-27-32-34"]

(** Type of regular expressions *)
type re =
  | Void  (** always fails *)
  | Empty  (** accepts empty string *)
  | Lit of char  (** Single literal character *)
  | Alt of re * re  (** [r1|r2], alternation *)
  | Cat of re * re  (** [r1 r2], concatenation *)
  | Star of re  (** [r*], Kleene star *)
[@@deriving compare]

module Brzozowski : S = struct
  type t = re

  (** [lit c] is an alias for the [Lit] constructor *)
  let lit c = Lit c

  (** [void] is an alias for the [Void] constructor *)
  let void = Void

  (** [empty] is an alias for the [Empty] constructor *)
  let empty = Empty

  (** Smart constructor for alternation *)
  let alt r1 r2 =
    match (r1, r2) with
    | _, Void -> r1
    | Void, _ -> r2
    | _, _ -> Alt (r1, r2)

  (** [r1 <|> r2] is the same as [alt r1 r2] *)
  let ( <|> ) r1 r2 : re = alt r1 r2

  (** Smart constructor for concatenation *)
  let cat r1 r2 =
    match (r1, r2) with
    | Void, _ -> Void
    | _, Void -> Void
    | _, Empty -> r1
    | Empty, _ -> r2
    | _, _ -> Cat (r1, r2)

  (** [r1 ^^ r2] is the same as [cat r1 r2] *)
  let ( ^^ ) r1 r2 : re = cat r1 r2

  (** Smart constructor for [star]. Note that:
      - Iterating the empty string gives the empty string, 
      - Zero or more occurrences of [Void] is empty
      - Two iterations is the same as one, i.e. [star (Star r) = Star r] *)
  let star (re : re) : re =
    match re with
    | Void | Empty -> Empty
    | Star re' -> Star re'
    | _ -> Star re

  (** Example regex, where [ex1 = a(b* | c)] *)
  let ex1 : re = Lit 'a' ^^ (Star (Lit 'b') <|> Lit 'c')

  (** [acceptsEmpty r] returns [true] when [r] can match the empty string *)
  let rec acceptsEmpty (re : re) : bool =
    match re with
    | Void | Lit _ -> false
    | Empty | Star _ -> true
    | Alt (r1, r2) -> acceptsEmpty r1 || acceptsEmpty r2
    | Cat (r1, r2) -> acceptsEmpty r1 && acceptsEmpty r2

  (** [deriv re c] takes the Brozozowski derivative of the regex [re] 
      with respect to the char [c] *)
  let rec deriv (re : re) (c : char) : re =
    match re with
    | Void | Empty -> Void
    | Lit c' when Char.(c = c') -> Empty
    | Lit _ -> Void
    | Alt (r1, r2) -> deriv r1 c <|> deriv r2 c
    | Cat (r1, r2) ->
      deriv r1 c ^^ r2 <|> if acceptsEmpty r1 then deriv r2 c else Void
    | Star r -> deriv r c ^^ Star r

  (** [match r s] determines whether the regex [r] matches the string [s] *)
  let matchString (r : re) (s : string) : bool =
    acceptsEmpty (String.fold s ~f:deriv ~init:r)

  (******************************************************************************)
  (** QuickCheck generators for regexes *)

  module G = Generator
  open G.Let_syntax

  (** Generates a regex of the form [Lit c], 
      i.e. a regex that accepts a particular char [c] *)
  let genLit : re G.t =
    let%map c = G.of_list [ 'a'; 'b'; 'c'; 'd' ] in
    Lit c

  (** Generates a character between 'a' and 'd' *)
  let genChar : char G.t = G.of_list [ 'a'; 'b'; 'c'; 'd' ]

  (** Generator for regexes *)

  let genRegex : re G.t =
    G.recursive_union
      [ G.return Void; G.return Empty; (genChar >>| fun c -> Lit c) ]
      ~f:(fun regexGen ->
        [ (let%map r1 = regexGen and r2 = regexGen in
           alt r1 r2);
          (let%map r1 = regexGen and r2 = regexGen in
           cat r1 r2);
          (let%map r = regexGen in
           star r)
        ])

  (** [genRegexString r] returns for the strings accepted by the regex [r], if any *)
  let rec genRegexString (r : re) : string G.t option =
    match r with
    | Void -> None
    | Empty -> Some (G.return "")
    | Lit c -> Some (G.return @@ Char.to_string c)
    | Alt (r1, r2) -> (
      match (genRegexString r1, genRegexString r2) with
      | None, None -> None
      | Some g1, _ -> Some g1
      | _, Some g2 -> Some g2)
    | Cat (r1, r2) ->
      let%bind_open.Option g1 = genRegexString r1 and g2 = genRegexString r2 in
      Some (G.map2 ~f:( ^ ) g1 g2)
    | Star r -> (
      match genRegexString r with
      | None -> Some (G.return "")
      | Some gen ->
        Some
          (let%bind n = G.int_uniform_inclusive 0 3 in
           let%bind s = G.list_with_length ~length:n gen in
           G.return @@ String.concat s))
end
(* To test, run [Option.value_map (genRegexString ex1) ~default:"void"
   ~f:random_value] *)
