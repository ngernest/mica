(** QuickCheck utiltiy generators for placeholder Latin text & other strings *)
open Base

open Base_quickcheck

(** [latin] is a lazy sequence of length 100, consisting of "Lorem Ipsum" latin placeholder words *)
let latin : string Sequence.t =
  Sequence.of_lazy
  @@ lazy
       (Sequence.of_list
          [ "Lorem";
            "ipsum";
            "dolor";
            "sit";
            "amet";
            "consectetur";
            "adipiscing";
            "elit";
            "Aenean";
            "turpis";
            "enim";
            "aliquam";
            "sed";
            "enim";
            "sit";
            "amet";
            "commodo";
            "pellentesque";
            "enim";
            "Integer";
            "vulputate";
            "posuere";
            "quam";
            "ut";
            "blandit";
            "Sed";
            "sodales";
            "auctor";
            "scelerisque";
            "Praesent";
            "id";
            "diam";
            "malesuada";
            "viverra";
            "velit";
            "scelerisque";
            "placerat";
            "neque";
            "Quisque";
            "venenatis";
            "felis";
            "at";
            "augue";
            "mattis";
            "ultrices";
            "Praesent";
            "diam";
            "enim";
            "molestie";
            "nec";
            "lacus";
            "id";
            "bibendum";
            "imperdiet";
            "nisi";
            "Pellentesque";
            "elementum";
            "enim";
            "ex";
            "et";
            "laoreet";
            "magna";
            "mollis";
            "et";
            "Suspendisse";
            "cursus";
            "auctor";
            "nunc";
            "ac";
            "porta";
            "Fusce";
            "viverra";
            "ac";
            "erat";
            "et";
            "eleifend";
            "Suspendisse";
            "eu";
            "turpis";
            "accumsan";
            "efficitur";
            "enim";
            "eu";
            "dictum";
            "libero";
            "Phasellus";
            "eget";
            "arcu";
            "sem";
            "Suspendisse";
            "nec";
            "nisi";
            "urna";
            "Vivamus";
            "a";
            "lectus";
            "ac";
            "neque";
            "pulvinar";
            "facilisis"
          ])

module G = Generator

(** Generator of a random "Lorem Ipsum" latin word *)
let genLatin : string G.t =
  let open G.Let_syntax in
  let%bind n = G.int_uniform_inclusive 0 (Sequence.length latin - 1) in
  G.return (Sequence.nth_exn latin n)

(** Generator of non-empty alpha-numeric strings *)
let genNonEmptyAlphaNumStr : string G.t = G.(string_non_empty_of char_alphanum)

(** Generator of non-empty alphabetic strings *)
let genNonEmptyAlphaStr : string G.t = G.(string_non_empty_of char_alpha)
