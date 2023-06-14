(** Implementation of Antimirov derivatives for regexes
    - Adapted from {{: https://semantic-domain.blogspot.com/2013/11/antimirov-derivatives-for-regular.html} Neel Krishnaswami}
*)

open Regex 

(** The module [R] is the type of finite sets of regexes *)
module RegexSet = Set.Make(struct 
  type t = re 
  let compare = compare 
end)

(** The module [M] represents finite maps where the type of the keys are sets of regexes *)
module RegexMap = Map.Make(RegexSet)

(** The module [I] is the type of finite sets of [int]s *)
module I = Set.Make(struct 
  type t = int 
  let compare = compare 
end)
  
(** [rmap f rs] maps a function [f] over a set [rs] of regexes, building a new regex *)
let rmap (f : re -> re) (rs : RegexSet.t) : RegexSet.t = 
  let open RegexSet in
  fold (fun r -> add @@ f r) rs empty 
  
(** [aderiv re c] is the Antimirov derivative of the regex [re] with respect to the char [c] *)  
let rec aderiv (re : re) (c : char) = 
  let open RegexSet in 
  match re with 
  | Lit c' when c = c' -> singleton Empty 
  | Lit _ | Empty | Void -> empty 
  | Alt (r1, r2) -> union (aderiv r1 c) (aderiv r2 c)
  | Cat (r1, r2) -> union (rmap (fun r1' -> r1' ^^ r2) @@ aderiv r1 c)
                          (if acceptsEmpty r1 then aderiv r2 c else empty)
  | Star r -> rmap (fun r' -> r' ^^ Star r) (aderiv r c)
  
(** [derivSet rs c] takes the derivative of a set [rs] of regexes 
    with respect to a char [c], taking the union *)  
let derivSet (rs : RegexSet.t) (c : char) = 
  let open RegexSet in 
  fold (fun r acc -> union (aderiv r c) acc) rs empty

(** A type of DFAs, where: 
    - [size] is the no. of states, which are labeled using ints in the range [[0..size]]) 
    - [fail] labels the sink state for non-matching strings
    - [trans] is a list of transitions
*)  
type dfa = {
  size : int; 
  fail : int; 
  trans : (int * char * int) list; 
  final : int list
}

(** [enum f v i max] is a "functional for-loop" looping from [i] to [max], 
    computing [f i v] for each iteration *)
let rec enum (f : int -> 'a -> 'a) (v : 'a) (i : int) (max : int) : 'a = 
  if i < max 
    then enum f (f i v) (i+1) max 
  else v

(** [charfold f init] folds [f] over all the ASCII characters *)  
let charfold (f : char -> 'a -> 'a) (init : 'a) : 'a = 
  enum (fun i -> f (Char.chr i)) init 0 256

(** [find rs (n, reMap)] takes a set [rs] of regexes and 
     returns a numeric index for it, along with a pair containing [n] and [reMap]
    - [n] represents the highest value index so far 
    - [reMap] is a map from regex sets to their index 
    - If a nuemric index is not found, we map [rs] to [n] and increment [n] *)  
let find (rs : RegexSet.t) (n, reMap : int * (int RegexMap.t)) 
  : int * (int * (int RegexMap.t)) = 
  try (RegexMap.find rs reMap, (n, reMap)) with 
  _ -> n, (n+1, RegexMap.add rs n reMap)

let dfa (r : re) : dfa =
  let rec loop st visited trans finalStates rs =
    let (x, s) = find rs st in
    if I.mem x visited then (s, visited, trans, finalStates)
    else charfold (fun c (s, v, t, f) ->
                      let rs' = derivSet rs c in
                      let (y, s) = find rs' s in
                      loop s v ((x,c,y) :: t) f rs')
            (s, I.add x visited, trans, 
            if RegexSet.exists acceptsEmpty rs then x::finalStates 
            else finalStates) in
  let (state, _, trans, final) = 
    loop (0, RegexMap.empty) I.empty [] [] (RegexSet.singleton r) in
  let (failState, (numStates, _)) = find RegexSet.empty state in 
  { size = numStates; fail = failState; trans; final }

type table = { 
  m : int array array; 
  accept : bool array; 
  error : int 
}

let table d = 
  { error = d.fail;
    accept = Array.init d.size (fun i -> List.mem i d.final);
    m = (let a = Array.init d.size (fun _ -> Array.make 256 0) in
          List.iter (fun (x, c, y) -> a.(x).(Char.code c) <- y) d.trans; a) }  

let rec matches' t s i x =
  if i < String.length s && x != t.error 
  then matches' t s (i+1) t.m.(x).(Char.code s.[i])
  else t.accept.(x)

let re_match t s = matches' t s 0 0          

let charset s = enum (fun i r -> Lit s.[i] <|> r) Void 0 (String.length s)
let string s = enum (fun i r -> r ^^ Lit s.[i]) Empty 0 (String.length s)
let seq rs = List.fold_right (fun r rs -> r ^^ rs) rs Empty
let alt rs = List.fold_right (fun r rs -> r <|> rs) rs Void
let opt r = r <|> Empty
let star r = Star r
let plus r = r ^^ star r

let print_table out t =
  Array.iteri (fun x row ->
    Array.iteri (fun c y ->
      if x != t.error && y != t.error then
        (Format.fprintf out "%d '%c' --> %d " x (Char.chr c) y;
         (if t.accept.(y) then Format.fprintf out "*");
         Format.fprintf out "\n"))
      row)
    t.m
   
module Test = struct
  let digit = charset "0123456789"
  let sign = charset "+-"
  let dot = Lit '.'
  let dotted = alt [ seq [star digit; dot; plus digit];
                     seq [plus digit; dot; star digit] ]
  let exponent = seq [charset "eE"; opt sign; plus digit]
  let float = alt [seq [opt sign; dotted; opt exponent];
                   seq [opt sign; plus digit; exponent] ]

  let t_float = table (dfa float)
end