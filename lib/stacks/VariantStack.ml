open StackInterface

module VariantStack : StackInterface = struct
  type 'a t = Nil | Cons of 'a * 'a t [@@deriving sexp, compare]

  let empty = Nil
  let push x s = Cons (x, s)
  let peek s = match s with Nil -> None | Cons (x, _) -> Some x
  let pop s = match s with Nil -> None | Cons (_, xs) -> Some xs
  let clear _ = ()
  let is_empty s = s = Nil
  let rec length s = match s with Nil -> 0 | Cons (_, xs) -> 1 + length xs
end
