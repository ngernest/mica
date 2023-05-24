open StackInterface 

module VariantStack : StackInterface = struct
  type 'a t = 
    | Nil 
    | Cons of 'a * 'a t
  [@@deriving sexp, compare]

  let empty = Nil
  
  let push x s = Cons (x, s)
  
  let peek s = 
    match s with 
    | Nil -> failwith "can't peek from an empty stack" 
    | Cons (x, _) -> x

  let pop s = 
    match s with 
    | Nil -> s
    | Cons (_, xs) -> xs

  let clear _ = ()

  let is_empty s = s = Nil
    
  let rec length s =
    match s with 
    | Nil -> 0
    | Cons (_, xs) -> 1 + length xs
end