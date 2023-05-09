module type SetIntf = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool 
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val rem : 'a -> 'a t -> 'a t
  val size: 'a t -> int
  val union: 'a t -> 'a t -> 'a t
  val intersection: 'a t -> 'a t -> 'a t  
  val invariant: 'a t -> bool
end