module type SetInterface = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val rem : 'a -> 'a t -> 'a t
  val size : 'a t -> int
  val union : 'a t -> 'a t -> 'a t
  val intersect : 'a t -> 'a t -> 'a t
  val invariant : 'a t -> bool

  (* extra functions *)
  val singleton : 'a -> 'a t
  val disjoint : 'a t -> 'a t -> bool
  val diff : 'a t -> 'a t -> 'a t
  val elements : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val min_elt : 'a t -> 'a
  val min_elt_opt : 'a t -> 'a option
  val max_elt : 'a t -> 'a
  val max_elt_opt : 'a t -> 'a option
  val choose : 'a t -> 'a
  val choose_opt : 'a t -> 'a option
  val equal : 'a t -> 'a t -> bool
  val compare : 'a t -> 'a t -> int
  val subset : 'a t -> 'a t -> bool
  val map : ('a -> 'b) -> 'a t -> 'b t

  (* newer *)
  val filter : ('a -> bool) -> 'a t -> 'a t
  val exists : ('a -> bool) -> 'a t -> bool
  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  val find : 'a -> 'a t -> 'a
  val find_opt : 'a -> 'a t -> 'a option
  val find_first : ('a -> bool) -> 'a t -> 'a
  val find_first_opt : ('a -> bool) -> 'a t -> 'a option
  val find_last : ('a -> bool) -> 'a t -> 'a
  val find_last_opt : ('a -> bool) -> 'a t -> 'a option

  (* even newer *)
  val of_seq : 'a t -> 'a Seq.t
  val to_seq_from : 'a -> 'a t -> 'a Seq.t
  val to_rev_seq : 'a t -> 'a Seq.t
  val from_seq : 'a Seq.t -> 'a t
  val add_seq : 'a Seq.t -> 'a t -> 'a t
end
