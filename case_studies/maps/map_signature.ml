module type S = sig
  (** The abstract type of maps. 
    For now, we require keys to be [int] and values to be [string]s. *)
  type t

  (** [empty] is the empty map *)
  val empty : t

  (** [insert (k, v) m] produces a new map that is the same as [m], but with an 
    additional binding from [k] to [v]. 
    If [k] was already bound in [m], that binding is replaced
    by the binding to [v] in the new map. *)
  val insert : int * string -> t -> t

  (** [find k m] is [Some v] if [k] is bound to [v] in [m], 
    and [None] if not. *)
  val find : int -> t -> string option

  (** [remove k m] produces a new map that is the same as [m], 
    but with {i all} the bindings for [k] removed. 
    If [k] was not bound in [m], then the map is unchanged. *)
  val remove : int -> t -> t

  (** [from_list lst] is a map containing the same bindings as the 
    association list [lst]. 
    Requirement: [lst] does not contain any duplicate keys. *)
  val from_list : AssocList.t -> t

  (** [bindings m] is an association list containing the same bindings 
    as [m]. There are no duplicate keys in the list. *)
  val bindings : t -> AssocList.t
end
