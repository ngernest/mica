(* Code adapted from Cornell CS 3110 
 * https://cs3110.github.io/textbook/chapters/ds/hash_tables.html#maps-as-hash-tables
 *)

 module type HashTableInf = sig
  (** [('k, 'v) t] is the type of mutable table-based maps that bind
      keys of type ['k] to values of type ['v]. *)
  type ('k, 'v) t

  (** [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was
      already bound in [m], that binding is replaced by the binding to
      [v]. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> unit

  (** [find k m] is [Some v] if [m] binds [k] to [v], and [None] if [m]
      does not bind [k]. *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] mutates [m] to remove any binding of [k]. If [k] was
      not bound in [m], the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> unit

  (** [create hash c] creates a new table map with capacity [c] that
      will use [hash] as the function to convert keys to integers.
      Requires: The output of [hash] is always non-negative, and [hash]
      runs in constant time. *)
  val create : ('k -> int) -> int -> ('k, 'v) t

  (** [bindings m] is an association list containing the same bindings
      as [m]. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list

  (** [of_list hash lst] creates a map with the same bindings as [lst],
      using [hash] as the hash function. Requires: [lst] does not
      contain any duplicate keys. *)
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t
end

module HashMap : HashTableInf = struct

  (** AF and RI: above *)
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable buckets : ('k * 'v) list array
  }

  (** [capacity tab] is the number of buckets in [tab].
      Efficiency: O(1) *)
  let capacity {buckets} =
    Array.length buckets

  (** [load_factor tab] is the load factor of [tab], i.e., the number of
      bindings divided by the number of buckets. *)
  let load_factor tab =
    float_of_int tab.size /. float_of_int (capacity tab)

  (** Efficiency: O(n) *)
  let create hash n =
    {hash; size = 0; buckets = Array.make n []}

  (** [index k tab] is the index at which key [k] should be stored in the
      buckets of [tab].
      Efficiency: O(1) *)
  let index k tab =
    (tab.hash k) mod (capacity tab)

  (** [insert_no_resize k v tab] inserts a binding from [k] to [v] in [tab]
      and does not resize the table, regardless of what happens to the
      load factor.
      Efficiency: expected O(L) *)
  let insert_no_resize k v tab =
    let b = index k tab in (* O(1) *)
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- (k,v) :: List.remove_assoc k old_bucket; (* O(L) *)
    if not (List.mem_assoc k old_bucket) then
      tab.size <- tab.size + 1;
    ()

  (** [rehash tab new_capacity] replaces the buckets array of [tab] with a new
      array of size [new_capacity], and re-inserts all the bindings of [tab]
      into the new array.  The keys are re-hashed, so the bindings will
      likely land in different buckets.
      Efficiency: O(n), where n is the number of bindings. *)
  let rehash tab new_capacity =
    (* insert [(k, v)] into [tab] *)
    let rehash_binding (k, v) =
      insert_no_resize k v tab
    in
    (* insert all bindings of bucket into [tab] *)
    let rehash_bucket bucket =
      List.iter rehash_binding bucket
    in
    let old_buckets = tab.buckets in
    tab.buckets <- Array.make new_capacity []; (* O(n) *)
    tab.size <- 0;
    (* [rehash_binding] is called by [rehash_bucket] once for every binding *)
    Array.iter rehash_bucket old_buckets (* expected O(n) *)

  (* [resize_if_needed tab] resizes and rehashes [tab] if the load factor
     is too big or too small.  Load factors are allowed to range from
     1/2 to 2. *)
  let resize_if_needed tab =
    let lf = load_factor tab in
    if lf > 2.0 then
      rehash tab (capacity tab * 2)
    else if lf < 0.5 then
      rehash tab (capacity tab / 2)
    else ()

  (** Efficiency: O(n) *)
  let insert k v tab =
    insert_no_resize k v tab; (* O(L) *)
    resize_if_needed tab (* O(n) *)

  (** Efficiency: expected O(L) *)
  let find k tab =
    List.assoc_opt k tab.buckets.(index k tab)

  (** [remove_no_resize k tab] removes [k] from [tab] and does not trigger
      a resize, regardless of what happens to the load factor.
      Efficiency: expected O(L) *)
  let remove_no_resize k tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- List.remove_assoc k tab.buckets.(b);
    if List.mem_assoc k old_bucket then
      tab.size <- tab.size - 1;
    ()

  (** Efficiency: O(n) *)
  let remove k tab =
    remove_no_resize k tab; (* O(L) *)
    resize_if_needed tab (* O(n) *)

  (** Efficiency: O(n) *)
  let bindings tab =
    Array.fold_left
      (fun acc bucket ->
         List.fold_left
           (* 1 cons for every binding, which is O(n) *)
           (fun acc (k,v) -> (k,v) :: acc)
           acc bucket)
      [] tab.buckets

  (** Efficiency: O(n^2) *)
  let of_list hash lst =
    let m = create hash (List.length lst) in  (* O(n) *)
    List.iter (fun (k, v) -> insert k v m) lst; (* n * O(n) is O(n^2) *)
    m
end