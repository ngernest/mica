open Ppxlib
open Ast_builder.Default

(** [mk_error ~loc ~f msg] produces an error extension node. 
    Note: [mk_error] is polymorphic over the type of AST node created.
    - [loc] is the desired location of the error extension node
    - [f] is some functino that takes a [Location.t], an [extension]
    and produces the appropriate AST node [e.g. [pexp_extension]]
    - [msg] is the desired error message *)
let mk_error ~(loc : Location.t) ~(f : loc:Location.t -> extension -> 'a) msg :
  'a =
  f ~loc (Location.error_extensionf ~loc msg)

(** [mk_error_expr] is [mk_error] instantiated with the [expression] type, 
    i.e. the extension nodes produced are [expression]s *)
let mk_error_expr ~(loc : Location.t) msg : expression =
  mk_error ~loc ~f:pexp_extension msg

(** [mk_error_pstr ~local ~global msg] creates an error extension node, 
    associated with an element in the AST at the location [local],
    and reports the error message [msg] at the location [global] *)
let mk_error_pstr ~(local : Location.t) ~(global : Location.t)
  (msg : (extension, Format.formatter, unit, extension) format4) :
  structure_item =
  let ext = Location.error_extensionf ~loc:local msg in
  pstr_extension ~loc:global ext []
