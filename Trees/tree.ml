(*
 * This is the signature file for the module type Tree. Documentation
 * for this module can be found on GitHub at TODO: fill in documentation
 *
 * @date 06/2013
 * @author bkc
 * @contact bkcmisc@gmail.com
 *)

module Tree = sig
  module type ORDERED = sig
    type t
    type comparison = L | E | G

    val compare : t * t -> comparison
  end

  module type T = sig
    type t
    type elt

    exception Empty
    exception Not_Found
    exception Function_Error of string

    val create   : unit -> t
    val is_empty : t    -> bool
    val height   : t    -> int

    (* Add/remove *)
    val add   : t -> elt -> t
    val rem   : t -> elt -> t
    val prune : t -> t   -> t
    val graft : t -> t   -> t

    (* Search *)
    val mem             : t -> elt -> bool
    val common_ancestor : t -> elt -> elt -> elt

    (* Traversals *)
    val dfs       : t -> elt list
    val bfs       : t -> elt list
    val postorder : t -> elt list

    (* display *)
    val show      : t -> unit
    val to_string : t -> string

    (* Conversion to other collections *)
    val to_array  : t -> elt array
  end

  module type Make = functor (M : ORDERED) -> T with type elt = M.t
end
