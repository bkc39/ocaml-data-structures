module type ORDERED = sig
  type t
  type comparison = L | E | G

  val compare : t * t -> comparison
end

module type TREE = sig
  type t
  type elt

  exception Empty
  exception Not_Found

  val create   : unit    -> t
  val is_empty : t    -> bool

  (* Add/remove *)
  val add   : t -> elt -> t
  val rem   : t -> elt -> t
  val prune : t -> elt -> t
  val graft : t -> elt -> t

  (* Search *)
  val mem             : t -> elt -> t
  val common_ancestor : t -> elt -> elt -> elt

  (* Traversals *)
  val dfs       : t -> elt list
  val bfs       : t -> elt list
  val postorder : t -> elt list

  (* display *)
  val show      : (elt -> string) -> t -> unit
  val to_string : (elt -> string) -> t -> string

  (* Conversion to other collections *)
  (* val to_set    : t -> Set.S *)
  val to_array  : t -> elt array
end

module type Make = functor (M : ORDERED) -> TREE with type elt = M.t
