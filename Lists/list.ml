(*
 * An assortment of purely functional List implementations
 * in OCaml. Designed to be equivalent to the build in OCaml
 * List module.
 *
 * Documentation can be found at:
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *
 * Version: 1
 * Modified: 2/27/2013
 * Author: Ben Carriel
 *)


module type LIST =
sig
  type 'a t

  val length : 'a t -> int
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val nth : 'a t -> int -> 'a
  val append : 'a t -> 'a t -> 'a t
  val rev_append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val rev_map : ('a -> 'b) -> 'a t -> 'b t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val rev_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a t -> bool
  val exists : ('a -> bool) -> 'a t -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val mem : 'a -> 'a t -> bool
  val memq : 'a -> 'a t -> bool
  val find : ('a -> bool) -> 'a t -> 'a
  val filter : ('a -> bool) -> 'a t -> 'a t
  val find_all : ('a -> bool) -> 'a t -> 'a t
  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  val assoc : 'a -> ('a * 'b) t -> 'b
  val assq : 'a -> ('a * 'b) t -> 'b
  val mem_assoc : 'a -> ('a * 'b) t -> bool
  val mem_assq : 'a -> ('a * 'b) t -> bool
  val remove_assoc : 'a -> ('a * 'b) t -> ('a * 'b) t
  val remove_assq : 'a -> ('a * 'b) t -> ('a * 'b) t
  val split : ('a * 'b) t -> 'a t * 'b t
  val combine : 'a t -> 'b t -> ('a * 'b) t
  val sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val fast_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
end

(*
 * My implementation of the standard list functions. Uses
 * Continuation Passing Style so that stack overflow concerns
 * for large lists are eliminated.
 *
 * The workhorse of this implementation is the fold_left function.
 *
 *)

module BKCList : LIST =
struct
  type 'a t = 'a list

  let fold_left f acc lst =
    let rec foldl lst k =
      match lst with
      | [] -> k acc
      | h::t -> foldl t (fun x -> f (k x) h) in
    foldl lst (fun x -> x)

  let length lst = fold_left (fun acc x -> acc+1) 0 lst

  let hd = function
    | [] -> failwith "EmptyList"
    | h::_ -> h

  let tl = function
    | [] -> []
    | _::t -> t

  let nth xs n =
    let rec nth' n k =
      if n = 0 then k xs else nth (n-1) (fun x -> k (List.hd x)) in
    
  let append = failwith "not implemented"
  let rev_append = failwith "not implemented"
  let concat = failwith "not implemented"
  let iter = failwith "not implemented"
  let iteri = failwith "not implemented"
  let map = failwith "not implemented"
  let mapi = failwith "not implemented"
  let rev_map = failwith "not implemented"
  let fold_right = failwith "not implemented"
  let iter2 = failwith "not implemented"
  let map2 = failwith "not implemented"
  let rev_map2 = failwith "not implemented"
  let fold_left2 = failwith "not implemented"
  let fold_right2 = failwith "not implemented"
  let for_all = failwith "not implemented"
  let exists = failwith "not implemented"
  let for_all2 = failwith "not implemented"
  let exists2 = failwith "not implemented"
  let mem = failwith "not implemented"
  let memq = failwith "not implemented"
  let find = failwith "not implemented"
  let filter = failwith "not implemented"
  let find_all = failwith "not implemented"
  let partition = failwith "not implemented"
  let assoc = failwith "not implemented"
  let assq = failwith "not implemented"
  let mem_assoc = failwith "not implemented"
  let mem_assq = failwith "not implemented"
  let remove_assoc = failwith "not implemented"
  let remove_assq = failwith "not implemented"
  let split = failwith "not implemented"
  let combine = failwith "not implemented"
  let sort = failwith "not implemented"
  let stable_sort = failwith "not implemented"
  let fast_sort = failwith "not implemented"
  let merge = failwith "not implemented"
end
