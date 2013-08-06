(*
 * Title says it all: Red Black Trees.
 *
 * TODO: finish up the remove and show operations
 *
 * @date 06/2013
 * @author bkc
 * @contact bkcmisc@gmail.com
 *)

module RedBlackTree : Tree.Make = functor (M : Tree.ORDERED) -> struct
  type color = R | B
  type elt = M.t
  type t   = N of color * elt * t * t | L

  exception Empty
  exception Not_Found
  exception Function_Error of string

(********************************* Helpers ************************************)

  let balance = function
    | N(B,u,N(R,v,N(R,w,a,b),c),d)
    | N(B,u,N(R,w,a,N(R,v,b,c)),d)
    | N(B,w,a,N(R,u,N(R,v,b,c),d))
    | N(B,w,a,N(R,v,b,N(R,u,c,d))) -> N(R,v,N(B,w,a,b),N(B,u,c,d))
    | other                        -> other

  let insert t x = match t with
    | L                -> N(B,x,L,L)
    | N(c,y,l,r) as t' -> begin
      match M.compare (x,y) with
      | L -> balance N(c, y, insert l x, r)
      | E -> t'
      | G -> balance N(c, y, l, insert r x)
    end

  let rec dfs' s acc =
    if Stack.is_empty s then acc
    else match Stack.pop s with
    | L          -> dfs' s acc
    | N(_,v,l,r) -> begin
      Stack.push l s;
      Stack.push r s;
      dfs' s (v::acc)
    end

  let rec bfs' q acc =
    if Queue.is_empty q then acc
    else match Queue.pop q with
    | L           -> bfs' q acc
    | N(_,v,l,r) -> begin
      Queue.add l q;
      Queue.add r q;
      bfs' q (v::acc)
    end

  let fold f g a = function
    | N(_,v,l,r) -> f (f (g a v) l) r
    | L          -> a

  let rec post acc = function
    | N(_,v,l,r) -> v::(post (post acc l) r)
    | L          -> acc

(****************************** For Printing **********************************)

  let rec take lst n = match lst with
    | []    -> if n > 0 then raise (Function_Error "to_string: index") else []
    | x::xs -> x::(take xs (n-1))

  let rec drop lst n = if n=0 then lst else drop (List.tl lst) (n-1)

  let rec reverse str =
    if String.length str = 0 then ""
    else begin
      let len = String.length str in
      let last = Char.escaped (String.get str (len-1)) in
      let rest = reverse (String.sub str 1 (len-1))
      last^rest
    end

  let rec print_help lst n =
    if List.length < n then raise (Function_Error "to_string: internal error")
    else (take lst n)::(print_help (drop lst n) (2*n))

(******************************************************************************)

  let create () = L
  let is_empty  = function L -> true | _ -> false

  let rec height = function
    | L          -> 0
    | N(_,v,l,r) -> max (height l) (height r) + 1

  let rec mem t x = match t with
    | N(_,v,l,r) -> begin
      match M.compare (x,v) with
      | L -> mem l x
      | E -> true
      | G -> mem r x
    end
    | L -> false

  let add t x = match insert t x with
     | N(_,v,l,r) -> N(B,v,l,r)
    | L          ->
      raise (Function_Error "add: element insertion returned empty tree")

  let rem t x = match t with
    | L -> raise Not_Found
    | N -> begin
      failwith "TODO"
    end

  let rec prune t t' = fold prune rem t t'

  let rec graft t = fold graft add t t'

  let rec common_ancestor t x y = match t with
    | N(_,v,l,r) as t' -> begin
      match M.compare (x,v), M.compare (y,v) with
      | L,L -> common_ancestor l x y
      | G,G -> common_ancestor r x y
      | _   -> t'
    end
    | L -> raise Not_Found

  let dfs t = let s = Stack.create () in dfs' (Stack.push t s; s) []

  let bfs t = let q = Queue.create () in bfs' (Queue.add t q; q) []

  let postorder t = post [] t

  let to_string print t =
    let s = List.fold_left (fun a x -> x^"\n"^a) "" (List.map print (bfs t)) in
    reverse s

  let show f t = print_endline (to_string f t)

  let to_array t = Array.of_list (bfs t)
end
