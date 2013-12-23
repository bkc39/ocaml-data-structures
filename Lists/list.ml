(**
 * An assortment of purely functional List implementations
 * in OCaml. Designed to be equivalent to the build in OCaml
 * List module.
 *
 * Documentation can be found at:
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *
 * Version: 2
 * Modified: 12/20/2013
 * Author: Ben Carriel
 *
 **)


(**
 *
 * My implementation of the standard list functions. Uses
 * Continuation Passing Style so that stack overflow concerns
 * for large lists are eliminated.
 *
 **)

module ContList = struct
  type 'a t = 'a list

  let rec length lst k = match lst with
    | []    -> k 0
    | x::xs -> length xs (fun n -> k (n+1))

  let hd lst k = match lst with
    | []    -> k None
    | x::xs -> k (Some x)

  let tl lst k = match lst with
    | []    -> k None
    | x::xs -> k (Some xs)

  let rec nth lst n k = match lst with
    | []    -> k None
    | x::xs -> if n=0 then k (Some x) else nth xs (n-1) (fun v -> k v)

  let rec append lst lst' k = match lst with
    | []    -> k lst'
    | x::xs -> append xs lst' (fun ys -> k (x::ys))

  let rec flatten xss k = match xss with
    | []      -> k []
    | xs::xtt -> flatten xtt (fun xt -> append xs xt (fun a -> k a))

  let rec iter f lst k = match lst with
    | []    -> k ()
    | x::xs -> iter f xs (fun () -> k (f x))

  let iteri f lst k =
    let rec iteri' f n lst k = match lst with
      | []    -> k ()
      | x::xs -> iteri' f (n-1) xs (fun () -> k (f n x)) in
    length lst (fun n -> iteri' f n lst k)

  let rec map f lst k = match lst with
    | []    -> k []
    | x::xs -> map f xs (fun ys -> k ((f x)::ys))

  let mapi f lst k =
    let rec mapi' f n lst k = match lst with
      | []    -> k []
      | x::xs -> mapi' f (n-1) xs (fun ys -> k ((f n x)::ys)) in
    length lst (fun n -> mapi' f n lst k)

  let rec fold_left f a lst k = match lst with
    | []    -> k a
    | x::xs -> fold_left f a xs (fun acc -> k (f acc x))

  let fold_right f xs a =
    fold_left
      (fun g x c -> g (fun v -> c (f x v))) (fun x -> x a) xs (fun x -> x)

  let rev xs       = fold_left (fun a x -> x::a) [] xs
  let rev_map f xs = fold_left (fun a x -> (f x)::a) [] xs

  let rev_append xs ys k = rev xs (fun sx -> append sx ys (fun zs -> k zs))

  let rec iter2 f xs ys k = match xs, ys with
    | [], []       -> k ()
    | x::xs, y::ys -> iter2 f xs ys (fun () -> k (f x y))
    | _            -> begin
      raise (Invalid_argument "iter2: Lists must be of equal length")
    end

  let rec map2 f xs ys k = match xs, ys with
    | [], []       -> k []
    | x::xs, y::ys -> map2 f xs ys (fun zs -> k ((f x y)::zs))
    | _            -> begin
      raise (Invalid_argument "map2: Lists must be of equal length")
    end

  let rev_map2 f xs ys k = map2 f xs ys (fun zs -> rev zs (fun rs -> k rs))

  let rec fold_left2 f a xs ys k = match xs, ys with
    | [], []       -> k a
    | x::xs, y::ys -> fold_left2 f a xs ys (fun a -> f a x y)
    | _            -> begin
      raise (Invalid_argument "fold_left2: Lists must be of equal length")
    end

  let fold_right2 f xs ys a =
    fold_left2 (fun g x y c -> g (fun v -> c (f x y a)))
      (fun k -> k a) xs ys (fun x -> x)

  let rec for_all p xs k = match xs with
    | []    -> k true
    | x::xs -> for_all p xs (fun b -> k (b && (p x)))

  let rec exists p xs k = match xs with
    | []    -> k false
    | x::xs -> exists p xs (fun b -> k (b || (p x)))

  let rec for_all2 p xs ys k = match xs, ys with
    | [], []       -> k true
    | x::xs, y::ys -> for_all2 p xs ys (fun b -> k (b && (p x y)))
    | _            -> begin
      raise (Invalid_argument "for_all2: Lists must be of equal length")
    end

  let rec exists2 p xs ys k = match xs, ys with
    | [], []       -> k false
    | x::xs, y::ys -> exists2 p xs ys (fun b -> k (b || (p x y)))
    | _            -> begin
      raise (Invalid_argument "for_all2: Lists must be of equal length")
    end

  let rec mem y xs k = match xs with
    | []    -> k false
    | x::xs -> if x=y then k true else mem y xs (fun b -> k b)

  let rec memq y xs k = match xs with
    | []    -> k false
    | x::xs -> if x==y then k true else memq y xs (fun b -> k b)

  let rec find p xs k = match xs with
    | []    -> raise Not_found
    | x::xs -> if p x then k x else find p xs (fun y -> k y)

  let rec filter p xs k = match xs with
    | []    -> k []
    | x::xs -> filter p xs (fun ps -> k (if p x then x::ps else ps))

  let find_all = filter

  let rec partition p xs k = match xs with
    | []    -> k ([], [])
    | x::xs -> begin
      partition p xs (fun (ts, fs) ->
        k (if p x then (x::ts,fs) else (ts,x::fs)))
    end

  let rec assoc x ps k = match ps with
    | []        -> raise Not_found
    | (a,b)::ps -> if x=a then k b else assoc x ps (fun b -> k b)

  let rec assq x ps k = match ps with
    | []        -> raise Not_found
    | (a,b)::ps -> if x==a then k b else assq x ps (fun b -> k b)

  let rec mem_assoc x xs k = match xs with
    | []        -> k false
    | (y,_)::xs -> if x=y then k true else mem_assoc x xs (fun b -> k b)

  let rec mem_assq x xs k = match xs with
    | []        -> k false
    | (y,_)::xs -> if x==y then k true else mem_assq x xs (fun b -> k b)

  let rec remove_assoc x xs k = match xs with
    | []        -> k []
    | (y,_)::xs -> if x=y then k xs else remove_assoc x xs (fun ys -> k ys)

  let rec remove_assq x xs k = match xs with
    | []        -> k []
    | (y,_)::xs -> if x==y then k xs else remove_assq x xs (fun ys -> k ys)

  let rec split ps k = match ps with
    | []        -> k ([], [])
    | (x,y)::ps -> split ps (fun (xs,ys) -> k (x::xs, y::ys))

  let rec combine xs ys k = match xs, ys with
    | [], []       -> k []
    | x::xs, y::ys -> combine xs ys (fun ps -> k ((x,y)::ps))
    | _            -> begin
      raise (Invalid_argument "combine: Lists must be equal length")
    end

  let rec merge f xs ys k = match xs, ys with
    | _, []          -> k xs
    | [], _          -> k ys
    | x::xs, y::ys   -> begin
      merge f xs ys (fun zs -> k (if (f x y) > 0 then x::y::zs else y::x::zs))
    end

  (* Randomized Quicksort *)
  let rec sort f xs k = match xs with
    | [] -> k []
    | _  -> begin
      length xs (fun l -> nth xs (Random.int l) (fun x ->
        match x with
        | None   -> failwith "stable_sort: Out of bounds"
        | Some x -> begin
          partition (fun y -> (f x y) > 0) xs (fun (lt,gt) ->
            sort f lt (fun slt -> sort f gt (fun sgt ->
              append lt [x] (fun st -> append st gt (fun rs -> k rs)))))
        end))
    end

  (* Quicksort *)
  let rec stable_sort f xs k = match xs with
    | [] -> k []
    | x::xs -> begin
      partition (fun y -> (f x y) > 0) xs (fun (lt,gt) ->
        stable_sort f lt (fun slt -> stable_sort f gt (fun sgt ->
          append lt [x] (fun st -> append st gt (fun rs -> k rs)))))
    end

  (* Mergesort *)
  let rec fast_sort (f : 'a -> 'a -> int) (xs : 'a list) k =
    let rec split n xs k =
      if n=0 then hd xs (fun h -> tl xs (fun t ->
        match h, t with
        | Some x, Some t -> k ([x], t)
        | _              -> failwith "split: Out of bounds"))
      else
        match xs with
        | []     -> failwith "split: Out of bounds"
        | x::xs  -> split (n-1) xs (fun (hd,tl) -> k (x::hd,tl)) in
    length xs (fun l -> split (l/2) xs (fun (h,t) ->
      fast_sort f h (fun shd -> fast_sort f t (fun stl ->
        merge f shd stl (fun st -> k st)))))
end
