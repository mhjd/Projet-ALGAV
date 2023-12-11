"open Cle" 
  
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree


let rec insert x = function
  | Empty -> Node (x, Empty, Empty)
  | Node (y, left, right) ->
      if x <= y
      then Node (x, insert y right, left)
      else Node (y, insert x right, left)
(* =============================================================================================== *)

let rec merge t1 t2 = match t1, t2 with
  | Empty, t | t, Empty -> t
  | Node (x1, left1, right1), Node (x2, left2, right2) ->
      if x1 <= x2
      then Node (x1, merge (Node (x2, left2, right2)) right1, left1)
      else Node (x2, merge (Node (x1, left1, right1)) right2, left2)
(* =============================================================================================== *)

let find_min = function
  | Empty -> failwith "Tas vide"
  | Node (x, _, _) -> x
(* =============================================================================================== *)

let rec find_max = function
  | Empty -> failwith "Tas vide"
  | Node (x, Empty, Empty) -> x
  | Node (x, left, right) -> max x (max (find_max left) (find_max right))
(* =============================================================================================== *)

let rec delete_max x = function
  | Empty -> Empty
  | Node (y, left, right) when y = x -> merge left right
  | Node (y, left, right) ->
      if x <= y
      then Node (y, delete_max x left, right)
      else Node (y, left, delete_max x right)
(* =============================================================================================== *)

let delete_min = function
  | Empty -> Empty
  | Node (_, left, right) as heap ->
      let max_val = find_max heap in
      let heap_without_max = delete_max max_val heap in
      let new_arbre = Node (max_val, left, heap_without_max) in
      descendre new_arbre;
(* =============================================================================================== *)
        
let construction lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (insert x acc) xs
  in
  aux Empty lst
(* =============================================================================================== *)
  
    
let union heap1 heap2 =
  let rec aux heap = function
    | Empty -> heap
    | Node (x, left, right) ->
        let heap = insert x heap in
        let heap = aux heap left in
        aux heap right
  in
  let heap = aux heap1 heap2 in
  merge heap1 heap
(* =============================================================================================== *)
