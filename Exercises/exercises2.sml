
(* Section 2 *)
datatype expr = 
  NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| TIMES of expr * expr
| DIV of expr * expr
| F of expr list * (int list -> int)

fun eval (NUM n) = n
  | eval (PLUS (e1, e2)) = eval e1 + eval e2
  | eval (MINUS (e1, e2)) = eval e1 - eval e2
  | eval (TIMES (e1, e2)) = eval e1 * eval e2
  | eval (DIV (e1, e2)) = (eval e1) div (eval e2)
  | eval (F (eList, f)) = f (map eval eList)

fun flatten lst = foldl (op @) [] lst

fun myMap f lst = foldr (fn (x, acc) => f x :: acc) [] lst

fun filter predicate list = foldr (fn (x, acc) => if predicate x then x :: acc else acc) [] list

fun count preducate list = foldl (fn (x, acc) => if preducate x then acc + 1 else acc) 0 list

fun mapPartial function list = foldr (fn (x, acc) => case x of 
    NONE => acc 
    | SOME y => y :: acc) [] list
