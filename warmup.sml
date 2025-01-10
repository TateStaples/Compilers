(* Section 1 *)
fun min(x1, x2) = if x1 < x2 then x1 else x2;

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib(n-1) + fib(n-2);

fun isPrime x = 
  let fun isPrimeHelper x i = 
    if i = 1 then true
    else if i < 1 then false  (* 0 and 1 are not prime *)
    else if x mod i = 0 then false
    else isPrimeHelper x (i-1)
  in
    isPrimeHelper x (x-1)
  end;

fun sumList [] = 0
  | sumList (x::xs) = x + sumList xs;

fun squareList [] = []
  | squareList (x::xs) = x*x :: squareList xs;

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
  | eval (F (eList, f)) = f (map eval eList);

fun flatten lst = foldl (op @) [] lst;

fun myMap f lst = foldr (fn (x, acc) => f x :: acc) [] lst;

fun filter predicate list = foldr (fn (x, acc) => if predicate x then x :: acc else acc) [] list;

fun count preducate list = foldl (fn (x, acc) => if preducate x then acc + 1 else acc) 0 list;

fun mapPartial function list = foldr (fn (x, acc) => case function x of None => acc | Some y => y :: acc) [] list;

(* Section 3 *)
functor F(M: ORD_MAP where type Key.ord_key = string) (S:ORD_SET where type Key.ord_key = string) :>
sig
end
val proc: string list -> S.set M.map
=
struct
...
end