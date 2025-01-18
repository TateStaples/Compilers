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
  | sumList (x::xs) = x + sumList xs

fun squareList [] = []
  | squareList (x::xs) = x*x :: squareList xs
