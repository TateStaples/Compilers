datatype regular_expression = Symbol of char
| Concat of regular_expression * regular_expression
| OR of regular_expression * regular_expression
| Star of regular_expression
| Epsilon

fun regexp_string (Symbol c) = String.str c
| regexp_string (Concat (r1, r2)) = "(" ^ regexp_string r1 ^ regexp_string r2 ^ ")"
| regexp_string (OR (r1, r2)) = "(" ^ regexp_string r1 ^ "|" ^ regexp_string r2 ^ ")"
| regexp_string (Star r) = regexp_string r ^ "*"
| regexp_string Epsilon = "ε"

fun regexp_or (r1, r2) = "("^r1^"|"^r2^")"
fun regexp_concat(r1, r2) = "("^r1^r2^")"
fun regexp_star(r) = r^"*"
fun regexp_epsilon () = "ε"

fun deterministic_finite_automaton (Symbol c) = 
    let
        val start_state = 0
        val final_state = 1
        val transition = fn 0 => fn c' => if c = c' then 1 else ~1
    in
        {start_state = start_state, final_state = final_state, transition = transition}
    end


fun test_regexp_string () = 
    let
        val r1 = Concat(Symbol #"a", Symbol #"b")
        val r2 = OR(Symbol #"a", Symbol #"b")
        val r3 = Star(Symbol #"a")
        val r4 = Epsilon
    in
        print(regexp_string r1 ^ "\n");
        print(regexp_string r2 ^ "\n");
        print(regexp_string r3 ^ "\n");
        print(regexp_string r4 ^ "\n")
    end