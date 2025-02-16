structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	  | NAME of Symbol.symbol * ty option ref
	  | UNIT
 
  fun toString (RECORD((s,t)::l, u)) = (Symbol.name s) ^ "=>" ^ (toString t) ^ " " ^ (toString (RECORD(l, u)))
    | toString (RECORD([], u)) = ""
    | toString (NIL) = "NIL"
    | toString (INT) = "INT"
    | toString (STRING) = "STRING"
    | toString (ARRAY(t, u)) = "ARRAY OF " ^ (toString t) ^ ""
    | toString (NAME(s, t)) = "NAME(" ^ (Symbol.name s) ^ "," ^ (case !t of NONE => "NONE" | SOME v => "...") ^ ")"
    | toString (UNIT) = "UNIT"
 
  fun tyEq(NIL, RECORD _) = true
    | tyEq(RECORD _, NIL) = true
    | tyEq(NIL, ARRAY _ )= true
    | tyEq(NIL, NIL) = true
    | tyEq(ARRAY _, NIL) = true
    | tyEq (RECORD(_, u1), RECORD(_, u2)) = (u1 = u2)
    | tyEq (INT, INT) = true
    | tyEq (STRING, STRING) = true
    | tyEq (ARRAY(_, u1), ARRAY(_, u2)) = (u1 = u2)
    | tyEq (NAME(s1, t1), NAME(s2, t2)) = (case (!t1, !t2) of
        (SOME ty1, SOME ty2) => tyEq(ty1, ty2)
      | (SOME ty1, NONE) => false
      | (NONE, SOME ty2) => false
      | (NONE, NONE) => (Symbol.name s1) = (Symbol.name s2))
    | tyEq (UNIT, UNIT) = true
    | tyEq _ = false 

end

