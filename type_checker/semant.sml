type venv = Env.enventry Symbol.table (* value environment *)
type tenv = Types.ty Symbol.table (* type environment *)
type expty = {exp: unit, ty: Types.ty} (* expression type - TODO: exp should later be Translation.exp *)

fun types_equal(t1: ty, t2: ty): bool = case of (t1, t2)
    | (Types.INT, Types.INT) => true
    | (Types.STRING, Types.STRING) => true
    | (Types.NIL, Types.NIL) => true
    | (Types.UNIT, Types.UNIT) => true
    | (Types.RECORD(fields1, _), Types.RECORD(fields2, _)) => let fun compare_fields fields1' fields2' = case of (fields1', fields2')
        | ([], []) => true
        | ((field1, ty1)::fields1'', (field2, ty2)::fields2'') => if field1 = field2 andalso types_equal(ty1, ty2) then compare_fields fields1'' fields2'' else false
        | _ => false
        in
            compare_fields fields1 fields2
        end
    | (Types.ARRAY(ty1, _), Types.ARRAY(ty2, _)) => types_equal(ty1, ty2)
    | (Types.NAME(_, ref1), Types.NAME(_, ref2)) => let val ty1 = !ref1 in let val ty2 = !ref2 in types_equal(ty1, ty2) end end
    | _ => false

fun transProg (e: Absyn.exp): unit = let val {exp=_, ty=_} = transExp(Symbol.base_venv, Symbol.base_tenv, e) in () end
fun transVar (venv, tenv, v: Absyn.var): expty = let fun transVar' v' = case of v'
    | Absyn.SimpleVar(id, pos) => (case of Symbol.look(venv, id)
        | SOME (Env.VarEntry {ty}) => {exp=(), ty=ty}
        | _ => error(pos, "undefined variable " ^ Symbol.name s)
        )
    | Absyn.FieldVar(var, field_id, pos) => let val {exp=_, ty=tyvar} = transVar'(var) in
        case of tyvar
            | Types.RECORD (fields, _) => let fun search remaining_fields = case of remaining_fields
                | [] => error(pos, field_id^" not found in record")
                | (field_id', ty)::remaining_fields' => if field_id = field_id' then {exp=(), ty=ty} else search remaining_fields'
                in
                    search fields
                end
            | NIL => error(pos, "record initialization required for field access")
            | _ => error(pos, "record required for field access")
    | Absyn.SubscriptVar(var, exp, pos) => let val {exp=_, ty=tyvar} = transVar'(var) in
        case of tyvar
            | Types.ARRAY (ty, _) => let val {exp=_, ty=tyexp} = transExp(venv, tenv, exp) in
                case of tyexp
                    | Types.INT => {exp=(), ty=ty}
                    | _ => error(pos, "subscript must be integer")
                end
            | _ => error(pos, "array required for indexing")
        end
    in
        transVar' v
    end
fun transExp (venv, tenv, e: Absyn.exp): expty = let fun transExp' e' = case of e'
    | Absyn.VarExp v => transVar(venv, tenv, v)
    | Absyn.NilExp => {exp=(), ty=Types.NIL}
    | Absyn.IntExp i => {exp=(), ty=Types.INT}
    | Absyn.StringExp(string, pos) => {exp=(), ty=Types.STRING}
    | Absyn.CallExp {func, args, pos} => let val {exp=_, ty=func_type} = case of Symbol.look(venv, func)
        | SOME (Env.FunEntry {formals, result}) => let fun check_args formals' args' = case of (formals, args)
            | ([], []) => {exp=(), ty=result}
            | ([], _) => error(pos, "too many arguments")
            | (_, []) => error(pos, "too few arguments")
            | ((formal, _)::more_formals, (arg, _)::more_args) => if types_equal(formal, arg) then check_args(more_formals, more_args) else error(pos, "type mismatch in argument")
            in 
                check_args formals args
            end
        | _ => error(pos, "undefined function " ^ Symbol.name func)
    | Absyn.OpExp {left, oper, right, pos} => case of oper
          Absyn.PlusOp => let 
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                    | _ => error(pos, "integer required")
            end
        | Absyn.MinusOp => let 
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                    | _ => error(pos, "integer required")
            end
        | Absyn.TimesOp => let
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                    | _ => error(pos, "integer required")
            end
        | Absyn.DivideOp => let 
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                    | _ => error(pos, "integer required")
            end
        | Absyn.EqOp => let
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.bool}
                    | (Types.STRING, Types.STRING) => {exp=(), ty=Types.bool}
                    | (Types.NIL, Types.NIL) => {exp=(), ty=Types.bool}
                    | (Types.Array (_, u1), Types.Array (_, u2)) => if u1 = u2 then {exp=(), ty=Types.bool} else error(pos, "tycon mismatch on = operator")
                    | (Types.RECORD(_, u1), Types.RECORD(_, u2)) => if u1 = u2 then {exp=(), ty=Types.bool} else error(pos, "tycon mismatch on = operator")
                    | _ => error(pos, "integer required")
            end
        | Absyn.NeqOp => let
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.bool}
                    | (Types.STRING, Types.STRING) => {exp=(), ty=Types.bool}
                    | (Types.NIL, Types.NIL) => {exp=(), ty=Types.bool}
                    | (Types.Array (_, u1), Types.Array (_, u2)) => if u1 = u2 then {exp=(), ty=Types.bool} else error(pos, "tycon mismatch on != operator")
                    | (Types.RECORD(_, u1), Types.RECORD(_, u2)) => if u1 = u2 then {exp=(), ty=Types.bool} else error(pos, "tycon mismatch on != operator")
                    | _ => error(pos, "tycon mismatch on != operator")
            end
        | Absyn.LtOp => let
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                    | _ => error(pos, "integer required")
            end
        | Absyn.LeOp => let
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                    | _ => error(pos, "integer required")
            end
        | Absyn.GtOp => let
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                    | _ => error(pos, "integer required")
            end
        | Absyn.GeOp => let 
            val {exp=_, ty=tyleft} = transExp'(left)
            val {exp=_, ty=tyright} = transExp'(right)
            in
                case of (tyleft, tyright)
                    | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                    | _ => error(pos, "integer required")
            end
    | Absyn.RecordExp {provided_fields, typ, pos} => let val record_type = case of Symbol.look(tenv, typ)
        | SOME TypeDec {name, ty, pos} => ty
        | _ => error(pos, "undefined type " ^ Symbol.name typ)
        in
            case of record_types
                | Types.RECORD(record_fields, _) => let fun check_field_match(available_fields, required_fields) = case of (available_fields, required_fields)
                    | ([], []) => {exp=(), ty=record_type}
                    | ((field_id, exp, pos)::more_provided, (field_id', ty)::more_required) => if field_id = field_id' then check_field_match(more_provided, more_required) else check_field_match(more_provided, required_fields)
                    | (x::xs, []) => error(pos, Symbol.name x ^ " not part of " ^ Symbol.name typ ^ " record")
                    | ([], x::xs) => error(pos, Symbol.name x ^ " not provided when making " ^ Symbol.name typ ^ " record")
                | _ => error(pos, "record required")
        end
    | Absyn.SeqExp exps => case exps of
        | [] => {exp=(), ty=Types.UNIT}
        | (exp, pos)::exps' => let val {
            exp=_, ty=tyexp
        } = transExp'(exp) in
            case of exps'
                | [] => {exp=(), ty=tyexp}
                | _ => transExp'(Absyn.SeqExp exps')
        end
    | Absyn.AssignExp {var, exp, pos} => let 
            val {env=_, ty=left_type} = transVar(venv, tenv, var)
            val {env=_, ty=right_type} = transExp(venv, tenv, exp) 
            in case of (left_type, right_type)
                | (Types.INT, Types.INT) => {exp=(), ty=Types.UNIT}
                | (Types.STRING, Types.STRING) => {exp=(), ty=Types.UNIT}
                | (Types.NIL, Types.NIL) => {exp=(), ty=Types.UNIT}
                | (Types.Array (_, u1), Types.Array (_, u2)) => if u1 = u2 then {exp=(), ty=Types.UNIT} else error(pos, "tycon mismatch on assignment")
                | (Types.RECORD(_, u1), Types.RECORD(_, u2)) => if u1 = u2 then {exp=(), ty=Types.UNIT} else error(pos, "tycon mismatch on assignment")
                | _ => error(pos, "type mismatch on assignment") (* TODO: can you assign a record onto a NIL lvalue? *)
        end
    | Absyn.IfExp {test, then', else', pos} => let val {env=_, ty=test_type} in transExp'(test) in
        case of test_type
            | Types.INT => let val {env=_, ty=then_type} = transExp'(then')
                in case of else'
                    | SOME else_exp => let val {env=_, ty=else_type} = transExp'(else_exp) in
                        case of (then_type, else_type)
                            | (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                            | _ => error(pos, "type mismatch on if-then-else")
                        end
                    | NONE => {exp=(), ty=then_type}
                end
            | _ => error(pos, "integer required for conditional test")
        end
    | Absyn.WhileExp {test, body, pos} => let val {env=_, ty=test_type} = transExp'(test) in
        case of test_type
            | Types.INT => let val {env=_, ty=body_type} = transExp'(body) in {exp=(), ty=Types.UNIT} end (* while loop always returns unit, but still type check the interior *)
            | _ => error(pos, "integer required for conditional test")
    | Absyn.ForExp {var, escape, lo, hi, body, pos} => let 
            val {env=_, ty=low_type} = transExp'(lo)
            val {env=_, ty=high_type} = transExp'(hi) in
            case of (loty, hity)
                | (Types.INT, Types.INT) => let {env=_, ty=body_type} = transExp'(body) in {exp=(), ty=Types.UNIT}
                | _ => error(pos, "integer required for for loop bounds")
        end
    | Absyn.BreakExp pos => {exp=(), ty=Types.UNIT}
    | Absyn.LetExp {decs, body, pos} => let val {v=venv', t=tenv'} = transDec(venv, tenv, decs) in transExp(venv', tenv', body) end (* update venv and tenv before semantic analysis of the body *)
    | Absyn.ArrayExp {typ: symbol, size: exp, init: exp, pos} => let 
        val {exp=_, ty=size_type} = transExp'(size)
        val {exp=_, ty=init_type} = transExp'(init)
        val name_type = case of Symbol.look(tenv, typ) (* TODO: I think this might be wrong *)
            | SOME TypeDec {name, ty, pos} => ty
            | _ => error(pos, "undefined type " ^ Symbol.name typ)
        in if types_equal(name_type, init_type) then 
            case of size_type
                | Types.INT => {exp=(), ty=Types.ARRAY(name_type, ref ())}
                | _ => error(pos, "integer required for array size")
        else
            error(pos, "Tried to initialize array with " ^ Symbol.name typ ^ " but got " ^ Symbol.name init_type)
        end
    in
        transExp' e
    end
fun transDec (venv, tenv, d: Absyn.dec): {v:venv, t: tenv} = case of d (* start with beginScope() and end with endScope() *)
    | Absyn.FunctionDec fundecs => raise Fail "Not implemented" (* pg. 119 *)
    | Absyn.VarDec {name, escape, typ, init, pos} => case of typ
        | NONE => let val {exp=_, ty=tyinit} = transExp(venv, tenv, init) in (* TODO: There's somethign about initializing expressions of type NIL, pg 118 *)
            {tenv=tenv, venv=Symbol.enter(tenv, name, Env.VarEntry {ty=tyinit})}
        end
        | SOME (type_id, pos) => raise Fail "Not implemented"
    | Absyn.TypeDec tydecs => raise Fail "Not implemented" (* Textbook literally says "The reader is invited to generalize this", pg. 118 *)
fun transTy (tenv, t: Absyn.ty): Types.ty = case of t
    | Absyn.NameTy(name, pos) => case of Symbol.look(tenv, name)
        | SOME ty => ty
        | _ => error(pos, "undefined type " ^ Symbol.name name)
    | Absyn.RecordTy fields => Types.RECORD(map (fn {name, escape, typ, pos} => (name, transTy(tenv, typ))) fields, ref ())
    | Absyn.ArrayTy(name, pos) => case of Symbol.look(tenv, name)
        | SOME ty => Types.ARRAY(ty, ref ())
        | _ => error(pos, "undefined type " ^ Symbol.name name)
