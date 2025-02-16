structure Semant = 
struct
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    type expty = {exp: unit, ty: Types.ty}

    fun error (pos, msg) = ErrorMsg.error pos msg

    fun transTypeField(tenv, symbol, pos): Types.ty = case Symbol.look(tenv, symbol) of
        SOME ty => ty
        | _ => (error(pos, "undefined type " ^ Symbol.name symbol); Types.UNIT)

    fun checkInt(ty: Types.ty, pos): unit = case ty of
        Types.INT => ()
        | _ => (error(pos, "integer required"); ())

    fun types_equal(t1: Types.ty, t2: Types.ty): bool = case (t1, t2) of
        (Types.INT, Types.INT) => true
        | (Types.STRING, Types.STRING) => true
        | (Types.NIL, Types.NIL) => true
        | (Types.UNIT, Types.UNIT) => true
        | (Types.RECORD(fields1, _), Types.RECORD(fields2, _)) => let fun compare_fields fields1' fields2' = case (fields1', fields2') of
            ([], []) => true
            | ((field1, ty1)::fields1'', (field2, ty2)::fields2'') => if field1 = field2 andalso types_equal(ty1, ty2) then compare_fields fields1'' fields2'' else false
            | _ => false
            in
                compare_fields fields1 fields2
            end
        | (Types.ARRAY(ty1, _), Types.ARRAY(ty2, _)) => types_equal(ty1, ty2)
        | (Types.NAME(_, u1), Types.NAME(_, u2)) => types_equal(u1(), u2()) (* This might not terminate *)
        | _ => false

    and transProg (e: Absyn.exp): unit = let val {exp=_, ty=_} = transExp(Env.base_venv, Env.base_tenv, e) in () end (* TODO: these constructors are wrong *)
    and transVar (venv, tenv, v: Absyn.var): expty = case v of
        Absyn.SimpleVar(id, pos) => (case Symbol.look(venv, id) of
            SOME (Env.VarEntry {ty}) => {exp=(), ty=ty}
            | _ => (error(pos, "undefined variable " ^ Symbol.name id); {exp=(), ty=Types.UNIT})
            )
        | Absyn.FieldVar(var, field_id, pos) => let val {exp=_, ty=tyvar} = transVar(venv, tenv, var) in
            case tyvar of
                Types.RECORD (fields, _) => let fun search remaining_fields = case remaining_fields of
                    [] => (error(pos, Symbol.name field_id^" not found in record"); {exp=(), ty=Types.UNIT})
                    | (field_id', ty)::remaining_fields' => if field_id = field_id' then {exp=(), ty=ty} else search remaining_fields'
                    in
                        search fields
                    end
                | NIL => (error(pos, "record initialization required for field access"); {exp=(), ty=Types.UNIT})
                | _ => (error(pos, "record required for field access"); {exp=(), ty=Types.UNIT})
            end
        | Absyn.SubscriptVar(var, exp, pos) => let val {exp=_, ty=tyvar} = transVar(venv, tenv, var) in
            case tyvar of
                Types.ARRAY (ty, _) => let val {exp=_, ty=tyexp} = transExp(venv, tenv, exp) 
                    in 
                        {exp=(), ty=ty}
                    end
                | _ => (error(pos, "array required for indexing"); {exp=(), ty=Types.UNIT})
            end

    and transExp (venv, tenv, e: Absyn.exp): expty = case e of
        Absyn.VarExp v => transVar(venv, tenv, v)
        | Absyn.NilExp => {exp=(), ty=Types.NIL}
        | Absyn.IntExp i => {exp=(), ty=Types.INT}
        | Absyn.StringExp(string, pos) => {exp=(), ty=Types.STRING}
        | Absyn.CallExp {func, args, pos} => 
            (case Symbol.look(venv, func) of
                SOME (Env.FunEntry {formals, result}) => 
                    let fun check_args [] [] = {exp=(), ty=result}
                        | check_args [] _ = (error(pos, "too many arguments"); {exp=(), ty=result})
                        | check_args _ [] = (error(pos, "too few arguments"); {exp=(), ty=result})
                        | check_args (formal::rest_formals) (arg::rest_args) =
                            let val {exp=_, ty=arg_ty} = transExp(venv, tenv, arg)
                            in if types_equal(formal, arg_ty)
                               then check_args rest_formals rest_args
                               else (error(pos, "type mismatch in argument"); {exp=(), ty=result})
                            end
                    in 
                        check_args formals args;
                        {exp=(), ty=result}
                    end
                | _ => (error(pos, "undefined function " ^ Symbol.name func); {exp=(), ty=Types.UNIT}))
        | Absyn.OpExp {left, oper, right, pos} => case oper of
            Absyn.PlusOp => let 
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
            | Absyn.MinusOp => let 
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
            | Absyn.TimesOp => let
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
            | Absyn.DivideOp => let 
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
            | Absyn.EqOp => let
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of (* might replace with types_equal *)
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | (Types.STRING, Types.STRING) => {exp=(), ty=Types.INT}
                        | (Types.NIL, Types.NIL) => {exp=(), ty=Types.INT}
                        | (Types.ARRAY (_, u1), Types.ARRAY (_, u2)) => if u1 = u2 then {exp=(), ty=Types.INT} else (error(pos, "tycon mismatch on = operator"); {exp=(), ty=Types.INT})
                        | (Types.RECORD(_, u1), Types.RECORD(_, u2)) => if u1 = u2 then {exp=(), ty=Types.INT} else (error(pos, "tycon mismatch on = operator"); {exp=(), ty=Types.INT})
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
            | Absyn.NeqOp => let
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | (Types.STRING, Types.STRING) => {exp=(), ty=Types.INT}
                        | (Types.NIL, Types.NIL) => {exp=(), ty=Types.INT}
                        | (Types.ARRAY (_, u1), Types.ARRAY (_, u2)) => if u1 = u2 then {exp=(), ty=Types.INT} else (error(pos, "tycon mismatch on != operator"); {exp=(), ty=Types.INT})
                        | (Types.RECORD(_, u1), Types.RECORD(_, u2)) => if u1 = u2 then {exp=(), ty=Types.INT} else (error(pos, "tycon mismatch on != operator"); {exp=(), ty=Types.INT})
                        | _ => (error(pos, "tycon mismatch on != operator"); {exp=(), ty=Types.INT})
                end
            | Absyn.LtOp => let
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
            | Absyn.LeOp => let
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
            | Absyn.GtOp => let
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
            | Absyn.GeOp => let 
                val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
                val {exp=_, ty=tyright} = transExp(venv, tenv, right)
                in
                    case (tyleft, tyright) of
                        (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                        | _ => (error(pos, "integer required"); {exp=(), ty=Types.INT})
                end
        | Absyn.RecordExp {fields=provided_fields, typ=typ, pos=pos} => case transTypeField(tenv, typ, pos) of
            Types.RECORD(record_fields, u) => let fun check_field_match(available_fields: (Absyn.symbol * Absyn.exp * Absyn.pos) list, required_fields: (Symbol.symbol * Types.ty) list) = case (available_fields, required_fields) of
                ([], []) => {exp=(), ty=Types.RECORD(record_fields, u)}
                | ((field_id, exp, pos)::more_provided, (field_id', ty)::more_required) => if field_id = field_id' then check_field_match(more_provided, more_required) else check_field_match(more_provided, required_fields)
                | ((field_id, exp, pos)::xs, []) => (error(pos, Symbol.name field_id ^ " not part of " ^ Symbol.name typ ^ " record"); {exp=(), ty=Types.RECORD(record_fields, u)})
                | ([], (field_id, _)::xs) => (error(pos, Symbol.name field_id ^ " not provided when making " ^ Symbol.name typ ^ " record"); {exp=(), ty=Types.RECORD(record_fields, u)})
                in
                    check_field_match(provided_fields, record_fields)
                end
            | _ => (error(pos, "record type required for record initialization. Got " ^ Symbol.name typ ^ " instead"); {exp=(), ty=Types.UNIT})
        | Absyn.SeqExp exps => case exps of
            [] => {exp=(), ty=Types.UNIT}
            | (exp, pos)::exps' => let val {exp=_, ty=tyexp} = transExp(venv, tenv, exp) in
                case exps' of
                    [] => {exp=(), ty=tyexp}
                    | _ => transExp(venv, tenv, Absyn.SeqExp exps')
                end
        | Absyn.AssignExp {var, exp, pos} => let 
                val {exp=_, ty=left_type} = transVar(venv, tenv, var)
                val {exp=_, ty=right_type} = transExp(venv, tenv, exp) 
                in case (left_type, right_type) of
                    (Types.INT, Types.INT) => {exp=(), ty=Types.UNIT}
                    | (Types.STRING, Types.STRING) => {exp=(), ty=Types.UNIT}
                    | (Types.NIL, Types.NIL) => {exp=(), ty=Types.UNIT}
                    | (Types.ARRAY (_, u1), Types.ARRAY (_, u2)) => if u1 = u2 then {exp=(), ty=Types.UNIT} else (error(pos, "tycon mismatch on assignment"); {exp=(), ty=Types.UNIT})
                    | (Types.RECORD(_, u1), Types.RECORD(_, u2)) => if u1 = u2 then {exp=(), ty=Types.UNIT} else (error(pos, "tycon mismatch on assignment"); {exp=(), ty=Types.UNIT})
                    | _ => (error(pos, "type mismatch on assignment"); {exp=(), ty=Types.UNIT}) (* TODO: can you assign a record onto a NIL lvalue? *)
                end
        | Absyn.IfExp {test, then', else', pos} => let val {env=_, ty=test_type} = transExp(venv, tenv, test) (* TODO: this can be cleaned up for nicer messages *)
            in
                case test_type of
                    Types.INT => let val {env=_, ty=then_type} = transExp(venv, tenv, then')
                        in case else' of
                            SOME else_exp => let val {env=_, ty=else_type} = transExp(venv, tenv, else_exp) in
                                case (then_type, else_type) of
                                    (Types.INT, Types.INT) => {exp=(), ty=Types.INT}
                                    | _ => (error(pos, "type mismatch on if-then-else"); {exp=(), ty=then_type})
                                end
                            | NONE => {exp=(), ty=then_type}
                        end
                    | _ => (error(pos, "integer required for conditional test"); {exp=(), ty=Types.UNIT})
            end
        | Absyn.WhileExp {test, body, pos} => let val {env=_, ty=test_type} = transExp(venv, tenv, test) in
            case test_type of
                Types.INT => let val {env=_, ty=body_type} = transExp(venv, tenv, body) in {exp=(), ty=Types.UNIT} end (* while loop always returns unit, but still type check the interior *)
                | _ => (error(pos, "integer required for conditional test"); {exp=(), ty=Types.UNIT})
            end
        | Absyn.ForExp {var, escape, lo, hi, body, pos} => let 
                val {env=_, ty=low_type} = transExp(venv, tenv, lo)
                val {env=_, ty=high_type} = transExp(venv, tenv, hi) 
            in
                case (low_type, high_type) of
                    (Types.INT, Types.INT) => let val {env=_, ty=body_type} = transExp(venv, tenv, body) in {exp=(), ty=Types.UNIT} end
                    | _ => (error(pos, "integer required for for loop bounds"); {exp=(), ty=Types.UNIT})
            end
        | Absyn.BreakExp pos => {exp=(), ty=Types.UNIT}
        | Absyn.LetExp {decs, body, pos} => let val {v=venv', t=tenv'} = transDec(venv, tenv, decs) in transExp(venv', tenv', body) end (* update venv and tenv before semantic analysis of the body *)
        | Absyn.ArrayExp {typ, size: Absyn.exp, init: Absyn.exp, pos} => let 
            val {exp=_, ty=size_type} = transExp(venv, tenv, size)
            val {exp=_, ty=init_type} = transExp(venv, tenv, init)
            val name_type = case Symbol.look(tenv, typ) of (* TODO: I think this might be wrong *)
                SOME(Types.ARRAY(ty, _)) => ty
                | _ => (error(pos, "undefined type " ^ Symbol.name typ); Types.UNIT)
            in 
                if types_equal(name_type, init_type) then 
                    case size_type of
                        Types.INT => {exp=(), ty=Types.ARRAY(name_type, ref ())}
                        | _ => (error(pos, "integer required for array size"); {exp=(), ty=name_type})
                else
                    (error(pos, "Tried to initialize array with " ^ Symbol.name typ ^ " but got " ^ Symbol.name init_type); {exp=(), ty=name_type})
            end

    and transDec (venv, tenv, d: Absyn.dec): {tenv:tenv, venv:venv} = case d of (* TODO:start with beginScope() and end with endScope() -> not if doing the functional version *)
        Absyn.FunctionDec fundecs => let 
            fun transFuncHeaders fundecs = case fundecs of
                [] => []
                | {name, params, result, body, pos}::more_fundecs => 
                    let 
                        val param_types = map (fn {name, escape, typ, typ_pos} => 
                            transTypeField(tenv, typ, typ_pos)
                        ) params 
                    in
                        case result of
                            SOME (result_type, result_pos) => Env.FunEntry {formal=param_types, result=transTypeField(tenv, result_type, result_pos)}::transFuncHeaders more_fundecs
                            | NONE => Env.FunEntry {formal=param_types, result=Types.UNIT}::transFuncHeaders more_fundecs
                    end
            and updateTypeEnv(fun_entries) = case fun_entries of
                [] => tenv
                | {name, params, result}::more_entries => (Symbol.enter(tenv, name, Types.NAME(name, ref ())); updateTypeEnv(more_entries)) (* TODO: I think this is wrong *)
            and typeCheckFuncBodies(body_tenv, fundecs) = case fundecs of
                [] => []
                | {name, params, result, body, pos}::more_fundecs => (transExp(venv, body_tenv, body); typeCheckFuncBodies(body_tenv, more_fundecs))
            and new_tenv: tenv = updateTypeEnv(transFuncHeaders fundecs)
            in
                typeCheckFuncBodies(new_tenv, fundecs);
                {venv=venv, tenv=new_tenv}
            end
        | Absyn.VarDec {name, escape, typ, init, pos} => let val {exp=_, ty=init_type} = transExp(venv, tenv, init) in
            case typ of
            NONE => {venv=Symbol.enter(venv, name, Env.VarEntry {ty=init_type}), tenv=tenv}
            | SOME (type_id, type_pos) => if types_equal(init_type, transTypeField(tenv, type_id, type_pos)) 
                then {venv=Symbol.enter(venv, name, Env.VarEntry {ty=init_type}), tenv=tenv}
                else (error(pos, "type mismatch in variable declaration"); {venv=venv, tenv=tenv})
            end 
        | Absyn.TypeDec tydecs => case tydecs of
            [] => {venv=venv, tenv=tenv}
            | {name, ty, pos}::more_tydecs => let val new_tenv = Symbol.enter(tenv, name, transTy(tenv, ty)) in transDec(venv, new_tenv, Absyn.TypeDec more_tydecs) end
    and transTy (tenv, t: Absyn.ty): Types.ty = case t of
        Absyn.NameTy(name, pos) => transTypeField(tenv, name, pos)
        | Absyn.RecordTy fields => Types.RECORD(
            map (fn {name, escape, typ, pos} => 
                (name, transTypeField(tenv, typ, pos)) (* TODO: check if this works -> Feels like there should be mutual recursion stuff here *)
            ) fields, 
            ref ())
        | Absyn.ArrayTy(name, pos) => Types.ARRAY(transTypeField(tenv, name, pos), ref ())
end