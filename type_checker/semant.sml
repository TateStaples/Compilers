structure Translate = struct type exp = unit end
structure A = Absyn
structure S = Symbol
structure E = Env

structure Semant =
struct

  type venv = E.enventry S.table 
  type tenv = Types.ty S.table

  val loops = ref 0
  
  exception TypeCheck

  fun error pos msg = (print ((Int.toString pos) ^ " " ^ msg ^ "\n"); raise TypeCheck)

  fun actual_ty(Types.NAME(name, typ), pos) = (case !typ of 
        SOME v => actual_ty (v, pos)
      | NONE => error pos "used undefined type")
    | actual_ty(ty, pos) = ty

  fun checkInt ({exp,ty},pos) = case ty of 
        Types.INT => ()
      | _ => (error pos ("integer required got " ^ (Types.toString ty) ^ "\n"); ())

  fun scopeWrapper(venv, tenv, f) = (
    S.beginScope(venv);
    S.beginScope(tenv);
    let 
      val a = f() 
    in (
      S.endScope(venv);
      S.endScope(tenv);
      a
    ) end
  )

  fun checkDupNames(target: string, l: string list, pos: int) = case List.filter (fn x => x = target) l of 
        [] => ()
      | _ => error pos "dupliate name in recursive declarations" 
 
  fun checkCycle(from: Types.ty, to: Types.ty, pos) = case (from, to) of (
        Types.NAME(s1, _), Types.NAME(s2, _)) => (if S.name s1 = (S.name s2) then error pos "cyclic type declaration" else 
          case from of 
            Types.NAME(s, r) => (case !r of
              SOME next => checkCycle (next, to, pos)
            | _ => ()
            )
          | _ => ()
        )
      | _ => ()
 
  fun transExp (venv: venv, tenv: tenv, exp: Absyn.exp) = 
    let 
      fun trexp (A.OpExp{left,oper=A.EqOp,right,pos}) = if not (Types.tyEq(actual_ty (#ty (trexp left), pos), actual_ty (#ty (trexp right), pos))) then error pos "comparing different types" else {exp=(), ty=Types.INT}
      | trexp (A.OpExp{left,oper=A.NeqOp,right,pos}) = if not (Types.tyEq(actual_ty (#ty (trexp left), pos), actual_ty (#ty (trexp right), pos))) then error pos "comparing different types" else {exp=(), ty=Types.INT}
      | trexp (A.OpExp{left,oper,right,pos}) = (checkInt (trexp left, pos); checkInt (trexp right, pos); {exp= () , ty=Types. INT} )
      | trexp (A.RecordExp{fields, typ, pos}) = let
          val constraint = (case S.look (tenv, typ) of
            NONE => error pos "record type undeclared"
          | SOME v => actual_ty (v, pos))
          val conFields = (case constraint of
            Types.RECORD(l, _) => l
          | _ => error pos "record exp type is not record")
          fun checkField ((s1,e,p)::l1, (s2,t)::l2) = (
            if (S.name s1) <> (S.name s2) then error pos "field names don't match" else ();
            print(Types.toString (#ty (trexp e)) ^ " & " ^ (Types.toString t) ^ "\n");
            if not (Types.tyEq(actual_ty (#ty (trexp e), pos), actual_ty (t, pos))) then error pos "expression doesn't match field type" else ();
            checkField(l1, l2)
          ) 
            | checkField ([], []) = ()
            | checkField ([], _) = error pos "too few fields"
            | checkField (_, []) = error pos "too many fields"
        in
          (checkField(fields, conFields); {exp=(), ty=constraint})
        end

      | trexp (A.VarExp(var)) = trvar(var)
      | trexp (A.NilExp) = {exp=(), ty=Types.NIL}
      | trexp (A.IntExp(i)) = {exp=(), ty=Types.INT}
      | trexp (A.StringExp(s,p)) = {exp=(), ty=Types.STRING}
      | trexp (A.CallExp{func, args, pos}) = let
          fun checkArgs (a::l1, b::l2) = (print ("COMPARING " ^ (Types.toString (#ty (trexp a))) ^ " AND " ^ (Types.toString (actual_ty (b, pos))) ^ "\n"); if not (Types.tyEq(actual_ty (#ty (trexp a), pos), actual_ty (b, pos))) then error pos "Argument does not match expected type" else checkArgs(l1, l2))
            | checkArgs ([], []) = ()
            | checkArgs (a::l1, []) = error pos "too many args"
            | checkArgs ([], b::l2) = error pos "too few args"
        in
         (case S.look (venv, func) of 
              SOME(E.FunEntry{formals, result}) => (checkArgs(args, formals); {ty=result, exp=()})
            | SOME(E.VarEntry{ty}) => error pos "Var not callable"
            | NONE => error pos "Func does not exist"   )
        end
      | trexp (A.SeqExp((exp,pos)::[])) = trexp exp
      | trexp (A.SeqExp((exp,pos)::l)) = (trexp exp; trexp (A.SeqExp l))
      | trexp (A.SeqExp([])) = {exp=(), ty=Types.UNIT}
      | trexp (A.AssignExp{var, exp, pos}) = (
          let
            val resty = #ty (trexp exp) 
          in 
            if not (Types.tyEq(actual_ty (#ty (trvar var), pos), actual_ty (resty, pos))) then error pos "right side doesn't match" else ();
            {ty=Types.UNIT, exp=()} 
          end) 
      | trexp (A.IfExp{test, then', else'=SOME(e), pos}) = let 
          val _ = checkInt (trexp test, pos)
          val a = trexp(then') 
          val b = trexp e
          val _ = if not (Types.tyEq(actual_ty (#ty a, pos), actual_ty (#ty b, pos))) then error pos "then and else clause different types" else () 
        in 
          {exp=(), ty=(#ty a)}
        end
      | trexp (A.IfExp{test, then', else'=NONE, pos}) = (checkInt(trexp test, pos); case #ty (trexp then') of Types.UNIT => () | _ => error pos "ifthen should return unit"; {exp=(), ty=Types.UNIT})
      | trexp (A.WhileExp{test, body, pos}) = (loops := (!loops + 1); print "checking while\n"; checkInt(trexp test, pos); print "while over\n"; case #ty (trexp body) of Types.UNIT => () | _ => error pos "while should return unit"; loops := (!loops - 1); {ty=Types.UNIT, exp=()})
      | trexp (A.ForExp{var, escape, lo, hi, body, pos}) = (
          loops := (!loops + 1);
          checkInt(trexp lo, pos);
          checkInt(trexp hi, pos);
          S.beginScope venv;
          print ((S.name var) ^ " => INT\n"); 
          S.enter(venv, var, E.VarEntry{ty=Types.INT});
          case #ty (trexp body) of 
              Types.UNIT => ()
            | _ => error pos "for should return unit";
          S.endScope venv;
          loops := (!loops - 1);
          {ty=Types.UNIT, exp=()}
        )
      | trexp (A.BreakExp(pos)) = if !loops = 0 then (error pos "break outside of loop"; {ty=Types.UNIT, exp=()}) else {ty=Types.UNIT, exp=()} (* have to check that we're in a for or while *)
      | trexp (A.LetExp{decs, body, pos}) = scopeWrapper(
          venv, 
          tenv, 
          fn () => (
            map (fn d => transDec(venv, tenv, d)) decs;
            trexp(body)
          )
        ) 
      | trexp (A.ArrayExp{typ, size, init, pos}) = (
          let 
            val t = case S.look (tenv, typ) of 
              SOME (v) => actual_ty (v, pos) 
            | NONE => error pos "array of undeclared type"
            val t' = case t of 
              Types.ARRAY(ty, u) => ty
            | _ => error pos "non array in array expression" 
          in
            checkInt(trexp size, pos);
            if not (Types.tyEq(actual_ty (t', pos), actual_ty (#ty (trexp init), pos))) then error pos "array init wrong type" else ();
            {ty=t, exp=()}
          end
        ) 
      and trvar (A.SimpleVar(id, pos)) =  
        (
          case S.look (venv, id) of 
            SOME (E.VarEntry{ty}) => {exp= (), ty=actual_ty (ty, pos)}
          | NONE => (error pos ("undefined variable " ^ S.name id); {exp= (), ty=Types. INT})
          | _ => error pos "function entry in venv" 
        )
      | trvar (A.FieldVar(v,id,pos)) = (case actual_ty(#ty (trvar v), pos) of
          Types.RECORD(fields, _) => (let
          fun findField ((sym, ty)::l, target) = if S.name sym = S.name target then {exp=(), ty=ty} else findField (l, target)
            | findField ([], target) = error pos "unknown field"
          in
            findField (fields, id)
          end)
        | _ => (error pos ("tried to take field '" ^(Symbol.name(id))^"' of non record " ^(Types.toString(#ty(trvar v)))); {exp=(), ty=Types.INT})
        )
      | trvar (A.SubscriptVar(v, exp, pos)) = case actual_ty(#ty (trvar v), pos) of
          Types.ARRAY(ty, unique) => if not (Types.tyEq(actual_ty (#ty (trexp exp), pos), Types.INT)) then error pos "index must be int" else {ty=ty, exp=()}
        | _ => error pos "tried to subscript non array"
    in
      trexp exp
    end

  
  and transDec (venv, tenv, A.VarDec{name, escape, typ=NONE, init, pos}) = 
    let val {exp, ty} = transExp(venv, tenv, init)
      in 
        (case ty of 
          Types.NIL => error pos "nil not allowed"
        | _ => print ((S.name name) ^ " => " ^ (Types.toString ty) ^ "\n"));
        {tenv=tenv, 
        venv=(S.enter(venv, name, E.VarEntry{ty=ty}); venv)}
    end
  | transDec (venv, tenv, A.VarDec{name, escape, typ=SOME(v, tpos), init, pos}) = 
    let 
      val {exp, ty} = transExp(venv, tenv, init)
      val _ = print ("COMPARING " ^ (Types.toString ty) ^ "\n")
      val _ = case S.look (tenv, v) of 
        SOME(constraint) => (print ("AND " ^ (Types.toString (actual_ty (constraint, pos))) ^ "\n"); if Types.tyEq(actual_ty (constraint, pos), actual_ty (ty, pos)) then () else (error tpos "types don't match"; ()))
      | NONE => (error tpos "constraint has undefined type"; ())
    in {tenv=tenv, venv=(print ((S.name name) ^ " => " ^ (Types.toString ty) ^ "\n"); S.enter(venv, name, E.VarEntry{ty=ty}); venv)}
    end
  | transDec (venv, tenv, A. TypeDec ({name,ty,pos}::l)) = let 
      val r = ref NONE  
    in (
      checkDupNames (S.name name, map (fn x => S.name (#name x)) l, pos);
      S.enter (tenv, name, Types.NAME(name, r));
      transDec(venv, tenv, A.TypeDec(l));
      {venv=venv,
        tenv= (
          r := SOME (transTy(tenv, ty));
          case !r of 
            SOME (Types.NAME(s, ptr)) => (
              case !ptr of 
                  SOME t => checkCycle (t, Types.NAME(s, ptr), pos) (* ty referts to t, can we get from t to ty *)
                | NONE => ()
            )
          | _ => ();
          print ((S.name name) ^ " => " ^ (case !r of SOME rr => Types.toString rr | NONE => "NONE") ^ "\n");
          tenv
        )}
    ) end
  | transDec (venv, tenv, A.TypeDec([])) = {tenv=tenv, venv=venv}
  | transDec(venv, tenv, A.FunctionDec({name, params, body, pos, result}::l)) = 
      let 
        val result_ty = case result of 
          SOME(rt, rpos) => (case S.look(tenv, rt) of 
            SOME(r) => r
          | NONE => error pos "function returns unknown type") 
        | NONE => Types.UNIT
        fun transpararm{name, typ, escape, pos} =
          case S.look(tenv, typ)
            of SOME t => {name=name, ty=t}
             | NONE => error pos "parameter has unknown type"
        val params' = map transpararm params
      in 
        (
          checkDupNames (S.name name, map (fn x => S.name (#name x)) l, pos);
          print ((S.name name) ^ " => Function(" ^ foldr (fn (x,s) => Types.toString (#ty x) ^ "," ^ s) "" params' ^ ")->" ^ (Types.toString result_ty) ^  "\n"); 
          S.enter(venv, name, E.FunEntry{formals=map #ty params', result=result_ty});  
          transDec(venv, tenv, A.FunctionDec(l)); 
          S.beginScope venv;
          map (fn {name, ty} => (print((S.name name) ^ " => " ^ (Types.toString ty) ^ "\n"); S.enter(venv, name, E.VarEntry {ty=ty}))) params'; 
          if not (Types.tyEq(actual_ty (#ty (transExp (venv, tenv, body)), pos), actual_ty (result_ty, pos))) then error pos "function returns unexpected type" else ();
          S.endScope venv;
          {tenv=tenv, venv=venv}
        )
      end
   | transDec(venv, tenv, A.FunctionDec[]) = {venv=venv, tenv=tenv}

  and transTy (tenv, A.NameTy(s, p)) = (case S.look (tenv, s) of 
        SOME v => v
      | NONE => Types.NAME(s, ref NONE) )
    | transTy (tenv, A.RecordTy(l)) = Types.RECORD(map (fn {name, escape, typ, pos} => (name, case S.look (tenv, typ) of NONE => error pos "field of undefined type" | SOME v => v)) l, ref ()) 
    | transTy (tenv, A.ArrayTy(s, pos)) = Types.ARRAY(
        case S.look (tenv, s) of 
          NONE => error pos "array of unknown type" 
        | SOME v => v, ref ())

  fun transProg(prog) =  transExp (E.base_venv, E.base_tenv, prog)

end
