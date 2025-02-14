type venv = Env.enventry Symbol.table (* value environment *)
type tenv = ty Symbol.table (* type environment *)
type expty = {exp: Translate.exp, ty: Types.ty} (* expression type *)

fun transVar (venv, tenv, v: Absyn.var) = raise Fail "Not implemented"
fun transExp (venv, tenv, e: Absyn.exp): expty = raise Fail "Not implemented"
fun transDec (venv, tenv, d: Absyn.dec): {v:venv, t: tenv} = raise Fail "Not implemented"
fun transTy (tenv, t: Absyn.ty): Types.ty = raise Fail "Not implemented"