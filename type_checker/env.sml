signature ENV = 
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result: ty}
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table
end