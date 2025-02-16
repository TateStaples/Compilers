signature ENV = sig
  type access
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty}
    | FunEntry of {formals: ty list, result: ty} 
  val base_tenv : ty Symbol.table (* predefined types *)
  val base_venv : enventry Symbol.table (*predefinedfunctions*) 
end

structure Env :> ENV =
struct
  type access = unit
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty}
    | FunEntry of {formals: ty list, result: ty} 
  val base_tenv = let
    val t : ty Symbol.table = Symbol.new()
  in
    (
      Symbol.enter (t, Symbol.symbol("int"), Types.INT);
      Symbol.enter (t, Symbol.symbol("string"), Types.STRING);
      t
    )
  end
  val base_venv = let
    val t : enventry Symbol.table = Symbol.new()
  in
    (
      Symbol.enter (t, Symbol.symbol("print"), FunEntry({formals=[Types.STRING], result=Types.UNIT}));
      Symbol.enter (t, Symbol.symbol("flush"), FunEntry({formals=[], result=Types.UNIT}));
      Symbol.enter (t, Symbol.symbol("getchar"), FunEntry({formals=[], result=Types.STRING}));
      Symbol.enter (t, Symbol.symbol("ord"), FunEntry({formals=[Types.STRING], result=Types.INT}));
      Symbol.enter (t, Symbol.symbol("chr"), FunEntry({formals=[Types.INT], result=Types.STRING}));
      Symbol.enter (t, Symbol.symbol("size"), FunEntry({formals=[Types.STRING], result=Types.INT}));
      Symbol.enter (t, Symbol.symbol("substring"), FunEntry({formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING}));
      Symbol.enter (t, Symbol.symbol("concat"), FunEntry({formals=[Types.STRING, Types.STRING], result=Types.STRING}));
      Symbol.enter (t, Symbol.symbol("not"), FunEntry({formals=[Types.INT], result=Types.INT}));
      Symbol.enter (t, Symbol.symbol("exit"), FunEntry({formals=[Types.INT], result=Types.UNIT}));
      t
    )
  end
end
