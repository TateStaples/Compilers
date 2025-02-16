signature SYMBOL =
sig
  eqtype symbol
  val symbol : string -> symbol
  val name : symbol -> string
  type 'a table
  val new : unit -> 'a table
  val enter : 'a table * symbol * 'a -> unit
  val look : 'a table * symbol -> 'a option
  val beginScope: 'a table -> unit 
  val endScope : 'a table -> unit 

  val testTable : unit -> unit
end

structure Symbol :> SYMBOL =
struct

  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string,int) H.hash_table = 
		H.mkTable(HashString.hashString, op = ) (sizeHint,Symbol)
  
  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
	           in nextsym := i+1;
		      H.insert hashtable (name,i);
		      (name,i)
		  end

  fun name(s,n) = s

  fun hash((s, n): symbol) = case H.find hashtable s of
    NONE => raise Symbol
    | SOME (h) => Word.fromInt h

  fun eq((s1, n1): symbol, (s2, n2): symbol) = n1 = n2

  structure Table = IntMapTable(type key = symbol
				fun getInt(s,n) = n)

  type 'a table = (symbol, 'a) H.hash_table * ((symbol, 'a option) H.hash_table) list ref
  fun new () = ((H.mkTable(hash, eq) (sizeHint, Symbol)), ref [H.mkTable(hash, eq) (sizeHint, Symbol)])
  fun enter ((t1, r), k, v) = case !r of 
    t2::l => let 
      val orig = case H.find t2 k of
        NONE => H.find t1 k
        | SOME(v) => v
    in
      (H.insert t2 (k, orig); H.insert t1 (k, v))
    end
    | [] => H.insert t1 (k, v)
  fun look ((t1, _), k) = H.find t1 k
  fun beginScope ((t1, r)) = (r := H.mkTable(hash, eq) (sizeHint, Symbol) :: !r)
  fun endScope((t1, r)) = case !r of t2::l => let
    fun undo (orig, (k, v)::rest) = let
      val _ = case v of 
          NONE => (H.findAndRemove t1 k; ())
        | SOME(a) => H.insert t1 (k, a)
    in
      undo (orig, rest)
    end
      | undo (orig, []) = ()
  in 
    (undo(t1, H.listItemsi(t2)); r := l)
  end
    | [] => raise Symbol
  
  fun printElem (t, k) = case look (t, k) of
    NONE => print "Not found\n"
    | SOME v => print ((Int.toString v) ^ "\n")
  
  fun testTable () = (let
      val t = new()
    in
      enter (t, symbol("1"), 1);
      printElem(t, symbol("1")) (* 1 *);
      beginScope(t);
      enter (t, symbol("1"), 2);
      printElem(t, symbol("1")) (* 2 *);
      endScope(t);
      printElem(t, symbol("1")) (* 1 *)
    end;
    let
      val t = new();
    in
       beginScope(t);
      enter (t, symbol("1"), 1);
      printElem(t, symbol("1")) (* 1 *);
      endScope(t);
      printElem(t, symbol("1")) (* not found *)
    end)
  
end
