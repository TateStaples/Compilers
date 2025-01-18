(* Section 3 *)
functor F (M: ORD_MAP where type Key.ord_key = string) (S: ORD_SET where type Key.ord_key = string) :> sig
    val proc: string list -> S.set M.map;
    val pretty_map: S.set M.map -> unit;
end =
struct
    structure M = M
    structure S = S

    fun proc_file(file_name: string): S.set M.map = 
        let
            val input: string = TextIO.input(TextIO.openIn file_name)
            val words = String.tokens (fn c => c = #" " orelse c = #"\n") input
        in  
            foldl (fn (word: string, map: S.set M.map) => M.insert(map, word, (S.singleton file_name))) M.empty words
        end;

    fun proc files = foldl (fn (file, acc_map) => M.unionWith S.union (proc_file file, acc_map)) M.empty files

    fun pretty_set(s: S.set): unit = S.app (fn (x:string) => TextIO.print (x ^ " ")) s

    fun pretty_map(m: S.set M.map): unit = M.appi (fn (k, v) => (TextIO.print (k ^ ": "); pretty_set v; TextIO.print "\n") ) m
end;

val files = ["a.txt", "b.txt"];
structure M = RedBlackMapFn (struct type ord_key = string val compare = String.compare end);
structure S = RedBlackSetFn (struct type ord_key = string val compare = String.compare end);
structure F1 = F(M)(S);
val _ = F1.pretty_map (F1.proc files);