signature ERRORMSG =
sig
    val anyErrors : bool ref
    val fileName : string ref
    val lineNum : int ref
    val linePos : int list ref
    val sourceStream : TextIO.instream ref
    val error : int -> string -> unit
    exception Error
    val impossible : string -> 'a   (* raises Error *)
    val reset : unit -> unit
    val look : int -> string
end

structure ErrorMsg : ERRORMSG =
struct

  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]
  val sourceStream = ref TextIO.stdIn

  fun reset() = (anyErrors:=false;
		 fileName:="";
		 lineNum:=1;
		 linePos:=[1];
		 sourceStream:=TextIO.stdIn)

  exception Error

  fun look pos =
      let fun lookAux (a::rest, n) =
	      if a<pos then
		  foldl (fn (s, text) => text ^ s) ""
			[":", Int.toString n, ".", Int.toString (pos-a)]
	      else lookAux (rest,n-1)
	    | lookAux _ = "0.0"
      in
	  lookAux(!linePos, !lineNum)
      end

  fun error pos (msg:string) = (
      anyErrors := true;
      print (!fileName);
      print (look pos);
      print ":";
      print msg;
      print "\n"
  );
      

  fun impossible msg =
      (app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)

end  (* structure ErrorMsg *)
  
