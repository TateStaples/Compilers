fun print(s: string) = TextIO.print s;

fun printi(i: int) = print(Int.toString i);

fun flush() = TextIO.flushOut TextIO.stdOut;

fun getChar() = Char.ord(TextIO.input1 TextIO.stdIn);

fun ord(c: char) = Char.ord c;

fun chr(i: int) = Char.chr i;

fun size(s: string) = String.size s;    

fun substring(s: string, i: int, j: int) = String.substring(s, i, j);

fun concat(s1: string, s2: string) = String.concat(s1, s2);

fun not(b: int) = if b = 0 then 1 else 0;

fun exit() = OS.Process.exit OS.Process.success; 