(* temp *)

type pos = int
type lexresult = Tokens.token



val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val comments: (int * int) list ref = ref []

val string_start = ref 0
val string_builder = ref ""

val in_string = ref false

fun err(p1, p2) = ErrorMsg.error p1
fun eof() = 
    let
        val pos = hd(!linePos)
    in 
        if !in_string then ErrorMsg.error(!string_start) ("Error open string") else ();
        case !comments of 
               [] => ()
             | (lineNum, linePos)::l => (map print ["Lex Error: Unclosed comment at ", Int.toString lineNum, ":", Int.toString linePos, "\n"]; ()); (* ErrorMsg.error appears to only work for errors on the line currently being lexed, not done retroactively at the end, and I'm not sure that we maintain linePos *)
        Tokens.EOF(pos,pos) 
    end

%% 
%s INITIAL COMMENT STRING ESCAPED_STRING; 
%%

\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
\t => (continue());
\r => (continue());
" " => (continue());

<INITIAL>"."    => (Tokens.DOT (yypos, yypos + 1));
<INITIAL>","	=> (Tokens.COMMA (yypos, yypos + 1));
<INITIAL>":"    => (Tokens.COLON (yypos, yypos + 1));
<INITIAL>";"    => (Tokens.SEMICOLON (yypos, yypos + 1));
<INITIAL>"+"    => (Tokens.PLUS (yypos, yypos + 1));
<INITIAL>"-"    => (Tokens.MINUS (yypos, yypos + 1));
<INITIAL>"*"    => (Tokens.TIMES (yypos, yypos + 1));
<INITIAL>"/"    => (Tokens.DIVIDE (yypos, yypos + 1));
<INITIAL>"="    => (Tokens.EQ (yypos, yypos + 1));
<INITIAL>"<>"   => (Tokens.NEQ (yypos, yypos + 2));
<INITIAL>"<"    => (Tokens.LT (yypos, yypos + 1));
<INITIAL>"<="   => (Tokens.LE (yypos, yypos + 2));
<INITIAL>">"    => (Tokens.GT (yypos, yypos + 1));
<INITIAL>">="   => (Tokens.GE (yypos, yypos + 2));
<INITIAL>"&"    => (Tokens.AND (yypos, yypos + 1));
<INITIAL>"|"    => (Tokens.OR (yypos, yypos + 1));
<INITIAL>":="   => (Tokens.ASSIGN (yypos, yypos + 2));
<INITIAL>"("    => (Tokens.LPAREN (yypos, yypos + 1));
<INITIAL>")"    => (Tokens.RPAREN (yypos, yypos + 1));
<INITIAL>"["    => (Tokens.LBRACK (yypos, yypos + 1));
<INITIAL>"]"    => (Tokens.RBRACK (yypos, yypos + 1));
<INITIAL>"{"    => (Tokens.LBRACE (yypos, yypos + 1));
<INITIAL>"}"    => (Tokens.RBRACE (yypos, yypos + 1));

<INITIAL>"for"    => (Tokens.FOR  (yypos, yypos + 3));
<INITIAL>"do"     => (Tokens.DO   (yypos, yypos + 2));
<INITIAL>"else"   => (Tokens.ELSE (yypos, yypos + 4));
<INITIAL>"end"    => (Tokens.END  (yypos, yypos + 3));
<INITIAL>"if"     => (Tokens.IF   (yypos, yypos + 2));
<INITIAL>"in"     => (Tokens.IN   (yypos, yypos + 2));
<INITIAL>"let"    => (Tokens.LET  (yypos, yypos + 3));
<INITIAL>"nil"    => (Tokens.NIL  (yypos, yypos + 3));
<INITIAL>"of"     => (Tokens.OF   (yypos, yypos + 2));
<INITIAL>"then"   => (Tokens.THEN (yypos, yypos + 4));
<INITIAL>"to"     => (Tokens.TO   (yypos, yypos + 2));
<INITIAL>"type"   => (Tokens.TYPE (yypos, yypos + 4));
<INITIAL>"var"    => (Tokens.VAR  (yypos, yypos + 3));
<INITIAL>"array"  => (Tokens.ARRAY (yypos, yypos + 5));
<INITIAL>"break"  => (Tokens.BREAK (yypos, yypos + 5));
<INITIAL>"while"  => (Tokens.WHILE (yypos, yypos + 5));
<INITIAL>"function"   => (Tokens.FUNCTION (yypos, yypos + 8));

<INITIAL>[a-zA-Z][a-zA-Z0-9]* => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>[0-9]+ => (Tokens.INT(valOf (Int.fromString yytext), yypos, yypos + size yytext));

<INITIAL>"/*" => (YYBEGIN COMMENT; comments := ((!lineNum, yypos)::(!comments)); continue());
<COMMENT>"*/" => ((case !comments of 
                     a::[] => (comments := []; YYBEGIN INITIAL) 
                   | a::l => comments := l
                   | [] => (* This will never happen just silencing warning *) ()); continue()); 

<COMMENT>. => (continue());

<INITIAL>"\"" => (YYBEGIN STRING; in_string := true; string_start := yypos; string_builder := ""; continue());
<STRING>"\"" => (YYBEGIN INITIAL; in_string := false; Tokens.STRING(!string_builder, !string_start, yypos + 1));
<STRING>\ => (YYBEGIN ESCAPED_STRING; continue());
<STRING>. => (string_builder := !string_builder ^ yytext; continue());

<ESCAPED_STRING>n => (string_builder := !string_builder ^ "\n"; YYBEGIN STRING; continue());
<ESCAPED_STRING>t => (string_builder := !string_builder ^ "\t"; YYBEGIN STRING; continue());
<ESCAPED_STRING>r => (string_builder := !string_builder ^ "\r"; YYBEGIN STRING; continue());
<ESCAPED_STRING>"\n" => (YYBEGIN STRING; continue());
<ESCAPED_STRING>"\"" => (string_builder := !string_builder ^ "\""; YYBEGIN STRING; continue());
<ESCAPED_STRING>\ => (string_builder := !string_builder ^ "\\"; YYBEGIN STRING; continue());
<ESCAPED_STRING>. => (string_builder := !string_builder ^ "\\" ^ yytext; YYBEGIN STRING; continue());

.       => (ErrorMsg.error yypos ("illegal character (test) " ^ yytext); continue());
