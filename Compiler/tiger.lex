type pos = int

type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun newLine pos = (lineNum := !lineNum + 1; linePos := pos :: !linePos)

val partialString = ref ""
val stringStartPos = ref ~1

val commentPos: int list ref = ref nil

fun getControlChar c = String.str(chr(ord(String.sub(c,2)) - 64))

fun eof() = let
    val pos = hd(!linePos);
in
    (if (!stringStartPos) >= 0
     then ErrorMsg.error pos
             ("Unterminated string starting at " ^
             (ErrorMsg.look(!stringStartPos)))
     else ());
    (if not (null (!commentPos))
     then ErrorMsg.error pos
             ("Unterminated comment starting at " ^
              ErrorMsg.look(hd (!commentPos)))
     else ());
    Tokens.EOF(pos,pos)
          
end
        
%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s COMMENT_STATE STRING_STATE FORMAT_STATE;
escapeDigits = ([0-1][0-9][0-9])|(2[0-4][0-9])|(25[0-5]);
eol = ("\013\010"|"\010"|"\013");
%%
<INITIAL>\n             => (newLine yypos; continue());
<INITIAL>[\ \n\t\r]   => (continue());

<INITIAL>"type"     => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"var"          => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"function" => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>"break"    => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>"of"       => (Tokens.OF(yypos,yypos+2));
<INITIAL>"end"      => (Tokens.END(yypos,yypos+3));
<INITIAL>"in"       => (Tokens.IN(yypos,yypos+2));
<INITIAL>"nil"      => (Tokens.NIL(yypos,yypos+3));
<INITIAL>"let"      => (Tokens.LET(yypos,yypos+3));
<INITIAL>"do"       => (Tokens.DO(yypos,yypos+2));
<INITIAL>"to"       => (Tokens.TO(yypos,yypos+2));
<INITIAL>"for"      => (Tokens.FOR(yypos,yypos+3));
<INITIAL>"while"    => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"else"     => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"then"     => (Tokens.THEN(yypos,yypos+4));
<INITIAL>"if"       => (Tokens.IF(yypos,yypos+2));
<INITIAL>"array"    => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>":="       => (Tokens.ASSIGN(yypos,yypos+6));
<INITIAL>"|"        => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"        => (Tokens.AND(yypos,yypos+1));
<INITIAL>">="       => (Tokens.GE(yypos,yypos+2));
<INITIAL>">"        => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="       => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"        => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>"       => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"="        => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"/"        => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"*"        => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"-"        => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"+"        => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"."        => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"}"        => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"{"        => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"]"        => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"["        => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>")"        => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"("        => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>";"        => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"        => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","        => (Tokens.COMMA(yypos,yypos+1));

<INITIAL>[0-9]+                 => (Tokens.INT(valOf (Int.fromString yytext),yypos,yypos+String.size(yytext)));
<INITIAL>[A-Za-z][A-Za-z0-9_]*  => (Tokens.ID(yytext,yypos,yypos+String.size(yytext)));
<INITIAL>[0-9_][A-Za-z0-9_]*    => (ErrorMsg.error yypos ("Invalid identifier \"" ^ yytext ^ "\""); continue());

<INITIAL>\/\* => (
    commentPos := yypos :: !commentPos;
    YYBEGIN COMMENT_STATE;
    continue()
);
<COMMENT_STATE>\/\* => (
    commentPos := yypos :: !commentPos;
    continue()
);
<COMMENT_STATE>\*\/ => (
    commentPos := tl (!commentPos);
    if null (!commentPos) then YYBEGIN INITIAL else ();
    continue()
);
<COMMENT_STATE>\n   => (newLine yypos; continue());
<COMMENT_STATE>.    => (continue());

<INITIAL>\"			=> (YYBEGIN STRING_STATE; partialString := ""; stringStartPos := yypos; continue());

<STRING_STATE>\"	=> (	YYBEGIN INITIAL; 
							let val result = (!partialString) 
								val spos = (!stringStartPos)
							in 
								partialString := ""; 
								stringStartPos := ~1; 
								Tokens.STRING(result, spos, yypos+1)
							end
						); 

<STRING_STATE>\\[\\\"nt]	           => (partialString := (!partialString) ^ yytext; continue());
<STRING_STATE>\\\^[@A-Z\[\\\]\^_]	   => (partialString := (!partialString) ^ (getControlChar yytext); continue());
<STRING_STATE>\\{escapeDigits}	       => (partialString := (!partialString) ^ (Char.toString(chr(valOf (Int.fromString (String.substring(yytext,1,3)))))); continue());
<STRING_STATE>\\(\t|" "|\r) => (YYBEGIN FORMAT_STATE; continue());
<STRING_STATE>\\\n          => (YYBEGIN FORMAT_STATE; newLine yypos; continue());
<FORMAT_STATE>\\            => (YYBEGIN STRING_STATE; continue());
<FORMAT_STATE>(" "|\t|\r)   => (continue());
<FORMAT_STATE>\n            => (newLine yypos; continue());
<FORMAT_STATE>.             => (ErrorMsg.error yypos ("Illegal character within string formatting \"" ^ yytext^"\""); continue());
<STRING_STATE>\\.			=> (ErrorMsg.error yypos ("Illegal escape character \"" ^ yytext^"\""); continue());
<STRING_STATE>\n 			=> (ErrorMsg.error yypos ("Illegal new line within a string"); newLine yypos; continue());
<STRING_STATE>.				=> (partialString := (!partialString) ^ yytext; continue());

.                           => (ErrorMsg.error yypos ("Failed parsing text \"" ^ yytext ^ "\""); continue());
