(*
Token Types

Reserved keywords:
array, break, do, else, end, for, function, if, in, let, nil, of, then, to, type, var, while

Integer Literals:
Positive integers are consequative sequences of digits

String Literals:
Strings are sequences of characters enclosed in double quotes with \ as an escape character
    Escape sequences: \n (newline), \t (tab), \\ (backslash), \^c (control character), \ddd (decimal character code), \...\ (any whitespace surrounded by \s is ignored)

Punctiation / Operators:
, : ; ( ) [ ] { } . + - * / = <> < <= > >= & | :=

Identifiers (ID): 
case-sensitive sequence of letters, digits, and underscores, starting with a letter
*)

(*
Outline:
1. Isolate the comments (not right now)
2. Isolate the string literals
3. Split the input into tokens by splitting on whitespace and punctuation
4. Classify the tokens
*)

datatype TOKEN = IDENTIFIER of string | INTEGER_CONSTANT of string | STRING_CONSTANT of string 
(* Keywords *) | ARRAY | BREAK | DO | ELSE | END | FOR | FUNCTION | IF | IN | LET | NIL | OF | THEN | TO | TYPE | VAR | WHILE
(* Punctiation *) | COMMA | COLON | SEMICOLON | LPAREN | RPAREN | LBRACKET | RBRACKET | LBRACE | RBRACE | PERIOD | PLUS | MINUS | TIMES | DIV | EQUAL | NOT_EQUAL | LESS_THAN | GREATER_THAN | LESS_THAN_EQUAL | GREATER_THAN_EQUAL | AND | OR | ASSIGN;

val TOKEN_str_pairs = [
    (IDENTIFIER _, "identifier"), (INTEGER_CONSTANT _, "integer"), (STRING_CONSTANT _, "string"),
    (ARRAY, "array"), (BREAK, "break"), (DO, "do"), (ELSE, "else"), (END, "end"), (FOR, "for"), (FUNCTION, "function"), (IF, "if"), (IN, "in"), (LET, "let"), (NIL, "nil"), (OF, "of"), (THEN, "then"), (TO, "to"), (TYPE, "type"), (VAR, "var"), (WHILE, "while"),
    (COMMA, ","), (COLON, ":"), (SEMICOLON, ";"), (LPAREN, "("), (RPAREN, ")"), (LBRACKET, "["), (RBRACKET, "]"), (LBRACE, "{"), (RBRACE, "}"), (PERIOD, "."), (PLUS, "+"), (MINUS, "-"), (TIMES, "*"), (DIV, "/"), (EQUAL, "="), (NOT_EQUAL, "<>"), (LESS_THAN, "<"), (GREATER_THAN, ">"), (LESS_THAN_EQUAL, "<="), (GREATER_THAN_EQUAL, ">="), (AND, "&"), (OR, "|"), (ASSIGN, ":=")
];

fun token_to_str token = 
    case token of
        IDENTIFIER id => "identifier: " ^ id
      | INTEGER_CONSTANT int => "integer: " ^ int
      | STRING_CONSTANT str => "string: " ^ str
      | t as _ => case List.find (fn (t', _) => t = t') TOKEN_str_pairs of
            SOME (_, str) => str
          | NONE => raise Fail "Invalid token";

fun str_to_token token = case List.find (fn (_, str') => token = str') TOKEN_str_pairs of
    SOME (t, _) => Some(token)
  | NONE => None

fun error(line: int, message: string) = raise Fail ("Error on line " ^ Int.toString line ^ ": " ^ message)

structure Scanner = struct
datatype context = TOKEN list
type program = {
    input: string,
    current_line: int,
    current_char: int,
    context: context
}
fun get_current_char(program): char = String.sub(program.input, program.current_char)

fun end_of_file(program): bool = program.current_char >= String.size program.input

fun keyword_to_token "array" = Some(ARRAY)
    | keyword_to_token "break" = Some(BREAK)
    | keyword_to_token "do" = Some(DO)
    | keyword_to_token "else" = Some(ELSE)
    | keyword_to_token "end" = Some(END)
    | keyword_to_token "for" = Some(FOR)
    | keyword_to_token "function" = Some(FUNCTION)
    | keyword_to_token "if" = Some(IF)
    | keyword_to_token "in" = Some(IN)
    | keyword_to_token "let" = Some(LET)
    | keyword_to_token "nil" = Some(NIL)
    | keyword_to_token "of" = Some(OF)
    | keyword_to_token "then" = Some(THEN)
    | keyword_to_token "to" = Some(TO)
    | keyword_to_token "type" = Some(TYPE)
    | keyword_to_token "var" = Some(VAR)
    | keyword_to_token "while" = Some(WHILE)
    | keyword_to_token _ = None

fun punctuation_to_token "," = Some(COMMA)
    | punctuation_to_token ":" = Some(COLON)
    | punctuation_to_token ";" = Some(SEMICOLON)
    | punctuation_to_token "(" = Some(LPAREN)
    | punctuation_to_token ")" = Some(RPAREN)
    | punctuation_to_token "[" = Some(LBRACKET)
    | punctuation_to_token "]" = Some(RBRACKET)
    | punctuation_to_token "{" = Some(LBRACE)
    | punctuation_to_token "}" = Some(RBRACE)
    | punctuation_to_token "." = Some(PERIOD)
    | punctuation_to_token "+" = Some(PLUS)
    | punctuation_to_token "-" = Some(MINUS)
    | punctuation_to_token "*" = Some(TIMES)
    | punctuation_to_token "/" = Some(DIV)
    | punctuation_to_token "=" = Some(EQUAL)
    | punctuation_to_token "<>" = Some(NOT_EQUAL)
    | punctuation_to_token "<" = Some(LESS_THAN)
    | punctuation_to_token "<=" = Some(LESS_THAN_EQUAL)
    | punctuation_to_token ">" = Some(GREATER_THAN)
    | punctuation_to_token ">=" = Some(GREATER_THAN_EQUAL)
    | punctuation_to_token "&" = Some(AND)
    | punctuation_to_token "|" = Some(OR)
    | punctuation_to_token ":=" = Some(ASSIGN)
    | punctuation_to_token _ = None



(* Punctuation: , : ; ( ) [ ] { } . + - * / = <> < <= > >= & | := *)
fun is_punction(character) = Char.contains ",:;()[]{}.+-*/=<>><=&|:" character

(* Keywords: array, break, do, else, end, for, function, if, in, let, nil, of, then, to, type, var, while *)

(* Between tokens: " starts string, digit starts int, letter starts identifier, punctuation starts punctuation *)
fun whitespace_consume(program) = let current_char = get_current_char(program) in
    if Char.isDigit then int_consume(program)
    else if current_char = #"\"" then string_consume(program with {current_char=current_char+1}, program.current_char, false)
    else if is_punction(current_char) then punct_consume(program)
    else if Char.isAlpha(current_char) | current_char = #"_" then id_consume(program)
    else if Char.isPunct punct_consume(program with {current_char=current_char}) (* punctuation *)
    else if current_char = #"\n" then whitespace_consume(program with {current_line=program.current_line+1, current_char=current_char+1}) (* newline *)
    else if Char.isSpace whitespace_consume(program with {current_char=current_char+1}) (* whitespace *)
    else error(program.current_line, "Invalid character: " ^ Char.toString current_char) (* error *)
    end

fun string_consume(program, prev_string, escaped) = (* Inside a string: " ends string, \ starts escape sequence *)
    let current_char = get_current_char(program) in
    if escaped then 
        case current_char of 
            #"n" => string_consume(program with {current_char=current_char+1}, prev_string ^ "\n", false)
            | #"t" => string_consume(program with {current_char=current_char+1}, prev_string ^ "\t", false)
            (* | #"\\" => string_consume(program with {current_char=current_char+1}, string_start, false) - editor highlighting is weird *)
            (* | #"^" => string_consume(program with {current_char=current_char+1}, string_start, false) - assuming we don't need to handle this*)
            | _ => error(program.current_line, "Invalid escape sequence: " ^ Char.toString current_char)
    else case of current_char of
        #"\"" => whitespace_consume(program with {current_char=current_char+1, context=STRING_CONSTANT prev_string::program.context})
        | _ => string_consume(program with {current_char=current_char+1}, prev_string ^ Char.toString current_char, false)
    end
    end

fun int_consume(program, current_value) = let current_char = get_current_char(program) in (* Inside an integer: digit continues int, punctuation ends int *)
    if Char.isDigit(current_char) then int_consume(program with {current_char=current_char+1}, current_value * 10 + Char.ord current_char - Char.ord #"0")
    else whitespace_consume(program)
    end

(* Inside an identifier: letter, digit, or _ continues id, punctuation ends id *)

fun id_consume(program, id_start) = let current_char = get_current_char(program) in (* Inside an identifier: letter, digit, or _ continues id, punctuation ends id *)
    if Char.isAlpha(current_char) | current_char = #"_" | Char.isDigit(current_char) then id_consume(program with {current_char=current_char+1}, id_start)
    else whitespace_consume(program with {context=IDENTIFIER(String.substring(program.input, id_start, program.current_char+1))::program.context})
    end

(* Inside a punctuation: punctuation ends punctuation, letter, digit, or _ starts id *)
val punctuation_chars = ".,:;()[]{}+-*/=<>><=&|"
val punctuation = [",", ":", ";", "(", ")", "[", "]", "{", "}", ".", "+", "-", "*", "/", "=", "<>", "<", "<=", ">", ">=", "&", "|", ":="]
fun punct_consume(program, prev_punct:str) = 
    let 
        val current_char = get_current_char(program) 
        val new_punct = prev_punct ^ current_char
    in (* Inside a punctuation: punctuation ends punctuation, letter, digit, or _ starts id *)
    if Char.contains punctuation_chars current_char then punct_consume(program with {current_char=current_char+1}, new_punct)
    else case of punctuation_to_token new_punct of
        Some(punct) => whitespace_consume(program with {context=P punct::program.context})
        | None => error(program.current_line, "Invalid punctuation: " ^ new_punct)
    end

fun tokenize (input: string): context = let tokens = #context whitespace_consume {input=input, current_line=1, current_char=0, context=[]} in
    List.map (fn token => case token of
        IDENTIFIER id => case of keyword_to_token id of
            Some(keyword) => K keyword
            | None => IDENTIFIER id
        | INTEGER_CONSTANT int => INTEGER_CONSTANT int
        | STRING_CONSTANT str => STRING_CONSTANT str
        | K keyword => K keyword
        | P punctuation => P punctuation
    ) tokens 
end

