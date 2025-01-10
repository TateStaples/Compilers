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

datatype TOKEN = IDENTIFIER of string | INTEGER_CONSTANT of int | STRING_CONSTANT of STRING | K of KEYWORD | P of PUNCTUATION
and KEYWORD = ARRAY | BREAK | DO | ELSE | END | FOR | FUNCTION | IF | IN | LET | NIL | OF | THEN | TO | TYPE | VAR | WHILE
and PUNCTUATION = COMMA | COLON | SEMICOLON | LPAREN | RPAREN | LBRACKET | RBRACKET | LBRACE | RBRACE | PERIOD | PLUS | MINUS | TIMES | DIV | EQUAL | NOT_EQUAL | LESS_THAN | GREATER_THAN | LESS_THAN_EQUAL | GREATER_THAN_EQUAL | AND | OR | ASSIGN

datatype PROGRAM_STRING = STRING_LITERAL of string | COMMENT of string | PROGRAM of string

fun isolate_string_literal (str: string) (start: int): string * string * string = (* find the index of the " that closes the string and not escape characters *)
    if String.index str start <> SOME "\"" then raise Fail "String literal not closed" else
    let rec find_closing_quote (str: string) (index: int): int = 
        if index = String.size str then raise Fail "String literal not closed"
        else if String.sub str index = #"\"" then index
        else if String.sub str index = #"\" then find_closing_quote str (index + 2)
        else find_closing_quote str (index + 1)
    in
    let closing_quote_index = find_closing_quote str (start + 1) in
    (String.substring (str, start, closing_quote_index + 1), String.substring (str, 0, start), String.substring (str, closing_quote_index + 1, String.size str))
    end
fun isolate_sections (input: string): PROGRAM_STRING list = 
    let rec isolate_sections_helper (input: string) (acc: PROGRAM_STRING list): PROGRAM_STRING list = 
        if size input = 0 then acc
        else 
            let quote_index: int option = findSubstring input "\"" in
            if quote_index = NONE then 
                let comment_index: int option = findSubstring input "(*" in
                if comment_index = NONE then 
                    let (section, rest) = splitAt input " " in
                    isolate_sections_helper rest (acc @ [PROGRAM section])
                else 
                    let (comment, rest) = splitAt input "*)" in
                    isolate_sections_helper rest (acc @ [COMMENT comment])
            else 
                let (string, rest) = splitAt input "\"" in
                isolate_sections_helper rest (acc @ [STRING_LITERAL string])
    in
    isolate_sections_helper input []
fun tokenize (input: string): TOKEN list = 
    let quote_index: int option = findSubstring input "\"" 

