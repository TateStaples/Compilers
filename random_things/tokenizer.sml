datatype ID = string

datatype TYPE_ID = string

datatype EXPR = NIL
| INTEGER of int
| STRING of string
| FUNCTION of LVALUE
| FUNCTION_CALL of ID * EXPR_LIST option
| ASSIGN of LVALUE * EXPR
| UNARY of UNARY_OP * EXPR
| BINARY of EXPR * BINARY_OP * EXPR
| IF_THEN of EXPR * EXPR
| IF_THEN_ELSE of EXPR * EXPR * EXPR
| WHILE of EXPR * EXPR
| FOR of ID * EXPR * EXPR * EXPR
| BREAK
| ARRAY of TYPE_ID * EXPR (* assign an array by aliasing not copying *)
| RECORD of FIELD_LIST
| LET_BINDING of DECLARATION list * EXPR_SEQ
and datatype LVALUE = FUNC_NAME of ID | IMPLICIT of LVALUE * ID | EXPLICIT of LVALUE * EXPR (* l-value represents a storage location that can be assigned to *)
and datatype UNARY_OP = NEGATE
and datatype BINARY_OP = PLUS | MINUS | TIMES | DIV | EQUAL | NOT_EQUAL | LESS_THAN | GREATER_THAN | LESS_THAN_EQUAL | GREATER_THAN_EQUAL | AND | OR
and EXPR_LIST = EXPR list (* expr-list ::= expr | expr , expr-list *)
and EXPR_SEQ = EXPR list (* expr-seq ::= expr | expr ; expr-seq *)
and FIELD_LIST = TODO (* TODO *)
and DECLARATION = TYPE_DECLARATION | VARIALBE_DECLARATION | FUNCTION_DECLARATION
and TYPE_DECLARATION = TYPE_ID * TYPE
and TYPE = TYPE_ID | TYPE_FIELDS option | ARRAY of TYPE | INTEGER | STRING
and TYPE_FIELD = ID * TYPE_ID
and TYPE_FIELDS = TYPE_FIELD list
and VARIALBE_DECLARATION = ID * TYPE_ID * EXPR | ID * EXPR
and FUNCTION_DECLARATION = ID * TYPE_FIELDS option * EXPR | ID * TYPE_FIELDS option * TYPE_ID * EXPR