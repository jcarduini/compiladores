open Temporarios

type exp =  CONST of int
	| NAME of Label
	| TEMP of Temp
	| BINOP of binop * exp * exp
	| MEM of exp
	| CALL of exp * exp list 
	| ESEQ of stm * exp

and stm = MOVE of exp * exp
	| EXP of exp
	| JUMP of exp * Label list 
	| CJUMP of relop * exp * exp * Label * Label
	| SEQ of stm * stm 
	| LABEL of Label 

and binop = PLUS 
	  | MINUS
	  | MUL
	  | DIV
	  | AND
	  | OR
	  | LSHIFT
	  | RSHIFT

and relop = EQ 
	  | NE
	  | LT
	  | GT
	  | LE
	  | GE


