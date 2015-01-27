%{
  open ArvSint;;

(* Cria uma expressao onde v é o valor da expressão e o é a ordem na regra gramatical *)
  let cria_exp o v =
    { valor = v;
      tipo = TGen;
      pos = Posicao.pos(o) }
  
(* Cria um comando onde c é o comando e o é a ordem na regra gramatical *)
  let cria_cmd o c =
    { vcmd = c;
      pcmd = Posicao.pos(o) }  

(* Cria um programa onde f sao as funcoes e c os comandos fora das funcoes *)
  let cria_programa f c =
    { funcsP = f;
      cmdsP = c } 

(* Cria uma funcao onde o é a ordem na regra gramatical, i é o nome da funcao, 
	p sao os parametros, t é o tipo de retorno e c sao os comandos dentro da funcao.*)
  let cria_funcao o i p t c =
    { idF = i;
      paramsF = p;
      cmdsF = c;
		returnF = t;
      (*match t with
			TIPOString -> returnF <- TString
      	| TIPOFloat -> returnF <- TFloat
      	| TIPOInt -> returnF <- TInt
      	| TIPOBool -> returnF <- TBool
      	| TIPOVoid -> returnF <- TVoid
      	| _ -> failwith "Tipo invalido na funcao "^idF*)
      posF = Posicao.pos(o);
      varLocaisF = Hashtbl.create 20 }      

(* Cria um parametro onde o é a ordem na regra gramatical e i o nome (id) do parametro.
	O tipo do parametro inicia como TGen *)
  let cria_parametro o i tipo =
    { idP = i;
		tipoP = tipo;
      (*match tipo with
      	TIPOString -> tipoP = TString
      	| TIPOFloat -> tipoP = TFloat
      	| TIPOInt -> tipoP = TInt
      	| TIPOBool -> tipoP = TBool
      	| _ -> failwith "Erro de tipos no parametro nao permitido"^idP*)
      posP = Posicao.pos(o)} 

%}

/* Definicao de tokens */

%token <int> Int
%token <float> Float
%token <bool> Bool
%token <string> Id
%token <string> String
/*%token Void_func, Int_func, Float_func, Bool_func, String_func*/
%token <int * int * token list> Linha
%token Indenta Dedenta NovaLinha
%token Pto PtVirg
%token Def Is From Return Seta
%token True False
%token AParen FParen ACol FCol ACha FCha 
%token If Else While DoisPontos 
%token For In Range Virg 
%token Not And Or
%token Atrib 
%token OpSoma OpSub Mult Div Modulo Pot
%token Maior Menor Igual Diferente MaiorIgual MenorIgual  
%token AtribMais AtribMenos AtribVezes AtribDiv AtribMod
%token EOF
%token Print Input IntParse
%token TIPOVoid TIPOInt TIPOFloat TIPOString TIPOBool   
/* o símbolo inicial da gramática (ponto de entrada) */
%start programa  
%type <ArvSint.programa> programa

%%

/* um programa eh definido por um conjunto de funcoes seguido por um conjunto de comandos */
programa: funcoes comandos EOF{ cria_programa $1 $2 };

/*  */
funcoes: { [] }
         | funcoes funcao { $1 @ [ $2 ] }
         ;

/* define a estrutura de uma funcao e cria a funcao */
funcao: Def Id AParen parametros FParen Seta tipo DoisPontos NovaLinha
        Indenta comandos Dedenta { cria_funcao 1 $2 $4 $7 $11 }; /*Mudar para dar suporte a parametros*/

/* define a estrutura de uma lista de funcoes e um parametro e cria o parametro */
parametros: { [] }
  | parametros parametro { $1 @ [ $2 ] }
  ;

/* um parametro pode estar seguido de virgula ou nao */
parametro: Id DoisPontos tipo Virg { cria_parametro 1 $1 $3} /*Mudar a cria parametro para suportar tipos*/
  		 | Id DoisPontos tipo { cria_parametro 1 $1 $3};

tipo: 
	TIPOString {TString}
	| TIPOInt {TInt}
	| TIPOFloat {TFloat}
	| TIPOBool {TBool}
	| TIPOVoid {TVoid}
	;


argumentos: { [] }
  | argumentos argumento { $1 @ [ $2 ] }
  ;

/*A diferenca entre o argumento e parametro eh que o argumento eh passado na chamada da funcao, e pode ser uma expressao, paramentros 
sao apenas valores atomicos*/
/* um argumento pode estar seguido de virgula ou nao */
argumento: expressao Virg { cria_exp 2 $1.valor}
	   | expressao { cria_exp 2 $1.valor }
	   ;

/* a chamada de funcao pode ser ou nao uma atribuicao */
cmd_chamada_func: Id AParen argumentos FParen NovaLinha { cria_cmd 1 (ChamaFuncaoVoid ($1, $3))};
	             | expressao Atrib Id AParen argumentos FParen NovaLinha { cria_cmd 1 (ChamaFuncaoAtrib ($1, $3, $5))}

/* tipos de comandos */
comando: cmd_atrib  { $1 }
		| cmd_if_else { $1 }
		| cmd_if { $1 }
		| cmd_while { $1 }
		| cmd_for { $1 }
		| cmd_range1 { $1 }
		| cmd_range2 { $1 }
		| cmd_range3 { $1 }
		| cmd_atribMAIS { $1 }
		| cmd_atribMENOS { $1 }
		| cmd_atribVEZES { $1 }
		| cmd_atribDIV { $1 }
		| cmd_atribMOD { $1 }
		| cmd_chamada_func { $1 }
		| cmd_retorno { $1 }
		| cmd_print { $1 }
		| cmd_input { $1 }
		| cmd_int_parse { $1 }
        ;

/* definicao da estrutura dos comandos */
comandos: { [] }
  		  | comandos comando { $1 @ [ $2 ] }
  		  ;

cmd_int_parse: expressao Atrib IntParse AParen expressao FParen NovaLinha 
	{ cria_cmd 1 ( CmdIntParse ( $1, $5 ))}

cmd_print: Print AParen expressao FParen NovaLinha 
    { cria_cmd 1 ( CmdPrint( $3 ) )}

cmd_input: expressao Atrib Input AParen expressao FParen NovaLinha 
	{ cria_cmd 1 ( CmdInput ( $1, $5 ) )}

cmd_retorno: Return expressao NovaLinha 
	{ cria_cmd 1 ( CmdReturn ( $2 ) ) }

cmd_atrib: expressao Atrib expressao NovaLinha 
	{ cria_cmd 2 ( CmdAtrib ( $1, $3 ) ) }

cmd_if: If expressao DoisPontos NovaLinha Indenta comandos Dedenta
	{ cria_cmd 1 ( CmdIf ( $2, $6, None ) ) }

cmd_if_else: If expressao DoisPontos NovaLinha Indenta comandos Dedenta
	     	 Else DoisPontos NovaLinha Indenta comandos Dedenta 
	{ cria_cmd 1 ( CmdIf ( $2, $6, Some( $12 ) ) ) }

cmd_while: While expressao DoisPontos NovaLinha Indenta comandos Dedenta
	{ cria_cmd 1 ( CmdWhile ( $2, $6 ) ) }

cmd_for: For expressao In comando DoisPontos NovaLinha Indenta comandos Dedenta
	{ cria_cmd 1 ( CmdFor ( $2, $4, $8 ) ) }


/* se o comando range tiver um parametro a range varia de 0 ao parametro com incremento 1*/
cmd_range1: Range AParen Int FParen
	{ cria_cmd 1 ( CmdRange ( 0, $3, 1 ) ) }

/* se o comando range tiver dois parametros a range varia entre os dois parametros com incremento 1*/
cmd_range2: Range AParen Int Virg Int FParen
	{ cria_cmd 1 ( CmdRange ( $3, $5, 1) ) }

/* se o comando range tiver tres parametros os tres sao passados para o comando */
cmd_range3: Range AParen Int Virg Int Virg Int FParen
	{ cria_cmd 1 ( CmdRange ( $3, $5, $7 ) ) }

/* para comandos +=, -= cria uma expressao e coloca na atribuicao */
cmd_atribMAIS: expressao AtribMais expressao 
	{ let exp = cria_exp 2 ( ExpBin ( Mais, $1, $3 ) ) in
    	      cria_cmd 2 ( CmdAtrib( $1, exp ) ) }

cmd_atribMENOS: expressao AtribMenos expressao 
	{ let exp = cria_exp 2 ( ExpBin ( Menos, $1, $3 ) ) in
    		    cria_cmd 2 ( CmdAtrib ( $1, exp ) ) }

cmd_atribVEZES: expressao AtribVezes expressao 
	{ let exp = cria_exp 2 ( ExpBin ( Mult, $1, $3 ) ) in
				cria_cmd 2 ( CmdAtrib ( $1, exp ) ) }

cmd_atribDIV: expressao AtribDiv expressao 
	{ let exp = cria_exp 2 ( ExpBin ( Div, $1, $3 ) ) in
   				cria_cmd 2 ( CmdAtrib( $1, exp ) ) }

cmd_atribMOD: expressao AtribMod expressao 
	{ let exp = cria_exp 2 ( ExpBin ( Modulo, $1, $3 ) ) in
    			cria_cmd 2 ( CmdAtrib( $1, exp ) ) }

/* criando expressoes */

expressao : expressao And expr1 {cria_exp 5 (ExpBin (And, $1, $3))}
			|expressao Or expr1 {cria_exp 5 (ExpBin (Or, $1, $3))}
			| expr1 {$1}
			;

expr1 : expr1 Maior expr2 { cria_exp 4 ( ExpBin ( Maior, $1, $3 ) ) }
	 | expr1 Menor expr2 { cria_exp 4 ( ExpBin ( Menor, $1, $3 ) ) }
	 | expr1 Igual expr2 { cria_exp 4 ( ExpBin ( Igual, $1, $3 ) ) }
	 | expr1 Diferente expr2 { cria_exp 4 ( ExpBin ( Diferente, $1, $3 ) ) }
	 | expr1 MaiorIgual expr2 { cria_exp 4 ( ExpBin ( MaiorIgual, $1, $3 ) ) }
	 | expr1 MenorIgual expr2 { cria_exp 4 ( ExpBin ( MenorIgual, $1, $3 ) ) }
	 | expr1 Modulo expr2 { cria_exp 4 ( ExpBin ( Modulo, $1, $3 ) ) }
	 | expr2 { $1 }

expr2 : expr2 OpSoma expr3 { cria_exp 3 ( ExpBin ( Mais, $1, $3 ) ) } 
		  | expr2 OpSub expr3 { cria_exp 3 ( ExpBin ( Menos, $1, $3 ) ) } 
		  | expr3 { $1 }
          ;

expr3: expr3 Mult expr4 { cria_exp 2 ( ExpBin ( Mult, $1, $3 ) ) }
 	 | expr3 Div expr4 { cria_exp 2 ( ExpBin ( Div, $1, $3 ) ) }
	 | expr4 { $1 }
	 ;

expr4: Not expr4 {cria_exp 1 (ExpUn(Not, $2))}
	 | expr5 {$1}
	 ;

expr5: operando  { cria_exp 0 $1 }
     | variavel  { cria_exp 0 ( ExpVar $1 ) }
     | AParen expressao FParen { $2 }
     ;

/* criando operando e variavel */
operando: Int   { ExpInt $1 }
	    | Float { ExpFloat $1 }
	    | String { ExpString $1 }
	    | Bool { ExpBool $1 }
        ;

variavel: Id { VarSimples $1 }
        ;

