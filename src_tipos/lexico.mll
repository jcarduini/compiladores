(* Para compilar digite:
ocamlyacc -v sintatico.mly
ocamlc -c sintatico.mli
ocamllex lexico.mll
ocamlc -c lexico.ml
ocamlc -c sintatico.ml
 *)
 
{
    open Sintatico   (* o tipo token eh definido em sintatico.mli *)
    open Lexing
    open Printf

	(* contador de nivel de parentizacao - utilizado para identacao *)
    let nivel_par = ref 0

    (* incrementa a contagem de linhas *)
    let incr_nlinha lexbuf =
        let pos = lexbuf.lex_curr_p in
                lexbuf.lex_curr_p <- { pos with
                                       pos_lnum = pos.pos_lnum + 1;
                                       pos_bol = pos.pos_cnum;
                                     }
    (* imprime mensagem de erro *)
    let msg_erro lexbuf c =
        let pos = lexbuf.lex_curr_p in
            let lin = pos.pos_lnum
            and col = pos.pos_cnum - pos.pos_bol - 1 in
            sprintf "%d-%d: Caracter Desconhecido %c" lin col c

    (* cria tabela hasg  *)
    let cria_tab_hash iniciais =
        let tbl = Hashtbl.create (List.length iniciais) in
            List.iter (fun (chave, valor) -> Hashtbl.add tbl chave valor) iniciais;
            tbl

    (* palavras reservadas *)
    let plv_res =
        cria_tab_hash 
        [
        ("def",   Def);
        ("else",  Else );
        ("for",   For);
        ("if",    If);
        ("in",    In);
        ("not",   Not);
        ("and",   And);
        ("or",    Or); 
        ("is",    Is);
        ("from",  From);
        ("return",Return);
        ("while", While);
        ("range", Range);
        ("print", Print);
        (*("int", Int);
        ("None", Void);
        ("str",String);
        ("float",Float);
        ("bool",Bool);*)
        ("raw_input", Input);
        ("int_parse",   IntParse)
        ]

(* Valores booleanos sao armazenados como 1 para true e 0 para false. *)
(* Operacoes com booleanos sao transformadas em operacoes com inteiros *)
    let booleano nbool =
    match nbool with
    | "True" -> 1
    | "False" -> 0  
    | _ -> failwith "Erro: nao eh valor booleano"
}

(* definicoes *)
let digito = ['0' - '9']
let letra = ['a'-'z' 'A'-'Z']
let id  = letra ( letra | digito | '_' )*
let comentario = '#' [^ '\n']*
let linha_em_branco = [' ' '\t' ]* comentario
let restante = [^ ' ' '\t' '\n' ] [^ '\n']+
let boolean = "True" | "False"
let strings = '"' id* digito* '"' | "'" id* digito* "'"
let floats = digito+ '.' digito+
let neg = '-' digito+

(* regras para identificar identacao para gerar tokens de abre e fecha escopo *)
rule preprocessador indentacao = parse
  linha_em_branco     { preprocessador 0 lexbuf } (* ignora brancos *)
| [' ' '\t' ]+ '\n'   { incr_nlinha lexbuf; 
                        preprocessador 0 lexbuf } (* ignora brancos *)
| ' '                 { preprocessador (indentacao + 1) lexbuf }
| '\t'                { let nova_ind = indentacao + 8 - (indentacao mod 8) in
                        preprocessador nova_ind lexbuf }
| '\n'                { incr_nlinha lexbuf;
                        preprocessador 0 lexbuf }
| eof                 { nivel_par := 0; EOF  }   
| restante as linha   {  
		  let rec tokenize lexbuf =
		    let tok = token lexbuf in
		    match tok with
		      EOF -> []
		    | _ -> tok :: tokenize lexbuf in
		  let toks = tokenize (Lexing.from_string linha) in
		  Linha(indentacao,!nivel_par, toks) 
  } 

(* identificacao dos tokens *)
and token  = parse
| ' '                 
| '\t'
| comentario          { token lexbuf }
| digito+ as numint   { let num = int_of_string numint in Int num }
| neg as numNeg       { let num = int_of_string numNeg in Int num }
| digito+ '.' digito+ as numfloat {let num = float_of_string numfloat in Float num}
| boolean as nbool    { Int (booleano nbool)}
| floats as numfloat  { Float (float_of_string numfloat) }
| id as palavra       { try Hashtbl.find plv_res palavra
                        with Not_found -> Id (palavra)}
| '"'                 { let buffer = Buffer.create 1 in 
                        String (cadeia buffer lexbuf) }
| '='                { Atrib  }
| '+'                 { OpSoma }
| '-'                 {OpSub  }
| '/'				  {  Div}
| '*'				  {  Mult}
| ':'				  { DoisPontos}
| '('                 { AParen }
| ')'                 { FParen }
| ';'                 { PtVirg }
| ','                 { Virg   }
| "=="				  { Igual}

| '>'				  {  Maior}
| '<'				  { Menor}
| ">="				  {  MaiorIgual}
| "<="				  {  MenorIgual}
| "!="				  { Diferente}
| "and"				  {  And}
| "or"				  { Or}
| "if"				  {  If}
| "else"		      {  Else}
| "not"				  { Not}
| "int"               {Int}
| "float"             {Float}
| "str"               {String}
| "bool"              {Bool}
| "None"              {Void}
| "->"                {Seta} (* tipo funcao *)
| "True"			  {  True}	
| "False"			  {  False}
| "for"				  {  For}
| "while"				  { While}
| "in"				  { In}
| "range"				  { Range}
| "int_parse"         {IntParse}
| "print"             {  Print  }
| "+="  	          { AtribMais}
| "-="  	          { AtribMenos}
| "*="  	          { AtribVezes}
| "/="  	          { AtribDiv}
| "%="  	          { AtribMod}
| "%"		          { Modulo }
| '('                 { incr(nivel_par); AParen }
| '['                 { incr(nivel_par); ACol }
| '{'                 { incr(nivel_par); ACha }
| ')'                 { decr(nivel_par); FParen }
| ']'                 { decr(nivel_par); FCol }
| '}'                 { decr(nivel_par); FCha }
| ':'                 { DoisPontos }
| ','		          { Virg }	
| ';'		          { PtVirg }
| '.'		          { Pto }
| eof                 {  EOF    }   
| _ as c              { failwith (msg_erro lexbuf c); }


(* para criar cadeias de strings *)
and cadeia buffer = parse
 | '"'        { Buffer.contents buffer }
 | "\\t"      { Buffer.add_char buffer '\t'; cadeia buffer lexbuf }
 | "\\n"      { Buffer.add_char buffer '\n'; cadeia buffer lexbuf }
 | '\\' '"'   { Buffer.add_char buffer '"';  cadeia buffer lexbuf }
 | '\\' '\\'  { Buffer.add_char buffer '\\'; cadeia buffer lexbuf }
 | eof        { failwith "string não foi fechada" }
 | _ as c     { Buffer.add_char buffer c;    cadeia buffer lexbuf }
 
