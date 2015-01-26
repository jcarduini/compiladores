open Sintatico
open Lexico
open Printf
(* Pre processa o arquivo gerando os tokens de indenta e dedenta *)
let preprocessa lexbuf =
	let pilha = Stack.create ()
	and npar = ref 0 in
		let _ = Stack.push 0 pilha in
		let off_side toks nivel =
		if !npar != 0 (* nova linha entre parenteses *)
		then toks(* nao faz nada *)
		else if nivel > Stack.top pilha
				then begin
				Stack.push nivel pilha;
				Indenta :: toks
			end
		else if nivel = Stack.top pilha
			then toks
		else begin
			let prefixo = ref toks in
			while nivel < Stack.top pilha do
				ignore (Stack.pop pilha);
				if nivel > Stack.top pilha
					then failwith "Erro de indentacao"
				else prefixo := Dedenta :: !prefixo
		done;
		!prefixo
		end
		
	in
	
	let rec dedenta sufixo =
		if Stack.top pilha != 0
			then let _ = Stack.pop pilha in
			dedenta (Dedenta :: sufixo)
		else sufixo
	in
	let rec get_tokens () =
		let tok = Lexico.preprocessador 0 lexbuf in
		match tok with
		Linha(nivel,npars,toks) ->
			let new_toks = off_side toks nivel in
			npar := npars;
			new_toks @ (if npars = 0 then NovaLinha :: get_tokens () else get_tokens ())
			| _ -> dedenta []
			in get_tokens ()
	
(* Imprime na tela cada token gerado *)
let print_tok tok =
	match tok with
	| Int i -> printf "INT %d\n" i
	| Float f -> printf "FLOAT %f\n" f
	| Id s -> printf "Id %s\n" s
	| String s -> printf "STRING %s\n" s
	| Linha (i, p, toks) -> printf "LINHA %d, %d, \n" i p
	| OpSoma -> print_endline("MAIS")
	| OpSub -> print_endline("MENOS")
	| Mult -> print_endline("VEZES")
	| Div -> print_endline("DIVIDIDO")
	| Pot -> print_endline("POT")
	| Maior -> print_endline("MAIOR")
	| Menor -> print_endline("MENOR")
	| Igual -> print_endline("IGUAL")
	| Diferente -> print_endline("DIFERENTE")
	| MaiorIgual -> print_endline("MAIORIGUAL")
	| MenorIgual -> print_endline("MENORIGUAL")
	| AParen -> print_endline("APAR")
	| FParen -> print_endline("FPAR")
	| ACol -> print_endline("ACOL")
	| FCol -> print_endline("FCOL")
	| ACha -> print_endline("ACHA")
	| FCha -> print_endline("FCHA")
	| DoisPontos -> print_endline("DPTOS")
	| Pto -> print_endline("PTO")
	| PtVirg -> print_endline("PTVIRG")
	| If -> print_endline("IF")
	| Else -> print_endline("ELSE")
	| While -> print_endline("WHILE")
	| For -> print_endline("FOR")
	| In -> print_endline("IN")
	| Range -> print_endline("RANGE")
	| Virg -> print_endline("VIRG")
	| Def -> print_endline("DEF")
	| Return -> print_endline("RETURN")
	| Indenta -> print_endline("INDENTA")
	| Dedenta -> print_endline("DEDENTA")
	| Not -> print_endline("NOT")
	| True -> print_endline("TRUE")
	| False -> print_endline("FALSE")
	| Atrib -> print_endline("ATRIB")
	| AtribMais -> print_endline("ATRIBMAIS")
	| AtribMenos -> print_endline("ATRIBMENOS")
	| AtribVezes -> print_endline("ATRIBVEZES")
	| AtribDiv -> print_endline("ATRIBDIV")
	| AtribMod -> print_endline("ATRIBMOD")
	| EOF -> print_endline("EOF")
	| And -> print_endline("AND")
	| Or -> print_endline("OR")
	| Is -> print_endline("IS")
	| From -> print_endline "FROM"
	| NovaLinha -> print_endline "NOVALINHA"
	| Modulo -> print_endline "MODULO"
	| Bool _ -> print_endline "BOOL"
	| Print -> print_endline "PRINT"
	| Input -> print_endline "INPUT"
	| IntParse -> print_endline "INT_PARSE"
	| _ -> print_endline "outra coisa"

	
(* Chama o analisador lexico *)
let lexico =
		let tokbuf = ref None in
			let carrega lexbuf =
				let toks = preprocessa lexbuf in
				(match toks with
				tok::toks ->
					tokbuf := Some toks;
					print_tok tok;
					tok
			| [] -> print_endline "EOF";
					EOF)
	in
	fun lexbuf ->
	match !tokbuf with
	Some tokens ->
		(match tokens with
		tok::toks ->
			tokbuf := Some toks;
			print_tok tok;
			tok
		| [] -> carrega lexbuf)
	| None -> carrega lexbuf	
