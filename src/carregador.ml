(* arquivo carregador.ml *)

#load "arvSint.cmo";;
#load "semantico.cmo";;
#load "sintatico.cmo";;
#load "lexico.cmo";;

(* do arquivo TlexIdenta.ml *)
#load "pre_processador.cmo";;

open ArvSint;;
open Semantico;;
open Printf;;



let sintatico lexbuf =
  try
    Sintatico.programa Pre_processador.lexico lexbuf
  with exn ->
    begin
      let tok  = Lexing.lexeme lexbuf in
      let pos  = lexbuf.Lexing.lex_curr_p in
      let nlin = pos.Lexing.pos_lnum in
      let ncol = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - String.length tok in
      let msg1 = sprintf "Erro na linha %d, coluna %d" nlin ncol in
      let msg2 = sprintf "\tA palavra \"%s\" nao era esperada aqui." tok in
        print_endline msg1;
        print_endline msg2;
        flush stdout;
        raise exn
    end

(*Gera a arvore sintatica*)
let arv_str arq = 
	let ic = open_in arq in
	let lexbuf = Lexing.from_channel ic in
	sintatico lexbuf


(*Gera a arvore semantica*)
let arvtab_str arq = 
	let ic = open_in arq in
	let lexbuf = Lexing.from_channel ic in
	let arvSint = sintatico lexbuf in
	let _ = close_in ic in
	let tabSimb = semantico arvSint in
	(arvSint,tabSimb)
