open ArvSint;;
open Printf;;
open Scanf;;
open ArvRi;;
open Temporarios;;
(*traduz uma expressao e retorna um tipo expr*)
let rec traduz_exp amb amb_ri expr = 
    match expr.valor with
    ExpInt i      -> CONSTI i
    | ExpFloat f  -> CONSTF f
    (*| ExpString s -> ExpString s (* o que fazer com string??*)*)
    | ExpBool b   -> CONSTI b
    | ExpVar (VarSimples v)-> 
        let entrada = Hashtbl.find amb v in 
        (match entrada with
            | EntVar var -> 
                let entrada_ri = Hashtbl.find amb_ri entrada in
                    match entrada_ri with
                    | Temp t -> Temp t
                    | _ ->  let novo_temp = new_temp () in
                            Hashtbl.add amb_ri entrada novo_temp;
                            novo_temp
            | _ -> failwith "Erro variavel");

    | ExpUn (not,expressao) -> 
        let traduz_e = traduz_exp expressao in
        UNOP traduz_e

    (*| ExpBin (op,e1,e2) ->
		let arg1 = traduz_exp amb e1  in 
        let arg2 = traduz_exp amb e2  in
        (match op with
            | Mais -> BINOP PLUS arg1 arg2 
            | Menos -> BINOP MINUS arg1 arg2 
            | Mult -> BINOP MUL arg1 arg2 
            | Div -> BINOP DIV arg1 arg2 
            | And ->  BINOP AND arg1 arg2 
            | Or -> BINOP OR arg1 arg2 
            | Maior -> RELOP GT arg1 arg2  
            | Menor -> RELOP LT arg1 arg2  
            | Igual -> RELOP EQ arg1 arg2  
            | MaiorIgual -> RELOP GE arg1 arg2  
            | MenorIgual -> RELOP LE arg1 arg2 );*)

and get_label nome amb amb_ri = 
    let entrada = Hashtbl.find amb nome in
    match entrada with
        EntFun f -> 
            try
                let label = Hashtbl.find amb_ri (EntFun f) in
                label
            with     
                Not_found -> 
                    let label_fun = new_label () in
                     Hashtbl.add amb_ri (EntFun f) label_fun
                     label_fun

and get_temp amb amb_ri e1 =
    let entrada = Hashtbl.find amb nome in
    match entrada with
        EntVar f -> 
            try
                let temp = Hashtbl.find amb_ri (EntVar f) in
                temp
            with     
                Not_found -> 
                    let temp = new_temp () in
                     Hashtbl.add amb_ri (EntVar f) temp
                     temp

and traduz_arg arg = 

and traduz_args args = 
    match args with
    [] -> ignore()
    | arg::args -> traduz_arg arg @ traduz_args args

(*traduz um comando*)
and traduz_cmd amb amb_ri cmd funcs =

    match cmd.vcmd with

    (*ChamaFuncaoAtrib (exp, nomeFunc, arg) -> 
        let func = procuraFunc nomeFunc funcs in
		let vexp = traduz_func func arg funcs in
        (match vexp with
            None -> failwith "Funcao sem retorno"
            | Some x -> atribui_var exp x amb);*)
		
    | ChamaFuncaoVoid (nomeFunc, arg) -> 
        let trad_args = traduz_args arg in
        let label_func = get_label nomeFunc amb amb_ri in 
        CALL label_func trad_args

    (*| CmdPrint(e) -> imprime_exp e amb*)

    (*| CmdInput(e1,e2) -> 
        imprime_exp e2 amb;
        flush_all();
        Scanf.scanf "%s\n" (fun s -> 
            let expressao = {
                tipo = TString;
                valor = ExpString s;
                pos = e1.pos
        } in
        atribui e1 expressao amb;
    )*)

	(*| CmdIntParse (e1, e2) -> *)
    							  
    
    | CmdAtrib(e1,e2) ->
        let trad_e2 = traduz_exp e2 amb amb_ri in
        let temp1  = get_temp amb amb_ri e1 in
        MOVE temp1 trad_e2

                
                
    | CmdIf(e, ce, cs) -> 
        let (op,esq,dir) = retorna_relop e in(*retorna a relop referente a expressao e*)
        let trad_ce = traduz_cmd amb ce funcs
        let trad_cs = traduz_cmd amb cs funcs
        let exp_dir = traduz_exp amb dir in
        let exp_esq = traduz_exp amb esq in
        let labelV = new_label () in
        let labelF = new_label () in
        let exit = new_label () in
		ESEQ((SEQ (CJUMP op exp_esq exp_dir labelV labelF) 
                  (SEQ  labelV
                        SEQ (trad_ce (*traduz bloco de comandos e insere na arvore??*)
                             SEQ JUMP (
                                 SEQ labelF
                                       exit)))))

    | CmdWhile (e, cs) -> 
         let (relop,esq,dir) = retorna_relop e in(*retorna a relop referente a expressao e*)
         let exit = new_label () in
         let labelV = new_label () in
         let trad_e = traduz_exp e
         let trad_cs = traduz_cmd amb cs funcs in
         let labelTeste = new_label () in
         ESEQ(labelTeste 
              SEQ (CJUMP EQ trad_e (CONST 1) labelV exit
                    (SEQ labelV 
                      SEQ trad_cs
                          JUMP labelTeste)))


         while(b) {c}
         CJUMP (=, Traducao_b, Const(1),continua,feito)                   
         continua:
            Traduz_c 
            Jump teste
            feito 

   (* | CmdFor (v, range, cmds) -> *)


    (*| CmdReturn (e) -> seta_retorno traduz_exp amb e*)

    | _ -> failwith "Erro de interpretação"


let rec tradutor_ri amb arv funcs = 
  (*imprime_funcs funcs;*)
  let amb_ri = Hashtbl.create 20
  match arv with
    [] -> Printf.printf "\nFim execucao\n"
  | cmd :: cmds -> traduz_cmd amb amb_ri cmd funcs;
   interpretador amb cmds funcs
