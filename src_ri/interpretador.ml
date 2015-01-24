open ArvSint;;
open Printf;;
open Scanf;;

(*avalia uma expressao e retorna um tipo expr*)
let rec avalia_exp amb expr = 
    match expr.valor with
    ExpInt i      -> ExpInt i
    | ExpFloat f  -> ExpFloat f
    | ExpString s -> ExpString s
    | ExpBool b   -> ExpBool b
    | ExpGen -> ExpGen 
    | ExpVar (VarSimples v)-> 
        let entrada = Hashtbl.find amb v in 
        (match entrada with
            | EntVar var -> var.v_inicial.valor;
            | _ -> failwith "Erro variavel");

    | ExpUn (not,expressao) -> 
        let expunaria = avalia_exp amb expressao in
        (match expunaria with
            | ExpBool true -> ExpBool false
            | ExpBool false -> ExpBool true            
            | _ -> failwith "Boolean usado incorretamente");
    | ExpBin (op,e1,e2) ->
		let arg1 = avalia_exp amb e1  in 
        let arg2 = avalia_exp amb e2  in
        (match (arg1,arg2) with
            | (ExpFloat f1,ExpFloat f2) -> 
                (match op with
                    | Maior -> ExpBool (f1 > f2)
                    | Menor -> ExpBool (f1 < f2)
                    | Igual -> ExpBool (f1 == f2)
                    | MaiorIgual -> ExpBool (f1 >= f2)
                    | MenorIgual -> ExpBool (f1 <= f2)
                    | _ -> ExpFloat (executa_op_float op f1 f2));
                    
            | (ExpInt i1, ExpInt i2) -> 
                (match op with
                    | Maior -> ExpBool (i1 > i2)
                    | Menor -> ExpBool (i1 < i2)
                    | Igual -> ExpBool (i1 == i2)
                    | MaiorIgual -> ExpBool (i1 >= i2)
                    | MenorIgual -> ExpBool (i1 <= i2)
                    | _ -> ExpInt (executa_op_int op i1 i2));
                    
            | (ExpBool b1, ExpBool b2) -> ExpBool (executa_op_bool op b1 b2)
            | _ -> failwith "Expressao binaria invalida");
    (*| _ -> ExpGen*)

and executa_op_bool op a b =
    match op with
    | And -> a && b
    | Or -> a || b
    | _ -> failwith "Operacao logica invalida"
and executa_op_int op a b =
    match op with
    | Mais -> a + b
    | Menos -> a - b
    | Div -> a/b
    | Mult -> a*b
    | _ -> failwith "Operacao com inteiro invalida"

and executa_op_float op a b =
        match op with 
        | Mais -> a +. b
        | Menos -> a -. b
        | Div -> a /. b
        | Mult -> a *. b            
        | _ -> failwith "Operacao com float invalida"


(*avalia um bloco de comandos*)
and avalia_bloco amb cmds funcs =
  match cmds with
    [] ->  ignore()
  | cmd :: cmds -> avalia amb cmd funcs; avalia_bloco amb cmds funcs  

(*imprime uma expressao*)
and imprime_exp e amb = 
    let c = avalia_exp amb e in
    (match c with
       | ExpInt i -> Printf.printf "\n%d \n(inteiro)" i
       | ExpFloat f -> Printf.printf "\n%f \n(float)" f
       | ExpString s -> Printf.printf "\n%s \n(string)" s
       | ExpBool true -> Printf.printf "\nTrue \n(bool)"
       | ExpBool false -> Printf.printf "\nFalse \n(bool)"
       | _ -> Printf.printf "\nImpressao invalida"); 

(*faz a atribuicao do valor de exp2 em exp1, que deve ser variavel*)
and atribui e1 e2 amb =
    let c = avalia_exp amb e2 in
        (match e1.valor with
            | ExpVar (VarSimples v)-> let entrada = Hashtbl.find amb v in
                    (match entrada with
                       | EntVar var -> 
                            let expressao = {
                                (*tipo = var.v_inicial.tipo;*)
                                tipo = e2.tipo;
                                valor = c;
                                pos = var.v_inicial.pos
                                } in
                                var.v_inicial <- expressao;
                                | _ -> failwith "Esperava variavel" );                     
        |_ -> failwith "Erro na atribuicao da conversao: Esperava variavel");

and procuraFunc nome funcs = 
  match  funcs with
  | [] -> failwith "Funcao nao encontrada"
  | func :: funcs -> 
    if(func.idF <> nome) then
        procuraFunc nome funcs
    else
        func

(*and seta_parametros_func amb func arg = 
    let func = Hashtbl.find func.idF amb in
    let var_locais = func.varLocais in
    seta_parametros var_locais arg

and seta_parametros var_locais args = 
    match args with
    | [] -> ignore()
    | arg::args ->
        ()*)

and avalia_func func arg funcs =
    (*seta_parametros_func amb func.idF arg;*)
    let comandos = func.cmdsF in
    avalia_comandos_func func.varLocaisF comandos funcs


and avalia_comandos_func amb comandos func= 
    match comandos with
    [] -> None
    | cmd::comandos ->
        (match cmd.vcmd with
            |  CmdReturn (e) -> Some(avalia_exp amb e)
            | _ -> avalia amb cmd func;
                   avalia_comandos_func amb comandos func);

and atribui_var e1 x amb = 
    (match e1.valor with
            | ExpVar (VarSimples v)-> let entrada = Hashtbl.find amb v in
                    (match entrada with
                       | EntVar var -> 
                            let expressao = {
                                (*tipo = var.v_inicial.tipo;*)
                                tipo = e1.tipo;
                                valor = x;
                                pos = var.v_inicial.pos
                                } in
                                var.v_inicial <- expressao;
                                | _ -> failwith "Esperava variavel" );                     
        |_ -> failwith "Erro na atribuicao da conversao: Esperava variavel");


(*avalia um comando*)
and avalia amb cmd funcs =
    match cmd.vcmd with
    ChamaFuncaoAtrib (exp, nomeFunc, arg) -> 
        let func = procuraFunc nomeFunc funcs in
		let vexp = avalia_func func arg funcs in
        (match vexp with
            None -> failwith "Funcao sem retorno"
            | Some x -> atribui_var exp x amb);
		
    | ChamaFuncaoVoid (nomeFunc, arg) -> 
                let func = procuraFunc nomeFunc funcs in
                ignore(avalia_func func arg funcs)

    | CmdPrint(e) -> imprime_exp e amb

    | CmdInput(e1,e2) -> 
        imprime_exp e2 amb;
        flush_all();
        Scanf.scanf "%s\n" (fun s -> 
            let expressao = {
                tipo = TString;
                valor = ExpString s;
                pos = e1.pos
        } in
        atribui e1 expressao amb;
    )

	| CmdIntParse (e1, e2) -> 
        let c  = avalia_exp amb e2  in
        (match c with
            ExpString s ->
             let i = int_of_string s in
             let expres = {
                tipo = TInt;
                valor = (ExpInt i);
                pos = e1.pos
            } in
            atribui e1 expres amb
        | _ -> failwith "Nao eh string, erro na conversao para inteiro");
        							  
    
    | CmdAtrib(e1,e2) -> atribui e1 e2 amb
                
                
    | CmdIf(e, ce, cs) -> 
		  let resultado_if = avalia_exp amb e  in
		  (
            match resultado_if with
                ExpBool v -> if v then 
                            begin
                                avalia_bloco amb ce funcs;
                            end
                        else
                            begin
                            match cs with
                                None -> ignore()                              
                                | Some x -> avalia_bloco amb x funcs
                            end                          
            | _ -> failwith "Erro while" 
			);
    | CmdWhile (e, cs) -> 
		  let resultado_while = avalia_exp amb e  in

          (match resultado_while with
            ExpBool v -> if v then 
                        begin
                            avalia_bloco amb cs funcs;
                            avalia amb cmd funcs
                        end

            | _ -> failwith "Erro while" )
            

   (* | CmdFor (v, range, cmds) -> *)


    (*| CmdReturn (e) -> seta_retorno avalia_exp amb e*)

    | _ -> failwith "Erro de interpretação"

(*let imprime_funcs funcs = 
    match funcs with
    [] -> Printf.printf "fim funcs"
    |fun::funcs ->
        Printf.printf "Nome funcao: %s" fun.idF;
        imprime_funcs funcs*)


let rec interpretador amb arv funcs = 
  (*imprime_funcs funcs;*)
  match arv with
    [] -> Printf.printf "\nFim execucao\n"
  | cmd :: cmds -> avalia amb cmd funcs;
   interpretador amb cmds funcs




