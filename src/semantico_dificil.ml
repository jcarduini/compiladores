open ArvSint;;
open Printf;;

let current_func = ref ""

(* Funcoes para mostrar os erros na tela *)
let erro nome pos msg =
	let nlin = pos.Posicao.lin_final
	and ncol = pos.Posicao.col_final in
	let msglc = sprintf "Erro na linha %d, coluna %d" nlin ncol in
	print_endline msglc;
	print_endline (msg ^ nome);
	failwith "Erro semantico"


(* Tabela de simbolos para as operacoes *)
let tab2list tab = Hashtbl.fold (fun c v ls -> (c,v) :: ls) tab []

let ambfun =
	let amb = Hashtbl.create 23 in
	Hashtbl.add amb Mais [ (TInt, TInt, TInt) ; (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb Menos [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb Mult [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb Div [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb Menor [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb Maior [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb Igual [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb Diferente [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb MaiorIgual [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb MenorIgual [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	Hashtbl.add amb Modulo [ (TInt, TInt, TInt); (TGen, TGen, TGen); (TFloat, TFloat, TFloat)] ;
	amb 

let ambfun2 = 
	let amb2 = Hashtbl.create 4 in
	Hashtbl.add amb2 Not [ (TBool, TInt); (TBool, TGen); (TBool, TFloat) ];
	amb2

let tipo e = e.tipo

(* Verifica se os tipos dos termos sao compativeis com o tipo do operador*)
let rec verifica_op t1 t2 ls =
    match ls with
    (a1,a2,r)::ls -> if ((t1 == a1) && (t2 == a2)) then r
                     else verifica_op t1 t2 ls
    | [] -> failwith "verifica_op: O tipo dos operandos deveria ser o mesmo"

let rec verifica_op2 t1 ls =
    match ls with
    (a1,r)::ls -> if (t1 == a1) then r
                     else verifica_op2 t1 ls
    | [] -> failwith "verifica_op: O tipo dos operandos deveria ser o mesmo"

(*Insere uma nova funcao na tabela*)
let insere_nova_funcao amb func =
    try
	let entrada = Hashtbl.find amb func.idF in
	    (match entrada with
	      | EntVar _ -> print_endline ("O nome '" ^ func.idF ^ "' esta associado a uma variavel."); failwith "Erro semantico: insere_nova_funcao "
          | _ -> print_endline ("A funcao '" ^ func.idF ^ "' ja foi definida.");
                 failwith "Erro semantico: insere_nova_funcao")
    with
        Not_found -> Hashtbl.add amb func.idF (EntFn (cria_ent_func None func.paramsF))

(* Retorna a tabela das variaveis locais de uma funcao *)
let var_locais_func tabGeral nomeFun =
    try
        let entFn = Hashtbl.find tabGeral nomeFun in
                    (match entFn with
                    EntFn entFunc -> entFunc.varLocais
                    | _ -> print_endline ("O nome '" ^ nomeFun ^ "' esta associado a uma variavel."); failwith "Erro semantico: var_locais_func ")
    with
        Not_found -> failwith "var_locais_func"

(* Insere variavel em uma tabela e retorna o tipo, caso já exista apenas retorna o tipo *)
let insere_var tab nome current =
    (* Variaveis na funcao corrente *)
    if (current <> "") then 
    (
        let tabVar = Hashtbl.find tab current in
                    (match tabVar with
                    EntFn entFun -> (try
                                        let var = Hashtbl.find entFun.varLocais nome in
                                                    (match var with
                                                    EntVar t -> t.tipagem
                                                    | EntFn _ -> print_endline ("O nome '" ^ nome ^ "' esta associado a uma funcao."); failwith "Erro semantico: insere_ver")
                                     with 
                                        Not_found -> Hashtbl.add entFun.varLocais nome (EntVar (cria_ent_var TGen)); TGen )
                    | _ -> failwith "Erro insere_var") 
    )
(*Variaveis fora das funcoes*)
    else 
    (
        try
            let ent = Hashtbl.find tab nome in        
                     (match ent with
                     EntVar t -> t.tipagem
                     | EntFn _ -> print_endline ("O nome '" ^ nome ^ "' esta associado a uma funcao."); failwith "Erro semantico: insere_var")
        with
            Not_found -> Hashtbl.add tab nome (EntVar (cria_ent_var TGen)); TGen 
    )

(* Adiciona variavel se nao existir *)
let rec verifica_var amb pos var current =
    match var with
    | VarSimples nome -> insere_var amb nome current

(* Retorna tabela de simbolos corrente (de variaveis locais ou a geral)*)
and ret_tabela amb current =
    if (current <> "") then
        let entFun = Hashtbl.find amb current in 
                    (match entFun with 
                    | EntFn f -> f.varLocais 
                    | _ -> print_endline ("O nome '" ^ current ^ "' esta associado a uma variavel."); failwith "Erro semantico: ret_var")
    else amb

(* Verifica os tipos da atribuicao e infere os tipos para as variaveis *)
and verifica_tipos_atrib e1 e2 amb current = 
    let t1 = tipo e1 and t2 = tipo e2 in 
    begin
        if(t1 <> TGen) then 
        (    
            if (t2 == TGen) then 
                match e2.valor with
                | ExpVar (VarSimples nome) -> 
                    let entVar = Hashtbl.find (ret_tabela amb current) nome in 
                                (match entVar with 
                                EntVar var -> var.tipagem <- t1; e2.tipo <- t1 
                                | _-> print_endline ("O nome '" ^ nome ^ "' esta associado a uma funcao."); failwith "Erro semantico: verifica_tipos_atrib")
                | _ -> erro " verifica_tipos_atrib " e2.pos "Expressao nao eh variavel."
            else
                (*if (t1 <> t2) then 
                    erro " verifica_tipos_atrib " e1.pos "Os tipos deveriam ser iguais."*)
                match e1.valor with
                | ExpVar (VarSimples nome) -> 
                    let entVar = Hashtbl.find (ret_tabela amb current) nome in 
                                (match entVar with 
                                EntVar var -> var.tipagem <- t2; e1.tipo <- t2
                                | _-> print_endline ("O nome '" ^ nome ^ "' esta associado a uma funcao."); failwith "Erro semantico: verifica_tipos_atrib")
                | _ -> erro " verifica_tipos_atrib " e2.pos "Expressao nao eh variavel."
        )
        else
            if (t2 <> TGen) then
                (match e1.valor with
                | ExpVar (VarSimples nome) -> 
                    let entVar = Hashtbl.find (ret_tabela amb current) nome in 
                                (match entVar with 
                                EntVar var -> var.tipagem <- t2; e1.tipo <- t2
                                | _-> print_endline ("O nome '" ^ nome ^ "' esta associado a uma funcao."); failwith "Erro semantico: verifica_tipos_atrib")
                | _ -> erro " verifica_tipos_atrib " e2.pos "Expressao nao eh variavel.")
    end

and verifica_tipos_atrib2 e1 amb2 current = 
    let t1 = tipo e1 in 
    begin
             if (t1 == TGen) then 
                match e1.valor with
                | ExpVar (VarSimples nome) -> 
                    let entVar = Hashtbl.find (ret_tabela amb2 current) nome in 
                                (match entVar with 
                                EntVar var -> var.tipagem <- t1; e1.tipo <- t1 
                                | _-> print_endline ("O nome '" ^ nome ^ "' esta associado a uma funcao."); failwith "Erro semantico: verifica_tipos_atrib")
                | _ -> erro " verifica_tipos_atrib " e1.pos "Expressao nao eh variavel."
            else
                (*if (t1 <> t2) then 
                    erro " verifica_tipos_atrib " e1.pos "Os tipos deveriam ser iguais."*)
                match e1.valor with
                | ExpVar (VarSimples nome) -> 
                    let entVar = Hashtbl.find (ret_tabela amb2 current) nome in 
                                (match entVar with 
                                EntVar var -> var.tipagem <- t1; e1.tipo <- t1
                                | _-> print_endline ("O nome '" ^ nome ^ "' esta associado a uma funcao."); failwith "Erro semantico: verifica_tipos_atrib")
                | _ -> erro " verifica_tipos_atrib " e1.pos "Expressao nao eh variavel."

    end

(* Verifica se as parcelas correspondem aos tipos dos operadores *)
and verifica_primitiva op t1 t2 =
    let tipos_op = Hashtbl.find ambfun op in verifica_op t1 t2 tipos_op   

(*Operacoes com not podem ser de qualquer tipo, por isso nao ha verifica*)
and verifica_primitiva2 op t1 =
    let tipos_op = Hashtbl.find ambfun2 op;
    tipos_op

(* Verifica expressao *)
and verifica_exp amb expr current = 
    match expr.valor with
    ExpInt _    -> expr.tipo <- TInt
    | ExpFloat _    -> expr.tipo <- TFloat
    | ExpString _ -> expr.tipo <- TString
    | ExpGen -> expr.tipo <- TGen
    | ExpVar v -> expr.tipo <- (verifica_var amb expr.pos v current)
    | ExpBin (op,e1,e2) ->
        verifica_exp amb e1 current;
        verifica_exp amb e2 current;
        (if (current <> "") then
            verifica_tipos_atrib e1 e2 amb current
        else
        (if (e1.tipo == TGen) then
            erro " verifica_exp " e1.pos "Variavel nao definida"
        else if (e2.tipo == TGen) then
            erro " verifica_exp " e2.pos "Variavel nao definida"));
            expr.tipo <- (verifica_primitiva op (tipo e1) (tipo e2))
    | ExpUn (op,e1) -> expr.tipo <- TBool

(* Retorna o tipo de uma variavel que sera retornada*)
let tipo_var_retorno v locais param =
  try
     let entrada = Hashtbl.find locais v in 
      (match entrada with
        EntVar entVar -> entVar.tipagem
      | _ -> print_endline ("O nome '" ^ v ^ "' esta associado a uma funcao."); failwith "Erro semantico: tipo_var_retorno")
  with
     Not_found -> (let arg = procuraParam v param in
	              (match arg with 
	               | Some param -> TGen  
	               | None -> print_endline ("A variavel '" ^ v ^ "' nao esta definida."); failwith "Erro semantico: tipo_var_retorno"))

(* Retorna o tipo de retorno de uma funcao *)
let rec tipo_retorno amb expr current param = 
    let entrada = Hashtbl.find amb current in
        (match entrada with
        EntFn entFun -> (match expr.valor with
        ExpInt _    -> expr.tipo <- TInt; expr.tipo
        | ExpFloat _    -> expr.tipo <- TFloat; expr.tipo
        | ExpString _ -> expr.tipo <- TString; expr.tipo
        | ExpGen -> expr.tipo <- TGen; expr.tipo
        | ExpVar (VarSimples v) -> expr.tipo <- (tipo_var_retorno v entFun.varLocais param); expr.tipo
        | ExpBin (op,e1,e2) ->
            tipo_retorno amb e1 current param;
            tipo_retorno amb e2 current param;
            expr.tipo <- (verifica_primitiva op (tipo e1) (tipo e2)); expr.tipo)
        | _ -> print_endline ("O nome '" ^ current ^ "' esta associado a uma variavel."); failwith "Erro semantico: tipo_var_retorno")

(* Verifica o retorno de uma funcao *)
let verifica_retorno amb expr current param =
    if (current <> "") then
        let entrada = Hashtbl.find amb current in  
                        (match entrada with
                        EntFn entFunc -> entFunc.tiporetorno <- Some (tipo_retorno amb expr current param)
                        | _ -> print_endline ("O nome '" ^ current ^ "' esta associado a uma variavel."); failwith "Erro semantico: verifica_retorno");
    else
        erro "verifica_retorno" expr.pos "O comando return deve ser usado dentro de uma funcao"

(* Verifica o retorno de uma funcao *)  
let retorna_tipo_funcao amb nomeFuncao = 
	try
		let tab = Hashtbl.find amb nomeFuncao in
		(match tab with
		| EntFn entFun -> (match entFun.tiporetorno with
							  Some tipo -> tipo
							| None -> print_endline ("A funcao '" ^ nomeFuncao ^ "' nao tem um tipo definido."); failwith "Erro semantico: retorna_tipo_funcao")
		| _ -> print_endline ("O nome '" ^ nomeFuncao ^ "' esta associado a uma variavel."); failwith "Erro semantico: retorna_tipo_funcao")
	with
		Not_found -> print_endline ("A funcao '" ^ nomeFuncao ^ "' nao foi definida."); failwith "Erro semantico: retorna_tipo_funcao"

(* Retorna a lista de parametros de uma funcao *) 
let retorna_param_funcao amb nomeFuncao = 
	try		
		let tab = Hashtbl.find amb nomeFuncao in
		(match tab with
		| EntFn entFun -> entFun.param
		| _ -> print_endline ("O nome '" ^ nomeFuncao ^ "' esta associado a uma variavel."); failwith "Erro semantico: retorna_param_funcao")
	with
		Not_found -> print_endline ("A funcao '" ^ nomeFuncao ^ "' nao foi definida."); failwith "Erro semantico: retorna_param_funcao"

(* Verifica os tipos dos parametros de uma funcao *)
let rec verifica_tipos_parametros param arg nomeFun =
    (match param with
    [] ->  ignore()
    | p1 :: param ->  (match arg with
                        [] -> ignore()
                        | arg1 :: arg ->  (if (p1.tipoP = arg1.tipo) then 
                                                verifica_tipos_parametros param arg nomeFun
                                          else 
                                               print_endline ("Os tipos dos argumentos nao correspondem aos tipos dos parametros da funcao " ^ nomeFun);
                                               failwith "Erro semantico: verifica_tipos_parametros")))

(* Verifica os argumentos que sao variaveis *)
let verifica_var_arg amb var =
    try 
        let entrada = Hashtbl.find amb var in
        (match entrada with
        | EntVar entVar -> entVar.tipagem
        | _ -> print_endline ("O nome '" ^ var ^ " 'esta associado a uma funcao."); failwith "Erro semantico: verifica_var_arg")
    with
        Not_found -> print_endline ("Variavel '" ^ var ^ "' nao definida."); failwith "Erro semantico: verifica_var_arg"

(* Verifica argumentos *)
let rec verifica_args amb args = 
    (match args with
    [] -> ignore()
    | arg :: args -> verifica_arg amb arg; verifica_args amb args)

(* Verifica argumento *)
and verifica_arg amb arg =
    
    (match arg.valor with
    ExpInt _    -> arg.tipo <- TInt
    | ExpFloat _    -> arg.tipo <- TFloat
    | ExpString _ -> arg.tipo <- TString
    | ExpGen -> arg.tipo <- TGen
    | ExpVar (VarSimples v) -> arg.tipo <- (verifica_var_arg amb v)
    | _ -> print_endline ("Nao e permitido usar operacao como argumento de chamada de funcao"); failwith "Erro semantico: verifica_arg")
    (*| ExpBin (op,e1,e2) ->
        verifica_arg amb e1 current;
        verifica_arg amb e2 current;
        verifica_tipos_atrib e1 e2 amb current;
        expr.tipo <- (verifica_primitiva op (tipo e1) (tipo e2))* <-- limitacao*) 

(* Verifica comandos *)
let rec verifica_cmds amb cmds current param = 
    match cmds with
    [] -> ignore()
    | cmd :: cmds -> verifica_cmd amb cmd current param; verifica_cmds amb cmds current param

(* Verifica comando *)
and verifica_cmd amb cmd current param =
    match cmd.vcmd with
    | ChamaFuncaoAtrib (e1, nomeFunc, arg) ->
        (* verifica se pode ser feita a atribuicao *)
        
 amb e1 current;
        let tipoFunc = retorna_tipo_funcao amb nomeFunc and t1 = tipo e1 in 
            (match tipoFunc with
            | TVoid -> erro "verifica_cmd" e1.pos ("Nao existe tipo para atribuicao. A funcao '" ^ nomeFunc ^ "' tem tipo Void.")
            | TGen -> ignore() (*limitacao*)
            | tip -> (match t1 with
                        | TGen -> (match e1.valor with
                                    | ExpVar (VarSimples var) -> 
                                        let entrada = Hashtbl.find (ret_tabela amb current) var in 
                                            (match entrada with 
                                            EntVar entVar -> entVar.tipagem <- tip; e1.tipo <- tip
                                            | _-> print_endline ("O nome" ^ current ^ "esta associado a uma funcao."); 
                                                  failwith "Erro semantico: verifica_cmd")
                                    | _ -> erro "verifica_cmd" e1.pos "Expressao nao eh variavel.")
                        | _ -> if (t1 <> tip) then
                                erro "verifica_cmd" e1.pos "Os tipos para a atribuicao devem ser iguais. ")
                        );
            verifica_args (ret_tabela amb current) arg;
            verifica_tipos_parametros param arg nomeFunc
    
    | ChamaFuncaoVoid (nomeFunc, arg) ->
        verifica_args (ret_tabela amb current) arg;
       (* if (List.length param = List.length arg) then*)
            verifica_tipos_parametros param arg nomeFunc
       (* else
			(print_endline ("Numero de argumentos nao corresponde com os parametros da funcao " ^ nomeFunc ); 
            failwith "Erro semantico: verifica_cmd")*)
            
    
    | CmdPrint (e) -> verifica_exp amb e current

    | CmdInput (e1, e2) -> verifica_exp amb e1 current; 
                           verifica_exp amb e2 current;
                           e1.tipo <- TString;
						   (match e1.valor with
							| ExpVar (VarSimples v) -> let ent = Hashtbl.find amb v in
													   (match ent with
														| EntVar var -> var.tipagem <- TString
														| _ -> failwith "Espera variavel, encontrou funcao")
							| _ -> failwith "Espera variavel")                  

	| CmdIntParse (e1, e2) -> verifica_exp amb e1 current; 
                           verifica_exp amb e2 current;
                           e1.tipo <- TInt;
						   (match e1.valor with
							| ExpVar (VarSimples v) -> let ent = Hashtbl.find amb v in
													   (match ent with
														| EntVar var -> var.tipagem <- TInt
														| _ -> failwith "Espera variavel, encontrou funcao")
							| _ -> failwith "Espera variavel")  
							  

    | CmdAtrib  (e1,e2) -> 
        verifica_exp amb e1 current;
        verifica_exp amb e2 current;
        verifica_tipos_atrib e1 e2 amb current 
   
    | CmdIf (e, ce, cs) -> 
        verifica_exp amb e current;
        begin      	
            verifica_cmds amb ce current param;
            (match cs with
            | None -> ignore()
            | Some cmds -> verifica_cmds amb cmds current param)
        end

    | CmdWhile (e, cs) -> 
        verifica_exp amb e current;
        let te = tipo e in
        begin      	
            if (te == TGen) then 
                erro " (comando while)" cmd.pcmd "Condicao possui tipo generico";
        verifica_cmds amb cs current param;
        end

    | CmdFor (v, range, cmds) -> 
        verifica_exp amb v current;
	v.tipo <- TInt;
        (match range.vcmd with
        | CmdRange (ini, fim, inc) -> ignore ()
        | _ -> erro "range" cmd.pcmd "Range invalida");
        verifica_cmds amb cmds current param

    | CmdReturn (e) -> verifica_retorno amb e current param

    | _ -> erro "verifica_cmd" cmd.pcmd "Comando nao definido. Erro Semantico."

(* Verifica lista de parametros *)
let rec verifica_params locais par  = 
    match par with
    [] -> []
    | par :: params -> let tpar = verifica_param locais par  in
                           tpar :: verifica_params locais params 

and verifica_param locais param =
  try
       let entVar = Hashtbl.find locais param.idP in 
       (match entVar with
        | EntVar var -> ( param.tipoP <- var.tipagem;
                          Hashtbl.remove locais param.idP;
                          param )
       | _-> print_endline ("O nome '" ^ param.idP ^ "' esta associado a uma funcao."); failwith "Erro semantico: verifica_param")
   with
      Not_found -> param

(* Verificacao das funcoes *)
let rec verifica_funcs amb funcs = 
  match funcs with
  [] -> ignore()
  | func :: funcs -> verifica_func amb func; verifica_funcs amb funcs

(* Verificacao de funcao *)
and verifica_func amb func =
    insere_nova_funcao amb func;
    current_func := func.idF;
    verifica_cmds amb func.cmdsF !current_func func.paramsF;

    let entFun = Hashtbl.find amb !current_func in 
        (match entFun with
        | EntFn funcao ->
            let params = verifica_params funcao.varLocais func.paramsF in
            let novo_reg = { varLocais = funcao.varLocais; 
                            tiporetorno = funcao.tiporetorno; 
                            param = params } in
            Hashtbl.replace amb !current_func (EntFn novo_reg); 
            func.varLocaisF <- funcao.varLocais
        | _ -> print_endline ("O nome '" ^ !current_func ^ "' esta associado a uma varivel."); failwith "Erro semantico: verifica_func");

    let entTab = Hashtbl.find amb !current_func in
        (match entTab with
        | EntFn entFunc -> (if (entFunc.tiporetorno == None) then
                            entFunc.tiporetorno <- Some TVoid); 
                            func.returnF <- entFunc.tiporetorno 		     
        | _ -> print_endline ("O nome '" ^ !current_func ^ "' esta associado a uma varivel."); failwith "Erro semantico: verifica_func")
	
(* Verifica o programa *)
let verifica_prog amb arv = 
    verifica_funcs amb arv.funcsP;
    current_func := "";
    verifica_cmds amb arv.cmdsP !current_func []

let semantico arv =
    let ambiente = Hashtbl.create 23 in 
    verifica_prog ambiente arv;
    ambiente


