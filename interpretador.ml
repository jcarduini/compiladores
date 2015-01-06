open ArvSint;;
open Printf;;



let rec avalia_exp amb expr = 
    match expr.valor with
    ExpInt i      -> ExpInt i
    | ExpFloat f  -> ExpFloat f
    | ExpString s -> ExpString s
    | ExpBool b   -> ExpBool b
    | ExpGen -> ExpGen 
    | ExpVar v -> 
        let entrada = Hashtbl.find amb v in entrada.valor
    | ExpUn (not,expressao) -> 
        let expunaria = avalia_exp amb expressao in
        match expunaria with
            | ExpBool true -> ExpBool false
            | ExpBool false -> ExpBool true            
    | ExpBin (op,e1,e2) ->
		  let arg1 = avalia_exp amb e1  in 
        let arg2 = avalia_exp amb e2  in
        match (arg1,arg2) with
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
	    | _ -> ExpGen

and executa_op_bool op a b =
    match op with
    | And -> a && b
    | Or -> a || b

and executa_op_int op a b =
    match op with
    | Mais -> a + b
    | Menos -> a - b
    | Div -> a/b
    | Mult -> a*b

and executa_op_float op a b =
        match op with 
        | Mais -> a +. b
        | Menos -> a -. b
        | Div -> a /. b
        | Mult -> a *. b            


and avalia_atrib ambiente v exp =
  let vexp = avalia_exp ambiente exp in
  let entrada = Hashtbl.find ambiente v in
  Hashtbl.replace ambiente v { entrada with valor = vexp}


and avalia amb cmd  =
    match cmd.vcmd with
   (* | ChamaFuncaoAtrib (ExpVar v, nomeFunc, arg) -> 
		let vexp = avalia_func amb nomeFunc, arg in    
		let entrada = Hashtbl.find amb v in
  		Hashtbl.replace amb v { entrada with valor = vexp}
    	
    | ChamaFuncaoVoid (nomeFunc, arg) -> avalia_func amb nomeFunc*)
    
    | CmdPrint (e) -> 
		(match e.valor with
			| ExpInt i -> Printf.printf "%d " i
			| ExpFloat f -> Printf.printf "%f " f
			| ExpString s -> Printf.printf "%s " s
			| ExpBool true -> Printf.printf "True "
			| ExpBool false -> Printf.printf "False "); 

	 | CmdIntParse (e1, e2) -> 
		let i = read_int() in 
		let entrada = Hashtbl.find amb e1.valor in
	  	Hashtbl.replace amb e1.valor { entrada with valor = ExpInt i}
							  

    | CmdAtrib  (e1,e2) -> avalia_atrib amb e1.valor e2
   
    | CmdIf (e, ce, cs) -> 
		  let resultado_if = avalia_exp amb e  in
		  (
			if(resultado_if == true) then
				avalia_bloco amb ce
			else
				avalia_bloco amb cs
			)

    | CmdWhile (e, cs) -> 
		  let resultado_while = avalia_exp amb e  in
		  if(resultado_while == true) then
				avalia_bloco amb cs

   (* | CmdFor (v, range, cmds) -> *)


    | CmdReturn (e) -> avalia_exp amb e

    | _ -> failwith "Erro de interpretação"

let avalia_bloco amb cmds =
  match cmds with
 	[] ->  ignore()
  | cmd :: cmds -> avalia amb cmd; avalia_bloco amb cmds	

let rec interpretador amb arv = 
  match arv with
    [] -> Printf.printf "Feito\n"
  | cmd :: cmds -> avalia amb cmd; interpretador amb cmds




