type programa = Programa of comandos
and comandos = comando list 
and comando = CmdAtrib of expressao * expressao
            | CmdPrint of expressao list
            | CmdInput  of expressao list

and expressao = ExpInt of int
              | ExpVar of string
              | ExpBin of operador * expressao * expressao
              | ExpUn of operador * expressao
              | ExpTrue
              | ExpFalse
and operador = Soma  | Sub | Mult |Div 
             | Not   | And 
             | Maior | MaiorIgual |Menor |MenorIgual
             
and tipo = TInt | TBool
