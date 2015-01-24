CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc

compInter: atesem interpretador.cmo

atesem: atesint arvSint.cmo semantico.cmo

atesint: pre_processador.cmo sintatico.cmo

interpretador.cmo: arvSint.cmi interpretador.ml
	$(CAMLC) -c interpretador.ml

compSint: lexico.cmo sintatico.cmo

arvSint.cmi: arvSint.ml
	$(CAMLC) -c arvSint.ml

arvSint.cmo: arvSint.ml
	$(CAMLC) -c arvSint.ml

sintatico.cmo: sintatico.cmi sintatico.ml
	$(CAMLC) -c sintatico.ml

sintatico.cmi: sintatico.mli
	$(CAMLC) -c sintatico.mli

sintatico.ml: sintatico.mly
	$(CAMLYACC) -v sintatico.mly

sintatico.mli: arvSint.cmi sintatico.mly
	$(CAMLYACC) -v sintatico.mly

lexico.cmo: sintatico.cmi lexico.ml
	$(CAMLC) -c lexico.ml

lexico.cmi: sintatico.cmi lexico.ml
	$(CAMLC) -c lexico.ml

lexico.ml: lexico.mll
	$(CAMLLEX) lexico.mll

semantico.cmo: arvSint.cmi semantico.ml
	$(CAMLC) -c semantico.ml

pre_processador.cmo: sintatico.cmi lexico.cmi pre_processador.ml
	$(CAMLC) -c pre_processador.ml


clean:
	rm *.cmo *.cmi lexico.ml sintatico.ml sintatico.mli  *.output *.ml~ *.mly~
