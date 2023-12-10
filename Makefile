all : clean cle.cmo tas.cmo graphe.cmo test.cmo main



main : cle.cmo tas.cmo graphe.cmo test.cmo main.cmo
	ocamlc cle.cmo tas.cmo graphe.cmo test.cmo main.cmo -o main.exe


cle.cmo : 
	ocamlc -c cle.ml

tas.cmo :
	ocamlc -c tas.ml

graphe.cmo : 
	ocamlc -c graphe.ml
test.cmo :
	ocamlc -c test.ml
main.cmo :
	ocamlc -c main.ml

clean : 
	rm -f *.cmo *.cmi *.exe *.dot *.cmx *.o *.plt

