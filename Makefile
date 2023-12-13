all : clean cle.cmo structure_de_donnee.cmo tas.cmo file.cmo graphe.cmo test.cmo main



main : cle.cmo structure_de_donnee.cmo tas.cmo file.cmo graphe.cmo test.cmo main.cmo
	ocamlc cle.cmo structure_de_donnee.cmo tas.cmo file.cmo graphe.cmo test.cmo main.cmo -o main.exe

structure_de_donnee.cmo :
	ocamlc -c structure_de_donnee.ml

cle.cmo : 
	ocamlc -c cle.ml

tas.cmo :
	ocamlc -c tas.ml
file.cmo :
	ocamlc -c file.ml

graphe.cmo : 
	ocamlc -c graphe.ml
test.cmo :
	ocamlc -c test.ml
main.cmo :
	ocamlc -c main.ml

clean : 
	rm -f *.cmo *.cmi *.exe *.dot *.cmx *.o *.plt

