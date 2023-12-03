all : clean cle.cmo tas.cmo graphe.cmo main



main : cle.cmo tas.cmo graphe.cmo main.cmo
	ocamlc cle.cmo tas.cmo graphe.cmo main.cmo -o main.exe


cle.cmo : 
	ocamlc -c cle.ml

tas.cmo :
	ocamlc -c tas.ml

graphe.cmo : 
	ocamlc -c graphe.ml


main.cmo :
	ocamlc -c main.ml

clean : 
	rm -f *.cmo *.cmi *.exe *.dot *.cmx *.o *.plt

