make:
	ocamlfind ocamlopt -package graphics,unix -linkpkg avions.ml io.ml traffic.ml main.ml -o simu

clean:
	rm -f *.cmi *.cmo simu *.cmx *.o