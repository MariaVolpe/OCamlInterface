all: interface

interface: interface.ml c_code.c cJSON.c
	ocamlfind ocamlopt -package ctypes,ctypes.foreign -linkpkg -o $@ $^

docs: interface.ml
	[ -d docs ] || mkdir -p docs
	ocamldoc -html -d docs $^

clean:
	-rm -r interface _build docs *.o *.cmo *.cmx *.mli *.cmi