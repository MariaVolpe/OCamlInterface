all: interface

interface: interface.ml c_code.c cJSON.c
	ocamlfind ocamlopt -package ctypes,ctypes.foreign -linkpkg -o $@ $^

clean:
	-rm -r interface _build *.o *.cmo *.cmx *.mli *.cmi
