#Craft Makefile in progress

LDFLAGS = lSDL2 
H_DIR = header_files
INC_DIR = include
LIB_DIR = /usr/local/lib
CFLAGS = -Wall -w `pkg-config --cflags --libs glib-2.0`

.PHONY : all
all : craft.native main.o #link c here?

.PHONY : craft.native
craft.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		craft.native

# "make clean" removes all generated files
.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff craft scanner.ml parser.ml parser.mli
	#rm -rf printbig
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe
	rm -rf ./test_suite/*.o ./test_suite/*.s ./test_suite/*.ll ./test_suite/*.out ./test_suite/*.exe


OBJS = ast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx craft.cmx

craft : $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o craft

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<


### Generated by "ocamldep *.ml *.mli" after building scanner.ml and parser.ml
ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
craft.cmo : semant.cmo scanner.cmo parser.cmi codegen.cmo ast.cmo
craft.cmx : semant.cmx scanner.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semant.cmo : ast.cmo
semant.cmx : ast.cmx
parser.cmi : ast.cmo
