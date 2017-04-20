#
# Makefile for Machine Learning tools
#

OCB_FLAGS = -use-ocamlfind -no-links
OCB_FLAGS_TEST = -use-ocamlfind -cflags -warn-error,+26 -no-links
OCB = ocamlbuild

#CFLAGS = -cflags -ccopt,-static,-ccopt,-fopenmp
CFLAGS =

main = work
main_test = test_runner
test_sources = lib/img_proc.ml
clib =

all: native byte # profile debug

clean:
	#$(OCB) -clean
	rm -rf _build
	rm -rf test_output
	rm -f $(main).native
	rm -f $(main).byte
	rm -f $(main_test).ml


native:
	#	rm -f $(main).native
	#	$(OCB) -cflags -ccopt,-fopenmp lib/perfect_number_stubs.o
	$(OCB)  $(main).native $(OCB_FLAGS)

byte:
	#	rm -f $(main).byte
	$(OCB) $(main).byte $(OCB_FLAGS) -lflags -custom

lib:
	$(OCB) $(OCB_FLAGS) img_proc.cma
	$(OCB) $(OCB_FLAGS) img_proc.cmxa

profile:
	$(OCB) $(OCB_FLAGS) -tag profile _build/$(main).native

debug:
	$(OCB) $(OCB_FLAGS) -tag debug _build/$(main).byte

run: native
	mkdir -p test_output
	_build/bin/$(main).native

# build c stubs
.o:.c
	$(OCB) $@

# iTeML tests
$(main_test).ml: $(test_sources)
	qtest -o ./bin/$@ extract $^

test: $(main_test).ml
	$(OCB) $(main_test).native $(OCB_FLAGS_TEST)
	_build/bin/$(main_test).native

.PHONY: 	all clean byte native profile debug test lib
