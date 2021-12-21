# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

.PHONY : all
all : drrty.native #htmltree.o 

drrty.native :
	opam config exec -- \
	rm -f *.o
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitreader drrty.native

# htmltree: htmltree.c
# 	cc -o htmltree -DBUILD_TEST htmltree.c


.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff drrty scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.ll *.html
	rm -rf *.err *.out *.exe *.s
	rm -f *.o *.bc


TESTS = \
	add1 arith1 arith2 arith3 fib float1 float2 float3 for1 for2 func1 func3 func4 \
	func5 func6 func7 func8 func9 gcd gcd2 global1 global2 global3 hello hello2 \
	if1 if2 if3 if4 if5 if6 local1 local2 ops1 ops2 remainder stringconcat \
	stringconcat2 var1 var2

FAILS = \
	assign1 assign2 assign3 assign4 dead1 expr1 expr2 float1 for1 for2 for3 func1  \
	func2 func3 func4 func5 func6 func7 global1 global2 if1 if2 if3 nomain \
	return1 return2 while1 while2


TESTFILES = $(TESTS:%=test-%.drrt) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.drrt) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags drrty.ml drrtyparse.mly \
	README scanner.mll semant.ml testall.sh \
 arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%)

drrty.tar.gz : $(TARFILES)
	cd .. && tar czf drrty/drrty.tar.gz \
		$(TARFILES:%=drrty/%)
