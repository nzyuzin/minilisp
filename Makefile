COMPILER = ocamlc
RELEASE_COMPILER = ocamlopt
EXECUTABLE = minilisp
MAIN = main.ml
SOURCES = ltype.ml parser.ml evaluator.ml primitives.ml repl.ml
TEST_SOURCES = test.ml
TEST_EXECUTABLE = test

.PHONY: test

all: release

clean:
	rm -f $(EXECUTABLE) $(TEST_EXECUTABLE) *.cmi *.cmo *.cmx *.o

release:
	$(RELEASE_COMPILER) -o $(EXECUTABLE) $(SOURCES) $(MAIN)

compile:
	$(COMPILER) -o $(EXECUTABLE) $(SOURCES) $(MAIN)

test:
	$(COMPILER) -o $(TEST_EXECUTABLE) -g $(SOURCES) $(TEST_SOURCES) && ./test

debug: test
	$(COMPILER) -o $(EXECUTABLE) -g $(SOURCES) $(MAIN)
