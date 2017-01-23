COMPILER = ocamlopt
EXECUTABLE = minilisp
MAIN = main.ml
SOURCES = ltype.ml parser.ml evaluator.ml primitives.ml
TEST_SOURCES = test.ml
TEST_EXECUTABLE = test

all: compile test

clean:
	rm -f $(EXECUTABLE) $(TEST_EXECUTABLE) *.cmi *.cmo *.cmx *.o

compile:
	$(COMPILER) -o $(EXECUTABLE) $(SOURCES) $(MAIN)

test:
	$(COMPILER) -o $(TEST_EXECUTABLE) $(SOURCES) $(TEST_SOURCES) && ./test
