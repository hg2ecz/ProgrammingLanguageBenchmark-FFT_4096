all:
	make -C ../C-fast-all-complex-float-types-opt-4x

clean:
	make -C ../C-fast-all-complex-float-types-opt-4x clean

PROG=benchmark.py
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py

PROG2=python benchmark.py
test2:
	$(PROG2)
	@($(PROG2);$(PROG2);$(PROG2);$(PROG2);$(PROG2);  $(PROG2);$(PROG2);$(PROG2);$(PROG2);$(PROG2) )|../avgcalc.py
