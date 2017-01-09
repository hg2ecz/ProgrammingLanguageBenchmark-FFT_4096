all:

clean:

PROG=benchmark.lua
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py

PROG2=lua benchmark.lua
test2:
	$(PROG2)
	@($(PROG2);$(PROG2);$(PROG2);$(PROG2);$(PROG2);  $(PROG2);$(PROG2);$(PROG2);$(PROG2);$(PROG2) )|../avgcalc.py
