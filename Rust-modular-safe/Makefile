RC=rustc
RCFLAGS=-O -L.
RLFLAGS=-C link-arg=-s -L.

OBJS=libfft.rlib
TARGET=benchmark

lib%.rlib: %.rs
	$(RC) $(RCFLAGS) --crate-type=lib $<

all: $(OBJS)
	$(RC) $(RLFLAGS) $(TARGET).rs

clean:
	rm -f $(OBJS) $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
