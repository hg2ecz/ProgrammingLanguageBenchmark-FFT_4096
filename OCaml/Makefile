CFLAGS=-O3
OBJS = fft.cmx benchmark.cmx
TARGET = benchmark

%.cmx : %.ml
	ocamlopt $(CFLAGS) -c $<

all: $(OBJS)
	ocamlopt $(OBJS) -o $(TARGET)

clean:
	rm -f $(OBJS:.cmx=.o) $(OBJS:.cmx=.cmi) $(OBJS) $(TARGET)
