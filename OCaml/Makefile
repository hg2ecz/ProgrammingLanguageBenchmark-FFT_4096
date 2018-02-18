CFLAGS=-O3
OBJS = fft.o benchmark.o
TARGET = benchmark

%.o : %.ml
	ocamlopt $(CFLAGS) -c $<

all: $(OBJS)
	ocamlopt $(OBJS:.o=.cmx) -o $(TARGET)

clean:
	rm -f $(OBJS:.o=.cmx) $(OBJS:.o=.cmi) $(OBJS) $(TARGET)
