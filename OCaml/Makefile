OBJS = fft.o benchmark.o
TARGET = benchmark

%.o : %.ml
	ocamlopt -c $<

all: $(OBJS)
	ocamlopt -o $(TARGET) $(OBJS:.o=.cmx)

clean:
	rm -f $(OBJS:.o=.cmx) $(OBJS:.o=.cmi) $(OBJS) $(TARGET)
