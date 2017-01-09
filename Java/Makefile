OBJS=benchmark.class

all:
	javac benchmark.java
	jar cfm benchmark.jar MANIFEST.MF $(OBJS)

clean:
	rm -rf $(OBJS) *jar

PROG=java -jar benchmark.jar
test:
	$(PROG)
	@($(PROG);$(PROG);$(PROG);$(PROG);$(PROG);  $(PROG);$(PROG);$(PROG);$(PROG);$(PROG) )|../avgcalc.py

PROG=/usr/lib/jvm/jdk-8-oracle-arm32-vfp-hflt/bin/java -jar benchmark.jar
test2:
	$(PROG)
	@($(PROG);$(PROG);$(PROG);$(PROG);$(PROG);  $(PROG);$(PROG);$(PROG);$(PROG);$(PROG) )|../avgcalc.py
