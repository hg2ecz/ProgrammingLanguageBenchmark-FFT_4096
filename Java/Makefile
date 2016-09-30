OBJS=benchmark.class

all:
	javac benchmark.java
	jar cfm benchmark.jar MANIFEST.MF $(OBJS)

clean:
	rm -rf $(OBJS) *jar
