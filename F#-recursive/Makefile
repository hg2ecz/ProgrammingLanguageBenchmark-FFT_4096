SRCS=main.fs
TARGET=benchmark.exe

FSFLAGS=--define:LOG2FFTSIZE=12 --define:FFT_REPEAT=1000


all:
	fsharpc -o $(TARGET) $(FSFLAGS) $(SRCS)
	@chmod +x $(TARGET)

clean:
	rm -f $(TARGET)
