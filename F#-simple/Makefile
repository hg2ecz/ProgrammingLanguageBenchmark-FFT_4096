SRCS=fft.fs main.fs
TARGET=benchmark.exe

all:
	fsharpc --out:$(TARGET) $(SRCS)
	@chmod +x $(TARGET)

clean:
	rm -f $(TARGET)
