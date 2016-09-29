SRCS=fft.fs main.fs
TARGET=benchmark.exe

all:
	fsharpc --out:$(TARGET) --optimizer+ $(SRCS)
	@chmod +x $(TARGET)

clean:
	rm -f $(TARGET)
