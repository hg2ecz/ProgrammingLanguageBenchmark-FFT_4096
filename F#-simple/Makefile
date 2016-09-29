SRCS=fft.fs main.fs
TARGET=benchmark.exe

all:
	fsharpc --out:$(TARGET) --optimize+ --standalone $(SRCS)
	@chmod +x $(TARGET)

clean:
	rm -f $(TARGET)
