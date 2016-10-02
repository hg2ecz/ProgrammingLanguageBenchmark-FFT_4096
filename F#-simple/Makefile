SRCS=fft.fs main.fs
TARGET=benchmark.exe

all:
	fsharpc --out:$(TARGET) --define:SPEED_UP $(SRCS)
	@chmod +x $(TARGET)

clean:
	rm -f $(TARGET)
