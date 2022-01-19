FROM debian:stable-slim
WORKDIR /app
COPY ./dist/ .

RUN apt-get update || true
RUN apt-get install -y libicu67

ENTRYPOINT ["./fft-benchmark"]
