# Packages the self-contained build from ./dist; build and package with: make docker
FROM mcr.microsoft.com/dotnet/runtime-deps:10.0
WORKDIR /app
COPY dist/ .
USER app
ENTRYPOINT ["./fft-benchmark"]
