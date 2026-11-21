# Build from the repository root (the C source lives outside C#/):
#   docker build -f C#/Dockerfile -t fft-benchmark .     (or: make docker)
FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
RUN apt-get update \
    && apt-get install -y --no-install-recommends gcc libc6-dev make cargo \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY other_tests/C-tests/C-fast_double/ other_tests/C-tests/C-fast_double/
COPY C#/ C#/
WORKDIR /src/C#
# A host-generated Cargo.lock may be newer than the image's cargo understands
RUN rm -f rust/Cargo.lock && make clean publish

FROM mcr.microsoft.com/dotnet/runtime-deps:10.0
WORKDIR /app
COPY --from=build /src/C#/dist/ .
USER app
ENTRYPOINT ["./fft-benchmark"]
