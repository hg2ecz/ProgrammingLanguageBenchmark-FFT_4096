# Build and package with: make docker
# The build context is the repository root, because the C source lives outside C#/.
# The .NET app comes from ./dist (make publish); the native libraries are rebuilt here
# with the Makefile's own rules, against the same distribution as the runtime image.

FROM mcr.microsoft.com/dotnet/runtime-deps:10.0 AS native
RUN apt-get update \
    && apt-get install -y --no-install-recommends gcc libc6-dev make cargo \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY other_tests/C-tests/C-fast_double/ other_tests/C-tests/C-fast_double/
COPY C#/Makefile C#/
COPY C#/rust/Cargo.toml C#/rust/build.sh C#/rust/
COPY C#/rust/src/ C#/rust/src/
WORKDIR /src/C#
RUN make native

FROM mcr.microsoft.com/dotnet/runtime-deps:10.0
WORKDIR /app
COPY C#/dist/ .
COPY --from=native /src/C#/libfft.so /src/C#/libfftr.so ./
USER app
ENTRYPOINT ["./fft-benchmark"]
