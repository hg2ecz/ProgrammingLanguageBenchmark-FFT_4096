CARGO=cargo
CARGO_BUILDOPTS=--release
CARGO_INSTALLOPTS=--root=/tmp --force

all: build

check:
	$(CARGO) check

build:
	$(CARGO) build $(CARGO_BUILDOPTS)

doc:
	$(CARGO) doc

test:
	$(CARGO) test

bench:
	$(CARGO) bench

install:
	$(CARGO) install $(CARGO_INSTALLOPTS)

clean:
	$(CARGO) clean
	rm -f Cargo.lock
