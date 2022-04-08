#!/bin/sh

RUSTFLAGS="-C target-cpu=native" cargo +nightly build --release
