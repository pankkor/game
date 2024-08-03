#!/bin/sh

die() {
  echo 'Error: ' "$@" >&2
  exit 1
}

mkdir -p build || die "failed to make 'build' directory!"

src=main.c
out=main
cc_flags="$(cat compile_flags.txt)"
frameworks="-framework OpenGL -framework CoreGraphics"

# Build macOS aarch64 OpenGL
build_cmd="clang $cc_flags -o build/$out $src $frameworks -e _start"

echo "Building '$src' -> '$out'"
echo "$build_cmd"

$build_cmd || die "failed to build '$src'!"
