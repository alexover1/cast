#!/bin/sh

LLVM_CFLAGS="`llvm-config --cflags`"
LLVM_LIBS="`llvm-config --libs`"

CC="clang"
CFLAGS="-std=c11 -Wall -Werror -Wextra -Wpedantic -Wfatal-errors -g $LLVM_CFLAGS"
SOURCE="token.c parser.c workspace.c typecheck.c llvm.c"
LIBS="-lm $LLVM_LIBS"

set -xe

$CC $CFLAGS -o main main.c $SOURCE $LIBS