#!/bin/sh

CC="clang"
CFLAGS="-std=c11 -Wall -Werror -Wextra -Wpedantic -Wfatal-errors -g"
LIBS="-lLLVM-15"

set -xe

$CC $CFLAGS -o main main.c token.c parser.c type_info.c program_printing.c ir_builder.c typecheck.c interp.c $LIBS
