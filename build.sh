#!/bin/sh

CC="clang"
CFLAGS="-std=c11 -Wall -Werror -Wextra -Wpedantic -Wfatal-errors -g"

set -xe

$CC $CFLAGS -o main main.c type_info.c program_printing.c ir_builder.c typecheck.c interp.c
