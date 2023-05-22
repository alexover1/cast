#!/bin/sh

set -xe

CC="clang"
CFLAGS="-std=c11 -Wall -Werror -Wextra -Wpedantic -Wfatal-errors"

$CC $CFLAGS -o main main.c type_info.c program_printing.c ir_builder.c typecheck.c pipe.c
