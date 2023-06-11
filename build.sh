#!/bin/sh

CC="clang"
CFLAGS="-std=c11 -Wall -Werror -Wextra -Wpedantic -Wfatal-errors -g"
SOURCE="token.c parser.c workspace.c typecheck.c"
LIBS="-lLLVM-15"

set -xe

$CC $CFLAGS -o main main.c $SOURCE $LIBS
