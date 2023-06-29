#define NOBUILD_IMPLEMENTATION
#include "vendor/nobuild.h"

#define WARNINGS "-Wall", "-Wextra", "-Wpedantic", "-Wfatal-errors"
#define CFLAGS WARNINGS, "-std=c11", "-g"
#define LIBS "-lm", "-lLLVM-15"

// TODO: All files in directory "src"
#define SOURCE "token.c", "parser.c", "workspace.c", "typecheck.c", "llvm.c"

int main(int argc, char **argv)
{
    GO_REBUILD_URSELF(argc, argv);

    const char *main_path = "main.c";

    CMD("clang", CFLAGS, "-o", NOEXT(main_path), main_path, SOURCE, LIBS);

    // FOREACH_FILE_IN_DIR(tool, "src", {
    //     if (ENDS_WITH(tool, ".c")) {
    //         build_tool(tool);
    //     }
    // });

    return 0;
}
