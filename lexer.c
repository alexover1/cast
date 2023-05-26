#include <ctype.h>
#include <assert.h>
#include <stdarg.h>

#ifndef _WIN32
#    ifdef __linux__
#        define _DEFAULT_SOURCE
#        define _POSIX_C_SOURCE 200112L
#    endif
#    include <sys/types.h>
#    include <sys/stat.h>
#    include <unistd.h>
#else
#    define  WIN32_LEAN_AND_MEAN
#    include "windows.h"
#endif // _WIN32

#include "lexer.h"
#include "vendor/context_alloc.h"
#include "vendor/stb_ds.h"

#define lexer_current_character_index(lexer) ((lexer)->current_line.data - (lexer)->current_line_start)
#define lexer_current_line_is_empty(lexer) ((lexer)->current_line.count == 0 || (lexer)->current_line.data[0] == '#')

static inline int lexer_peek_character(Lexer *lexer)
{
    if (lexer->current_line.count) return (int) *lexer->current_line.data;
    return -1;
}

static inline int lexer_eat_character(Lexer *lexer)
{
    assert(lexer->current_line.count > 0);
    int character = (int) *lexer->current_line.data;
    lexer->current_line.data += 1;
    lexer->current_line.count -= 1;
    return character;
}

static inline bool isident(char x)
{
    return isalnum(x) || x == '_';
}

static bool parse_int_value(String_View s, unsigned int base, uint64_t *out)
{
    uint64_t result = 0;
    size_t i = 0;
    for (; i < s.count && isdigit(s.data[i]); ++i) {
        result = result * base + (uint64_t) s.data[i] - '0';
    }
    if (out) *out = result;
    return i == s.count-1;
}

static inline void lexer_parse_maybe_equals_token(Lexer *lexer, Token *token, Token_Type type_with_equals)
{
    if (lexer_peek_character(lexer) == '=') {
        lexer_eat_character(lexer);
        token->c1 += 1;
        token->type = type_with_equals;
    }
}

#define c_to_sv(cstr_lit) sv_from_parts((cstr_lit), sizeof(cstr_lit)-1)

static Token_Type parse_keyword_or_ident_token_type(String_View s)
{
    if (sv_eq(s, c_to_sv("if"))) return TOKEN_KEYWORD_IF;
    if (sv_eq(s, c_to_sv("then"))) return TOKEN_KEYWORD_THEN;
    if (sv_eq(s, c_to_sv("else"))) return TOKEN_KEYWORD_ELSE;
    if (sv_eq(s, c_to_sv("return"))) return TOKEN_KEYWORD_RETURN;
    if (sv_eq(s, c_to_sv("struct"))) return TOKEN_KEYWORD_STRUCT;
    if (sv_eq(s, c_to_sv("while"))) return TOKEN_KEYWORD_WHILE;
    if (sv_eq(s, c_to_sv("break"))) return TOKEN_KEYWORD_BREAK;
    if (sv_eq(s, c_to_sv("continue"))) return TOKEN_KEYWORD_CONTINUE;
    if (sv_eq(s, c_to_sv("using"))) return TOKEN_KEYWORD_USING;
    if (sv_eq(s, c_to_sv("defer"))) return TOKEN_KEYWORD_DEFER;
    if (sv_eq(s, c_to_sv("size_of"))) return TOKEN_KEYWORD_SIZE_OF;
    if (sv_eq(s, c_to_sv("type_of"))) return TOKEN_KEYWORD_TYPE_OF;
    if (sv_eq(s, c_to_sv("initializer_of"))) return TOKEN_KEYWORD_INITIALIZER_OF;
    if (sv_eq(s, c_to_sv("type_info"))) return TOKEN_KEYWORD_TYPE_INFO;
    if (sv_eq(s, c_to_sv("null"))) return TOKEN_KEYWORD_NULL;
    if (sv_eq(s, c_to_sv("enum"))) return TOKEN_KEYWORD_ENUM;
    if (sv_eq(s, c_to_sv("true"))) return TOKEN_KEYWORD_TRUE;
    if (sv_eq(s, c_to_sv("false"))) return TOKEN_KEYWORD_FALSE;
    if (sv_eq(s, c_to_sv("union"))) return TOKEN_KEYWORD_UNION;
    return TOKEN_IDENT;
}

static bool is_path_sep(char x)
{
#ifdef _WIN32
    return x == '/' || x == '\\';
#else
    return x == '/';
#endif
}

static String_View dot_guard(String_View name)
{
    if (sv_eq(name, SV(".")) || sv_eq(name, SV(".."))) {
        return SV("");
    }

    return name;
}

String_View path_get_file_name(const char *begin)
{
    if (begin == NULL) {
        return SV_NULL;
    }

    // ./foooooo/hello.basm
    // ^         ^    ^
    // begin     sep  dot

    const size_t n = strlen(begin);
    const char *dot = begin + n - 1;

    while (begin <= dot && *dot != '.' && !is_path_sep(*dot)) {
        dot -= 1;
    }

    if (begin > dot) {
        return dot_guard(sv_from_cstr(begin));
    }

    if (is_path_sep(*dot)) {
        return dot_guard(sv_from_cstr(dot + 1));
    }

    const char *sep = dot;

    while (begin <= sep && !is_path_sep(*sep)) {
        sep -= 1;
    }
    sep += 1;

    assert(sep <= dot);
    return dot_guard((String_View) {
        .count = (size_t) (dot - sep),
        .data = sep,
    });
}

bool path_file_exist(const char *file_path)
{
#ifndef _WIN32
    struct stat statbuf = {0};
    return stat(file_path, &statbuf) == 0 &&
           S_ISREG(statbuf.st_mode);
#else
    // https://stackoverflow.com/a/6218957
    DWORD dwAttrib = GetFileAttributes(file_path);
    return (dwAttrib != INVALID_FILE_ATTRIBUTES &&
            !(dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#endif
}

Token lexer_eat_token(Lexer *lexer)
{
    lexer->current_line = sv_trim_left(lexer->current_line);

    // Find next non-empty line.

    while (lexer_current_line_is_empty(lexer) && lexer->current_input.count > 0) {
       lexer_next_line(lexer); 
    }

    Token token;
    token.type = TOKEN_END_OF_INPUT;
    token.l0 = lexer->current_line_number;
    token.c0 = lexer_current_character_index(lexer);
    token.c1 = token.c0 + 1;

    if (lexer_current_line_is_empty(lexer)) return token;

    int c = lexer_peek_character(lexer);

    if (isalnum(c)) {
        String_View literal = sv_chop_left_while(&lexer->current_line, isident);
        token.c1 = lexer_current_character_index(lexer);

        if (isdigit(c)) {
            bool ok = parse_int_value(literal, 10, &token.int_value);
            token.type = ok ? TOKEN_NUMBER : TOKEN_ERROR;
            return token;
        }

        token.type = parse_keyword_or_ident_token_type(literal);
        if (token.type == TOKEN_IDENT) {
            token.string_value = literal;
        }
        return token;
    }

    token.type = lexer_eat_character(lexer);

    switch (token.type) {
    case '-': {
        int d = lexer_peek_character(lexer);
        if (d == '=') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_MINUSEQUALS;
        } else if (d == '>') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_RIGHT_ARROW;
        }
    } break;
    case '+': lexer_parse_maybe_equals_token(lexer, &token, TOKEN_PLUSEQUALS); break;
    case '*': lexer_parse_maybe_equals_token(lexer, &token, TOKEN_TIMESEQUALS); break;
    case '/': lexer_parse_maybe_equals_token(lexer, &token, TOKEN_DIVEQUALS); break;
    case '%': lexer_parse_maybe_equals_token(lexer, &token, TOKEN_MODEQUALS); break;
    case '=': lexer_parse_maybe_equals_token(lexer, &token, TOKEN_ISEQUAL); break;
    case '!': lexer_parse_maybe_equals_token(lexer, &token, TOKEN_ISNOTEQUAL); break;
    case '&': {
        int d = lexer_peek_character(lexer);
        if (d == '&') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_LOGICAL_AND;
        } else if (d == '=') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_BITWISE_AND_EQUALS;
        }
    } break;
    case '|': {
        int d = lexer_peek_character(lexer);
        if (d == '|') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_LOGICAL_OR;
        } else if (d == '=') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_BITWISE_OR_EQUALS;
        }
    } break;
    case '^': lexer_parse_maybe_equals_token(lexer, &token, TOKEN_BITWISE_XOR_EQUALS); break;
    case '<': {
        int d = lexer_peek_character(lexer);
        if (d == '<') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT;
        } else if (d == '=') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_LESSEQUALS;
        }
    } break;
    case '>': {
        int d = lexer_peek_character(lexer);
        if (d == '>') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_SHIFT_RIGHT;
        } else if (d == '=') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_GREATEREQUALS;
        }
    } break;
    case '.':
        if (lexer_peek_character(lexer) == '.') {
            lexer_eat_character(lexer); token.c1 += 1;
            token.type = TOKEN_DOUBLE_DOT;
        }
        break;
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
    case ',':
    case ':':
    case ';':
        break;
    default:
        token.type = TOKEN_ERROR;
    }

    return token;
}

void lexer_next_line(Lexer *lexer)
{
    lexer->current_line = sv_chop_by_delim(&lexer->current_input, '\n');
    lexer->current_line_start = lexer->current_line.data;
    lexer->current_line_number += 1;
    arrput(lexer->lines, lexer->current_line);
}

Token lexer_peek_token(Lexer *lexer)
{
    if (lexer->peek_full) return lexer->peek_token;
    lexer->peek_token = lexer_eat_token(lexer);
    lexer->peek_full = true;
    return lexer->peek_token;
}

Token lexer_next_token(Lexer *lexer)
{
    if (lexer->peek_full) {
        lexer->peek_full = false;
        return lexer->peek_token;
    }
    return lexer_eat_token(lexer);
}

Lexer_Position position_from_lexer(const Lexer *lexer)
{
    Lexer_Position pos;
    pos.l0 = lexer->current_line_number;
    pos.c0 = lexer_current_character_index(lexer);
    pos.c1 = pos.c0 + 1;
    return pos;
}

Lexer_Position position_from_token(Token token)
{
    Lexer_Position pos;
    pos.l0 = token.l0;
    pos.c0 = token.c0;
    pos.c1 = token.c1;
    return pos;
}

#define RED   "\x1B[31m"
#define GRN   "\x1B[32m"
#define YEL   "\x1B[33m"
#define BLU   "\x1B[34m"
#define MAG   "\x1B[35m"
#define CYN   "\x1B[36m"
#define WHT   "\x1B[37m"
#define RESET "\x1B[0m"
#define TAB   "    "

void lexer_report_error(const Lexer *lexer, Lexer_Position pos, const char *fmt, ...)
{
    if (pos.l0 == 0) {
        pos = position_from_lexer(lexer);
    }
    va_list args;
    va_start(args, fmt);
    const char *msg = vtprint(fmt, args);
    assert(msg);

    // Display the error message.
    fprintf(stderr, Loc_Fmt": Error: %s\n", SV_Arg(lexer->path_name), pos.l0, pos.c0, msg);

    fputc('\n', stderr);

    // Display the previous line if it exists.
    if (pos.l0 >= 2) {
        fprintf(stderr, TAB CYN "\t"SV_Fmt"\n" RESET, SV_Arg(lexer->lines[pos.l0-1]));
    }

    // Highlight the token in red.
    int n = pos.c1 - pos.c0;
    String_View line = lexer->lines[pos.l0-1];
    fprintf(stderr, TAB CYN "%.*s" RED "%.*s" CYN "%.*s\n" RESET,
        pos.c0, line.data,                  // before
        n, line.data + pos.c0,              // token
        (int)line.count - n, line.data + pos.c1  // after
    );
    
    exit(1);
    va_end(args);
}

void lexer_init(Lexer *l, String_View input)
{
    assert(input.data != NULL);
    
    l->file_name = SV_NULL;
    l->path_name = SV_NULL;
    l->lines = NULL;

    l->current_input = input;
    l->current_line = SV_NULL;
    l->current_line_start = NULL;
    l->current_line_number = 0;

    // l->peek_token can stay uninitialized as we should never read from it directly
    l->peek_full = false;
}

// TODO: We should probably support at least UTF-8 at some point.
// TODO: We literally only support comments where '#' is the first character of the line.
