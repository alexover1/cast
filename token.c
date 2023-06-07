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

#include "common.h"
#include "parser.h"
#include "string_builder.h"
#include "vendor/stb_ds.h"

Token find_next_token(Parser *parser); // @Interal
void parser_next_line(Parser *parser); // @Interal
Source_Location parser_current_location(Parser *parser); // @Interal

#define parser_current_character_index(parser) ((parser)->current_line.data - (parser)->current_line_start + 1)

static inline int peek_character(Parser *parser)
{
    if (parser->current_line.count) return (int) *parser->current_line.data;
    return -1;
}

static inline int eat_character(Parser *parser)
{
    assert(parser->current_line.count > 0);
    int character = (int) *parser->current_line.data;
    parser->current_line.data += 1;
    parser->current_line.count -= 1;
    return character;
}

static inline bool starts_identifier(char x)
{
    return isalpha(x) || x == '_';
}

static inline bool continues_identifier(char x)
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
    return i == s.count;
}

static inline void parse_maybe_equals_token(Parser *parser, Token *token, Token_Type type_with_equals)
{
    if (peek_character(parser) == '=') {
        eat_character(parser);
        token->type = type_with_equals;
    }
}

static Token_Type parse_keyword_or_ident_token_type(String_View s)
{
    switch (s.count) {
    case 2:
        if (sv_eq2(s, SV("if"))) return TOKEN_KEYWORD_IF;
        break;
    case 3:
        if (sv_eq2(s, SV("for"))) return TOKEN_KEYWORD_FOR;
        break;
    case 4:
        if (sv_eq2(s, SV("then"))) return TOKEN_KEYWORD_THEN;
        if (sv_eq2(s, SV("else"))) return TOKEN_KEYWORD_ELSE;
        if (sv_eq2(s, SV("null"))) return TOKEN_KEYWORD_NULL;
        if (sv_eq2(s, SV("enum"))) return TOKEN_KEYWORD_ENUM;
        if (sv_eq2(s, SV("true"))) return TOKEN_KEYWORD_TRUE;
        break;
    case 5:
        if (sv_eq2(s, SV("while"))) return TOKEN_KEYWORD_WHILE;
        if (sv_eq2(s, SV("break"))) return TOKEN_KEYWORD_BREAK;
        if (sv_eq2(s, SV("using"))) return TOKEN_KEYWORD_USING;
        if (sv_eq2(s, SV("defer"))) return TOKEN_KEYWORD_DEFER;
        if (sv_eq2(s, SV("false"))) return TOKEN_KEYWORD_FALSE;
        if (sv_eq2(s, SV("union"))) return TOKEN_KEYWORD_UNION;
        break;
    case 6:
        if (sv_eq2(s, SV("return"))) return TOKEN_KEYWORD_RETURN;
        if (sv_eq2(s, SV("struct"))) return TOKEN_KEYWORD_STRUCT;
        break;
    case 7:
        if (sv_eq2(s, SV("size_of"))) return TOKEN_KEYWORD_SIZE_OF;
        if (sv_eq2(s, SV("type_of"))) return TOKEN_KEYWORD_TYPE_OF;
        break;
    case 8:
        if (sv_eq2(s, SV("continue"))) return TOKEN_KEYWORD_CONTINUE;
        break;
    case 9:
        if (sv_eq2(s, SV("type_info"))) return TOKEN_KEYWORD_TYPE_INFO;
        break;
    case 14:
        if (sv_eq2(s, SV("initializer_of"))) return TOKEN_KEYWORD_INITIALIZER_OF;
        break;
    }
    return TOKEN_IDENT;
}

inline void parser_next_line(Parser *parser)
{
    parser->current_line = sv_chop_by_delim(&parser->current_input, '\n');
    parser->current_line = sv_trim_left(parser->current_line); // TODO: if we want to preserve indentation this must be called after arrput.
    parser->current_line_start = parser->current_line.data;
    parser->current_line_number += 1;
    arrput(parser->lines, parser->current_line);
}

inline Source_Location parser_current_location(Parser *parser)
{
    Source_Location loc;
    loc.l0 = parser->current_line_number;
    loc.l1 = -1;
    loc.c0 = parser_current_character_index(parser);
    loc.c1 = -1;
    return loc;
}

Token find_next_token(Parser *parser)
{
    parser->current_line = sv_trim_left(parser->current_line);

    // Find next non-empty line.

    while (parser->current_line.count == 0 && parser->current_input.count > 0) {
        parser_next_line(parser); 
    }

    Token token;
    token.type = TOKEN_END_OF_INPUT;
    token.location = parser_current_location(parser);

    if (parser->current_line.count == 0) return token;

    int c = peek_character(parser);

    if (isdigit(c)) {
        String_View literal = sv_chop_left_while(&parser->current_line, continues_identifier);
        token.location.c1 = parser_current_character_index(parser);
        token.number_flags = 0;
        if (!parse_int_value(literal, 10, &token.int_value)) {
            parser_report_error(parser, token.location, "Illegal characters in number literal.");
        }
        token.type = TOKEN_NUMBER;
        return token;
    }

    if (starts_identifier(c)) {
        String_View literal = sv_chop_left_while(&parser->current_line, continues_identifier);
        token.location.c1 = parser_current_character_index(parser);
        token.type = parse_keyword_or_ident_token_type(literal);
        if (token.type == TOKEN_IDENT) {
            token.string_value = literal;
        }
        return token;
    }

    token.type = eat_character(parser);

    switch (token.type) {
    case '"': {
        size_t n = 0;
        while (n < parser->current_line.count) {
            if (parser->current_line.data[n] == '"') {
                // Copy because the input string will be freed before the Ast_Literal is freed. 
                token.string_value = arena_sv_copy(parser->arena, sv_chop_left(&parser->current_line, n));
                token.type = TOKEN_STRING;
                token.location.c1 = parser_current_character_index(parser);
                eat_character(parser);
                return token;
            }
            n += 1;
        }
        token.location.c1 = parser_current_character_index(parser);
        parser_report_error(parser, token.location, "While parsing a string literal, we encountered the end of the input.");
    }
    case '-': {
        int d = peek_character(parser);
        if (d == '=') {
            eat_character(parser);
            token.type = TOKEN_MINUSEQUALS;
        } else if (d == '>') {
            eat_character(parser);
            token.type = TOKEN_RIGHT_ARROW;
        }
    } break;
    case '+': parse_maybe_equals_token(parser, &token, TOKEN_PLUSEQUALS); break;
    case '*': parse_maybe_equals_token(parser, &token, TOKEN_TIMESEQUALS); break;
    case '/': parse_maybe_equals_token(parser, &token, TOKEN_DIVEQUALS); break;
    case '%': parse_maybe_equals_token(parser, &token, TOKEN_MODEQUALS); break;
    case '=': parse_maybe_equals_token(parser, &token, TOKEN_ISEQUAL); break;
    case '!': parse_maybe_equals_token(parser, &token, TOKEN_ISNOTEQUAL); break;
    case '&': {
        int d = peek_character(parser);
        if (d == '&') {
            eat_character(parser);
            token.type = TOKEN_LOGICAL_AND;
        } else if (d == '=') {
            eat_character(parser);
            token.type = TOKEN_BITWISE_AND_EQUALS;
        }
    } break;
    case '|': {
        int d = peek_character(parser);
        if (d == '|') {
            eat_character(parser);
            token.type = TOKEN_LOGICAL_OR;
        } else if (d == '=') {
            eat_character(parser);
            token.type = TOKEN_BITWISE_OR_EQUALS;
        }
    } break;
    case '^': parse_maybe_equals_token(parser, &token, TOKEN_BITWISE_XOR_EQUALS); break;
    case '<': {
        int d = peek_character(parser);
        if (d == '<') {
            eat_character(parser);
            token.type = TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT;
        } else if (d == '=') {
            eat_character(parser);
            token.type = TOKEN_LESSEQUALS;
        }
    } break;
    case '>': {
        int d = peek_character(parser);
        if (d == '>') {
            eat_character(parser);
            token.type = TOKEN_SHIFT_RIGHT;
        } else if (d == '=') {
            eat_character(parser);
            token.type = TOKEN_GREATEREQUALS;
        }
    } break;
    case '.':
        if (peek_character(parser) == '.') {
            eat_character(parser);
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
    case '~':
        break;
    default:
        token.type = TOKEN_ERROR;
    }

    token.location.c1 = parser_current_character_index(parser);
    return token;
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

inline Token peek_next_token(Parser *parser)
{
    if (parser->peek_full) return parser->peek_token;
    parser->peek_token = find_next_token(parser);
    parser->peek_full = true;
    return parser->peek_token;
}

inline Token eat_next_token(Parser *parser)
{
    if (parser->peek_full) {
        parser->peek_full = false;
        return parser->peek_token;
    }
    return find_next_token(parser);
}

// TODO: We should probably support at least UTF-8 at some point.
