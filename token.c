#include <ctype.h>
#include <assert.h>
#include <stdarg.h>

#ifndef _WIN32
#    include <sys/types.h>
#    include <sys/stat.h>
#    include <unistd.h>
#else
#    define  WIN32_LEAN_AND_MEAN
#    include "windows.h"
#endif // _WIN32

#include "common.h"
#include "parser.h"
#include "workspace.h"
#include "string_builder.h"
#include "vendor/stb_ds.h"

// All of these are @Internal.
Token find_next_token(Parser *parser);
void parser_next_line(Parser *parser);
void parser_add_line_to_source_file(Parser *parser);

#define parser_current_character_index(parser) ((parser)->current_line.data - (parser)->current_line_start)

inline int peek_character(Parser *parser)
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

inline void parser_add_line_to_source_file(Parser *parser)
{
    String_View added_line = parser->current_line;
    added_line.count += 1; // To include the newline.
    arrput(parser->workspace->files[parser->file_index].lines, added_line);
}

inline void parser_next_line(Parser *parser)
{
    parser->current_line = sv_chop_by_delim(&parser->current_input, '\n');

    parser_add_line_to_source_file(parser);

    parser->current_line_start = parser->current_line.data;
    parser->current_line = sv_trim_left(parser->current_line);
    parser->current_line_number += 1;
}

inline Source_Location parser_current_location(Parser *parser)
{
    Source_Location loc;
    loc.l0 = parser->current_line_number;
    loc.l1 = -1;
    loc.c0 = parser_current_character_index(parser);
    loc.c1 = -1;
    loc.fid = parser->file_index;
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
        token.number_flags = 0;
        token.location.c1 = parser_current_character_index(parser);
        if (!parse_int_value(literal, 10, &token.integer_value)) {
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
        String_Builder sb = {0};

        while (peek_character(parser) != -1) {
            c = eat_character(parser);

            if (c == '"') {
                token.type = TOKEN_STRING;
                token.string_value.data = sb.data;
                token.string_value.count = sb.count;
                token.location.c1 = parser_current_character_index(parser);
                return token;
            }

            if (c == '\\') {
                c = eat_character(parser);
                switch (c) {
                case -1:
                    parser_report_error(parser, parser_current_location(parser),
                        "While parsing a string literal, we encountered a backslash with no following character.");
                case '0':  sb_append(&sb, "\0", 1); break;
                case 'n':  sb_append(&sb, "\n", 1); break;
                case 't':  sb_append(&sb, "\t", 1); break;
                case '"':  sb_append(&sb, "\"", 1); break;
                case '\'': sb_append(&sb, "'", 1);  break;
                default:
                    parser_report_error(parser, parser_current_location(parser),
                        "Illegal escape sequence '%c'.", (char)c);
                }
                continue;
            }

            char str = (char)c;
            sb_append(&sb, &str, 1);
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
    // Single-character tokens.
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
    case '#':
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

inline Token parser_fill_peek_buffer(Parser *parser)
{
    assert(parser->peek_count < PARSER_PEEK_CAPACITY); // Peeked too many times.
    const size_t internal_index = (parser->peek_begin + parser->peek_count) % PARSER_PEEK_CAPACITY;
    const Token token = find_next_token(parser);
    parser->peek_buffer[internal_index] = token;
    parser->peek_count += 1;
    return token;
}

inline Token peek_token(Parser *parser, size_t user_index)
{
    assert(user_index < PARSER_PEEK_CAPACITY);
    
    if (user_index < parser->peek_count) {
        const size_t internal_index = (parser->peek_begin + user_index) % PARSER_PEEK_CAPACITY;
        return parser->peek_buffer[internal_index];
    }

    user_index -= parser->peek_count;
    while (user_index > 0) parser_fill_peek_buffer(parser);
    return parser_fill_peek_buffer(parser);
}

inline Token peek_next_token(Parser *parser)
{
    return peek_token(parser, 0);
}

inline Token eat_next_token(Parser *parser)
{
    if (parser->peek_count == 0) return find_next_token(parser);
    const size_t internal_index = parser->peek_begin % PARSER_PEEK_CAPACITY;
    const Token result = parser->peek_buffer[internal_index];
    parser->peek_begin = (parser->peek_begin + 1) % PARSER_PEEK_CAPACITY;
    parser->peek_count -= 1;
    return result;
}

const char *token_type_to_string(int type)
{
    if (type < 256) {
        return tprint("%c", (char) type);
    }

    switch (type) {
    case TOKEN_IDENT: return "identifier";
    case TOKEN_NUMBER: return "number";
    case TOKEN_STRING: return "string";
    case TOKEN_PLUSEQUALS: return "+=";
    case TOKEN_MINUSEQUALS: return "-=";
    case TOKEN_TIMESEQUALS: return "*=";
    case TOKEN_DIVEQUALS: return "/=";
    case TOKEN_MODEQUALS: return "%=";
    case TOKEN_ISEQUAL: return "==";
    case TOKEN_ISNOTEQUAL: return "!=";
    case TOKEN_LOGICAL_AND: return "&&";
    case TOKEN_LOGICAL_OR: return "||";
    case TOKEN_LESSEQUALS: return "<=";
    case TOKEN_GREATEREQUALS: return ">=";

    case TOKEN_RIGHT_ARROW: return "->";
    case TOKEN_DOUBLE_DOT: return "..";

    case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT: return "<<";
    case TOKEN_SHIFT_RIGHT: return ">>";
    case TOKEN_BITWISE_AND_EQUALS: return "&=";
    case TOKEN_BITWISE_OR_EQUALS: return "|=";
    case TOKEN_BITWISE_XOR_EQUALS: return "^=";
    
    case TOKEN_KEYWORD_IF: return "if";
    case TOKEN_KEYWORD_THEN: return "then";
    case TOKEN_KEYWORD_ELSE: return "else";
    // case TOKEN_KEYWORD_CASE: return "case";
    case TOKEN_KEYWORD_RETURN: return "return";
    case TOKEN_KEYWORD_STRUCT: return "struct";
    case TOKEN_KEYWORD_WHILE: return "while";
    case TOKEN_KEYWORD_BREAK: return "break";
    case TOKEN_KEYWORD_CONTINUE: return "continue";
    case TOKEN_KEYWORD_USING: return "using";

    case TOKEN_KEYWORD_DEFER: return "defer";
    case TOKEN_KEYWORD_SIZE_OF: return "size_of";
    case TOKEN_KEYWORD_TYPE_OF: return "type_of";
    case TOKEN_KEYWORD_INITIALIZER_OF: return "initializer_of";
    case TOKEN_KEYWORD_TYPE_INFO: return "type_info";
    case TOKEN_KEYWORD_NULL: return "null";

    case TOKEN_KEYWORD_ENUM: return "enum";
    case TOKEN_KEYWORD_TRUE: return "true";
    case TOKEN_KEYWORD_FALSE: return "false";
    case TOKEN_KEYWORD_UNION: return "union";

    case TOKEN_NOTE: return "note";
    case TOKEN_END_OF_INPUT: return "end of input";

    case TOKEN_ERROR: return "invalid token";

    default: return "**INVALID**";
    }
}

// TODO: We should probably support at least UTF-8 at some point.
