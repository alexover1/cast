#include <ctype.h>
#include <assert.h>
#include "lexer.h"

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
    token.l1 = lexer->current_line_number;
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

void lexer_init(Lexer *l, String_View input)
{
    assert(input.data != NULL);
    
    l->current_file_name = SV_NULL;
    l->current_path_name = SV_NULL;

    l->current_input = input;
    l->current_line = SV_NULL;
    l->current_line_start = NULL;
    l->current_line_number = 0;

    // l->peek_token can stay uninitialized as we should never read from it directly
    l->peek_full = false;
}

// TODO: We should probably support at least UTF-8 at some point.
// TODO: We literally only support comments where '#' is the first character of the line.
