#pragma(once)

typedef unsigned int uint32;

struct String {
    uint32 count = 0;
    char  *data  = 0;
};

struct Token {
    int col, row;
    int from_end;

    int type;
    String text;

    float f;
    int   s;
};

struct Tokenizer {
    int col, row;
    char at[2];

    String input;
};

void refill(Tokenizer *tokenizer) {
    
    if (tokenizer->input.count == 0) {
        tokenizer->at[0] = 0;
        tokenizer->at[1] = 0;
    } else if (tokenizer->input.count == 1) {
        tokenizer->at[0] = tokenizer->input.data[0];
        tokenizer->at[1] = 0;
    } else {
        tokenizer->at[0] = tokenizer->input.data[0];
        tokenizer->at[1] = tokenizer->input.data[1];
    }
}

void advance_chars(Tokenizer *tokenizer, uint32 count) {
    tokenizer->col += count;

    if (tokenizer->input.count >= count) {
        tokenizer->input.data  += count;
        tokenizer->input.count -= count;
    } else {
        tokenizer->input.data += tokenizer->input.count;
        tokenizer->input.count = 0;
    }
    
    refill(tokenizer);
}

Tokenizer get_tokenizer(String file) {
    Tokenizer result;

    result.col = 1;
    result.row = 1;
    result.input = file;
    refill(&result);

    return result;
}

bool is_end_of_line(char c) {
    if (c == '\n' || c == '\r') return true;
    return false;
}
bool is_space(char c) {
    if (c == ' ' || c == '\t' || c == '\v' || c == '\f') return true;
    return false;
}

bool is_numeric(char c) {
    if (c >= '0' && c <= '9') return true;
    return false;
}

bool is_alpha(char c) {
    if (c >= 'a' && c <='z') return true;
    if (c >= 'A' && c <='Z') return true;
    if (c >= '_')            return true;
    return false;
}

bool double_return_sequence(char c0, char c1) {
    if (c0 == '\r' && c1 == '\n') return true;
    if (c0 == '\n' && c1 == '\r') return true;
    return false;
}

enum Token_Types {
    Token_Unknown,
    Token_Identifier,
    Token_Number,
    Token_Comment,

    Token_Open_Parenthesis,
    Token_Close_Parenthesis,
    Token_Semicolon,
    Token_Plus,
    Token_Minus,
    Token_Asterisk,
    Token_Percent,
    Token_Slash,
    Token_Comma,
    Token_Pound,

    Token_Equals,

    Token_Spaces,
    Token_End_Of_Line,
    Token_End_Of_File,

    Token_Types_Count,
};

Token get_raw_token(Tokenizer *tokenizer) {
    
    Token token;

    token.col      = tokenizer->col;
    token.row      = tokenizer->row;
    token.from_end = tokenizer->input.count;

    token.text = tokenizer->input;

    char c = tokenizer->at[0];
    advance_chars(tokenizer, 1);
    
    switch (c) {
        case '\0': token.type = Token_End_Of_File; break;

        case '(': token.type = Token_Open_Parenthesis;  break;
        case ')': token.type = Token_Close_Parenthesis; break;
        case ';': token.type = Token_Semicolon;         break;
        case ',': token.type = Token_Comma;             break;
        case '*': token.type = Token_Asterisk;          break;
        case '+': token.type = Token_Plus;              break;
        case '-': token.type = Token_Minus;             break;
        case '%': token.type = Token_Percent;           break;
        case '=': token.type = Token_Equals;            break;
        case '#': token.type = Token_Pound;             break;

        case '/': {
            if (tokenizer->at[0] == '/') {
                token.type = Token_Comment;
                
                advance_chars(tokenizer, 1);
                while (tokenizer->at[0] && !is_end_of_line(tokenizer->at[0])) {
                    advance_chars(tokenizer, 1);
                }
            } else {
                token.type = Token_Slash;
            }
        } break;

        default: {
            if (is_alpha(c)) {
                token.type = Token_Identifier;
                
                while (tokenizer->at[0] && (is_alpha(tokenizer->at[0]) || is_numeric(tokenizer->at[0]))) {
                    advance_chars(tokenizer, 1);
                }
            } else if (is_numeric(c)) {
                token.type = Token_Number;
                
                float parsed_f = (float) (c - '0');
                while (tokenizer->at[0] && is_numeric(tokenizer->at[0])) {
                    float digit = (float) (tokenizer->at[0] - '0');
                    parsed_f = parsed_f * 10.0f + digit;
                    advance_chars(tokenizer, 1);
                }
                
                if (tokenizer->at[0] == '.') {
                    advance_chars(tokenizer, 1);
                    float coefficient = 0.1f;
                    while (tokenizer->at[0] && is_numeric(tokenizer->at[0])) {
                        float digit = (float) (tokenizer->at[0] - '0');
                        parsed_f = parsed_f + digit * coefficient;
                        coefficient *= 0.1f;
                        advance_chars(tokenizer, 1);
                    }
                }

                while (tokenizer->at[0] && is_alpha(tokenizer->at[0])) {
                    advance_chars(tokenizer, 1);
                }

                token.f = parsed_f;
                token.s = (int) parsed_f;
            } else if (is_end_of_line(c)) {
                token.type = Token_End_Of_Line;
                if (double_return_sequence(c, tokenizer->at[0])) {
                    advance_chars(tokenizer, 1);
                }

                tokenizer->row++;
                tokenizer->col = 1;
            } else if (is_space(c)) {
                token.type = Token_Spaces;

                while (tokenizer->at[0] && is_space(tokenizer->at[0])) {
                    advance_chars(tokenizer, 1);
                }
            } else {
                token.type = Token_Unknown;
            }
        }
    }
    token.text.count = tokenizer->input.data - token.text.data;

    return token;
}

Token get_token(Tokenizer *tokenizer) {
    Token token;

    while (true) {
        token = get_raw_token(tokenizer);
        switch (token.type) {
            case Token_Spaces:
            case Token_Comment: continue;
        }
        break;
    }

    return token;
}

Token peek_token(Tokenizer *tokenizer) {
    Tokenizer tokenizer_copy = *tokenizer;

    Token token = get_token(&tokenizer_copy);
    return token;
}

Token peek_raw_token(Tokenizer *tokenizer) {
    Tokenizer tokenizer_copy = *tokenizer;

    Token token = get_raw_token(&tokenizer_copy);
    return token;
}

int string_length(char *s) {
    int len = 0;
    while (s[len] != '\0') len++;

    return len;
}

void eat_spaces_new_lines_and_comments(Tokenizer *tokenizer) {
	
	while (true) {
		Token token = peek_raw_token(tokenizer);
		switch (token.type) {
			case Token_Spaces:
			case Token_Comment:
			case Token_End_Of_Line: {
				get_raw_token(tokenizer);
				continue;
			}
		}
		break;
	}
}

bool peek_token_type(Tokenizer *tokenizer, Token *token, int token_type, bool eat_token_if_match = true) {
	*token = peek_token(tokenizer);
	if (token->type == token_type) {
		if (eat_token_if_match) get_token(tokenizer);
		return true;
	}
	
	return false;
}

bool maybe_parse_number(Tokenizer *tokenizer, Token *token) {
	Tokenizer t = *tokenizer;
	*token = get_token(&t);

	int sign = 1;
	if (token->type == Token_Minus) {
		sign = -1;
		*token = get_token(&t);
	}

	if (token->type != Token_Number) return false;
	
    token->f = sign * token->f;
    token->s = sign * token->s;

    *tokenizer = t;

	return true;
}