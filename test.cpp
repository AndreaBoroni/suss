#pragma(once)

#import "parser.h"
#import "main.cpp"

struct Result {
	String name;
	f64    value;
};

struct Result_Array {
	Result *memory;
	int length;

	Result * operator [] (int index) {
		return &memory[index];
	}
};

Result_Array get_result_array(int length) {
	Result_Array results;
	results.memory = (Result *) calloc(sizeof(Result), length);
	results.length = length;

	return results;
}

void reset_functions_and_variables() {
	functions.reset();
	variables.reset();
}

Result_Array parse_expected_results(Tokenizer *tokenizer) {

	Result_Array results = {0};

	Token token = peek_token(tokenizer);
	if (token.type == Token_Pound) {
		get_token(tokenizer);

		token = peek_token(tokenizer);
		if (token.type == Token_Number && token.s > 0) {
			get_token(tokenizer);
			results = get_result_array(token.s);
		} else {
			return results;
		}
	} else {
		return results;
	}
	eat_spaces_new_lines_and_comments(tokenizer);

	int results_parsed = 0;
	while (true) {

		if (!peek_token_type(tokenizer, &token, Token_Pound)) break;
		if (results_parsed >= results.length) break;

		if (peek_token_type(tokenizer, &token, Token_Identifier)) {
			results[results_parsed]->name = token.text;
		} else {
			break;
		}

		if (!peek_token_type(tokenizer, &token, Token_Equals)) {
			break;
		}
		
		if (peek_token_type(tokenizer, &token, Token_Number)) {
			results[results_parsed]->value = token.f;
		} else {
			break;
		}

		results_parsed++;
		eat_spaces_new_lines_and_comments(tokenizer);
	}
	
	return results;
}

bool test_program(char *file_name) {
	
	reset_functions_and_variables();
	initialize_functions_and_constants();

	String file;
	file.data = load_file_memory(file_name, &file.count);
	if (file.data == NULL) return false;
	Tokenizer tokenizer = get_tokenizer(file);

	Result_Array expected_results = parse_expected_results(&tokenizer);

	Node **lines = NULL;
	int number_of_lines = 0;

    while (true) {
        Token token = peek_token(&tokenizer);
        if (token.type == Token_End_Of_File) break;
        
        auto parsed_line = parse_line(&tokenizer);
        if (parsed_line) lines = add(lines, parsed_line, &number_of_lines);
    }

	bit_array line_computed = get_bit_array(number_of_lines);
	int at = 0;
	int last_line_computed = 0;
	while (true) {
		Node *line = lines[at];
		int result = isolate_unknown(line);
		
		if (line_computed[at]) goto loop_end;
		
		if (result == Root_Node_Is_Not_Equals)      line_computed.set_bit(at);
		if (result == Unknown_Not_Found)            line_computed.set_bit(at);
		if (result == Found_More_Than_One_Unknown)  goto loop_end;
		if (result == Found_Unknown_More_Than_Once) goto loop_end;
		
		if (result == Unknown_Found) {
			f64 eval_result = eval_tree(line);
			line_computed.set_bit(at);
			last_line_computed = at;
		}
		
	loop_end:
		int total_lines_computed = 0;
		for (int i = 0; i < number_of_lines; i++) total_lines_computed += line_computed[i];
		if (total_lines_computed == number_of_lines) break;
		
		at = (at+1) % number_of_lines;
		if (at == last_line_computed) return false;
	}

	for (int r = 0; r < expected_results.length; r++) {
		for (int i = 0; i < variables.length; i++) {
			if (!are_strings_equal(expected_results[r]->name, variables[i].name)) continue;
			if (!variables[i].initialized) return false;
			if (expected_results[r]->value != variables[i].value) return false;			
		}
	}

	return true;
}

int main(void) {
	if (test_program("tests/sum.txt")) printf("Success\n");
	else printf("Failed\n");
}