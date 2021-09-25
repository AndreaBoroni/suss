#pragma(once)

#import "parser.h"
#import "main.cpp"

#define epsilon 0.00001

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

int count_pounds(Tokenizer *tokenizer) {
	Tokenizer t = *tokenizer;

	int result = 0;
	while (true) {
		Token token = get_token(&t);
		if (token.type == Token_Pound) result++;
		if (token.type == Token_End_Of_File) break;
	}

	return result;
}

Result_Array parse_expected_results(Tokenizer *tokenizer) {

	Result_Array results = {0};

	int number_of_unknowns = count_pounds(tokenizer);
	if (number_of_unknowns == 0) return results;
	results = get_result_array(number_of_unknowns);

	Token token;
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
		
		if (maybe_parse_number(tokenizer, &token)) {
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
		
		if (!line_computed[at]) {
			if (result == Root_Node_Is_Not_Equals) line_computed.set_bit(at);
			if (result == Unknown_Not_Found)       line_computed.set_bit(at);
			if (result == Unknown_Found) {
				f64 eval_result = eval_tree(line);
				line_computed.set_bit(at);
				last_line_computed = at;
			}
		}
		
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
			if (abs(expected_results[r]->value - variables[i].value) < epsilon) {
			}
		}
	}

	return true;
}

int main(void) {

	WIN32_FIND_DATA find_file_data;
	char *tests_folder_path = "tests/*";
	HANDLE handle = FindFirstFile(tests_folder_path, &find_file_data);
	
	do {
		if (!(find_file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {

			char *file_name = find_file_data.cFileName;
			int name_length   = strlen(file_name);
			int folder_length = strlen(tests_folder_path);

			char *full_file_name = (char *) malloc(folder_length + name_length);
			memcpy(full_file_name, tests_folder_path, folder_length);
			memcpy(full_file_name + folder_length - 1, file_name, name_length);
			full_file_name[folder_length + name_length - 1] = '\0';
			printf("Testing: %s    --->    ", full_file_name);

			if (test_program(full_file_name)) printf("Success\n");
			else printf("Failed\n");
			
			free(full_file_name);
		}
	} while (FindNextFile(handle, &find_file_data) != 0);
	
	FindClose(handle);
}