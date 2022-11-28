#pragma(once)

#import "parser.h"
#import "main.cpp"

#define epsilon 0.00001

struct Result {
	String name;
	f64    value;
};

enum Fail_Code {
	All_Good             = 0,
	Nothing_To_Check     = 1,
	Unsolvable_Equations = 2,
	Unsolved_Variable    = 3,
	Wrong_Result         = 4,
	Unparsed_Results     = 5,
	Less_Results_Defined = 6,
	Could_Not_Load_File  = 7,
};

struct Result_Array {
	Result *memory;
	int length;

	Fail_Code code;

	Result * operator [] (int index) {
		return &memory[index];
	}
};

struct Fail_Info {
	String variable_name;
	f64 variable_value;
	f64 variable_actual_value;

	Fail_Code code;
};

Result_Array get_result_array(int length) {
	Result_Array results;
	results.memory = (Result *) calloc(sizeof(Result), length);
	results.length = length;
	results.code   = All_Good;

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
	if (number_of_unknowns == 0) {
		results.code = Nothing_To_Check;
		return results;
	}
	results = get_result_array(number_of_unknowns);

	Token token;
	int results_parsed = 0;
	while (true) {

		if (!peek_token_type(tokenizer, &token, Token_Pound)) {
			if (results_parsed != number_of_unknowns) results.code = Less_Results_Defined;
			break;
		}
		if (results_parsed >= results.length) break;

		if (peek_token_type(tokenizer, &token, Token_Identifier)) {
			results[results_parsed]->name = token.text;
		} else {
			results.code = Unparsed_Results;
			break;
		}

		if (!peek_token_type(tokenizer, &token, Token_Equals)) {
			results.code = Unparsed_Results;
			break;
		}
		
		if (maybe_parse_number(tokenizer, &token)) {
			results[results_parsed]->value = token.f;
		} else {
			results.code = Unparsed_Results;
			break;
		}

		results_parsed++;
		eat_spaces_new_lines_and_comments(tokenizer);
	}
	
	return results;
}

Fail_Info test_program(char *file_name) {

	Fail_Info info;
	info.code = All_Good;
	
	reset_functions_and_variables();
	initialize_functions_and_constants();

	String file;
	file.data = load_file_memory(file_name, &file.count);
	if (file.data == NULL) {
		info.code = Could_Not_Load_File;
		return info;
	}
	Tokenizer tokenizer = get_tokenizer(file);

	Result_Array expected_results = parse_expected_results(&tokenizer);

	if (expected_results.code != All_Good) {
		info.code = expected_results.code;
		return info;
	}

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
		if (at == last_line_computed) {
			info.code = Unsolvable_Equations;
			return info;
		}
	}

	for (int r = 0; r < expected_results.length; r++) {
		bool found = false;
		for (int i = 0; i < variables.length; i++) {
			if (!are_strings_equal(expected_results[r]->name, variables[i].name)) continue;
			if (!variables[i].initialized) {
				info.code = Unsolved_Variable;
				info.variable_name  = expected_results[r]->name;
				info.variable_value = expected_results[r]->value;

				return info;
			}
		
			if (abs(expected_results[r]->value - variables[i].value) > epsilon) {
				info.code = Wrong_Result;
				info.variable_name  = expected_results[r]->name;
				info.variable_value = expected_results[r]->value;
				info.variable_actual_value = variables[i].value;

				return info;
			}

			found = true;
			break;
		}
		if (!found) {
			info.code = Unsolved_Variable;
			info.variable_name  = expected_results[r]->name;
			info.variable_value = expected_results[r]->value;

			return info;
		}
	}

	return info;
}

int longest_file_name(char *folder_name) {
	
	WIN32_FIND_DATA find_file_data;
	char *tests_folder_path = "tests/*";
	HANDLE handle = FindFirstFile(tests_folder_path, &find_file_data);
	
	int result = 0;

	do {
		if ((find_file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) continue;

		char *file_name = find_file_data.cFileName;
		int name_length   = strlen(file_name);

		if (name_length > result) result = name_length;
	} while (FindNextFile(handle, &find_file_data) != 0);

	FindClose(handle);

	return result;
}

int main(void) {

	char *tests_folder_path = "tests/*";

	int longest_name = longest_file_name(tests_folder_path);

	WIN32_FIND_DATA find_file_data;
	HANDLE handle = FindFirstFile(tests_folder_path, &find_file_data);

	do {
		if ((find_file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) continue;

		char *file_name = find_file_data.cFileName;
		int name_length   = strlen(file_name);
		int folder_length = strlen(tests_folder_path);

		char *full_file_name = (char *) malloc(folder_length + name_length);
		memcpy(full_file_name, tests_folder_path, folder_length);
		memcpy(full_file_name + folder_length - 1, file_name, name_length);
		full_file_name[folder_length + name_length - 1] = '\0';
		printf("Testing: %s", full_file_name);
		for (int i = 0; i < longest_name - name_length; i++) printf(" ");
		printf(" ----> ");

		Fail_Info info = test_program(full_file_name);
		if (info.code == All_Good) printf("Success\n");
		else {
			printf("Failed\n");
			switch(info.code) {
				case Nothing_To_Check: {
					printf("No results defined\n");
				} break;
				case Unsolvable_Equations: {
					printf("Possible circular dependencies in the equations\n");
				} break;
				case Unsolved_Variable: {
					printf("Variable ");
					for (int c = 0; c < info.variable_name.count; c++) printf("%c", info.variable_name.data[c]);
					printf(" is not computed\n");
				} break;
				case Wrong_Result: {
					printf("Variable ");
					for (int c = 0; c < info.variable_name.count; c++) printf("%c", info.variable_name.data[c]);
					printf(" computed to a wrong result.\nComputed: %f\nStored:   %f\n", info.variable_actual_value, info.variable_value);
				} break;
				case Unparsed_Results: {
					printf("Unable to parse results\n");
				} break;
				case Less_Results_Defined: {
					printf("# is in the file and it is not used to define a result\n");
				} break;
				case Could_Not_Load_File : {
					printf("Could not load file\n");
				} break;

			}
		}
		

		free(full_file_name);
	} while (FindNextFile(handle, &find_file_data) != 0);
	
	FindClose(handle);
}