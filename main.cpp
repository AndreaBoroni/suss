#include "windows_handling.h"
#include "parser.h"

#define assert(Expression) if (!(Expression)) {                                               \
                               printf("Asertion failed here:\n %s:%d\n", __FILE__, __LINE__); \
                               *(int *)0 = 0; }

typedef unsigned char u8;
typedef unsigned int  u32;
typedef long long     s64;
typedef double        f64;

bool are_strings_equal(String s1, String s2) {
    if (s1.count != s2.count) return false;

    for (int i = 0; i < s1.count; i++) {
        if (s1.data[i] != s2.data[i]) return false;
    }
    return true;
}

bool are_strings_equal(String s1, char *s2) {
    if (s1.count != string_length(s2)) return false;

    for (int i = 0; i < s1.count; i++) {
        if (s1.data[i] != s2[i]) return false;
    }
    return true;
}

String _s(char *s) {
    String result;
    result.count = string_length(s);
    result.data  = s;
    return result;
}

void report_error(char *error, int line = -1, int col = -1) {
    printf("Error: %s\n", error);
    if (line != -1 && col != -1) {
        printf("In line %d col %d\n", line, col);
    }
    *(int *)0 = 0;
}

struct Function {
    String name;
    void *pointer;
    int number_of_arguments;
};

struct Variable {
    String name;
    f64    value;

    bool initialized;
};

enum Node_Type {
    Node_Type_Unknown,

    Node_Number,
    Node_Variable,
    Node_Function_Call,
    
    Node_Plus,
    Node_Minus,
    Node_Multiply,
    Node_Divide,
    Node_Modulus,
    Node_Unary_Minus,
    Node_Equals,
};

enum Expression_Flags {
    Exp_Inside_Paren  = 0x01,
    Exp_Ends_In_Comma = 0x02,
};

Node_Type token_to_binary_op(int token_type) {
    if (token_type == Token_Plus)     return Node_Plus;
    if (token_type == Token_Minus)    return Node_Minus;
    if (token_type == Token_Asterisk) return Node_Multiply;
    if (token_type == Token_Slash)    return Node_Divide;
    if (token_type == Token_Percent)  return Node_Modulus;

    return Node_Type_Unknown;
}

struct Node {
    Node_Type type;
    f64 value;

    Function *fun_pointer;
    Variable *var_ptr;

    Node *left;
    Node *right;

    bool parenthesized;
};

int get_precedence(Node *node) {

    if (node->parenthesized) return 1000;
    
    switch(node->type) {
        case Node_Equals: return 0;
        
        case Node_Plus:
        case Node_Minus: return 100;
        
        case Node_Multiply:
        case Node_Divide:
        case Node_Modulus: return 150;

        case Node_Unary_Minus: return 200;

        case Node_Number:
        case Node_Variable:
        case Node_Function_Call: return 1000;
    }
    return -1;
}

bool is_token_end_of_line(int type) {
    if (type == Token_Semicolon)   return true;
    if (type == Token_End_Of_Line) return true;
    if (type == Token_End_Of_File) return true;

    return false;
}

void print_node(Node *node) {
    if (node->parenthesized) printf("(");
    if (node->type == Node_Number) printf("%.1f", node->value);

    if (node->type == Node_Plus)     printf("+");
    if (node->type == Node_Minus)    printf("-");
    if (node->type == Node_Multiply) printf("*");
    if (node->type == Node_Divide)   printf("/");
    if (node->type == Node_Modulus)  printf("%%");
    if (node->type == Node_Equals)   printf("=");

    if (node->type == Node_Unary_Minus) printf("-");

    if (node->type == Node_Function_Call) {
        for (int i = 0; i < node->fun_pointer->name.count; i++) printf("%c", node->fun_pointer->name.data[i]);
    }
    if (node->type == Node_Variable) {
        for (int i = 0; i < node->var_ptr->name.count; i++) printf("%c", node->var_ptr->name.data[i]);
    }
    if (node->parenthesized) printf(")");
}

void rotate_tree(Node *node) {
    Node *bottom_node = node->right;
    
    Node *left_top     = node->left;
    Node *left_bottom  = bottom_node->left;
    Node *right_bottom = bottom_node->right;

    Node_Type top_op = node->type;
    node->type = bottom_node->type;
    bottom_node->type = top_op;

    node->left  = bottom_node;
    node->right = right_bottom;

    bottom_node->left  = left_top;
    bottom_node->right = left_bottom;
}

#define Node_Bucket_Length 4096
struct Node_Bucket_Array {
    Node nodes[Node_Bucket_Length];
    int used = 0;

    Node *get_node_mem() {
        assert(used < Node_Bucket_Length);
        Node *result = &nodes[used++];
        memset(result, 0, sizeof(Node));
    }

    Node *make(Node_Type type) {
        Node *result = get_node_mem();
        result->type = type;
        return result;
    }

    Node *make_variable(Variable *pointer) {
        Node *result = get_node_mem();
        result->type = Node_Variable;
        result->var_ptr = pointer;
        return result;
    }

    Node *make_number(f64 value) {
        Node *result  = get_node_mem();
        result->type  = Node_Number;
        result->value = value;
        return result;
    }

    Node *make_function_call(Function *pointer) {
        Node *result = get_node_mem();
        result->type = Node_Function_Call;
        result->fun_pointer = pointer;
        return result;
    }
};

//
// Functions
//
f64 sin_ (f64 a) { return sin(a); }
f64 cos_ (f64 a) { return cos(a); }
f64 tan_ (f64 a) { return tan(a); }
f64 cot_ (f64 a) { return cos(a)/sin(a); }
f64 asin_(f64 a) { return asin(a); }
f64 acos_(f64 a) { return acos(a); }
f64 atan_(f64 a) { return atan(a); }

f64 pow_ (f64 b, f64 e) { return pow(b, e); }
f64 sqrt_(f64 a)        { return pow(a, 0.5); }
f64 ln_  (f64 a)        { return log(a); }
f64 log_ (f64 a, f64 b) { return log(a)/log(b); }

f64 max_(f64 a, f64 b) { return a > b ? a : b; }
f64 min_(f64 a, f64 b) { return a > b ? b : a; }

const int n_functions = 13;
Function functions[n_functions];

Function *initialize_functions() {
    functions[0]  = {_s("sqrt"), (void *) &sqrt_, 1};
    functions[1]  = {_s("sin"),  (void *) &sin_,  1};
    functions[2]  = {_s("cos"),  (void *) &cos_,  1};
    functions[3]  = {_s("tan"),  (void *) &tan_,  1};
    functions[4]  = {_s("cot"),  (void *) &cot_,  1};
    functions[5]  = {_s("asin"), (void *) &asin_, 1};
    functions[6]  = {_s("acos"), (void *) &acos_, 1};
    functions[7]  = {_s("atan"), (void *) &atan_, 1};
    functions[8]  = {_s("ln"),   (void *) &ln_,   1};
    functions[9]  = {_s("min"),  (void *) &min_,  2};
    functions[10] = {_s("max"),  (void *) &max_,  2};
    functions[11] = {_s("log"),  (void *) &log_,  2};
    functions[12] = {_s("pow"),  (void *) &pow_,  2};
}

Function *find_function(String name) {
    for (int i = 0; i < n_functions; i++) {
        if (are_strings_equal(functions[i].name, name)) return &functions[i];
    }
    return NULL;
}

Function *find_function(char *name) {
    return find_function(_s(name));
}

#define Variable_Bucket_Length 128
struct Variable_Bucket_Array {
    Variable variables[Variable_Bucket_Length];
    int length = 0;

    Variable *add(String name, f64 value) {
        Variable *result = &variables[length];
        length++;
        assert(length <= Variable_Bucket_Length);

        result->name        = name;
        result->value       = value;
        result->initialized = true;

        return result;
    }

    Variable *add(String name) {
        Variable *result = &variables[length];
        length++;
        assert(length <= Variable_Bucket_Length);

        result->name        = name;
        result->value       = 0;
        result->initialized = false;

        return result;
    }

    Variable operator [] (int index) {
        assert(index < Variable_Bucket_Length);
        return variables[index];
    }

    Variable *find(String name) {
        for (int i = 0; i < length; i++) {
            if (are_strings_equal(name, variables[i].name)) return &variables[i];
        }
        return NULL;
    }

    Variable *find(char *name) {
		return find(_s(name));
	}

	void reset() {
		length = 0;
	}
};

Variable_Bucket_Array variables;
Node_Bucket_Array     nodes;

void initialize_constants() {
    variables.add(_s("pi"), 3.14159265359);
    variables.add(_s("e"),  2.71828182845);
}

Node *parse_expression(Tokenizer *tokenizer, int flags);
Node *parse_subexpression(Tokenizer *tokenizer) {
    Node *node = NULL;

    Token token = get_token(tokenizer);
    switch (token.type) {
        case Token_Plus: {
            node = parse_subexpression(tokenizer);
        } break;
        case Token_Minus: {
            node = nodes.make(Node_Unary_Minus);
            node->left = parse_subexpression(tokenizer);
        } break;
        case Token_Number: {
            node = nodes.make_number(token.f);
        } break;
        case Token_Open_Parenthesis: {
            node = parse_expression(tokenizer, Exp_Inside_Paren);
            node->parenthesized = true;
        } break;
        case Token_Identifier: {
            Function *fun = find_function(token.text);
            if (fun) {
                Token peek_paren = get_token(tokenizer);

                if (peek_paren.type != Token_Open_Parenthesis) {
                    report_error("Expected '(' in function call", peek_paren.row, peek_paren.col);
                }

                node = nodes.make_function_call(fun);

                if (fun->number_of_arguments == 1) {
                    node->left  = parse_expression(tokenizer, Exp_Inside_Paren);
                }
                if (fun->number_of_arguments == 2) {
                    node->left  = parse_expression(tokenizer, Exp_Ends_In_Comma);
                    node->right = parse_expression(tokenizer, Exp_Inside_Paren);
                }
            } else {
                Variable *var = variables.find(token.text);
                if (!var) var = variables.add(token.text);

                node = nodes.make_variable(var);
            }
        } break;
        default: report_error("Invalid token", token.row, token.col);
    }

    return node;
}

Node *parse_expression(Tokenizer *tokenizer, int flags) {

    Node *left = parse_subexpression(tokenizer);

    Token peeked_token = peek_token(tokenizer);
    if (peeked_token.type == Token_Equals) return left;

    Token next_token = get_token(tokenizer);
    
    Node_Type binary_op = token_to_binary_op(next_token.type);
    if (binary_op != Node_Type_Unknown) {
        Node *tree = nodes.make(binary_op);
        tree->left = left;
        tree->right = parse_expression(tokenizer, flags);

        if (get_precedence(tree) > get_precedence(tree->right)) {
            rotate_tree(tree);
        }

        return tree;
    } else if (is_token_end_of_line(next_token.type)) {
        
        if (flags & Exp_Inside_Paren) {
            report_error("Expected ')' before end of line", tokenizer->row, tokenizer->col);
        }
        if (flags & Exp_Ends_In_Comma) {
            report_error("Expected ',' before end of line", tokenizer->row, tokenizer->col);
        }
        
        return left;

    } else if (next_token.type == Token_Close_Parenthesis) {
        if (!(flags & Exp_Inside_Paren)) {
            report_error("Unexpected ')'", tokenizer->row, tokenizer->col);
        }

        return left;

    } else if (next_token.type == Token_Comma) {
        if (!(flags & Exp_Ends_In_Comma)) {
            report_error("Unexpected ','", tokenizer->row, tokenizer->col);
        }
        return left;
    }

    report_error("Unexpected token.", tokenizer->row, tokenizer->col);
    return NULL;
}

typedef f64 one_argument_function(f64);
typedef f64 two_argument_function(f64, f64);

f64 eval_tree(Node *node) {
    assert(node);

    if (node->type == Node_Number) return node->value;
    if (node->type == Node_Variable) {
        Variable *var = node->var_ptr;
        if (!var)              report_error("Variable node created incorrectly");
        if (!var->initialized) report_error("Variable not initialized");

        return var->value;
    }

    if (node->type == Node_Plus)     return eval_tree(node->left) + eval_tree(node->right);
    if (node->type == Node_Minus)    return eval_tree(node->left) - eval_tree(node->right);
    if (node->type == Node_Multiply) return eval_tree(node->left) * eval_tree(node->right);
    if (node->type == Node_Divide)   return eval_tree(node->left) / eval_tree(node->right);
    if (node->type == Node_Modulus) {
        s64 dividend = (s64) eval_tree(node->left);
        s64 divisor  = (s64) eval_tree(node->right);

        return dividend % divisor;
    }

    if (node->type == Node_Unary_Minus) return -eval_tree(node->left);

    if (node->type == Node_Equals) {
        Variable *var = node->left->var_ptr;
        if (!var)             report_error("Left side is not a variable");
        if (var->initialized) report_error("Trying to assign a value to an initialized variable");

        var->value = eval_tree(node->right);
        var->initialized = true;
        return var->value;
    }

    if (node->type == Node_Function_Call) {
        Function *node_function = node->fun_pointer;
        if (!node_function) report_error("Function node created incorrectly");
        
        if (node_function->number_of_arguments == 1) {
            one_argument_function *fun = (one_argument_function *) node_function->pointer;
            return fun(eval_tree(node->left));

        } else if (node_function->number_of_arguments == 2) {
            two_argument_function *fun = (two_argument_function *) node_function->pointer;
            return fun(eval_tree(node->left), eval_tree(node->right));
        }
    }

    report_error("Node type not supported/recognized");
}

Node **add(Node **queue, Node *node, int *length) {
    queue = (Node **) realloc(queue, ((*length) + 1) * sizeof(Node **));

    queue[(*length)] = node;
    *length = *length + 1;

    return queue;
}

enum Branch {
    No_Branch,
    Left_Branch,
    Right_Branch,
};

struct Unknown_Search_Result {
    bool error = false;
    Variable *found_variable = NULL;

    Branch global_branch_1 = No_Branch;
    Branch global_branch_2 = No_Branch;
};

void find_unknown(Node *tree, Unknown_Search_Result *result, Branch b1 = No_Branch, Branch b2 = No_Branch) {
    assert(tree);

    if (tree->var_ptr && !tree->var_ptr->initialized) {
        if (result->found_variable) {
            result->error = true;
        } else {
            result->found_variable = tree->var_ptr;
            result->global_branch_1 = b1;
            result->global_branch_2 = b2;
        }
    } else {
        if (b2 != No_Branch) {
            if (tree->left)  find_unknown(tree->left,  result, b1, b2);
            if (tree->right) find_unknown(tree->right, result, b1, b2);
        } else if (b1 != No_Branch) {
            if (tree->left)  find_unknown(tree->left,  result, b1, Left_Branch);
            if (tree->right) find_unknown(tree->right, result, b1, Right_Branch);
        } else {
            if (tree->left)  find_unknown(tree->left,  result, Left_Branch,  No_Branch);
            if (tree->right) find_unknown(tree->right, result, Right_Branch, No_Branch);
        }
    }
}

Function *maybe_inverse_function(Function *fun) {
    if      (are_strings_equal(fun->name, "sin"))  return find_function("asin");
    else if (are_strings_equal(fun->name, "asin")) return find_function("sin");
    else if (are_strings_equal(fun->name, "cos"))  return find_function("acos");
    else if (are_strings_equal(fun->name, "acos")) return find_function("cos");
    else if (are_strings_equal(fun->name, "tan"))  return find_function("atan");
    else if (are_strings_equal(fun->name, "atan")) return find_function("tan");

    return NULL;
}

#define Unknown_Found     0
#define Multiple_Unknowns 1
#define Unknown_Not_Found 2

int isolate_unknown(Node *tree) {
    assert(tree);
    
    if (tree->type != Node_Equals) {
        report_error("Top node must be operator =");
        return -1;
    }
    
    while (true) {
        if (!tree || !tree->left || !tree->right) {
            report_error("Error while isolating unknown");
            return -1;
        }

        Unknown_Search_Result result;
        find_unknown(tree, &result);

        if (result.error)                        return Multiple_Unknowns;
        if (!result.found_variable)              return Unknown_Not_Found;
        if (result.global_branch_1 == No_Branch) return Unknown_Not_Found;

        // If the unknown is on the right we swap right and left so we don't copy paste
        // a lot of code with right and left swapped
        if (result.global_branch_1 == Right_Branch) {
            result.global_branch_1 = Left_Branch;

            Node *temp  = tree->left;
            tree->left  = tree->right;
            tree->right = temp;
        }

        Branch level_2 = result.global_branch_2;

        // This checks if the unknown is on the top level
        if (level_2 == No_Branch) return Unknown_Found;
      
        if (tree->left->type == Node_Function_Call) {
            Function *fun = tree->left->fun_pointer;
            if (!fun) report_error("Function node created incorrectly");
            Function *simple_inverse_fun = maybe_inverse_function(fun);
            
            Node *unknown    = level_2 == Left_Branch ? tree->left->left  : tree->left->right;
            Node *argument   = level_2 == Left_Branch ? tree->left->right : tree->left->left;
            Node *right_side = tree->right;

            tree->right = tree->left;

            if (simple_inverse_fun) {
                tree->right->fun_pointer = simple_inverse_fun;
                tree->right->left        = right_side;

            } else if (fun == find_function("sqrt")) {
                tree->right->fun_pointer = find_function("pow");
                tree->right->left        = right_side;
                tree->right->right       = nodes.make_number(2);

            } else if (fun == find_function("ln")) {
                tree->right->fun_pointer = find_function("pow");
                tree->right->left        = nodes.make_variable(variables.find("e"));
                tree->right->right       = right_side;

            } else if (fun == find_function("cot")) {
                auto divide_node   = nodes.make(Node_Divide);
                divide_node->left  = nodes.make_variable(variables.find("pi"));
                divide_node->right = nodes.make_number(2);

                auto minus_node   = nodes.make(Node_Minus);
                minus_node->left  = divide_node;
                minus_node->right = tree->right;

                tree->right->fun_pointer = find_function("atan");
                tree->right->left        = right_side;

                tree->right = minus_node;
                
            } else if (fun == find_function("pow")) {
                if (level_2 == Left_Branch) {
                    auto division_node   = nodes.make(Node_Divide);
                    division_node->left  = nodes.make_number(1);
                    division_node->right = argument;

                    tree->right->fun_pointer = find_function("pow");
                    tree->right->left        = right_side;
                    tree->right->right       = division_node;

                } else {
                    tree->right->fun_pointer = find_function("log");
                    tree->right->left        = right_side;
                    tree->right->right       = argument;
                }
            } else if (fun == find_function("log")) {
                if (level_2 == Left_Branch) {
                    tree->right->fun_pointer = find_function("pow");
                    tree->right->left        = argument;
                    tree->right->right       = right_side;

                } else {
                    auto division_node   = nodes.make(Node_Divide);
                    division_node->left  = nodes.make_number(1);
                    division_node->right = right_side;

                    tree->right->fun_pointer = find_function("pow");
                    tree->right->left        = argument;
                    tree->right->right       = division_node;

                }
            } else report_error("This function cannot be inverted");

            tree->left = unknown;
        } else if (tree->left->type == Node_Unary_Minus) {
            
            Node *unary_minus = tree->left;
            Node *right = tree->right;
            Node *left  = tree->left->left;

            tree->left = left;
            tree->right = unary_minus;
            unary_minus->left = right;

        } else {

            Node *left  = tree->left;
            Node *right = tree->right;
            Node *left_left  = tree->left->left;
            Node *left_right = tree->left->right;
            
            Node_Type inv_operator = Node_Type_Unknown;
            if (left->type == Node_Plus)     inv_operator = Node_Minus;
            if (left->type == Node_Minus)    inv_operator = Node_Plus;
            if (left->type == Node_Divide)   inv_operator = Node_Multiply;
            if (left->type == Node_Multiply) inv_operator = Node_Divide;
            if (left->type == Node_Modulus)  report_error("Modulus (%) operator cannot be inverted");

            assert(inv_operator != Node_Type_Unknown);

            if (level_2 == Left_Branch) {
                left->type  = inv_operator;
                left->left  = right;
                left->right = left_right;

                tree->left  = left_left;
                tree->right = left;
            } else if (level_2 == Right_Branch) {
                if (left->type == Node_Minus || left->type == Node_Divide) {
                    Node *temp = left_left;
                    left_left  = right;
                    right      = temp;
                }

                left->type  = inv_operator;
                left->left  = right;
                left->right = left_left;

                tree->left  = left_right;
                tree->right = left;
            } else assert(false);
        }
    }
}

#define print_spaceing 5
void print_tree_horizontally(Node *node, int space) { 
    if (node == NULL) return;

    print_tree_horizontally(node->right, space + print_spaceing);

    for (int i = 0; i < space; i++) printf(" ");
    print_node(node);
    printf("\n");

    print_tree_horizontally(node->left, space + print_spaceing);

}

int depth(Node *tree) {

    int left_depth  = 0;
    int right_depth = 0;
    if (tree->left)  left_depth  = depth(tree->left);
    if (tree->right) right_depth = depth(tree->right);

    if (left_depth > right_depth) return left_depth + 1;
    return right_depth + 1;
}

Node *parse_line(Tokenizer *tokenizer, bool verbose = false) {
    
    Token peek_end_of_line = peek_token(tokenizer);
    if (is_token_end_of_line(peek_end_of_line.type)) {
        get_token(tokenizer);
        return NULL;
    }
    
    Node *tree = nodes.make(Node_Equals);

    tree->left = parse_expression(tokenizer, 0);

    Token token = get_token(tokenizer);
    if (token.type != Token_Equals) {
        report_error("Expression without equal sign.");
        return NULL;
    }

    tree->right = parse_expression(tokenizer, 0);
    
    if (verbose) {
        printf("\nSintax Tree (depth: %d):\n", depth(tree));
        print_tree_horizontally(tree, 0);
    }

    return tree;
}

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

	return results;
}

void reset_variables() {
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

Fail_Code parse_expected_results(Tokenizer *tokenizer, Result_Array *results) {

	int number_of_unknowns = count_pounds(tokenizer);
	if (number_of_unknowns == 0) return Nothing_To_Check;

	*results = get_result_array(number_of_unknowns);

	Token token;
	int results_parsed = 0;

	while (true) {
		eat_spaces_new_lines_and_comments(tokenizer);
		
		if (!peek_token_type(tokenizer, &token, Token_Pound)) {
			if (results_parsed < number_of_unknowns) return Less_Results_Defined;
		}
		if (results_parsed >= number_of_unknowns) return All_Good;

		if (peek_token_type(tokenizer, &token, Token_Identifier)) {
			results->memory[results_parsed].name = token.text;
		} else {
			return Unparsed_Results;
		}

		if (!peek_token_type(tokenizer, &token, Token_Equals)) {
			return Unparsed_Results;
		}
		
		if (maybe_parse_number(tokenizer, &token)) {
			results->memory[results_parsed].value = token.f;
		} else {
			return Unparsed_Results;
		}

		results_parsed++;
	}
}

Fail_Info test_program(char *file_name) {

	Fail_Info info;
	info.code = All_Good;
	
	reset_variables();
	initialize_constants();

	String file;
	file.data = load_file_memory(file_name, &file.count);
	if (file.data == NULL) {
		info.code = Could_Not_Load_File;
		return info;
	}
	Tokenizer tokenizer = get_tokenizer(file);

	Result_Array results;
	info.code = parse_expected_results(&tokenizer, &results);

	if (info.code != All_Good) return info;

	Node **lines = NULL;
	int n_lines = 0;

    while (true) {
        Token token = peek_token(&tokenizer);
        if (token.type == Token_End_Of_File) break;
        
        Node *parsed_line = parse_line(&tokenizer);
        if (parsed_line) lines = add(lines, parsed_line, &n_lines);
    }

	int *line_computed = (int *) calloc(n_lines, sizeof(int));
	int at = 0;
	int last_line_computed = 0;
	while (true) {
		Node *line = lines[at];
		int result = isolate_unknown(line);
		
		if (!line_computed[at]) {
			if (result == Unknown_Not_Found) {
				line_computed[at] = true;
			} else if (result == Unknown_Found) {
				f64 eval_result = eval_tree(line);
				line_computed[at] = true;
				last_line_computed = at;
			}
		}
		
		int total_lines_computed = 0;
		for (int i = 0; i < n_lines; i++) total_lines_computed += line_computed[i];
		if (total_lines_computed == n_lines) break;
		
		at = (at+1) % n_lines;
		if (at == last_line_computed) {
			info.code = Unsolvable_Equations;
			return info;
		}
	}

	for (int r = 0; r < results.length; r++) {
		bool found = false;
		for (int i = 0; i < variables.length; i++) {
			if (!are_strings_equal(results[r]->name, variables[i].name)) continue;

            info.variable_name  = results[r]->name;
            info.variable_value = results[r]->value;

			if (!variables[i].initialized) {
				info.code = Unsolved_Variable;
				return info;
			}
		
			if (abs(results[r]->value - variables[i].value) > epsilon) {
				info.code = Wrong_Result;
				info.variable_actual_value = variables[i].value;
				return info;
			}

			found = true;
			break;
		}
		if (!found) {
			info.code = Unsolved_Variable;
			return info;
		}
	}

	return info;
}

char *tests_folder_path = "tests/*";

int longest_file_name(char *folder_name) {
	
	WIN32_FIND_DATA find_file_data;
	HANDLE handle = FindFirstFile(tests_folder_path, &find_file_data);
	
	int result = 0;
	do {
		if ((find_file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) continue;

		int name_length = strlen(find_file_data.cFileName);
		if (name_length > result) result = name_length;

	} while (FindNextFile(handle, &find_file_data) != 0);

	FindClose(handle);

	return result;
}

void run_tests() {
	int longest_name = longest_file_name(tests_folder_path);

	WIN32_FIND_DATA find_file_data;
	HANDLE handle = FindFirstFile(tests_folder_path, &find_file_data);

    initialize_functions();

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

		if (info.code == All_Good) {
            printf("Success\n");
		} else {
			printf("Failed\n");
			switch(info.code) {
				case Nothing_To_Check:     printf("No results defined\n");                                     break;
				case Could_Not_Load_File : printf("Could not load file\n");                                    break;
				case Unparsed_Results:     printf("Unable to parse results\n");                                break;
				case Unsolvable_Equations: printf("Possible circular dependencies in the equations\n");        break;
				case Less_Results_Defined: printf("# is in the file and it is not used to define a result\n"); break;

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
			}
		}

		free(full_file_name);
	} while (FindNextFile(handle, &find_file_data) != 0);
	
	FindClose(handle);
}

int main(int argc, char** argv) {

    initialize_constants();
    initialize_functions();

    String file;

    if (argc > 1) {
        if (are_strings_equal(_s("test"), argv[1])) {
            run_tests();
            return 0;
        } else {
            file.data = load_file_memory(argv[1], &file.count);
        }
    } else {
        file.data = load_file_memory("calc.txt", &file.count);
    }
    assert(file.data);

    Node **lines = NULL;
    int n_lines = 0;

    Tokenizer tokenizer = get_tokenizer(file);
    while (true) {
        Token token = peek_token(&tokenizer);
        if (token.type == Token_End_Of_File) break;
        
        Node *parsed_line = parse_line(&tokenizer);
        
        if (parsed_line) lines = add(lines, parsed_line, &n_lines);
    }

    int *line_computed = (int *) calloc(n_lines, sizeof(int));
    int at = 0;
    int last_line_computed = 0;
    while (true) {

        if (line_computed[at] == false) {
            Node *line = lines[at];
            int result = isolate_unknown(line);
            
            if (result == Unknown_Not_Found) {
                line_computed[at] = true;
            } else if (result == Unknown_Found) {
                f64 eval_result = eval_tree(line);
                line_computed[at] = true;
                last_line_computed = at;
            }
        }

        int total_lines_computed = 0;
        for (int i = 0; i < n_lines; i++) total_lines_computed += line_computed[i];
        if (total_lines_computed == n_lines) break;

        at = (at+1) % n_lines;
        if (at == last_line_computed) report_error("Possible circular dependency, could not solve the equasions :(");
    }

    for (int i = 0; i < variables.length; i++) {        
        for (int c = 0; c < variables[i].name.count; c++) printf("%c", variables[i].name.data[c]);
        printf(" = %.3f\n", variables[i].value);
    }
}