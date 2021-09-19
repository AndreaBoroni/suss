#include "windows_handling.h"
#include "parser.h"

// Todo: separate int and float and bool
// Todo: add clamp function
// (Todo: if and loops)

#define Assert(Expression) if (!(Expression)) {                                               \
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
    bool constant;
};

enum Node_Types {
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
    Node_Plus_Equals,
    Node_Minus_Equals,
    Node_Times_Equals,
    Node_Divide_Equals,
    Node_Mod_Equals,

    Node_Parenthesized_Expression,

    Node_Types_Count,
};

enum Expression_Flags {
    Exp_Inside_Paren  = 0x01,
    Exp_Ends_In_Comma = 0x02,
};

bool is_special_equals(int type) {
    if (type == Node_Plus_Equals)   return true;
    if (type == Node_Minus_Equals)  return true;
    if (type == Node_Times_Equals)  return true;
    if (type == Node_Divide_Equals) return true;
    if (type == Node_Mod_Equals)    return true;

    return false;
}

int get_precedence(int type) {
    if (is_special_equals(type)) return 0;

    switch(type) {
        case Node_Equals: return 0;
        
        case Node_Plus:
        case Node_Minus: return 100;
        
        case Node_Multiply:
        case Node_Divide:
        case Node_Modulus: return 150;

        case Node_Unary_Minus: return 200;

        case Node_Number:
        case Node_Variable:
        case Node_Function_Call:
        case Node_Parenthesized_Expression: return 1000;
    }
    return -1;
}

struct Node {
    int type;
    f64 value;

    Function *fun_pointer;
    Variable *var_ptr;

    Node *left;
    Node *right;
};

bool is_token_end_of_line(int type) {
    if (type == Token_Semicolon)   return true;
    if (type == Token_End_Of_Line) return true;
    if (type == Token_End_Of_File) return true;

    return false;
}

void free_tree(Node *tree) {
    if (tree->right) {
        free_tree(tree->right);
        free(tree->right);
    }
    if (tree->left) {
        free_tree(tree->left);
        free(tree->left);
    }
}

void print_node(Node *node) {
    if (node->type == Node_Number) printf("%.1f", node->value);

    if (node->type == Node_Plus)       printf(" + ");
    if (node->type == Node_Minus)      printf(" - ");
    if (node->type == Node_Multiply)   printf(" * ");
    if (node->type == Node_Divide)     printf(" / ");
    if (node->type == Node_Modulus)    printf(" %% ");
    if (node->type == Node_Equals) printf(" = ");

    if (node->type == Node_Unary_Minus) printf(" - ");

    if (node->type == Node_Function_Call) {
        for (int i = 0; i < node->fun_pointer->name.count; i++) printf("%c", node->fun_pointer->name.data[i]);
    }
    if (node->type == Node_Variable) {
        for (int i = 0; i < node->var_ptr->name.count; i++) printf("%c", node->var_ptr->name.data[i]);
    }
 
    if (node->type == Node_Parenthesized_Expression) {
        printf("( )");
    }
}

// Todo: print a better tree (graphically?)
void print_tree(Node *tree, int level, char where) {

    printf("[%d %c] ", level, where);
    print_node(tree);
    printf("\n");

    if (tree->left)  print_tree(tree->left,  level + 1, 'l');
    if (tree->right) print_tree(tree->right, level + 1, 'r');
}

void *rotate_tree(Node *node) {
    Node *bottom_node = node->right;
    
    Node *left_top     = node->left;
    Node *left_bottom  = bottom_node->left;
    Node *right_bottom = bottom_node->right;

    int top_op = node->type;
    node->type = bottom_node->type;
    bottom_node->type = top_op;

    node->left  = bottom_node;
    node->right = right_bottom;

    bottom_node->left  = left_top;
    bottom_node->right = left_bottom;
}

bool adjust_precedence(Node *node) {
    int type = node->type;
    bool skip = (type == Node_Function_Call);
    bool something_changed = false;

    if (node->right && !skip) {
        Node *bottom_node = node->right;
        int top_node_precedence    = get_precedence(node->type);
        int bottom_node_precedence = get_precedence(bottom_node->type);

        if (top_node_precedence == bottom_node_precedence) {
            rotate_tree(node);
            adjust_precedence(node);
            return true;
        }
    }

    if (node->left)  something_changed |= adjust_precedence(node->left);
    if (node->right) something_changed |= adjust_precedence(node->right);

    if (node->right && !skip) {
        Node *bottom_node = node->right;
        int top_node_precedence    = get_precedence(node->type);
        int bottom_node_precedence = get_precedence(bottom_node->type);

        if (top_node_precedence > bottom_node_precedence) {
            rotate_tree(node);
            return true;
        }
    }

    return something_changed;
}

Node *parse_expression(Tokenizer *tokenizer, int flags);
Node *parse_subexpression(Tokenizer *tokenizer);

#define Node_Bucket_Length 4096
struct Node_Bucket_Array {
    Node nodes[Node_Bucket_Length];
    int length = 0;

    Node *get_node_mem() {
        Assert(length < Node_Bucket_Length);
        Node *result = &nodes[length++];
        memset(result, 0, sizeof(Node));
    }

    Node *make_variable(Variable *pointer) {
        Node *result = get_node_mem();
        result->type = Node_Variable;
        result->var_ptr = pointer;
        return result;
    }

    Node *make(int type) {
        Node *result = get_node_mem();
        result->type = type;
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

#define Function_Bucket_Length 128
struct Function_Bucket_Array {
    Function functions[Function_Bucket_Length];
    int length = 0;

    // Function_Bucket_Array *next = NULL;
    // int bucket_index = 0;

    Function *add(String name, void *fun_pointer, int number_of_arguments) {
        Function *result = &functions[length];
        length++;
        Assert(length <= Function_Bucket_Length);

        result->name                = name;
        result->pointer             = fun_pointer;
        result->number_of_arguments = number_of_arguments;

        return result;
    }

    Function operator [] (int index) {
        Assert(index < Function_Bucket_Length);
        return functions[index];
    }

    Function *find(String name) {
        for (int i = 0; i < length; i++) {
            if (are_strings_equal(functions[i].name, name)) return &functions[i];
        }
        return NULL;
    }
    Function *find(char *name) { return find(_s(name)); }
};

#define Variable_Bucket_Length 128
struct Variable_Bucket_Array {
    Variable variables[Variable_Bucket_Length];
    int length = 0;

    // Variable_Bucket_Array *next = NULL;
    // int bucket_index = 0;

    Variable *add_constant(String name, f64 value) {
        Variable *result = &variables[length];
        length++;
        Assert(length <= Variable_Bucket_Length);

        result->name        = name;
        result->value       = value;
        result->initialized = true;
        result->constant    = true;

        return result;
    }

    Variable *add(String name) {
        Variable *result = &variables[length];
        length++;
        Assert(length <= Variable_Bucket_Length);

        result->name        = name;
        result->value       = 0;
        result->initialized = false;
        result->constant    = false;

        return result;
    }

    Variable operator [] (int index) {
        Assert(index < Variable_Bucket_Length);
        return variables[index];
    }

    Variable *find(String name) {
        for (int i = 0; i < length; i++) {
            if (are_strings_equal(name, variables[i].name)) return &variables[i];
        }
        return NULL;
    }
    Variable *find(char *name) { return find(_s(name)); }
};

Function_Bucket_Array functions;
Variable_Bucket_Array variables;
Node_Bucket_Array     nodes;

f64 sin_(f64 a)  { return sin(a); }
f64 cos_(f64 a)  { return cos(a); }
f64 tan_(f64 a)  { return tan(a); }
f64 cot_(f64 a)  { return cos(a)/sin(a); }
f64 asin_(f64 a) { return asin(a); }
f64 acos_(f64 a) { return acos(a); }
f64 atan_(f64 a) { return atan(a); }

f64 pow_(f64 b, f64 e) { return pow(b, e); }
f64 sqrt_(f64 a)       { return pow(a, 0.5); }
f64 ln_(f64 a)         { return log(a); }
f64 log_(f64 a, f64 b) { return log(a)/log(b); }

f64 max_(f64 a, f64 b) { return a > b ? a : b; }
f64 min_(f64 a, f64 b) { return a > b ? b : a; }

void initialize_functions_and_constants() {
    
    functions.add(_s("sqrt"), (void *) &sqrt_, 1);
    functions.add(_s("sin"),  (void *) &sin_,  1);
    functions.add(_s("cos"),  (void *) &cos_,  1);
    functions.add(_s("tan"),  (void *) &tan_,  1);
    functions.add(_s("cot"),  (void *) &cot_,  1);
    functions.add(_s("asin"), (void *) &asin_, 1);
    functions.add(_s("acos"), (void *) &acos_, 1);
    functions.add(_s("atan"), (void *) &atan_, 1);
    functions.add(_s("ln"),   (void *) &ln_,   1);
    functions.add(_s("min"),  (void *) &min_,  2);
    functions.add(_s("max"),  (void *) &max_,  2);
    functions.add(_s("log"),  (void *) &log_,  2);
    functions.add(_s("pow"),  (void *) &pow_,  2);

    variables.add_constant(_s("pi"), 3.14159265359);
    variables.add_constant(_s("e"),  2.71828182845);
}

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
            node = nodes.make(Node_Parenthesized_Expression);
            node->left = parse_expression(tokenizer, Exp_Inside_Paren);
        } break;
        case Token_Identifier: {
            Function *fun = functions.find(token.text);
            if (fun) {
                Token peek_paren = get_token(tokenizer);

                if (peek_paren.type != Token_Open_Parenthesis) {
                    report_error("Expected '('", peek_paren.row, peek_paren.col);
                }

                node = nodes.make_function_call(fun);

                if (fun->number_of_arguments == 1) {
                    node->left  = parse_expression(tokenizer, Exp_Inside_Paren);
                }
                if (fun->number_of_arguments == 2) { // Todo: loop this (when the tree has more than 2 leafs)
                    node->left  = parse_expression(tokenizer, Exp_Ends_In_Comma);
                    node->right = parse_expression(tokenizer, Exp_Inside_Paren);
                }

                break;
            }
            
            Variable *var = variables.find(token.text);
            if (!var) var = variables.add(token.text);
            node = nodes.make_variable(var);
        } break;
        default: report_error("Invalid token", token.row, token.col);
    }

    return node;
}

Node *parse_expression(Tokenizer *tokenizer, int flags) {
    Node *tree = NULL;

    Node *left = parse_subexpression(tokenizer);
    Token next_token = get_token(tokenizer);
    
    // Binary operators
    if (next_token.type == Token_Plus)     tree = nodes.make(Node_Plus);
    if (next_token.type == Token_Minus)    tree = nodes.make(Node_Minus);
    if (next_token.type == Token_Asterisk) tree = nodes.make(Node_Multiply);
    if (next_token.type == Token_Slash)    tree = nodes.make(Node_Divide);
    if (next_token.type == Token_Percent)  tree = nodes.make(Node_Modulus);

    if (next_token.type == Token_Equals) tree = nodes.make(Node_Equals);

    if (next_token.type == Token_Plus_Equals)   tree = nodes.make(Node_Plus_Equals);
    if (next_token.type == Token_Minus_Equals)  tree = nodes.make(Node_Minus_Equals);
    if (next_token.type == Token_Times_Equals)  tree = nodes.make(Node_Times_Equals);
    if (next_token.type == Token_Divide_Equals) tree = nodes.make(Node_Divide_Equals);
    if (next_token.type == Token_Mod_Equals)    tree = nodes.make(Node_Mod_Equals);

    if (tree) { // Todo: check if it is a binary operator
        tree->left = left;
        tree->right = parse_expression(tokenizer, flags);

    } else if (is_token_end_of_line(next_token.type)) {
        
        if (flags & Exp_Inside_Paren) {
            report_error("Expected ')'", tokenizer->row, tokenizer->col);
        }
        if (flags & Exp_Ends_In_Comma) {
            report_error("Expected ','", tokenizer->row, tokenizer->col);
        }
        
        if (next_token.type == Token_Semicolon) {
            Token peek_new_line = peek_token(tokenizer);
            if (peek_new_line.type == Token_End_Of_Line) get_token(tokenizer);
        }
        
        tree = left;
    } else if (next_token.type == Token_Close_Parenthesis) {
        if (!(flags & Exp_Inside_Paren)) {
            report_error("Unexpected ')'", tokenizer->row, tokenizer->col);
        }

        tree = left;
    } else if (next_token.type == Token_Comma) {
        if (!(flags & Exp_Ends_In_Comma)) {
            report_error("Unexpected ','", tokenizer->row, tokenizer->col);
        }
        tree = left;
    }

    return tree;
}

typedef f64 one_argument_function(f64);
typedef f64 two_argument_function(f64, f64);

f64 eval_tree(Node *node) {
    Assert(node);

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
        if (!var)          report_error("Left side is not a variable");
        if (var->constant) report_error("Trying to assign a value to a constant variable");

        var->value = eval_tree(node->right);
        var->initialized = true;
        return var->value;
    }

    if (is_special_equals(node->type)) {
        Variable *var = node->left->var_ptr;
        if (!var)              report_error("Variable node created incorrectly");
        if (!var->initialized) report_error("Variable not initialized");

        if (node->type == Node_Plus_Equals)   var->value += eval_tree(node->right);
        if (node->type == Node_Minus_Equals)  var->value -= eval_tree(node->right);
        if (node->type == Node_Times_Equals)  var->value *= eval_tree(node->right);
        if (node->type == Node_Divide_Equals) var->value /= eval_tree(node->right);
        if (node->type == Node_Mod_Equals)    var->value = (s64) var->value % (s64) eval_tree(node->right);

        return var->value;
    }

    if (node->type == Node_Function_Call) {
        auto node_function = node->fun_pointer;
        if (!node_function) report_error("Function node created incorrectly");
        
        if (node_function->number_of_arguments == 1) {
            one_argument_function *fun = (one_argument_function *) node_function->pointer;
            return fun(eval_tree(node->left));
        }
        else if (node_function->number_of_arguments == 2) {
            two_argument_function *fun = (two_argument_function *) node_function->pointer;
            f64 argument_1 = eval_tree(node->left);
            f64 argument_2 = eval_tree(node->right);

            return fun(argument_1, argument_2);
        }
    }

    if (node->type == Node_Parenthesized_Expression) return eval_tree(node->left);

    report_error("Node type not supported/recognized");
}

enum find_unkown_result {
    Unknown_Found,
    Unknown_Not_Found,
    Found_More_Than_One_Unknown,
    Found_Unknown_More_Than_Once,
    Root_Node_Is_Not_Equals,

    On_Right_Node,
    On_Left_Node,
    
    On_Right_Left_Node,
    On_Left_Left_Node,

    On_Right_Right_Node,
    On_Left_Right_Node,
};

Node **add(Node **queue, Node *node, int *length) {
    Node **result = (Node **) calloc((*length) + 1, sizeof(Node **));
    
    if (queue) {
        memcpy(result, queue, (*length) * sizeof(Node **));
        free(queue);
    }

    result[(*length)] = node;
    *length = *length + 1;

    return result;
}

int where_in_the_tree(int first_level, int second_level) {
    if (first_level == 1 && second_level == -1) return On_Left_Node;
    if (first_level == 2 && second_level == -1) return On_Right_Node;

    if (first_level == 1 && second_level == 1) return On_Left_Left_Node;
    if (first_level == 1 && second_level == 2) return On_Left_Right_Node;
    if (first_level == 2 && second_level == 1) return On_Right_Left_Node;
    if (first_level == 2 && second_level == 2) return On_Right_Right_Node;

    Assert(false);
    return -1;
}

Variable *var_found;
int find_unknown(Node *tree, int first_level = -1, int second_level = -1) {
    if (first_level == -1 && second_level == -1) {
        var_found = NULL;
        if (tree->type != Node_Equals) return Root_Node_Is_Not_Equals;
    }
    
    if (tree && tree->var_ptr && !tree->var_ptr->initialized) {
        if (var_found) {
            if (are_strings_equal(var_found->name, tree->var_ptr->name)) return Found_Unknown_More_Than_Once;
            return Found_More_Than_One_Unknown;
        }
        var_found = tree->var_ptr;
        return where_in_the_tree(first_level, second_level);
    }

    int result_left  = -1;
    if (tree->left) {
        int next_first_level  =  first_level  == -1                       ? 1 : first_level;
        int next_second_level = (second_level == -1 && first_level != -1) ? 1 : second_level;

        result_left = find_unknown(tree->left, next_first_level, next_second_level);
    }

    int result_right = -1;
    if (tree->right) {
        int next_first_level  =  first_level  == -1                       ? 2 : first_level;
        int next_second_level = (second_level == -1 && first_level != -1) ? 2 : second_level;

        result_right = find_unknown(tree->right, next_first_level, next_second_level);
    }
    
    if (result_right == Found_Unknown_More_Than_Once || result_left == Found_Unknown_More_Than_Once) return Found_Unknown_More_Than_Once;
    if (result_right == Found_More_Than_One_Unknown  || result_left == Found_More_Than_One_Unknown)  return Found_More_Than_One_Unknown;

    if (result_left == -1 && result_right == -1) return Unknown_Not_Found;
    if (result_left  >= On_Right_Node)           return result_left; // Hack
    if (result_right >= On_Right_Node)           return result_right;

    return Unknown_Not_Found;
}

Function *maybe_inverse_function(Function *fun) {
    if      (fun == functions.find("sin"))  return functions.find("asin");
    else if (fun == functions.find("asin")) return functions.find("sin");
    else if (fun == functions.find("cos"))  return functions.find("acos");
    else if (fun == functions.find("acos")) return functions.find("cos");
    else if (fun == functions.find("tan"))  return functions.find("atan");
    else if (fun == functions.find("atan")) return functions.find("tan");

    return NULL;
}

int isolate_unknown(Node *tree) {
    if (tree->type != Node_Equals) report_error("Top node must be operator =");
    
    while (true) {
        if (!tree->left || !tree->right) report_error("Error while isolating unknown");

        if (tree->left && tree->left->type == Node_Parenthesized_Expression) {
            Node *expression = tree->left->left;
            free(tree->left);
            tree->left = expression;
        }
        if (tree->right && tree->right->type == Node_Parenthesized_Expression) {
            Node *expression = tree->right->left;
            free(tree->right);
            tree->right = expression;
        }

        int where = find_unknown(tree);
        if (where < On_Right_Node) return where; // Hack

        if (where == On_Right_Node || where == On_Right_Left_Node || where == On_Right_Right_Node) {
            Node *temp  = tree->left;
            tree->left  = tree->right;
            tree->right = temp;
            where++; // Hack
        }

        if (where == On_Left_Node) return Unknown_Found;
      
        if (tree->left->type == Node_Function_Call) {
            auto fun = tree->left->fun_pointer;
            if (!fun) report_error("Function node created incorrectly");
            auto simple_inverse_fun = maybe_inverse_function(fun);
            
            Node *unknown    = where == On_Left_Left_Node ? tree->left->left  : tree->left->right;
            Node *argument   = where == On_Left_Left_Node ? tree->left->right : tree->left->left;
            Node *right_side = tree->right;

            tree->right = tree->left;

            if (simple_inverse_fun) {
                tree->right->fun_pointer = simple_inverse_fun;
                tree->right->left        = right_side;

            } else if (fun == functions.find("sqrt")) {
                tree->right->fun_pointer = functions.find("pow");
                tree->right->left        = right_side;
                tree->right->right       = nodes.make_number(2);

            } else if (fun == functions.find("ln")) {
                tree->right->fun_pointer = functions.find("pow");
                tree->right->left        = nodes.make_variable(variables.find("e"));
                tree->right->right       = right_side;

            } else if (fun == functions.find("cot")) {
                auto divide_node   = nodes.make(Node_Divide);
                divide_node->left  = nodes.make_variable(variables.find("pi"));
                divide_node->right = nodes.make_number(2);

                auto minus_node   = nodes.make(Node_Minus);
                minus_node->left  = divide_node;
                minus_node->right = tree->right;

                tree->right->fun_pointer = functions.find("atan");
                tree->right->left        = right_side;

                tree->right = minus_node;
                
            } else if (fun == functions.find("pow")) {
                if (where == On_Left_Left_Node) {
                    auto division_node   = nodes.make(Node_Divide);
                    division_node->left  = nodes.make_number(1);
                    division_node->right = argument;

                    tree->right->fun_pointer = functions.find("pow");
                    tree->right->left        = right_side;
                    tree->right->right       = division_node;

                } else {
                    tree->right->fun_pointer = functions.find("log");
                    tree->right->left        = right_side;
                    tree->right->right       = argument;
                }
            } else if (fun == functions.find("log")) {
                if (where == On_Left_Left_Node) {
                    tree->right->fun_pointer = functions.find("pow");
                    tree->right->left        = argument;
                    tree->right->right       = right_side;

                } else {
                    auto division_node   = nodes.make(Node_Divide);
                    division_node->left  = nodes.make_number(1);
                    division_node->right = right_side;

                    tree->right->fun_pointer = functions.find("pow");
                    tree->right->left        = argument;
                    tree->right->right       = division_node;

                }
            } else report_error("This function cannot be inverted");

            tree->left = unknown;
            continue;
        }

        if (where == On_Left_Left_Node) {
            Node *left  = tree->left;
            Node *right = tree->right;
            Node *left_left  = tree->left->left;
            Node *left_right = tree->left->right;
            
            int inv_operator = -1;
            if (left->type == Node_Plus)     inv_operator = Node_Minus;
            if (left->type == Node_Minus)    inv_operator = Node_Plus;
            if (left->type == Node_Divide)   inv_operator = Node_Multiply;
            if (left->type == Node_Multiply) inv_operator = Node_Divide;
            if (left->type == Node_Modulus)  report_error("Modulus (%) operator cannot be inverted");
            
            if (inv_operator != -1) {
                left->type  = inv_operator;
                left->left  = right;
                left->right = left_right;

                tree->left  = left_left;
                tree->right = left;
            }
        }

        if (where == On_Left_Right_Node) {
            Node *left  = tree->left;
            Node *right = tree->right;
            Node *left_left  = tree->left->left;
            Node *left_right = tree->left->right;
            
            int inv_operator = -1;
            if (left->type == Node_Plus)     inv_operator = Node_Minus;
            if (left->type == Node_Minus)    inv_operator = Node_Minus;
            if (left->type == Node_Multiply) inv_operator = Node_Divide;
            if (left->type == Node_Divide)   inv_operator = Node_Divide;
            
            if (inv_operator != -1) {
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
            }
        }
    }
}

#define print_spaceing 5
void print_tree_horizontally(Node *node, int space)
{ 
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

    return max(left_depth, right_depth) + 1;
}

Node *parse_line(Tokenizer *tokenizer, bool verbose = false) {
    
    Token peek_end_of_line = peek_token(tokenizer);
    if (is_token_end_of_line(peek_end_of_line.type)) {
        get_token(tokenizer);
        return NULL;
    }
    
    Node *tree = parse_expression(tokenizer, 0);
    
    if (verbose) {
        printf("\nSintax Tree (depth: %d):\n", depth(tree));
        print_tree_horizontally(tree, 0);
    }

    while (adjust_precedence(tree)) { /*printf("\nNew tree\n"); print_tree_horizontally(tree, 0);*/ }
    
    if (verbose) {
        printf("\nSintax Tree after adjustment (depth: %d):\n", depth(tree));
        print_tree_horizontally(tree, 0);
    }

    return tree;
}

struct bit_array {
    u8 *mem = NULL;

    int length          = 0;
    int bytes_allocated = 0;

    bool operator [] (int index) {
        if (index >= length) return false;
        if (index < 0)       return false;

        int byte_index = index / 8;
        int bit_index  = index % 8;

        return (mem[byte_index] >> bit_index) & 1;
    }

    void set_bit(int index) {
        if (index >= length) return;
        if (index < 0)       return;

        int byte_index = index / 8;
        int bit_index  = index % 8;

        mem[byte_index] |= (1 << bit_index);
    }
    
    void unset_bit(int index) {
        if (index >= length) return;
        if (index < 0)       return;

        int byte_index = index / 8;
        int bit_index  = index % 8;

        mem[byte_index] &= ~(1 << bit_index);
    }
};

bit_array get_bit_array(int length) {
    bit_array result;
    result.bytes_allocated = length / 8 + 1;

    result.mem = (u8 *) malloc(result.bytes_allocated);
    memset(result.mem, 0, result.bytes_allocated);
    result.length = length;

    return result;
}

int main(void) {
    
    initialize_functions_and_constants();

    String file;
    file.data = load_file_memory("calc.txt", &file.count);
    Assert(file.data);

    Node **lines = NULL;
    int number_of_lines = 0;

    Tokenizer tokenizer = get_tokenizer(file);
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
        if (at == last_line_computed) report_error("Possible circular dependency, could not solve the equasions :(");
    }

    for (int i = 0; i < variables.length; i++) {
        if (variables[i].constant) continue;
        
        for (int c = 0; c < variables[i].name.count; c++) printf("%c", variables[i].name.data[c]);
        printf(" = %.3f\n", variables[i].value);
    }
}

