#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <algorithm>
#include <unordered_map>


enum LISP_OBJ_TYPE {
        SYM_TYPE = 0,
        CHAR_TYPE,
        INT_TYPE,
        NIL_TYPE,
        CONS_TYPE,
        LAMBDA_TYPE,
        PRIMITIVE_FUNCTION_TYPE,
        USER_FUNCTION_TYPE,
};

struct lisp_obj;
typedef lisp_obj* (*primitive_function)(lisp_obj *env, lisp_obj *args);

struct lisp_obj {
        lisp_obj() {}
        ~lisp_obj() {}
        
        // a symbol of integers or characters
        LISP_OBJ_TYPE type;
        union {
                std::string *symbol;
                char character;
                int integer;
                struct {
                        lisp_obj *car;
                        lisp_obj *cdr;
                } cons;
                struct {
                        lisp_obj *args;
                        lisp_obj *body;
                } lambda;
                primitive_function primitive;
        };
};

lisp_obj *LISP_T;
lisp_obj *LISP_NIL;
lisp_obj *LISP_QUOTE;
lisp_obj *LISP_IF;
lisp_obj *LISP_OR;
lisp_obj *LISP_AND;
lisp_obj *LISP_PROGN;
lisp_obj *LISP_LAMBDA;
lisp_obj *LISP_SETQ;
lisp_obj *LISP_BASE_ENVIRONMENT;

std::string pretty_print(const lisp_obj *obj);


lisp_obj *intern_symbol(const std::string &symbol_name) 
{
        static std::unordered_map<std::string, lisp_obj*> interned_symbols;
        auto it = interned_symbols.find(symbol_name);
        if (it != interned_symbols.end())
                return it->second;
        auto symbol = new lisp_obj();
        symbol->type = SYM_TYPE;
        symbol->symbol = new std::string(symbol_name);
        interned_symbols[symbol_name] = symbol;
        return symbol;
}

lisp_obj *create_lisp_obj_symbol(const std::string &in)
{
        return intern_symbol(in);
}

lisp_obj *create_lisp_obj_character(char in)
{
        lisp_obj *ret = new lisp_obj();
        ret->type = CHAR_TYPE;
        ret->character = in;
        return ret;
}

lisp_obj *create_lisp_obj_integer(int in)
{
        lisp_obj *ret = new lisp_obj();
        ret->type = INT_TYPE;
        ret->integer = in;
        return ret;
}

lisp_obj *create_lisp_obj_cons(lisp_obj *car, lisp_obj *cdr)
{
        lisp_obj *ret = new lisp_obj();
        ret->type = CONS_TYPE;
        ret->cons.car = car;
        ret->cons.cdr = cdr;
        return ret;
}

lisp_obj *create_lisp_obj_lambda(lisp_obj *args, lisp_obj *body)
{
        lisp_obj *ret = new lisp_obj();
        ret->type = LAMBDA_TYPE;
        ret->lambda.args = args;
        ret->lambda.body = body;
        return ret;
}

lisp_obj *create_lisp_obj_primitive_function(primitive_function primitive) 
{
        lisp_obj *ret = new lisp_obj();
        ret->type = PRIMITIVE_FUNCTION_TYPE;
        ret->primitive = primitive;
        return ret;
}


bool is_whitespace(char c)
{
        return (c == ' ' || c == '\n' || c == '\t');
}

bool is_digit(char c)
{
        return (c >= '0' && c <= '9');
}

bool is_symbol_start_char(char c)
{
        if (c == '(' || c == ')' || c == '\''
            || is_whitespace(c) 
            || is_digit(c))
                return false;
        else
                return true;
}

bool is_symbol_char(char c)
{
        return is_symbol_start_char(c) || is_digit(c);
}

std::string str_lower(std::string in)
{
        std::transform(in.begin(), in.end(), in.begin(), [](unsigned char c){ return std::tolower(c) ; } );
        return in;
}

std::string str_upper(std::string in)
{
        std::transform(in.begin(), in.end(), in.begin(), [](unsigned char c){ return std::toupper(c) ; } );
        return in;
}

void consume_whitespace(const std::string &data, int &i)
{
        while (i < data.size() && is_whitespace(data[i]))
               i++;
}

lisp_obj *parse(const std::string &data, int &i)
{
        // symbols, ints, a char
        // symbol = anything not (), is not whitespace, doesnt start with int
        // an int = series of numbas :^) basically
        // a char: #\<anything>
        while (i < data.size()) { 
                consume_whitespace(data, i);
                if (is_digit(data[i])) {
                        const int start_index = i;
                        i++;
                        while (i < data.size() && is_digit(data[i])) {
                                i++;
                        }
                        const int end_index = i;
                        const int count = end_index - start_index;
                        int integer = std::stoi(data.substr(start_index, count));
                        return create_lisp_obj_integer(integer);
                }
                else if (data[i] == '#') {
                        i++;
                        if (data[i] == '\\') {
                                i++;
                                const int start_index = i;
                                while (i < data.size() && !is_whitespace(data[i])) {
                                        i++;
                                }
                                const int end_index = i;
                                const int count = end_index - start_index;
                                std::string character = data.substr(start_index, count);
                                if (character.size() == 1) {
                                        return create_lisp_obj_character(character[0]);
                                }
                                else  {
                                        character = str_upper(character);
                                        if (character == "SPACE") {
                                                return create_lisp_obj_character(' ');
                                        }
                                        else if (character == "RETURN") {
                                                return create_lisp_obj_character('\r');
                                        }
                                        else if (character == "NEWLINE") {
                                                return create_lisp_obj_character('\n');
                                        }
                                        else if (character == "TAB") {
                                                return create_lisp_obj_character('\t');
                                        }
                                        else {
                                                // @TODO: pooper error handling
                                                printf("[ERROR] Unknown character name %s\n", character.c_str());
                                                abort();
                                        }
                                }
                        }
                }
                else if (data[i] == '\'') {
                        i++;
                        auto quoted_val = parse(data, i);
                        return create_lisp_obj_cons(LISP_QUOTE, create_lisp_obj_cons(quoted_val, LISP_NIL));
                }
                else if (is_symbol_start_char(data[i])) {
                        const int start_index = i;
                        i++;
                        while (i < data.size() && is_symbol_char(data[i])) {
                                i++;
                        }
                        const int end_index = i;
                        const int count = end_index - start_index;
                        std::string symbol = data.substr(start_index, count);
                        symbol = str_upper(symbol);
                        if (symbol == "NIL")
                                return LISP_NIL;
                        if (symbol == "T")
                                return LISP_T;
                        return create_lisp_obj_symbol(symbol);
                }
                else if (data[i] == '(') {
                        i++;
                        consume_whitespace(data, i);
                        if (data[i] == ')') {
                                i++;
                                return LISP_NIL;
                        }
                        auto car = parse(data, i);
                        consume_whitespace(data, i);
                        if (data[i] == '.') {
                                i++;
                                auto cdr = parse(data, i);
                                if (data[i] == ')')
                                        i++;
                                return create_lisp_obj_cons(car, cdr);
                        }
                        auto head = create_lisp_obj_cons(car, LISP_NIL);
                        car = head;
                        while (data[i] != ')') {
                                auto elem = parse(data, i);
                                car->cons.cdr = create_lisp_obj_cons(elem, LISP_NIL);
                                car = car->cons.cdr;
                                consume_whitespace(data, i);
                        }
                        if (data[i] == ')')
                                i++;
                        return head;
                }
        }
        return nullptr;
}

lisp_obj *car(lisp_obj *obj) {if (obj == LISP_NIL) return LISP_NIL; return obj->cons.car;}
lisp_obj *cdr(lisp_obj *obj) {if (obj == LISP_NIL) return LISP_NIL; return obj->cons.cdr;}
lisp_obj *cddr(lisp_obj *obj) { return cdr(cdr(obj)); }
lisp_obj *cadr(lisp_obj *obj) {return car(cdr(obj));}
lisp_obj *caddr(lisp_obj *obj) {return car(cdr(cdr(obj)));}
lisp_obj *cadddr(lisp_obj *obj) {return car(cdr(cdr(cdr(obj))));}
lisp_obj *caar(lisp_obj *obj) { return car(car(obj)); }
lisp_obj *first(lisp_obj *obj) {return car(obj);}
lisp_obj *rest(lisp_obj *obj) {return cdr(obj);}
lisp_obj *second(lisp_obj *obj) {return cadr(obj);}
lisp_obj *third(lisp_obj *obj) {return caddr(obj);}
lisp_obj *fourth(lisp_obj *obj) {return cadddr(obj);}

lisp_obj *push(lisp_obj *item, lisp_obj *place) 
{
        if (place == LISP_NIL) {
                return create_lisp_obj_cons(item, LISP_NIL);
        }
        auto original = car(place);
        place->cons.car = item;
        place->cons.cdr = create_lisp_obj_cons(original, cdr(place));
        return place;
}

lisp_obj *bind_primitive(lisp_obj **env, const std::string &symbol_name, primitive_function primitive) 
{
        auto prim_object = create_lisp_obj_primitive_function(primitive);
        auto symbol = create_lisp_obj_symbol(symbol_name);
        auto binding = create_lisp_obj_cons(symbol, prim_object);
        *env = push(binding, *env);
        return binding;
}

lisp_obj *symbol_lookup(lisp_obj *env, lisp_obj *symbol) 
{
        /* env is a list of pairs mapping symbols to their corresponding value in the form of 
         * ((symbol . value) (symbol . value) (symbol . value))
         */
        if (env->type == CONS_TYPE) {
                while (env != LISP_NIL) {
                        auto pair = car(env);
                        auto s = car(pair);
                        auto v = cdr(pair);
                        if (s == symbol)
                                return pair;
                        env = cdr(env);
                }
        }
        return nullptr;
}

lisp_obj *assoc(lisp_obj *item, lisp_obj *alist)
{
        if (alist == LISP_NIL)
                return alist;
        if (item == caar(alist))
                return car(alist);
        else
                return assoc(item, cdr(alist));
}

lisp_obj *lisp_prim_plus(lisp_obj *env, lisp_obj *args) 
{
        int result = 0;
        while (args != LISP_NIL) {
                // @TODO: Type validation, (+ 1 2 3) OK
                //                         (+ 1 #\a) BAD
                auto tmp = car(args);
                result += tmp->integer;
                args = cdr(args);
        }
        return create_lisp_obj_integer(result);
}

lisp_obj *lisp_prim_minus(lisp_obj *env, lisp_obj *args)
{
        int result = 0;
        // @TODO: Type validation
        if (cdr(args) == LISP_NIL) {
                result = -car(args)->integer;
        }
        else {
                result = car(args)->integer;
                args = cdr(args);
                while (args != LISP_NIL) {
                        result -= car(args)->integer;
                        args = cdr(args);
                }
        }
        return create_lisp_obj_integer(result);
}


lisp_obj *lisp_prim_multiply(lisp_obj *env, lisp_obj *args)
{
        int result = 1;
        while (args != LISP_NIL) {
                // @TODO: Type validation
                auto tmp = car(args);
                result *= tmp->integer;
                args = cdr(args);
        }
        return create_lisp_obj_integer(result);
}

lisp_obj *lisp_prim_print(lisp_obj *env, lisp_obj *args) 
{
        while (args != LISP_NIL) {
                auto tmp = car(args);
                pretty_print(tmp);
                args = cdr(args);
        }
        return LISP_NIL;
}

lisp_obj *lisp_prim_num_less(lisp_obj *env, lisp_obj *args) 
{
        // (< 1)
        if (cdr(args) == LISP_NIL) {
                return LISP_T;
        }

        auto a = first(args);
        auto b = second(args);
        bool result = a->integer < b->integer;
        if (result) {
                args = cddr(args);
                a = b;
                while (args != LISP_NIL) {
                        b = car(args);
                        result = a->integer < b->integer;
                        if (result == false) {
                                break;
                        }
                        a = b;
                        args = cdr(args);
                }
        }
        return result ? LISP_T : LISP_NIL;
}


lisp_obj *lisp_prim_dump_env(lisp_obj *env, lisp_obj *) 
{
        pretty_print(env);
        return LISP_NIL;
}

lisp_obj *evaluate(lisp_obj *env, lisp_obj *obj);
lisp_obj *evaluate_list(lisp_obj *env, lisp_obj *list) 
{
        if (list == LISP_NIL)
                return list;
        auto expr = evaluate(env, car(list));
        auto head = create_lisp_obj_cons(expr, LISP_NIL);
        list = cdr(list);
        expr = head;
        while (list != LISP_NIL) {
                auto next_val = evaluate(env, car(list));
                expr->cons.cdr = create_lisp_obj_cons(next_val, LISP_NIL);
                expr = cdr(expr);
                list = cdr(list);
        }
        return head;
}

template<typename T>
lisp_obj *for_each(lisp_obj *head, T function)
{
        auto current = head;
        while (current != LISP_NIL) {
                auto elem = car(current);
                function(elem);
                current = cdr(current);
        }
        return head;
}

lisp_obj *bind(lisp_obj *env, lisp_obj *symbol, lisp_obj *value) 
{
        auto tmp = symbol_lookup(env, symbol);
        if (tmp == nullptr) {
                tmp = create_lisp_obj_cons(symbol, value);
                push(tmp, env);
        }
        else {
                tmp->cons.cdr = value;
        }
        return cdr(tmp);
}

lisp_obj *shadow(lisp_obj *env, lisp_obj *symbol, lisp_obj *value) 
{
        auto binding = create_lisp_obj_cons(symbol, value);
        auto shadow_env = create_lisp_obj_cons(binding, env);
        return shadow_env;
}

lisp_obj *evaluate(lisp_obj *env, lisp_obj *obj) 
{
        switch (obj->type) {
                case SYM_TYPE: {
                        auto val = symbol_lookup(env, obj); 
                        if (val == nullptr) {
                                printf("UNBOUND VARIABLE: %s\n", obj->symbol->c_str()); // @TODO: POOPER ERROR HANDLING
                                abort();
                        }
                        return cdr(val);
                }
                case CHAR_TYPE: return obj;
                case INT_TYPE: return obj;
                case NIL_TYPE: return obj;
                case CONS_TYPE: {
                        auto car = first(obj);
                        if (car == LISP_QUOTE) {
                                return second(obj);
                        }
                        else if (car == LISP_IF) {
                                auto condition = evaluate(env, second(obj));
                                if (condition != LISP_NIL) {
                                        return evaluate(env, third(obj));
                                }
                                else {
                                        return evaluate(env, fourth(obj));
                                }
                        }
                        else if (car == LISP_OR) {
                        }
                        else if (car == LISP_AND) {
                        }
                        else if (car == LISP_PROGN) {
                        }
                        else if (car == LISP_LAMBDA) {
                                auto args = second(obj);
                                auto body = cddr(obj);
                                auto tmp = create_lisp_obj_lambda(args, body);
                                return tmp;
                        }
                        else if (car == LISP_SETQ) {
                                auto variable_name = second(obj);
                                auto value = evaluate(env, third(obj));
                                return bind(env, variable_name, value);
                        }
                        else {
                                auto function = evaluate(env, car);
                                if (function->type == PRIMITIVE_FUNCTION_TYPE) {
                                        return function->primitive(env, evaluate_list(env, rest(obj)));
                                }
                                else if (function->type == LAMBDA_TYPE) {
                                        auto args = evaluate_list(env, rest(obj));
                                        auto params = function->lambda.args;
                                        // THERE IS A PROBLEM WHEN LEN(ARGS) != LEN(PARAMS)
                                        auto shadowed_env = env;
                                        while (params != LISP_NIL) {
                                                shadowed_env = shadow(shadowed_env, first(params), evaluate(env, first(args)));
                                                params = cdr(params);
                                                args = cdr(args);
                                        }
                                        
                                        auto body = function->lambda.body;
                                        auto result = LISP_NIL;
                                        while (body != LISP_NIL) {
                                                result = evaluate(shadowed_env, first(body));
                                                body = cdr(body);
                                        }
                                        return result;
                                }
                        }
                } break;
        }
        return nullptr;
}

std::string repr(const lisp_obj *obj)
{
        std::string result;
        switch (obj->type) {
        case SYM_TYPE:
                result = *obj->symbol;
                break;
        case CHAR_TYPE:
                switch (obj->character) {
                        default: 
                                result = std::string("#\\") + obj->character; //std::string(1, obj->character);
                                break;
                        case ' ':
                                result = "#\\Space";
                                break;
                        case '\t':
                                result = "#\\Tab";
                                break;
                        case '\n':
                                result = "#\\Newline";
                                break;
                        case '\r':
                                result = "#\\Return";
                                break;
                }
                break;
        case INT_TYPE:
                result = std::to_string(obj->integer);
                break;
        case CONS_TYPE:
                result += "(";
                result += repr(obj->cons.car);
                if (obj->cons.cdr == LISP_NIL) {
                        ; /* poo flower */
                }
                else if (obj->cons.cdr->type == CONS_TYPE) {
                        lisp_obj *current = obj->cons.cdr;
                        while (current->type == CONS_TYPE && current != LISP_NIL) {
                                result += " ";
                                result += repr(current->cons.car);
                                current = current->cons.cdr;
                        }
                }
                else {
                        result += " . ";
                        result += repr(obj->cons.cdr);
                }
                result += ")";
                break;
        case NIL_TYPE:
                result = "NIL";
                break;
        case LAMBDA_TYPE:
                result += "(LAMBDA ";
                if (obj->lambda.args == LISP_NIL) {
                        result += "() ";
                }
                else {
                        result += repr(obj->lambda.args);
                        result += " ";
                }
                result += repr(obj->lambda.body);
                result += ")";
        }
        return result;
}

std::string pretty_print(const lisp_obj *obj)
{
        std::string result  = repr(obj);
        printf("%s\n", result.c_str());
        return result;
}

int main(int argc, char *argv[])
{
        LISP_NIL = new lisp_obj();
        LISP_NIL->type = NIL_TYPE;
        
        LISP_QUOTE  = create_lisp_obj_symbol("QUOTE");
        LISP_T      = create_lisp_obj_symbol("T");
        LISP_IF     = create_lisp_obj_symbol("IF");
        LISP_OR     = create_lisp_obj_symbol("OR");
        LISP_AND    = create_lisp_obj_symbol("AND");
        LISP_PROGN  = create_lisp_obj_symbol("PROGN");
        LISP_LAMBDA = create_lisp_obj_symbol("LAMBDA");
        LISP_SETQ   = create_lisp_obj_symbol("SETQ");
        
        LISP_BASE_ENVIRONMENT = LISP_NIL;
        
        bind_primitive(&LISP_BASE_ENVIRONMENT, "+", lisp_prim_plus);
        bind_primitive(&LISP_BASE_ENVIRONMENT, "PRINT", lisp_prim_print);
        bind_primitive(&LISP_BASE_ENVIRONMENT, "DUMP-ENV", lisp_prim_dump_env);
        bind_primitive(&LISP_BASE_ENVIRONMENT, "-", lisp_prim_minus);
        bind_primitive(&LISP_BASE_ENVIRONMENT, "<", lisp_prim_num_less);
        bind_primitive(&LISP_BASE_ENVIRONMENT, "*", lisp_prim_multiply);

        while (1) {
                int curr_index = 0;
                char *input = readline("lisp_nasa> ");
                if (!input) break;
                std::string data(input);
                add_history(input);
                while (curr_index < data.size()) {
                        //printf("[DEBUG] curr_index %d\n", curr_index);
                        lisp_obj *obj = parse(input, curr_index);
                        //if (obj != nullptr) {
                        //        printf("[DEBUG] lisp_obj @ %p\n", obj);
                        //        pretty_print(obj);
                        //}
                        if (obj != nullptr) {
                                lisp_obj *result = evaluate(LISP_BASE_ENVIRONMENT, obj);
                                if (result != nullptr) {
                                        pretty_print(result);
                                }
                        }
                }
                free(input);
        }
        return 0;
}
