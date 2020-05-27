#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <algorithm>
#include <unordered_map>
#include <sstream>
#include <cassert>

#include "lisp.hpp"
#include "primitives.hpp"

using namespace lisp;

struct lisp_stream {
        static const int end_of_file = -1;
        virtual int getc() = 0;
        virtual int peekc(int n=0) const = 0;
};

struct lisp_string_stream : lisp_stream {
        inline
        lisp_string_stream()
                : m_index(0) {}

        inline
        lisp_string_stream(const std::string &data)
                : m_index(0)
                , m_data(data) {}

        inline
        lisp_string_stream(const char *data)
                : m_index(0)
                , m_data(data) {}

        int getc() {
                if (m_index >= m_data.size()) {
                        return end_of_file;
                }
                return m_data[m_index++];
        }

        int peekc(int n = 0) const {
                const size_t idx = m_index + n;
                if (idx >= m_data.size()) {
                        return end_of_file;
                }
                return m_data[idx];
        }

        inline
        void clear() {
                m_index = 0;
                m_data = "";
        }

        inline
        void append(const std::string &data) {
                m_data.append(data);
        }

        inline
        void append(const char *data) {
                m_data.append(data);
        }

        inline
        void append(char c) {
                m_data.push_back(c);
        }

        inline
        void puts() const {
                printf("%s\n", m_data.c_str() + m_index);
        }

        inline
        size_t index() const { return m_index; };
        inline
        void index(size_t idx) { m_index = idx; };

private:
        size_t m_index;
        std::string m_data;
};

std::unordered_map<std::string, lisp_value> LISP_MACROS;
lisp_value LISP_BASE_ENVIRONMENT;
namespace lisp {
        lisp_value LISP_T;
        lisp_value LISP_NIL;
        lisp_value LISP_SYM_QUOTE;
        lisp_value LISP_SYM_IF;
        lisp_value LISP_SYM_LAMBDA;
        lisp_value LISP_SYM_SETQ;
        lisp_value LISP_SYM_DEFMACRO;
        lisp_value LISP_SYM_FIXNUM;
        lisp_value LISP_SYM_CONS;
        lisp_value LISP_SYM_CHARACTER;
        lisp_value LISP_SYM_FUNCTION;
        lisp_value LISP_SYM_SYMBOL;
        lisp_value LISP_SYM_NULL;
        lisp_value LISP_SYM_BOOLEAN;

        lisp_value intern_symbol(const std::string &symbol_name) 
        {
                static std::unordered_map<std::string, lisp_value> interned_symbols;
                auto it = interned_symbols.find(symbol_name);
                if (it != interned_symbols.end())
                        return it->second;
                auto symbol = new lisp_obj();
                symbol->type = SYM_TYPE;
                symbol->symbol = new std::string(symbol_name);
                interned_symbols[symbol_name] = lisp_value(symbol);
                return symbol;
        }

        std::string repr(const lisp_value obj)
        {
                std::string result;
                std::stringstream ss;
                if (obj.is_fixnum()) {
                        result = std::to_string(obj.as_fixnum());
                }
                else if (obj.is_nil()) {
                        result = "NIL";
                }
                else if (obj.is_object()) {
                        switch (obj.as_object()->type) {
                                case SYM_TYPE:
                                        result = *(obj.as_object()->symbol);
                                        break;
                                case CHAR_TYPE:
                                        switch (obj.as_object()->character) {
                                                default: 
                                                        result = std::string("#\\") + obj.as_object()->character; 
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
                                case CONS_TYPE:
                                        result += "(";
                                        result += repr(car(obj));
                                        if (cdr(obj) == LISP_NIL) {
                                                ; /* poo flower */
                                        }
                                        else if (cdr(obj).as_object()->type) {
                                                lisp_value current = cdr(obj);
                                                while (current != LISP_NIL) {
                                                        result += " ";
                                                        result += repr(car(current));
                                                        current = cdr(current);
                                                }
                                        }
                                        else {
                                                result += " . ";
                                                result += repr(cdr(obj));
                                        }
                                        result += ")";
                                        break;
                                case PRIMITIVE_FUNCTION_TYPE:
                                        ss << "#<PRIMITIVE 0x";
                                        ss << std::hex << reinterpret_cast<uintptr_t>(obj.as_object()->primitive);
                                        ss << ">";
                                        result = ss.str();
                                        break;
                                case LAMBDA_TYPE:
                                        result += "(LAMBDA ";
                                        if (obj.as_object()->lambda.args == LISP_NIL) {
                                                result += "() ";
                                        }
                                        else {
                                                result += repr(obj.as_object()->lambda.args);
                                                result += " ";
                                        }
                                        auto body = obj.as_object()->lambda.body;
                                        while (body != LISP_NIL) {
                                                result += repr(car(body));
                                                body = cdr(body);
                                        }
                                        result += ")";
                        }
                }
                return result;
        }

        std::string pretty_print(const lisp_value obj)
        {
                std::string result  = repr(obj);
                printf("%s\n", result.c_str());
                return result;
        }
}

static inline
bool is_whitespace(int c)
{
        return (c == ' ' || c == '\n' || c == '\t');
}

static inline
bool is_digit(int c)
{
        return (c >= '0' && c <= '9');
}

static inline
bool is_symbol_start_char(int c)
{
        if (c == lisp_stream::end_of_file)
                return false;
        if (c == '(' || c == ')' || c == '\''
            || is_whitespace(c) 
            || is_digit(c))
                return false;
        else
                return true;
}

static inline
bool is_symbol_char(int c)
{
        return is_symbol_start_char(c) || is_digit(c);
}

static inline
std::string str_lower(std::string in)
{
        std::transform(in.begin(), in.end(), in.begin(), [](unsigned char c){ return std::tolower(c) ; } );
        return in;
}

static inline
std::string str_upper(std::string in)
{
        std::transform(in.begin(), in.end(), in.begin(), [](unsigned char c){ return std::toupper(c) ; } );
        return in;
}

static inline
void consume_whitespace(lisp_stream &stream)
{
        while (is_whitespace(stream.peekc()))
                stream.getc();
}

static
lisp_value parse(lisp_stream &stream)
{
        // symbols, ints, a char
        // symbol = anything not (), is not whitespace, doesnt start with int
        // an int = series of numbas :^) basically
        // a char: #\<anything>
        while (stream.peekc() != stream.end_of_file) { 
                consume_whitespace(stream);
                if (stream.peekc() == ';') {
                        stream.getc();
                        while (stream.peekc() != '\n') {
                                stream.getc();
                        }
                }
                else if (is_digit(stream.peekc())) {
                        std::string number_str;
                        number_str += stream.getc();
                        while (is_digit(stream.peekc())) {
                                number_str += stream.getc();
                        }
                        int64_t integer = std::stoll(number_str);
                        return create_lisp_obj_integer(integer);
                }
                else if (stream.peekc() == '#') {
                        stream.getc();
                        if (stream.peekc() == '\\') {
                                stream.getc();
                                std::string character;
                                while (is_symbol_char(stream.peekc())) {
                                        character += stream.getc();
                                }
                                if (character.size() == 0) {
                                        // error
                                }
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
                else if (stream.peekc() == '\'') {
                        stream.getc();
                        auto quoted_val = parse(stream);
                        if (quoted_val == nullptr)
                                return nullptr;
                        return cons(LISP_SYM_QUOTE, cons(quoted_val, LISP_NIL));
                }
                else if (is_symbol_start_char(stream.peekc())) {
                        std::string symbol;
                        symbol += stream.getc();
                        while (is_symbol_char(stream.peekc())) {
                                symbol += stream.getc();
                        }
                        symbol = str_upper(symbol);
                        if (symbol == "NIL")
                                return LISP_NIL;
                        if (symbol == "T")
                                return LISP_T;
                        return intern_symbol(symbol);
                }
                else if (stream.peekc() == '(') {
                        stream.getc();
                        consume_whitespace(stream);
                        if (stream.peekc() == ')') {
                                stream.getc();
                                return LISP_NIL;
                        }
                        auto car_obj = parse(stream);
                        if (car_obj == nullptr)
                                return nullptr;
                        consume_whitespace(stream);
                        if (stream.peekc() == '.') {
                                stream.getc();
                                auto cdr_obj = parse(stream);
                                if (cdr_obj == nullptr)
                                        return nullptr;
                                if (stream.peekc() == ')')
                                        stream.getc();
                                return cons(car_obj, cdr_obj);
                        }
                        auto head = cons(car_obj, LISP_NIL);
                        car_obj = head;
                        while (stream.peekc() != ')') {
                                auto elem = parse(stream);
                                if (elem == nullptr)
                                        return nullptr;
                                
                                set_cdr(car_obj, cons(elem, LISP_NIL));
                                car_obj = cdr(car_obj);
                                consume_whitespace(stream);
                        }
                        if (stream.peekc() == ')')
                                stream.getc();
                        return head;
                }
        }
        return nullptr;
}

static inline
lisp_value push(lisp_value item, lisp_value place) 
{
        if (place == LISP_NIL) {
                return cons(item, LISP_NIL);
        }
        auto original = car(place);
        set_car(place, item);
        set_cdr(place, cons(original, cdr(place)));
        return place;
}

static
lisp_value symbol_lookup(lisp_value env, lisp_value symbol) 
{
        /* env is a list of pairs mapping symbols to their corresponding value in the form of 
         * ((symbol . value) (symbol . value) (symbol . value))
         */
        if (env.as_object()->type == CONS_TYPE) {
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

static
lisp_value assoc(lisp_value item, lisp_value alist)
{
        if (alist == LISP_NIL)
                return alist;
        if (item == caar(alist))
                return car(alist);
        else
                return assoc(item, cdr(alist));
}

static lisp_value evaluate(lisp_value env, lisp_value obj);
lisp_value evaluate_list(lisp_value env, lisp_value list) 
{
        if (list == LISP_NIL)
                return list;
        auto expr = evaluate(env, car(list));
        auto head = cons(expr, LISP_NIL);
        list = cdr(list);
        expr = head;
        while (list != LISP_NIL) {
                auto next_val = evaluate(env, car(list));
                set_cdr(expr, cons(next_val, LISP_NIL));
                expr = cdr(expr);
                list = cdr(list);
        }
        return head;
}

template<typename T>
lisp_value for_each(lisp_value head, T function)
{
        auto current = head;
        while (current != LISP_NIL) {
                auto elem = car(current);
                function(elem);
                current = cdr(current);
        }
        return head;
}

static inline
lisp_value bind(lisp_value env, lisp_value symbol, lisp_value value) 
{
        auto tmp = symbol_lookup(env, symbol);
        if (tmp == nullptr) {
                tmp = cons(symbol, value);
                push(tmp, env);
        }
        else {
                set_cdr(tmp, value);
        }
        return cdr(tmp);
}

static inline
lisp_value shadow(lisp_value env, lisp_value symbol, lisp_value value) 
{
        auto binding = cons(symbol, value);
        auto shadow_env = cons(binding, env);
        return shadow_env;
}

static
lisp_value evaluate(lisp_value env, lisp_value obj) 
{
        if (obj.is_fixnum()) {
                return obj;
        }
        if (obj.is_nil()) {
                return obj;
        }
        if (obj.is_object()) {
                switch (obj.as_object()->type) {
                        case SYM_TYPE: {
                                if (obj == LISP_T) return obj;
                                auto val = symbol_lookup(env, obj); 
                                if (val == nullptr) {
                                        printf("UNBOUND VARIABLE: %s\n", obj.as_object()->symbol->c_str()); // @TODO: POOPER ERROR HANDLING
                                        abort();
                                }
                                return cdr(val);
                        }
                        case CHAR_TYPE: return obj;
                        case CONS_TYPE: {
                                auto car = first(obj);
                                if (car == LISP_SYM_QUOTE) {
                                        return second(obj); // intentionally not evaluated.
                                }
                                else if (car == LISP_SYM_IF) {
                                        auto condition = evaluate(env, second(obj));
                                        if (condition != LISP_NIL) {
                                                return evaluate(env, third(obj));
                                        }
                                        else {
                                                return evaluate(env, fourth(obj));
                                        }
                                }
                                else if (car == LISP_SYM_DEFMACRO) {
                                        // (defmacro NAME (PARAMS-LIST) &body BODY...)
                                        auto macro_name = second(obj);
                                        auto params_list = third(obj);
                                        auto body = cdddr(obj);
                                        auto macro = create_lisp_obj_lambda(env, params_list, body);
                                        LISP_MACROS[*(macro_name.as_object()->symbol)] = macro;
                                        return bind(env, macro_name, macro);
                                        return macro_name;
                                }
                                else if (car == LISP_SYM_LAMBDA) {
                                        auto args = second(obj);
                                        auto body = cddr(obj);
                                        auto tmp = create_lisp_obj_lambda(env, args, body);
                                        return tmp;
                                }
                                else if (car == LISP_SYM_SETQ) {
                                        auto variable_name = second(obj);
                                        auto value = evaluate(env, third(obj));
                                        return bind(env, variable_name, value);
                                }
                                else {
                                        auto function = evaluate(env, car);
                                        if (function.as_object()->type == PRIMITIVE_FUNCTION_TYPE) {
                                                return function.as_object()->primitive(env, evaluate_list(env, rest(obj)));
                                        }
                                        else if (function.as_object()->type == LAMBDA_TYPE) {
                                                auto args = rest(obj);
                                                auto params = function.as_object()->lambda.args;
                                                // THERE IS A PROBLEM WHEN LEN(ARGS) != LEN(PARAMS)
                                                auto shadowed_env = function.as_object()->lambda.env;
                                                while (params != LISP_NIL) {
                                                        auto sym = first(params);
                                                        if (*(sym.as_object()->symbol) == "&REST") {
                                                                sym = second(params);
                                                                shadowed_env = shadow(shadowed_env, sym, evaluate_list(env, args));
                                                                break;
                                                        }
                                                        auto arg = evaluate(env, first(args));
                                                        shadowed_env = shadow(shadowed_env, sym, arg);
                                                        params = rest(params);
                                                        args = rest(args);
                                                }
                                        
                                                auto body = function.as_object()->lambda.body;
                                                auto result = LISP_NIL;
                                                while (body != LISP_NIL) {
                                                        result = evaluate(shadowed_env, first(body));
                                                        body = rest(body);
                                                }
                                                return result;
                                        }
                                }
                        } break;
                }
        }
        return nullptr;
}

static
lisp_value map(lisp_value list, lisp_value (func)(lisp_value))
{
        if (list == LISP_NIL)
                return list;
        
        auto head = cons(func(car(list)), LISP_NIL);
        auto current = head;
        list = cdr(list);
        while (list != LISP_NIL) {
                set_cdr(current, cons(func(car(list)), LISP_NIL));
                current = cdr(current);
                list = cdr(list);
        }
        return head;
}

static
int length(lisp_value obj) 
{
        if (obj == LISP_NIL)
                return 0;
        int i = 0;
        if (obj.as_object()->type == CONS_TYPE) {
                while (obj != LISP_NIL) {
                        i += 1;
                        obj = cdr(obj);
                }
        }
        return i;
}

static
lisp_value macro_expand(lisp_value obj)
{
        if (obj.is_fixnum()) {
                return obj;
        }
        if (obj.is_nil()) {
                return obj;
        }
        if (obj.as_object()->type != CONS_TYPE) {
                return obj;
        }
        auto car = first(obj);
        if (car.as_object()->type == SYM_TYPE) {
                auto &sym_name = *car.as_object()->symbol;
                auto it = LISP_MACROS.find(sym_name);
                if (it != LISP_MACROS.end()) {
                        auto macro = it->second;
                        auto args = rest(obj);
                        auto params = macro.as_object()->lambda.args;
                        // THERE IS A PROBLEM WHEN LEN(ARGS) != LEN(PARAMS)
                        auto shadowed_env = macro.as_object()->lambda.env;
                        while (params != LISP_NIL) {
                                auto sym = first(params);
                                if (*(sym.as_object()->symbol) == "&REST") {
                                        sym = second(params);
                                        shadowed_env = shadow(shadowed_env, sym, args);
                                        break;
                                }
                                auto arg = first(args);
                                shadowed_env = shadow(shadowed_env, sym, arg);
                                params = rest(params);
                                args = rest(args);
                        }
                                        
                        auto body = macro.as_object()->lambda.body;
                        auto result = LISP_NIL;
                        while (body != LISP_NIL) {
                                result = evaluate(shadowed_env, first(body));
                                body = rest(body);
                        }
                        return result;
                }
        }
        return map(obj, macro_expand);
}

static
void initialize_globals()
{
        LISP_T = intern_symbol("T");

#define INTERN_GLOBAL(name) LISP_SYM_##name = intern_symbol(#name)
        INTERN_GLOBAL(QUOTE);
        INTERN_GLOBAL(IF);
        INTERN_GLOBAL(LAMBDA);
        INTERN_GLOBAL(SETQ);
        INTERN_GLOBAL(DEFMACRO);
        INTERN_GLOBAL(FIXNUM);
        INTERN_GLOBAL(CONS);
        INTERN_GLOBAL(CHARACTER);
        INTERN_GLOBAL(FUNCTION);
        INTERN_GLOBAL(SYMBOL);
        INTERN_GLOBAL(NULL);
        INTERN_GLOBAL(BOOLEAN);
        
        LISP_BASE_ENVIRONMENT = LISP_NIL;
        primitives::bind_primitives(LISP_BASE_ENVIRONMENT);
}

int main(int argc, char *argv[])
{
        initialize_globals();

        lisp_string_stream stream;
        static const char *prompt_lisp = "lisp_nasa> ";
        static const char *prompt_ws   = ".......... ";
        while (1) {
                char *input = readline(prompt_lisp);
                if (!input) break;
                stream.clear();
                stream.append(input);
                stream.append('\n');

                add_history(input);
                free(input);
                while (stream.peekc() != stream.end_of_file) {
                        auto idx = stream.index();
                        lisp_value obj = parse(stream);
                        while (obj == nullptr) {
                                char *continued = readline(prompt_ws);
                                if (!continued) break;
                                stream.append(continued);
                                stream.append('\n');
                                add_history(continued);
                                free(continued);
                                stream.index(idx);
                                obj = parse(stream);
                        }
                        consume_whitespace(stream);
                        if (obj == nullptr) break;
                        obj = macro_expand(obj);
                        pretty_print(obj);
                        lisp_value result = evaluate(LISP_BASE_ENVIRONMENT, obj);
                        if (result != nullptr) {
                                pretty_print(result);
                        }
                }
        }
        return 0;
}
