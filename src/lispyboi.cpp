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
#include <filesystem>
#include <set>

#include "lisp.hpp"
#include "primitives.hpp"
#include "platform.hpp"

using namespace lisp;

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

        int getc()
        {
                if (m_index >= m_data.size()) {
                        return end_of_file;
                }
                return m_data[m_index++];
        }

        int peekc()
        {
                if (m_index >= m_data.size()) {
                        return end_of_file;
                }
                return m_data[m_index];
        }

        bool eof()
        {
                return m_index >= m_data.size();
        }

        inline
        void clear()
        {
                m_index = 0;
                m_data = "";
        }

        inline
        void append(const std::string &data)
        {
                m_data.append(data);
        }

        inline
        void append(const char *data)
        {
                m_data.append(data);
        }

        inline
        void append(char c)
        {
                m_data.push_back(c);
        }

        inline
        void puts() const
        {
                printf("%s\n", m_data.c_str() + m_index);
        }

        inline
        size_t index() const
        {
                return m_index;
        };

        inline
        void index(size_t idx)
        {
                m_index = idx;
        };

private:
        size_t m_index;
        std::string m_data;
};

std::unordered_map<std::string, lisp_value> LISP_MACROS;
lisp_value LISP_BASE_ENVIRONMENT;
namespace lisp {
        const lisp_value LISP_NIL;
        lisp_value LISP_T;
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
        lisp_value LISP_SYM_STRING;
        lisp_value LISP_SYM_NULL;
        lisp_value LISP_SYM_BOOLEAN;
        lisp_value LISP_SYM_QUASIQUOTE;
        lisp_value LISP_SYM_UNQUOTE;
        lisp_value LISP_SYM_UNQUOTESPLICING;
        lisp_value LISP_SYM_SIMPLE_ARRAY;
        lisp_value LISP_SYM_ARRAY;
        lisp_value LISP_SYM_AMP_REST;
        lisp_value LISP_SYM_AMP_BODY;
        lisp_value LISP_SYM_AMP_OPTIONAL;
}

lisp_value lisp::intern_symbol(const std::string &symbol_name)
{
        static std::unordered_map<std::string, lisp_value> interned_symbols;
        auto it = interned_symbols.find(symbol_name);
        if (it != interned_symbols.end())
                return it->second;
        auto symbol = lisp_obj::create_symbol(symbol_name);
        interned_symbols[symbol_name] = symbol;
        return symbol;
}

std::string repr_impl(lisp_value obj, std::set<uint64_t> &seen)
{
        if (seen.find(obj.bits()) != seen.end()) {
                return "#<CIRCULAR REFERENCE>";
        }
        if (obj.is_cons() || obj.is_type(SIMPLE_ARRAY_TYPE)) {
                seen.insert(obj.bits());
        }
        std::string result;
        std::stringstream ss;
        if (obj.is_fixnum()) {
                result = std::to_string(obj.as_fixnum());
        }
        else if (obj.is_nil()) {
                result = "NIL";
        }
        else if (obj.is_cons()) {
                result += "(";
                result += repr_impl(car(obj), seen);
                if (cdr(obj) == LISP_NIL) {
                        /* List with one element */
                        ;
                }
                else if (cdr(obj).is_cons()) {
                        lisp_value current = cdr(obj);
                        while (current != LISP_NIL) {
                                result += " ";
                                result += repr_impl(car(current), seen);
                                current = cdr(current);
                                if (!current.is_nil() && !current.is_cons()) {
                                        result += " . ";
                                        result += repr_impl(current, seen);
                                        break;
                                }
                        }
                }
                else {
                        result += " . ";
                        result += repr_impl(cdr(obj), seen);
                }
                result += ")";
        }
        else if (obj.is_character()) {
                auto codepoint = obj.as_character();
                switch (codepoint) {
                        default:
                                result = std::string("#\\") + reinterpret_cast<const char*>(&codepoint);
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
        }
        else if (obj.is_byte()) {
                auto byte = obj.as_byte();
                result = std::to_string(byte);
        }
        else if (obj.is_object()) {
                switch (obj.as_object()->type()) {
                        case SYM_TYPE:
                                result = *(obj.as_object()->symbol());
                                break;
                        case LAMBDA_TYPE: {
                                result += "(LAMBDA ";
                                if (obj.as_object()->lambda()->args == LISP_NIL) {
                                        result += "() ";
                                }
                                else {
                                        result += repr(obj.as_object()->lambda()->args);
                                        result += " ";
                                }
                                auto body = obj.as_object()->lambda()->body;
                                while (body != LISP_NIL) {
                                        result += repr_impl(car(body), seen);
                                        body = cdr(body);
                                }
                                result += ")";
                        } break;
                        case SIMPLE_ARRAY_TYPE: {
                                auto array = obj.as_object()->simple_array();
                                result += "#(";
                                if (array->length() != 0) {
                                        size_t i = 0;
                                        size_t end = array->length() - 1;
                                        for (; i < end; ++i) {
                                                result += repr_impl(array->get(i), seen);
                                                result += " ";
                                        }
                                        if (i < array->length()) {
                                                result += repr_impl(array->get(i), seen);
                                        }
                                }
                                result += ")";
                        } break;
                        case FILE_STREAM_TYPE: {
                                result += "#<FILE-STREAM ";
                                result += obj.as_object()->file_stream()->path();
                                result += ">";
                        } break;
                }
        }
        else if (obj.is_lisp_primitive()) {
                ss << "#<PRIMITIVE 0x";
                ss << std::hex << reinterpret_cast<uintptr_t>(obj.as_lisp_primitive());
                ss << ">";
                result = ss.str();
        }
        return result;
}
std::string lisp::repr(lisp_value obj)
{
        std::set<uint64_t> seen;
        return repr_impl(obj, seen);
}
std::string lisp::repr(const lisp_value *obj)
{
        return repr(*obj);
}

std::string lisp::pretty_print(lisp_value obj)
{
        std::string result  = repr(obj);
        printf("%s\n", result.c_str());
        return result;
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
        if (c == '(' || c == ')'
            || c == '\'' || c == '"' || c == '`' || c == ','
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

lisp_value lisp::parse(lisp_stream &stream)
{
        // symbols, ints, a char
        // symbol = anything not (), is not whitespace, doesnt start with int
        // an int = series of numbas :^) basically
        // a char: #\<anything>
        while (!stream.eof()) {
                consume_whitespace(stream);
                if (stream.peekc() == ';') {
                        stream.getc();
                        while (stream.peekc() != '\n') {
                                stream.getc();
                        }
                }
                if (is_digit(stream.peekc())) {
                        std::string number_str;
                        number_str += stream.getc();
                        while (is_digit(stream.peekc())) {
                                number_str += stream.getc();
                        }

                        int64_t result = 0;
                        for (char digit : number_str)
                        {
                                result *= 10;
                                result += digit - '0';
                        }
                        return lisp_value(result);
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
                                        fprintf(stderr, "character literal of length 0?\n");
                                        abort();
                                }
                                if (character.size() == 1) {
                                        return lisp_value(static_cast<int32_t>(character[0]));
                                }
                                else  {
                                        character = str_upper(character);
                                        if (character == "SPACE") {
                                                return lisp_value(static_cast<int32_t>(' '));
                                        }
                                        else if (character == "RETURN") {
                                                return lisp_value(static_cast<int32_t>('\r'));
                                        }
                                        else if (character == "NEWLINE") {
                                                return lisp_value(static_cast<int32_t>('\n'));
                                        }
                                        else if (character == "TAB") {
                                                return lisp_value(static_cast<int32_t>('\t'));
                                        }
                                        else {
                                                // @TODO: pooper error handling
                                                //printf("[ERROR] Unknown character name %s\n", character.c_str());
                                                auto it = *reinterpret_cast<const int32_t*>(character.c_str());
                                                return lisp_value(it);
                                        }
                                }
                        }
                }
                else if (stream.peekc() == '"') {
                        stream.getc(); // consume opening "
                        std::string str;
                        while (stream.peekc() != '"') {
                                str += stream.getc();
                        }
                        stream.getc(); // consume closing "
                        return lisp_obj::create_string(str);
                }
                else if (stream.peekc() == '\'') {
                        stream.getc();
                        auto quoted_val = parse(stream);
                        if (quoted_val.is_invalid())
                                return quoted_val;
                        return list(LISP_SYM_QUOTE, quoted_val);
                }
                else if (stream.peekc() == '`') {
                        stream.getc();
                        auto quoted_val = parse(stream);
                        if (quoted_val.is_invalid())
                                return quoted_val;
                        return list(LISP_SYM_QUASIQUOTE, quoted_val);
                }
                else if (stream.peekc() == ',') {
                        stream.getc();
                        auto symbol = LISP_SYM_UNQUOTE;
                        if (stream.peekc() == '@') {
                                stream.getc();
                                symbol = LISP_SYM_UNQUOTESPLICING;
                        }
                        auto val = parse(stream);
                        if (val.is_invalid())
                                return val;
                        return list(symbol, val);
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
                        if (car_obj.is_invalid())
                                return car_obj;
                        consume_whitespace(stream);
                        if (stream.peekc() == '.') {
                                stream.getc();
                                auto cdr_obj = parse(stream);
                                if (cdr_obj.is_invalid())
                                        return cdr_obj;
                                if (stream.peekc() == ')')
                                        stream.getc();
                                return cons(car_obj, cdr_obj);
                        }
                        auto head = cons(car_obj, LISP_NIL);
                        car_obj = head;
                        while (stream.peekc() != ')') {
                                auto elem = parse(stream);
                                if (elem.is_invalid())
                                        return elem;

                                set_cdr(car_obj, cons(elem, LISP_NIL));
                                car_obj = cdr(car_obj);
                                consume_whitespace(stream);
                        }
                        if (stream.peekc() == ')')
                                stream.getc();
                        return head;
                }
        }
        return lisp_value::invalid_object();
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

static FORCE_INLINE
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

static FORCE_INLINE
lisp_value bind(lisp_value env, lisp_value symbol, lisp_value value)
{
        auto tmp = symbol_lookup(env, symbol);
        if (tmp.is_invalid()) {
                tmp = cons(symbol, value);
                push(tmp, env);
        }
        else {
                set_cdr(tmp, value);
        }
        return cdr(tmp);
}

static FORCE_INLINE
lisp_value shadow(lisp_value env, lisp_value symbol, lisp_value value)
{
        auto binding = cons(symbol, value);
        auto shadow_env = cons(binding, env);
        return shadow_env;
}

static
bool apply_arguments(lisp_value &shadowed_env, lisp_value params, lisp_value args)
{
        // &BODY and &REST are synonyms here.
        // (defun foo ()) => no arguments
        // (defun foo (a b &optional c)) => two required and one optional
        // (defun foo (a b &rest rest)) => two required and any number more
        // (defun foo (a b &optional c &rest rest)) => two required, one optional, and any number more
        // (defun foo (&optional a b c &rest rest)) => three optional and any number more
        bool allowing_optional = false;
        while (params != LISP_NIL && args != LISP_NIL) {
                auto sym = first(params);
                if (allowing_optional && sym.is_cons()) {
                        sym = first(sym);
                }
                else if (sym == LISP_SYM_AMP_REST || sym == LISP_SYM_AMP_BODY) {
                        sym = second(params);
                        shadowed_env = shadow(shadowed_env, sym, args);
                        return true;
                }
                else if (sym == LISP_SYM_AMP_OPTIONAL) {
                        allowing_optional = true;
                        params = rest(params);
                        continue;
                }
                shadowed_env = shadow(shadowed_env, sym, first(args));
                params = rest(params);
                args = rest(args);
        }
        
        if (params != LISP_NIL) {
                if (first(params) == LISP_SYM_AMP_REST || first(params) == LISP_SYM_AMP_BODY) {
                        shadowed_env = shadow(shadowed_env, second(params), LISP_NIL);
                        return true;
                }
                else if (first(params) == LISP_SYM_AMP_OPTIONAL) {
                        allowing_optional = true;
                        params = rest(params);
                }
        
                if (allowing_optional) {
                        while (params != LISP_NIL) {
                                auto sym = first(params);
                                auto val = LISP_NIL;
                                if (sym.is_cons()) {
                                        val = evaluate(shadowed_env, second(sym));
                                        sym = car(sym);
                                }
                                else if (sym == LISP_SYM_AMP_REST || sym == LISP_SYM_AMP_BODY) {
                                        sym = second(params);
                                        shadowed_env = shadow(shadowed_env, sym, val);
                                        return true;
                                }
                                shadowed_env = shadow(shadowed_env, sym, val);
                                params = rest(params);
                        }
                }
        }
        return params == LISP_NIL;
}

lisp_value lisp::apply(lisp_value env, lisp_value function, lisp_value args)
{
        if (function.is_lisp_primitive()) {
                return function.as_lisp_primitive()(env, args);
        }
        else if (function.as_object()->type() == LAMBDA_TYPE) {
                auto params = function.as_object()->lambda()->args;
                auto shadowed_env = function.as_object()->lambda()->env;
                if (!apply_arguments(shadowed_env, params, args)) {
                        pretty_print(function);
                        abort();
                }
                auto body = function.as_object()->lambda()->body;
                auto result = LISP_NIL;
                while (body != LISP_NIL) {
                        result = evaluate(shadowed_env, first(body));
                        body = rest(body);
                }
                return result;
        }
        fprintf(stderr, "not a function: %s\n", repr(function).c_str());
        abort();
}

lisp_value lisp::evaluate(lisp_value env, lisp_value obj)
{
tailcall:
        if (obj.is_fixnum()) {
                return obj;
        }
        if (obj.is_nil()) {
                return obj;
        }
        if (obj.is_cons()) {
                auto car = first(obj);
                if (car == LISP_SYM_QUOTE) {
                        return second(obj); // intentionally not evaluated.
                }
                else if (car == LISP_SYM_IF) {
                        auto condition = evaluate(env, second(obj));
                        if (condition != LISP_NIL) {
                                obj = third(obj);
                                goto tailcall;
                        }
                        else {
                                obj = fourth(obj);
                                goto tailcall;
                        }
                }
                else if (car == LISP_SYM_DEFMACRO) {
                        auto macro_name = second(obj);
                        auto params_list = third(obj);
                        auto body = cdddr(obj);
                        auto macro = lisp_obj::create_lambda(env, params_list, body);
                        LISP_MACROS[*(macro_name.as_object()->symbol())] = macro;
                        return macro_name;
                }
                else if (car == LISP_SYM_LAMBDA) {
                        auto args = second(obj);
                        auto body = cddr(obj);
                        auto tmp = lisp_obj::create_lambda(env, args, body);
                        return tmp;
                }
                else if (car == LISP_SYM_SETQ) {
                        auto variable_name = second(obj);
                        auto value = evaluate(env, third(obj));
                        auto place = symbol_lookup(env, variable_name);
                        if (place.is_invalid()) {
                                push(cons(variable_name, value), LISP_BASE_ENVIRONMENT);
                        }
                        else {
                                set_cdr(place, value);
                        }
                        return value;
                }
                else {
                        auto function = evaluate(env, car);
                        auto args = evaluate_list(env, rest(obj));
                        //return apply(env, function, args);
                        if (function.is_lisp_primitive()) {
                                return function.as_lisp_primitive()(env, args);
                        }
                        else if (function.as_object()->type() == LAMBDA_TYPE) {
                                auto params = function.as_object()->lambda()->args;
                                auto shadowed_env = function.as_object()->lambda()->env;
                                if (!apply_arguments(shadowed_env, params, args)) {
                                        pretty_print(function);
                                        abort();
                                }
                                auto body = function.as_object()->lambda()->body;
                                while (rest(body) != LISP_NIL) {
                                        evaluate(shadowed_env, first(body));
                                        body = rest(body);
                                }
                                env = shadowed_env;
                                obj = first(body);
                                goto tailcall;
                                //return result;
                        }
                }
        }
        if (obj.is_character()) {
                return obj;
        }
        if (obj.is_type(SYM_TYPE)) {
                if (obj == LISP_T) return obj;
                auto val = symbol_lookup(env, obj);
                if (val.is_invalid()) {
                        printf("UNBOUND VARIABLE: %s\n", obj.as_object()->symbol()->c_str()); // @TODO: POOPER ERROR HANDLING
                        abort();
                }
                return cdr(val);
        }
        if (obj.is_object()) {
                return obj;
        }
        return lisp_value::invalid_object();
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
        if (obj.is_cons()) {
                while (obj != LISP_NIL) {
                        i += 1;
                        obj = cdr(obj);
                }
        }
        return i;
}

lisp_value lisp::macro_expand(lisp_value obj)
{
        if (!obj.is_cons()) {
                return obj;
        }
        auto car = first(obj);
        if (car.is_type(SYM_TYPE)) {
                if (car == LISP_SYM_QUOTE) {
                        return obj;
                }
                if (car == LISP_SYM_IF) {
                        auto condition = macro_expand(second(obj));
                        auto consequence = macro_expand(third(obj));
                        auto alternative = macro_expand(fourth(obj));
                        return list(LISP_SYM_IF, condition, consequence, alternative);
                }
                if (car == LISP_SYM_DEFMACRO) {
                        auto macro_name = second(obj);
                        auto params_list = third(obj);
                        auto body = map(cdddr(obj), macro_expand);
                        return cons(LISP_SYM_DEFMACRO, cons(macro_name, cons(params_list, body)));
                }
                if (car == LISP_SYM_LAMBDA) {
                        auto args = second(obj);
                        auto body = map(cddr(obj), macro_expand);
                        return cons(LISP_SYM_LAMBDA, cons(args, body));
                }
                if (car == LISP_SYM_SETQ) {
                        auto variable_name = second(obj);
                        auto value = macro_expand(third(obj));
                        return list(LISP_SYM_SETQ, variable_name, value);
                }
                auto &sym_name = *car.as_object()->symbol();
                auto it = LISP_MACROS.find(sym_name);
                if (it != LISP_MACROS.end()) {
                        auto function = it->second;
                        auto args = rest(obj);
                        return macro_expand(apply(LISP_BASE_ENVIRONMENT, function, args));
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
        INTERN_GLOBAL(STRING);
        INTERN_GLOBAL(NULL);
        INTERN_GLOBAL(BOOLEAN);
        INTERN_GLOBAL(QUASIQUOTE);
        INTERN_GLOBAL(UNQUOTE);
        INTERN_GLOBAL(ARRAY);

        LISP_SYM_AMP_REST = intern_symbol("&REST");
        LISP_SYM_AMP_BODY = intern_symbol("&BODY");
        LISP_SYM_AMP_OPTIONAL = intern_symbol("&OPTIONAL");
        
        LISP_SYM_UNQUOTESPLICING = intern_symbol("UNQUOTE-SPLICING");
        LISP_SYM_SIMPLE_ARRAY = intern_symbol("SIMPLE-ARRAY");

        LISP_BASE_ENVIRONMENT = LISP_NIL;
        primitives::bind_primitives(LISP_BASE_ENVIRONMENT);
}

bool lisp::read_stdin(const char *prompt_top_level, const char *prompt_continued, lisp_value &out_value)
{
        static lisp_string_stream stream;
        if (stream.eof()) {
                char *input = readline(prompt_top_level);
                if (!input) return false;
                stream.clear();
                stream.append(input);
                stream.append('\n');
                add_history(input);
                free(input);
        }
        auto idx = stream.index();
        lisp_value obj = parse(stream);
        while (obj.is_invalid()) {
                char *continued = readline(prompt_continued);
                if (!continued) break;
                stream.append(continued);
                stream.append('\n');
                add_history(continued);
                free(continued);
                stream.index(idx);
                obj = parse(stream);
        }
        if (obj.is_invalid()) {
                stream.clear();
                return false;
        }
        /* readline doesn't return a string with a line terminator so we end up
           appending one ourselves, this makes it so (+\n1\n2\n) isn't parsed as
           (+12). This means upon a successful parse there should be some form of
           trailing whitespace and we need to discard that whitespace to future
           calls to this function will display prompt_top_level correctly.
        */
        consume_whitespace(stream);
        out_value = obj;
        return true;
}

void eval_fstream(const std::filesystem::path filepath, lisp_stream &stm)
{
        auto here_path = std::filesystem::current_path();
        auto there_path = filepath.parent_path();
        if (there_path != "") {
                std::filesystem::current_path(there_path);
        }

        auto variable_name = intern_symbol("*FILE-PATH*");
        auto value = lisp_obj::create_string(filepath);
        auto place = symbol_lookup(LISP_BASE_ENVIRONMENT, variable_name);
        if (place.is_invalid()) {
                push(cons(variable_name, value), LISP_BASE_ENVIRONMENT);
        }
        else {
                set_cdr(place, value);
        }
        while (!stm.eof()) {
                lisp_value obj = parse(stm);
                if (obj.is_invalid()) {
                        consume_whitespace(stm);
                        if (stm.eof()) break;
                        abort();
                }
                lisp_value expanded = macro_expand(obj);
                evaluate(LISP_BASE_ENVIRONMENT, expanded);
                consume_whitespace(stm);
        }
        std::filesystem::current_path(here_path);
}

int main(int argc, char *argv[])
{
        bool repl = false;
        std::vector<std::string> file_paths;

        for (int i = 1; i < argc; ++i) {
                const char *arg = argv[i];
                if (strcmp("-i", arg) == 0) {
                        repl = true;
                }
                else {
                        file_paths.push_back(arg);
                }
        }

        if (!repl && file_paths.size() == 0)
                repl = true;

        std::vector<lisp_file_stream*> fstreams;

        {
                auto exe_dir = plat::get_executable_path().parent_path();
                auto boot_path = exe_dir/"lisp"/"boot.lisp";
                auto f = new lisp_file_stream();
                f->open(boot_path, lisp_file_stream::io_mode::read);
                if (f->ok()) {
                        fstreams.push_back(f);
                }
                else {
                        fprintf(stderr, "boot.lisp inaccessible at %s\n", boot_path.c_str());
                        return -1;
                }
        }
        
        for (auto &path : file_paths) {
                auto f = new lisp_file_stream();
                f->open(std::filesystem::absolute(path), lisp_file_stream::io_mode::read);
                if (f->ok()) {
                        fstreams.push_back(f);
                }
                else {
                        fprintf(stderr, "File not accessible: %s\n", path.c_str());
                        return -1;
                }
        }

        initialize_globals();
        for (auto fs : fstreams) {
                eval_fstream(fs->path(), *fs);
                fs->close();
                delete fs;
        }

        if (repl) {
                static const char *prompt_lisp = "lisp_nasa> ";
                static const char *prompt_ws   = ".......... ";
                int result_counter = 0;
                auto last_result_sym = intern_symbol("$$");
                while (1) {
                        lisp_value parsed;
                        if (!read_stdin(prompt_lisp, prompt_ws, parsed)) {
                                break;
                        }
                        lisp_value expanded = macro_expand(parsed);
                        lisp_value result = evaluate(LISP_BASE_ENVIRONMENT, expanded);
                        std::string result_sym_name = "$$" + std::to_string(result_counter++);
                        //bind(LISP_BASE_ENVIRONMENT, intern_symbol(result_sym_name), result);
                        //bind(LISP_BASE_ENVIRONMENT, last_result_sym, result);
                        printf("%5s => ", result_sym_name.c_str());
                        pretty_print(result);
                }
        }
        return 0;
}
