#include <iostream>
#include <iomanip>
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
#include "backtrace.hpp"

using namespace lisp;

static
struct lisp_cons_pool {
    lisp_cons_pool()
        : m_pool_index(0)
        , m_cons_index(0)
    {
        m_pools.reserve(10000);
        m_current_pool = new lisp_cons[pool_size];
        m_pools.push_back(m_current_pool);
    }

    lisp_cons *pop()
    {
        if (m_cons_index >= pool_size) {
            m_pool_index++;
            m_current_pool = new lisp_cons[pool_size];
            m_pools.push_back(m_current_pool);
            m_cons_index = 0;
        }
        return &m_current_pool[m_cons_index++];
    }

  private:
    static constexpr size_t pool_size = 10000; // number of conses to allocate per pool
    size_t m_pool_index; // index of current pools
    size_t m_cons_index; // index next cons
    lisp_cons *m_current_pool;
    std::vector<lisp_cons*> m_pools;

} cons_pool;

lisp_cons *lisp::cons_pool_pop()
{
    return cons_pool.pop();
}

struct lisp_signal_handler {
    lisp_value tag;
    lisp_lambda lambda;
};

struct lisp_signal_handler_cases {
    std::vector<lisp_signal_handler> handlers;
};

// A stack of handler cases; the last element of this is the most recent HANDLER-CASE
static
std::vector<lisp_signal_handler_cases> LISP_SIGNAL_HANDLER_CASES;

struct lisp_exception {
    lisp_value what;
};
struct lisp_unhandleable_exception : lisp_exception {
    const char *msg;
};
struct lisp_signal_exception : lisp_exception {
    size_t popped;
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

    inline
    std::string substr(size_t start, size_t end)
    {
        return m_data.substr(start, end);
    }

  private:
    size_t m_index;
    std::string m_data;
};

std::unordered_map<std::string, lisp_value> LISP_MACROS;
namespace lisp {
lisp_value LISP_BASE_ENVIRONMENT;

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
lisp_value LISP_SYM_FUNCALL;
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
lisp_value LISP_SYM_HANDLER_CASE;
lisp_value LISP_SYM_FILE_STREAM;
}

lisp_value lisp::intern_symbol(const std::string &symbol_name)
{
    static std::unordered_map<std::string, lisp_value> interned_symbols;
    auto it = interned_symbols.find(symbol_name);
    if (it != interned_symbols.end())
        return it->second;
    auto symbol = lisp_obj::create_symbol(symbol_name);
    symbol.as_object()->symbol()->interned = true;
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
        if (cdr(obj).is_nil()) {
            /* List with one element */
            ;
        }
        else if (cdr(obj).is_cons()) {
            lisp_value current = cdr(obj);
            while (current.is_not_nil()) {
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
    else if (obj.is_object()) {
        switch (obj.as_object()->type()) {
            case SYM_TYPE:
                if (obj.as_object()->symbol()->interned == false) {
                    ss << "#0x" << std::hex << reinterpret_cast<uintptr_t>(obj.as_object()) << ":"
                       << obj.as_object()->symbol()->name;
                    result = ss.str();
                }
                else {
                    result = obj.as_object()->symbol()->name;
                }
                break;
            case LAMBDA_TYPE: {
                result += "(LAMBDA ";
                if (obj.as_object()->lambda()->params.is_nil()) {
                    result += "() ";
                }
                else {
                    result += repr(obj.as_object()->lambda()->params);
                    result += " ";
                }
                auto body = obj.as_object()->lambda()->body;
                while (body.is_not_nil()) {
                    result += repr_impl(car(body), seen);
                    body = cdr(body);
                }
                result += ")";
            } break;
            case SIMPLE_ARRAY_TYPE: {
                auto array = obj.as_object()->simple_array();
                if (array->type() == LISP_SYM_CHARACTER) {
                    result += '"';
                    for (size_t i = 0; i < array->length(); ++i) {
                        auto c = array->get(i).as_character();
                        if (c == '"') {
                            result += "\\\"";
                        }
                        else {
                            result += reinterpret_cast<char*>(&c);
                        }
                    }
                    result += '"';
                }
                else {
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
                }
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

static FORCE_INLINE
bool is_whitespace(int c)
{
    return (c == ' ' || c == '\n' || c == '\t');
}

static FORCE_INLINE
bool is_digit(int c)
{
    return (c >= '0' && c <= '9');
}

static FORCE_INLINE
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

static FORCE_INLINE
bool is_symbol_char(int c)
{
    return is_symbol_start_char(c) || is_digit(c);
}

static FORCE_INLINE
std::string str_lower(std::string in)
{
    std::transform(in.begin(), in.end(), in.begin(), [](unsigned char c){ return std::tolower(c) ; } );
    return in;
}

static FORCE_INLINE
std::string str_upper(std::string in)
{
    std::transform(in.begin(), in.end(), in.begin(), [](unsigned char c){ return std::toupper(c) ; } );
    return in;
}

static FORCE_INLINE
void consume_whitespace(lisp_stream &stream)
{
    while (is_whitespace(stream.peekc()))
        stream.getc();
}

lisp_value lisp::parse(lisp_stream &stream)
{
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

            bool is_number = !is_symbol_char(stream.peekc());
            while (is_symbol_char(stream.peekc())) {
                number_str += stream.getc();
            }

            if (is_number) {
                int64_t result = 0;
                for (char digit : number_str)
                {
                    result *= 10;
                    result += digit - '0';
                }
                return lisp_value::wrap_fixnum(result);
            }
            else {
                return intern_symbol(number_str);
            }
        }
        else if (stream.peekc() == '#') {
            stream.getc();
            if (stream.peekc() == '\'') {
                stream.getc();
                auto func = parse(stream);
                return list(LISP_SYM_FUNCTION, func);
            }
            else if (stream.peekc() == '\\') {
                stream.getc();
                std::string character;
                while (is_symbol_char(stream.peekc())) {
                    character += stream.getc();
                }
                if (character.size() == 0) {
                    character += stream.getc();
                }
                if (character.size() == 1) {
                    return lisp_value::wrap_character(character[0]);
                }
                else  {
                    character = str_upper(character);
                    if (character == "SPACE") {
                        return lisp_value::wrap_character(' ');
                    }
                    else if (character == "RETURN") {
                        return lisp_value::wrap_character('\r');
                    }
                    else if (character == "NEWLINE") {
                        return lisp_value::wrap_character('\n');
                    }
                    else if (character == "TAB") {
                        return lisp_value::wrap_character('\t');
                    }
                    else {
                        auto it = *reinterpret_cast<const int32_t*>(character.c_str());
                        return lisp_value::wrap_character(it);
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
        else if (stream.peekc() == ')') {
            break;
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



struct lisp_vm_state {
    lisp_vm_state()
    {
        param_stack_bottom = new lisp_value[10000];
        param_stack_top = param_stack_bottom;
    }
    lisp_value *param_stack_bottom;
    lisp_value *param_stack_top;
};

static void vm_execute(lisp_vm_state &state, lisp_value env, const std::vector<uint8_t> &bytecode);

static FORCE_INLINE
lisp_value bind(lisp_value env, lisp_value symbol, lisp_value value)
{
    auto tmp = symbol_lookup(env, symbol);
    if (tmp.is_nil()) {
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
    while (params.is_not_nil() && args.is_not_nil()) {
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

    if (params.is_not_nil()) {
        if (first(params) == LISP_SYM_AMP_REST || first(params) == LISP_SYM_AMP_BODY) {
            shadowed_env = shadow(shadowed_env, second(params), LISP_NIL);
            return true;
        }
        else if (first(params) == LISP_SYM_AMP_OPTIONAL) {
            allowing_optional = true;
            params = rest(params);
        }

        if (allowing_optional) {
            while (params.is_not_nil()) {
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
    return params.is_nil();
}

//static inline
//void try_handle_lisp_signal(lisp_value signal_tag, lisp_value signal_args)
//{
//        bool found_handler = false;
//        lisp_signal_handler handler;
//        size_t npop = 0;
//        if (!LISP_SIGNAL_HANDLER_CASES.empty()) {
//                for (auto it = LISP_SIGNAL_HANDLER_CASES.rbegin();
//                     it != LISP_SIGNAL_HANDLER_CASES.rend();
//                     ++it) {
//                        auto &handlers = it->handlers;
//                        for (auto &h : handlers) {
//                                if (h.tag == LISP_T || h.tag == signal_tag) {
//                                        found_handler = true;
//                                        handler = h;
//                                        break;
//                                }
//                        }
//                        npop++;
//                        if (found_handler)
//                                break;
//                }
//        }
//        if (found_handler) {
//
//                {
//                        auto to_pop = std::min(npop, LISP_SIGNAL_HANDLER_CASES.size());
//                        if (to_pop != npop) {
//                                fprintf(stderr, "WARNING: over-popping signal handlers!\n");
//                        }
//                        LISP_SIGNAL_HANDLER_CASES.resize(LISP_SIGNAL_HANDLER_CASES.size() - to_pop);
//                }
//
//                auto params = handler.lambda.params;
//                if (handler.tag == LISP_T) {
//                        signal_args = cons(signal_tag, signal_args);
//                }
//                auto shadowed_env = handler.lambda.env;
//                if (!apply_arguments(shadowed_env, params, signal_args)) {
//                        throw lisp_unhandleable_exception{ cons(signal_tag, signal_args), "Unable to APPLY arguments: " };
//                }
//                auto result = LISP_NIL;
//                auto body = handler.lambda.body;
//                while (body.is_not_nil()) {
//                        result = evaluate(shadowed_env, first(body));
//                        body = rest(body);
//                }
//                throw lisp_signal_exception{ result, npop };
//        }
//        else {
//                throw lisp_unhandleable_exception{ cons(signal_tag, signal_args), "Unhandled SIGNAL: " };
//        }
//}


lisp_value lisp::apply(lisp_value env, lisp_value function, lisp_value args)
{
    if (function.is_lisp_primitive()) {
        bool raised_signal = false;
        auto result = function.as_lisp_primitive()(env, args, raised_signal);
        if (raised_signal) {
            fprintf(stderr, "signals nyi\n");
            abort();
            //try_handle_lisp_signal(car(result), cdr(result));
        }
        else {
            return result;
        }
    }
    if (function.is_type(LAMBDA_TYPE)) {
        lisp_vm_state vm;
        auto lambda = function.as_object()->lambda();
        auto shadowed = lambda->env;
        apply_arguments(shadowed, lambda->params, args);
        vm_execute(vm, shadowed, lambda->bytecode);
        return *(vm.param_stack_top-1);
    }
    throw lisp_unhandleable_exception{ function, "Cannot APPLY because not a FUNCTION: " };
}



static FORCE_INLINE
bool is_callable(lisp_value val)
{
    return val.is_lisp_primitive() || val.is_type(LAMBDA_TYPE);
}

static
lisp_value map(lisp_value list, lisp_value (func)(lisp_value))
{
    if (list.is_nil())
        return list;

    auto head = cons(func(car(list)), LISP_NIL);
    auto current = head;
    list = cdr(list);
    while (list.is_not_nil()) {
        set_cdr(current, cons(func(car(list)), LISP_NIL));
        current = cdr(current);
        list = cdr(list);
    }
    return head;
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
        const auto &sym_name = car.as_object()->symbol()->name;
        auto it = LISP_MACROS.find(sym_name);
        if (it != LISP_MACROS.end()) {
            auto function = it->second;
            auto args = rest(obj);
            return macro_expand(apply(LISP_BASE_ENVIRONMENT, function, args));
        }
    }
    return map(obj, macro_expand);
}


enum class bytecode_op : uint8_t {
#define BYTECODE_DEF(name, opcode, noperands, nargs, docstring) op_ ## name = opcode,
#include "bytecode.def"
};

struct bytecode_emitter {
    
    void emit_push_value(lisp_value val);
    void emit_push_nil();
    void emit_push_fixnum_0();
    void emit_push_fixnum_1();
    void emit_funcall(uint32_t how_many);
    void emit_funcall(lisp_value what, uint32_t how_many);
    void emit_return();
    
    // These branch emitters return the bytecode offset of their 'where' component,
    // this is useful for backfilling labels upon their location discovery
    size_t emit_goto(uint32_t where);
    size_t emit_goto();
    size_t emit_pop_jump_if_nil(uint32_t where);
    size_t emit_pop_jump_if_nil();
    
    void emit_get_value(lisp_value symbol);
    void emit_set_value();
    void emit_set_value(lisp_value symbol, lisp_value val);
    
    void emit_function_value(lisp_value symbol);
    void emit_define_function(lisp_value symbol, lisp_value val);
    void emit_define_function();

    void emit_pop();
    
    void emit_instantiate_lambda(lisp_value lambda);
    void emit_instantiate_lambda();
    
    void set_raw_8(size_t offset, uint8_t v);
    void set_raw_16(size_t offset, uint16_t v);
    void set_raw_32(size_t offset, uint32_t v);
    void set_raw_lisp_value(size_t offset, lisp_value v);

    size_t position() const;
    std::vector<uint8_t> bytecode() const;
    std::vector<uint8_t> &&move_bytecode();
  private:
    template<typename T>
    void append(T val)
    {
        m_bytecode.resize(m_bytecode.size() + sizeof(T), 0xCC);
        auto end = m_bytecode.data() + m_bytecode.size();
        auto slot = reinterpret_cast<T*>(end - sizeof(T));
        *slot = val;
    }

    template<typename T>
    void set_raw(size_t offset, T val)
    {
        assert(offset + sizeof(val) - 1 < m_bytecode.size());
        auto slot = reinterpret_cast<decltype(val)*>(m_bytecode.data() + offset);
        *slot = val;
    }
    std::vector<uint8_t> m_bytecode;
};

template<>
void bytecode_emitter::append(bytecode_op val)
{
    m_bytecode.push_back(static_cast<uint8_t>(val));
}

template<>
void bytecode_emitter::append(lisp_value val)
{
    append(val.bits());
}

std::vector<uint8_t> &&bytecode_emitter::move_bytecode()
{
    return std::move(m_bytecode);
}

void bytecode_emitter::set_raw_8(size_t offset, uint8_t v)
{
    set_raw<decltype(v)>(offset, v);
}

void bytecode_emitter::set_raw_16(size_t offset, uint16_t v)
{
    set_raw<decltype(v)>(offset, v);
}

void bytecode_emitter::set_raw_32(size_t offset, uint32_t v)
{
    set_raw<decltype(v)>(offset, v);
}

void bytecode_emitter::set_raw_lisp_value(size_t offset, lisp_value v)
{
    set_raw<decltype(v)>(offset, v);
}

size_t bytecode_emitter::position() const
{
    return m_bytecode.size();
}

std::vector<uint8_t> bytecode_emitter::bytecode() const
{
    return m_bytecode;
}

void bytecode_emitter::emit_push_value(lisp_value val)
{
    if (val.is_nil()) {
        emit_push_nil();
    }
    else if (val == lisp_value::wrap_fixnum(0)) {
        emit_push_fixnum_0();
    }
    else if (val == lisp_value::wrap_fixnum(1)) {
        emit_push_fixnum_1();
    }
    else {
        append(bytecode_op::op_push_value);
        append(val);
    }
}

void bytecode_emitter::emit_push_nil() 
{
    append(bytecode_op::op_push_nil);
}

void bytecode_emitter::emit_push_fixnum_0() 
{
    append(bytecode_op::op_push_fixnum_0);
}

void bytecode_emitter::emit_push_fixnum_1() 
{
    append(bytecode_op::op_push_fixnum_1);
}


void bytecode_emitter::emit_funcall(uint32_t how_many) 
{
    append(bytecode_op::op_funcall);
    append(how_many);
}
void bytecode_emitter::emit_funcall(lisp_value what, uint32_t how_many)
{
    emit_push_value(what);
    emit_funcall(how_many);
}
void bytecode_emitter::emit_return()
{
    append(bytecode_op::op_return);
}

size_t bytecode_emitter::emit_goto(uint32_t where)
{
    append(bytecode_op::op_goto);
    auto here = position();
    append(where);
    return here;
}
size_t bytecode_emitter::emit_goto()
{
    return emit_goto(0xdeadbeef);
}
size_t bytecode_emitter::emit_pop_jump_if_nil(uint32_t where)
{
    append(bytecode_op::op_pop_jump_if_nil);
    auto here = position();
    append(where);
    return here;
}
size_t bytecode_emitter::emit_pop_jump_if_nil()
{
    return emit_pop_jump_if_nil(0xdeadbeef);
}

void bytecode_emitter::emit_get_value(lisp_value symbol)
{
    append(bytecode_op::op_get_value);
    append(symbol);
}
void bytecode_emitter::emit_set_value()
{
    append(bytecode_op::op_set_value);
}
void bytecode_emitter::emit_set_value(lisp_value symbol, lisp_value val)
{
    emit_push_value(val);
    emit_push_value(symbol);
    emit_set_value();
}

void bytecode_emitter::emit_function_value(lisp_value symbol)
{
    append(bytecode_op::op_function_value);
    append(symbol);
}
void bytecode_emitter::emit_define_function()
{
    append(bytecode_op::op_define_function);
}
void bytecode_emitter::emit_define_function(lisp_value symbol, lisp_value val)
{
    emit_push_value(val);
    emit_push_value(symbol);
    emit_define_function();
}

void bytecode_emitter::emit_pop()
{
    append(bytecode_op::op_pop);
}

void bytecode_emitter::emit_instantiate_lambda()
{
    append(bytecode_op::op_instantiate_lambda);
}

void bytecode_emitter::emit_instantiate_lambda(lisp_value lambda)
{
    emit_push_value(lambda);
    emit_instantiate_lambda();
}

void pewpewpew()
{
    printf("         NAME            |  OPCODE  | OPERANDS |   ARGS   |\n");
    printf("-------------------------|----------|----------|----------|\n");
#define BYTECODE_DEF(name, opcode, noperands, nargs, docstring)         \
    printf("%24s |      0x%02X|%10d|%10d|\n", #name, opcode, noperands, nargs);
#include "bytecode.def"

#define BYTECODE_DEF(name, opcode, noperands, nargs, docstring) \
    printf("\n\n=======\n%s\n%s", #name, docstring);
#include "bytecode.def"
}

static
void put_bytes(std::ostream &out, const uint8_t *bytes, size_t offset, size_t nbytes, size_t min_width=10*3)
{
    size_t column = 0;
    for (size_t i = 0; i < nbytes; ++i) {
        out << std::setfill('0') << std::setw(2) << std::hex << (int)bytes[offset+i] << ' ';
        column += 3;
    }
    
    for (; column < min_width; ++column) {
        out << ' ';
    }
}

static
size_t disassemble1(std::ostream &out, size_t ip, const uint8_t *code, bool here)
{
    auto opcode = static_cast<bytecode_op>(code[ip]);
    if (here) {
        out << ">> ";
    }
    else {
        out << "   ";
    }
    out << std::setfill('0') << std::setw(8) << std::hex << ip << std::setfill(' ') << "  ";
    switch (opcode) {
        default: {
            put_bytes(out, code, ip, 1);
            out << "??";
            ip += 1;
        } break;
        case bytecode_op::op_funcall: {
            auto addr = *reinterpret_cast<const uint32_t*>(code+ip+1);
            put_bytes(out, code, ip, 1 + sizeof(addr));
            out << "FUNCALL " << addr;
            ip += 1 + sizeof(addr);
        } break;
        case bytecode_op::op_return: {
            put_bytes(out, code, ip, 1);
            out << "RETURN";
            ip += 1;
        } break;

        case bytecode_op::op_goto: {
            auto addr = *reinterpret_cast<const uint32_t*>(code+ip+1);
            put_bytes(out, code, ip, 1 + sizeof(addr));
            out << "GOTO " << addr;
            ip += 1 + sizeof(addr);
        } break;

        case bytecode_op::op_pop_jump_if_nil: {
            auto addr = *reinterpret_cast<const uint32_t*>(code+ip+1);
            put_bytes(out, code, ip, 1 + sizeof(addr));
            out << "POP_JUMP_IF_NIL " << addr;
            ip += 1 + sizeof(addr);
        } break;

        case bytecode_op::op_get_value: {
            auto sym = *reinterpret_cast<const lisp_value*>(code+ip+1);
            put_bytes(out, code, ip, 1 + sizeof(sym));
            out << "GET_VALUE " << sym.bits();
            out << "  [" << repr(sym) << "]";
            ip += 1 + sizeof(sym);
        } break;

        case bytecode_op::op_set_value: {
            put_bytes(out, code, ip, 1);
            out << "SET_VALUE";
            ip += 1;
        } break;

        case bytecode_op::op_function_value: {
            auto obj = *reinterpret_cast<const lisp_value*>(code+ip+1);
            put_bytes(out, code, ip, 1 + sizeof(obj));
            out << "FUNCTION_VALUE " << obj.bits();
            out << "  (" << repr(obj) << ")";
            ip += 1 + sizeof(obj);
        } break;

        case bytecode_op::op_define_function: {
            put_bytes(out, code, ip, 1);
            out << "DEFINE_FUNCTION";
            ip += 1;
        } break;
            
        case bytecode_op::op_pop: {
            put_bytes(out, code, ip, 1);
            out << "POP";
            ip += 1;
        } break;

        case bytecode_op::op_push_value: {
            auto obj = *reinterpret_cast<const lisp_value*>(code+ip+1);
            put_bytes(out, code, ip, 1 + sizeof(obj));
            out << "PUSH_VALUE " << obj.bits();
            auto obj_repr = repr(obj);
            const int n = 25;
            if (obj_repr.size() < n) {
                out << "  [" << obj_repr << "]";
            }
            else {
                out << "  [" << obj_repr.substr(0, n-3) << "... ]";
            }
            ip += 1 + sizeof(obj);
        } break;

        case bytecode_op::op_push_nil: {
            put_bytes(out, code, ip, 1);
            out << "PUSH NIL";
            ip += 1;
        } break;

        case bytecode_op::op_push_fixnum_0: {
            put_bytes(out, code, ip, 1);
            out << "PUSH 0";
            ip += 1;
        } break;

        case bytecode_op::op_push_fixnum_1: {
            put_bytes(out, code, ip, 1);
            out << "PUSH 1";
            ip += 1;
        } break;

        case bytecode_op::op_instantiate_lambda: {
            put_bytes(out, code, ip, 1);
            out << "INSTANTIATE_LAMBDA";
            ip += 1;
        }
    }
    out << '\n';
    return ip;
}

static
void disassemble(std::ostream &out, const std::string &tag, const std::vector<uint8_t> &bytecode)
{
    out << "Disassembly for \"" << tag << "\"\n";
    for (size_t ip = 0; ip < bytecode.size();) {
        ip = disassemble1(out, ip, bytecode.data(), false);
    }
}

static
void disassemble(std::ostream &out, const std::string &tag, const std::vector<uint8_t> &bytecode, size_t show_here)
{
    out << "Disassembly for \"" << tag << "\"\n";
    for (size_t ip = 0; ip < bytecode.size();) {
        ip = disassemble1(out, ip, bytecode.data(), ip == show_here);
    }
}


static
void vm_execute(lisp_vm_state &state, lisp_value env, const std::vector<uint8_t> &bytecode)
{
    size_t ip = 0;
    auto code = bytecode.data();
    auto p = state.param_stack_top;
    
#define push_param(val) (*p++) = (val)
#define pop_param() (*--p)
#define param_top() (*(p-1))

    while (ip < bytecode.size()) {
        auto opcode = static_cast<bytecode_op>(code[ip]);
        switch (opcode) {
            case bytecode_op::op_funcall: {
                auto func = pop_param();
                auto nargs = *reinterpret_cast<const uint32_t*>(code+ip+1);
                auto args = LISP_NIL;
                for (size_t i = 0; i < nargs; ++i) {
                    auto arg = pop_param();
                    args = cons(arg, args);
                }
                if (func.is_type(SYM_TYPE)) {
                    func = func.as_object()->symbol()->function;
                }
                if (func.is_lisp_primitive()) {
                    bool raised_signal = false;
                    auto result = func.as_lisp_primitive()(env, args, raised_signal);
                    if (raised_signal) {
                        fprintf(stderr, "signals nyi\n");
                        abort();
                    }
                    push_param(result);
                    ip += 1 + sizeof(nargs);
                    break;
                }
                if (func.is_type(LAMBDA_TYPE)) {
                    auto lambda = func.as_object()->lambda();
                    auto shadowed = lambda->env;
                    apply_arguments(shadowed, lambda->params, args);
                    state.param_stack_top = p;
                    vm_execute(state, shadowed, lambda->bytecode);
                    p = state.param_stack_top;
                    ip += 1 + sizeof(nargs);
                    break;
                }
                printf("not a callable: ");
                pretty_print(func);
                goto error_and_abort;
            } break;

            case bytecode_op::op_return: {
                goto done;
            } break;

            case bytecode_op::op_goto: {
                auto addr = *reinterpret_cast<const uint32_t*>(code+ip+1);
                ip = addr;
            } break;

            case bytecode_op::op_pop_jump_if_nil: {
                p -= 1;
                if (p->is_nil()) {
                    auto addr = *reinterpret_cast<const uint32_t*>(code+ip+1);
                    ip = addr;
                }
                else {
                    ip += 5;
                }
            } break;

            case bytecode_op::op_get_value: {
                auto sym = *reinterpret_cast<const lisp_value*>(code+ip+1);
                if (sym == LISP_T) push_param(sym);
                else {
                    auto val = symbol_lookup(env, sym);
                    if (val.is_nil()) {
                        throw lisp_unhandleable_exception{ sym, "Unbound variable: " };
                    }
                    push_param(cdr(val));
                }
                ip += 1 + sizeof(sym);
            } break;

            case bytecode_op::op_set_value: {
                auto sym = pop_param();
                auto val = param_top();
                auto place = symbol_lookup(env, sym);
                if (place.is_nil()) {
                    push(cons(sym, val), LISP_BASE_ENVIRONMENT);
                }
                else {
                    set_cdr(place, val);
                }
                ip += 1;
            } break;

            case bytecode_op::op_function_value: {
                auto obj = *reinterpret_cast<const lisp_value*>(code+ip+1);
                if (obj.is_type(SYM_TYPE)) {
                    push_param(obj.as_object()->symbol()->function);
                }
                else {
                    push_param(obj);
                }
                ip += 1 + sizeof(obj);
            } break;

            case bytecode_op::op_define_function: {
                auto sym = *reinterpret_cast<const lisp_value*>(code+ip+1);
                fprintf(stderr, "op_define_function NYI\n"); abort();
                ip += 1 + sizeof(sym);
            } break;

            case bytecode_op::op_pop: {
                pop_param();
                ip += 1;
            } break;

            case bytecode_op::op_push_value: {
                auto val = *reinterpret_cast<const lisp_value*>(code+ip+1);
                push_param(val);
                ip += 1 + sizeof(val);
            } break;

            case bytecode_op::op_push_nil: {
                push_param(LISP_NIL);
                ip += 1;
            } break;

            case bytecode_op::op_push_fixnum_0: {
                push_param(lisp_value::wrap_fixnum(0));
                ip += 1;
            } break;

            case bytecode_op::op_push_fixnum_1: {
                push_param(lisp_value::wrap_fixnum(1));
                ip += 1;
            } break;
            case bytecode_op::op_instantiate_lambda: {
                auto obj = pop_param();
                auto lambda_copy = obj.as_object()->lambda()->copy();
                lambda_copy->env = env;
                push_param(lisp_obj::create_lambda(lambda_copy));
                ip += 1;
            } break;
        }
    }
    
    done:
    state.param_stack_top = p;
    return;
    error_and_abort:
    std::cout << "IP @ " << std::hex << ip << '\n';
    disassemble(std::cout, "ERROR", bytecode, ip);
    abort();
}

lisp_value lisp_prim_get_num_handlers(lisp_value, lisp_value, bool &)
{
    return lisp_value::wrap_fixnum(LISP_SIGNAL_HANDLER_CASES.size());
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
    INTERN_GLOBAL(FUNCALL);
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

    LISP_SYM_HANDLER_CASE = intern_symbol("HANDLER-CASE");
    LISP_SYM_FILE_STREAM = intern_symbol("FILE-STREAM");

    LISP_BASE_ENVIRONMENT = LISP_NIL;
    primitives::bind_primitives(LISP_BASE_ENVIRONMENT);

    {
        intern_symbol("%%-INTERNAL-GET-NUM-CASE-HANDLERS")
            .as_object()
            ->symbol()
            ->function = lisp_value::wrap_primitive(lisp_prim_get_num_handlers);
    }
}

bool lisp::read_stdin(const char *prompt_top_level, const char *prompt_continued, lisp_value &out_value, std::string *out_input)
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
    if (out_input) {
        *out_input = stream.substr(idx, stream.index());
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


void compile(bytecode_emitter &e, lisp_value expr) 
{
    if (expr.is_cons()) {
        auto thing = first(expr);
        if (thing == LISP_SYM_QUOTE) {
            e.emit_push_value(second(expr));
        }
        else if (thing == LISP_SYM_IF) {
            auto test = second(expr);
            auto consequence = third(expr);
            auto alternative = fourth(expr);
            compile(e, test);
            auto alt_offs = e.emit_pop_jump_if_nil();
            compile(e, consequence);
            auto out_offs = e.emit_goto();
            auto label_alt = e.position();
            compile(e, alternative);
            auto label_out = e.position();
            
            e.set_raw_32(out_offs, label_out);
            e.set_raw_32(alt_offs, label_alt);
        }
        else if (thing == LISP_SYM_DEFMACRO) {
            // nothing is emitted to e because there's no concept of macros existing at "runtime"
            auto name = second(expr);
            auto lambda_list = third(expr);
            auto body = cdddr(expr);
            bytecode_emitter lambda_body;
            while (cdr(body).is_not_nil()) {
                compile(lambda_body, car(body));
                lambda_body.emit_pop();
                body = cdr(body);
            }
            compile(lambda_body, car(body));
            lambda_body.emit_return();
            auto macro = lisp_obj::create_macro(LISP_BASE_ENVIRONMENT, lambda_list, body, lambda_body.move_bytecode());
            LISP_MACROS[name.as_object()->symbol()->name] = macro;
        }
        else if (thing == LISP_SYM_LAMBDA) {
            auto lambda_list = second(expr);
            auto body = cddr(expr);
            bytecode_emitter lambda_body;
            while (cdr(body).is_not_nil()) {
                compile(lambda_body, car(body));
                lambda_body.emit_pop();
                body = cdr(body);
            }
            compile(lambda_body, car(body));
            lambda_body.emit_return();
            auto lambda_template = lisp_obj::create_lambda(lambda_list, body, lambda_body.move_bytecode());
            e.emit_instantiate_lambda(lambda_template);
        }
        else if (thing == LISP_SYM_SETQ) {
            compile(e, third(expr));
            e.emit_push_value(second(expr));
            e.emit_set_value();
        }
        else if (thing == LISP_SYM_HANDLER_CASE) {
        }
        else if (thing == LISP_SYM_FUNCTION) {
            auto thing = second(expr);
            if (thing.is_cons()) {
                if (first(thing) == LISP_SYM_LAMBDA) {
                    compile(e, thing);
                }
                else {
                    // ???
                }
            }
            else {
                e.emit_function_value(second(expr));
            }
        }
        else if (thing == LISP_SYM_FUNCALL) {
            auto func = second(expr);
            auto args = cddr(expr);
            uint32_t nargs = 0;
            while (args.is_not_nil()) {
                compile(e, car(args));
                nargs++;
                args = cdr(args);
            }
            if (func.is_type(SYM_TYPE)) {
                e.emit_get_value(func);
                e.emit_funcall(nargs);
            }
            else {
                e.emit_funcall(func, nargs);
            }
        }
        else {
            auto func = first(expr);
            auto args = rest(expr);
            uint32_t nargs = 0;
            while (args.is_not_nil()) {
                compile(e, car(args));
                nargs++;
                args = cdr(args);
            }
            if (func.is_cons() && first(func) == LISP_SYM_LAMBDA) {
                compile(e, func);
                e.emit_funcall(nargs);
            }
            else {
                e.emit_funcall(func, nargs);
            }
        }
    }
    else if (expr.is_type(SYM_TYPE)) {
        e.emit_get_value(expr);
    }
    else if (expr.is_invalid()) {
        fprintf(stderr, "WARNING: invalid object in compile stream.\n");
    }
    else {
        e.emit_push_value(expr);
    }
}


lisp_value lisp::evaluate(lisp_value env, lisp_value expr)
{
    lisp_value expanded = macro_expand(expr);
    bytecode_emitter emitter;
    compile(emitter, expanded);
    if (emitter.position() != 0) {
        lisp_vm_state vm;
        vm_execute(vm, env, emitter.move_bytecode());
        assert(vm.param_stack_top != vm.param_stack_bottom);
        vm.param_stack_top -= 1;
        return *vm.param_stack_top;
    }
    return LISP_NIL;
}

void eval_fstream(lisp_vm_state &vm, const std::filesystem::path filepath, lisp_stream &stm, bool show_disassembly)
{
    auto here_path = std::filesystem::current_path();
    auto there_path = filepath.parent_path();
    if (there_path != "") {
        std::filesystem::current_path(there_path);
    }

    auto variable_name = intern_symbol("*FILE-PATH*");
    auto value = lisp_obj::create_string(filepath);
    auto place = symbol_lookup(LISP_BASE_ENVIRONMENT, variable_name);
    if (place.is_nil()) {
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
        bytecode_emitter emitter;
        compile(emitter, expanded);
        if (emitter.position() != 0) {
            auto bytecode = emitter.move_bytecode();
            if (show_disassembly) {
                disassemble(std::cout, filepath, bytecode);
            }
            vm_execute(vm, LISP_BASE_ENVIRONMENT, bytecode);
            assert(vm.param_stack_top != vm.param_stack_bottom);
            vm.param_stack_top -= 1;
        }
        consume_whitespace(stm);
    }
    std::filesystem::current_path(here_path);
}

void repl_compile_and_execute(lisp_vm_state &vm, bool show_disassembly)
{
    static const char *prompt_lisp = "compile> ";
    static const char *prompt_ws   = "........ ";
    int result_counter = 0;
    while (1) {
        lisp_value parsed;
        std::string input;
        if (!read_stdin(prompt_lisp, prompt_ws, parsed, &input)) {
            break;
        }
        auto expanded = macro_expand(parsed);
        bytecode_emitter emitter;
        compile(emitter, expanded);
        if (emitter.position() != 0) {
            auto bytecode = emitter.move_bytecode();
            if (show_disassembly) {
                disassemble(std::cout, input, bytecode);
            }
            vm_execute(vm, LISP_BASE_ENVIRONMENT, bytecode);
            assert(vm.param_stack_top != vm.param_stack_bottom);
            vm.param_stack_top -= 1;
            auto result = *vm.param_stack_top;
            printf(" ==>");
            pretty_print(result);
        }
    }
}

int main(int argc, char *argv[])
{
    bool repl = false;
    bool use_boot = true;
    bool show_disassembly = false;
    std::vector<std::string> file_paths;

    for (int i = 1; i < argc; ++i) {
        const char *arg = argv[i];
        if (strcmp("-i", arg) == 0) {
            repl = true;
        }
        else if (strcmp("--no-boot", arg) == 0) {
            use_boot = false;
        }
        else if (strcmp("--boot", arg) == 0) {
            use_boot = true;
        }
        else if (strcmp("--disassemble", arg) == 0) {
            show_disassembly = true;
        }
        else {
            file_paths.push_back(arg);
        }
    }

    if (!repl && file_paths.size() == 0)
        repl = true;


    std::vector<lisp_file_stream*> fstreams;
    if (use_boot) {
        auto exe_dir = plat::get_executable_path().parent_path();
        auto boot_path = exe_dir/"lib"/"boot.lisp";
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
    lisp_vm_state vm;

    try {
        for (auto fs : fstreams) {
            eval_fstream(vm, fs->path(), *fs, show_disassembly);
            fs->close();
            delete fs;
        }
    }
    catch (lisp_unhandleable_exception e) {
        printf("%s\n", e.msg);
        if (e.what.is_not_nil()) {
            printf("    %s\n", repr(e.what).c_str());
        }
        return 1;
    }

    if (repl) {
        repl_compile_and_execute(vm, show_disassembly);
    }
    return 0;
}
