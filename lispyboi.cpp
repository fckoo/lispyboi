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


struct lisp_stream {
        static const int end_of_file = -1;
        virtual int getc() = 0;
        virtual int peekc(int n=0) const = 0;
};

struct lisp_string_stream : lisp_stream {
        lisp_string_stream()
                : m_index(0) {}

        lisp_string_stream(const std::string &data)
                : m_index(0)
                , m_data(data) {}

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

        void clear() {
                m_index = 0;
                m_data = "";
        }

        void append(const std::string &data) {
                m_data.append(data);
        }

        void append(const char *data) {
                m_data.append(data);
        }

        void append(char c) {
                m_data.push_back(c);
        }

        void puts() const {
                printf("%s\n", m_data.c_str() + m_index);
        }

        size_t index() const { return m_index; };
        void index(size_t idx) { m_index = idx; };

private:
        size_t m_index;
        std::string m_data;
};


enum LISP_OBJ_TYPE {
        SYM_TYPE = 0,
        CHAR_TYPE,
        CONS_TYPE,
        LAMBDA_TYPE,
        PRIMITIVE_FUNCTION_TYPE,
};

struct lisp_obj;

struct lisp_value {
        /*
          on 64-bit arch a pointer is 8 bytes so word-aligned addresses have the LSB(3) bits
          set to 0. We can exploit this by giving meaning to those bits.

          bits 210
          001 -> integer
          010 -> NIL
          110 -> TBD
          100 -> TBD
          000 -> pointer
        */
        static constexpr uint64_t BITS_MASK = 0b111;

        static constexpr uint64_t NIL_TAG     = 0b010;
        static constexpr uint64_t TBD1_TAG    = 0b110;
        static constexpr uint64_t TBD2_TAG    = 0b100;
        static constexpr uint64_t POINTER_TAG = 0b000;

        lisp_value()
        {
                u.bits = 0b010;
        }

        lisp_value(int64_t integer) 
        {
                u.integer_layout.tag = 1;
                u.integer_layout.number = integer;
        }

        lisp_value(lisp_obj *pointer) 
        {
                u.obj = pointer;
        }

        bool is_fixnum() const 
        {
                return u.integer_layout.tag != 0;
        }
        
        bool is_nil() const
        {
                return (u.bits & BITS_MASK) == NIL_TAG;
        }
        
        bool is_object() const 
        {
                return (u.bits & BITS_MASK) == POINTER_TAG;
        }
        
        int64_t as_fixnum() const
        {
                assert(is_fixnum());
                return u.integer_layout.number;
        }
        
        lisp_value as_nil() const
        {
                assert(is_nil());
                return *this;
        }
        
        lisp_obj *as_object() const 
        {
                assert(is_object());
                return u.obj;
        }

        const lisp_obj *as_cobject() const 
        {
                assert(is_object());
                return u.obj;
        }
        
        bool operator==(lisp_value other) const 
        {
                return other.u.bits == u.bits;
        }

        bool operator!=(lisp_value other) const 
        {
                return other.u.bits != u.bits;
        }
        
        union 
        {
                uint64_t bits;
                struct {
                        int64_t tag : 1; // bit 0 == integer tag
                        int64_t number : 63;
                } integer_layout;
                lisp_obj *obj;
        } u;
};

typedef lisp_value (*primitive_function)(lisp_value env, lisp_value args);

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
                        lisp_value car;
                        lisp_value cdr;
                } cons;
                struct {
                        lisp_value env;
                        lisp_value args;
                        lisp_value body;
                } lambda;
                primitive_function primitive;
        };
};


lisp_value LISP_T;
lisp_value LISP_NIL;

/* Commonly used symbols for easier access without having to call intern */
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


lisp_value LISP_BASE_ENVIRONMENT;
std::unordered_map<std::string, lisp_value> LISP_MACROS;

std::string pretty_print(lisp_value obj);
std::string repr(lisp_value obj);


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

lisp_value create_lisp_obj_character(char in)
{
        lisp_obj *ret = new lisp_obj();
        ret->type = CHAR_TYPE;
        ret->character = in;
        return lisp_value(ret);
}

lisp_value create_lisp_obj_integer(int64_t in)
{
        return lisp_value(in);
}

lisp_value create_lisp_obj_cons(lisp_value car, lisp_value cdr)
{
        lisp_obj *ret = new lisp_obj();
        ret->type = CONS_TYPE;
        ret->cons.car = car;
        ret->cons.cdr = cdr;
        return lisp_value(ret);
}

lisp_value cons(lisp_value car, lisp_value cdr)
{
        return create_lisp_obj_cons(car, cdr);
}

lisp_value create_lisp_obj_lambda(lisp_value env, lisp_value args, lisp_value body)
{
        lisp_obj *ret = new lisp_obj();
        ret->type = LAMBDA_TYPE;
        ret->lambda.env = env;
        ret->lambda.args = args;
        ret->lambda.body = body;
        return lisp_value(ret);
}

lisp_value create_lisp_obj_primitive_function(primitive_function primitive) 
{
        lisp_obj *ret = new lisp_obj();
        ret->type = PRIMITIVE_FUNCTION_TYPE;
        ret->primitive = primitive;
        return lisp_value(ret);
}


bool is_whitespace(int c)
{
        return (c == ' ' || c == '\n' || c == '\t');
}

bool is_digit(int c)
{
        return (c >= '0' && c <= '9');
}

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

bool is_symbol_char(int c)
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

void consume_whitespace(lisp_stream &stream)
{
        while (is_whitespace(stream.peekc()))
                stream.getc();
}

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
                        auto car = parse(stream);
                        if (car == nullptr)
                                return nullptr;
                        consume_whitespace(stream);
                        if (stream.peekc() == '.') {
                                stream.getc();
                                auto cdr = parse(stream);
                                if (cdr == nullptr)
                                        return nullptr;
                                if (stream.peekc() == ')')
                                        stream.getc();
                                return cons(car, cdr);
                        }
                        auto head = cons(car, LISP_NIL);
                        car = head;
                        while (stream.peekc() != ')') {
                                auto elem = parse(stream);
                                if (elem == nullptr)
                                        return nullptr;
                                car.as_object()->cons.cdr = cons(elem, LISP_NIL);
                                car = car.as_object()->cons.cdr;
                                consume_whitespace(stream);
                        }
                        if (stream.peekc() == ')')
                                stream.getc();
                        return head;
                }
        }
        return nullptr;
}

lisp_value car(lisp_value obj) {if (obj == LISP_NIL) return LISP_NIL; return obj.as_object()->cons.car;}
lisp_value cdr(lisp_value obj) {if (obj == LISP_NIL) return LISP_NIL; return obj.as_object()->cons.cdr;}
lisp_value cddr(lisp_value obj) { return cdr(cdr(obj)); }
lisp_value cdddr(lisp_value obj) { return cdr(cdr(cdr(obj))); }
lisp_value cadr(lisp_value obj) {return car(cdr(obj));}
lisp_value caddr(lisp_value obj) {return car(cdr(cdr(obj)));}
lisp_value cadddr(lisp_value obj) {return car(cdr(cdr(cdr(obj))));}
lisp_value caar(lisp_value obj) { return car(car(obj)); }
lisp_value cdar(lisp_value obj) { return cdr(car(obj)); }
lisp_value first(lisp_value obj) {return car(obj);}
lisp_value rest(lisp_value obj) {return cdr(obj);}
lisp_value second(lisp_value obj) {return cadr(obj);}
lisp_value third(lisp_value obj) {return caddr(obj);}
lisp_value fourth(lisp_value obj) {return cadddr(obj);}

lisp_value push(lisp_value item, lisp_value place) 
{
        if (place == LISP_NIL) {
                return cons(item, LISP_NIL);
        }
        auto original = car(place);
        place.as_object()->cons.car = item;
        place.as_object()->cons.cdr = cons(original, cdr(place));
        return place;
}

lisp_value bind_primitive(lisp_value *env, const std::string &symbol_name, primitive_function primitive) 
{
        auto prim_object = create_lisp_obj_primitive_function(primitive);
        auto symbol = intern_symbol(symbol_name);
        auto binding = cons(symbol, prim_object);
        *env = push(binding, *env);
        return binding;
}

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

lisp_value assoc(lisp_value item, lisp_value alist)
{
        if (alist == LISP_NIL)
                return alist;
        if (item == caar(alist))
                return car(alist);
        else
                return assoc(item, cdr(alist));
}

lisp_value lisp_prim_plus(lisp_value env, lisp_value args) 
{
        int64_t result = 0;
        while (args != LISP_NIL) {
                // @TODO: Type validation, (+ 1 2 3) OK
                //                         (+ 1 #\a) BAD
                auto tmp = car(args);
                result += tmp.as_fixnum();
                args = cdr(args);
        }
        return create_lisp_obj_integer(result);
}

lisp_value lisp_prim_minus(lisp_value env, lisp_value args)
{
        int64_t result = 0;
        // @TODO: Type validation
        if (cdr(args) == LISP_NIL) {
                result = -car(args).as_fixnum();
        }
        else {
                result = car(args).as_fixnum();
                args = cdr(args);
                while (args != LISP_NIL) {
                        result -= car(args).as_fixnum();
                        args = cdr(args);
                }
        }
        return create_lisp_obj_integer(result);
}


lisp_value lisp_prim_multiply(lisp_value env, lisp_value args)
{
        int result = 1;
        while (args != LISP_NIL) {
                // @TODO: Type validation
                auto tmp = car(args);
                result *= tmp.as_fixnum();
                args = cdr(args);
        }
        return create_lisp_obj_integer(result);
}

lisp_value lisp_prim_print(lisp_value env, lisp_value args) 
{
        printf("%s", repr(car(args)).c_str());
        //while (args != LISP_NIL) {
        //        auto tmp = car(args);
        //        pretty_print(tmp);
        //        args = cdr(args);
        //}
        return LISP_NIL;
}

lisp_value lisp_prim_num_less(lisp_value env, lisp_value args) 
{
        // (< 1)
        if (cdr(args) == LISP_NIL) {
                return LISP_T;
        }

        auto a = first(args);
        auto b = second(args);
        bool result = a.as_fixnum() < b.as_fixnum();
        if (result) {
                args = cddr(args);
                a = b;
                while (args != LISP_NIL) {
                        b = car(args);
                        result = a.as_fixnum() < b.as_fixnum();
                        if (result == false) {
                                break;
                        }
                        a = b;
                        args = cdr(args);
                }
        }
        return result ? LISP_T : LISP_NIL;
}
lisp_value lisp_prim_car(lisp_value env, lisp_value args) 
{
        return caar(args);
}

lisp_value lisp_prim_cdr(lisp_value env, lisp_value args) 
{
        return cdar(args);
}

lisp_value lisp_prim_cons(lisp_value env, lisp_value args) 
{
        return cons(first(args), second(args));
}


lisp_value lisp_prim_eq(lisp_value env, lisp_value args) 
{
        if (cdr(args) == LISP_NIL) {
                return LISP_T;
        }

        auto a = first(args);
        auto b = second(args);
        bool result = a == b;
        if (result) {
                args = cddr(args);
                a = b;
                while (args != LISP_NIL) {
                        b = car(args);
                        result = a == b;
                        if (result == false) {
                                break;
                        }
                        a = b;
                        args = cdr(args);
                }
        }
        return result ? LISP_T : LISP_NIL;
}

lisp_value lisp_prim_putchar(lisp_value env, lisp_value args) 
{
        putchar(car(args).as_object()->character);
        return LISP_NIL;
}


lisp_value lisp_prim_dump_env(lisp_value env, lisp_value) 
{
        pretty_print(env);
        return LISP_NIL;
}

lisp_value lisp_prim_dump_macros(lisp_value, lisp_value)
{
        for (auto &[k,v] : LISP_MACROS) {
                printf("%s -> %s\n", k.c_str(), repr(v).c_str());
        }
        return LISP_NIL;
}

lisp_value lisp_prim_type_of(lisp_value, lisp_value args)
{
        auto it = car(args);
        if (it.is_fixnum()) {
                return LISP_SYM_FIXNUM;
        }
        if (it.is_nil()) {
                return LISP_SYM_NULL;
        }
        if (it == LISP_T) {
                return LISP_SYM_BOOLEAN;
        }
        if (it.is_object()) {
                switch (it.as_object()->type) {
                        case SYM_TYPE: return LISP_SYM_SYMBOL;
                        case CHAR_TYPE: return LISP_SYM_CHARACTER;
                        case CONS_TYPE: return LISP_SYM_CONS;
                        case LAMBDA_TYPE: return LISP_SYM_FUNCTION;
                        case PRIMITIVE_FUNCTION_TYPE: return LISP_SYM_FUNCTION;
                }
        }
        return LISP_NIL;
}

lisp_value evaluate(lisp_value env, lisp_value obj);
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
                expr.as_object()->cons.cdr = cons(next_val, LISP_NIL);
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

lisp_value bind(lisp_value env, lisp_value symbol, lisp_value value) 
{
        auto tmp = symbol_lookup(env, symbol);
        if (tmp == nullptr) {
                tmp = cons(symbol, value);
                push(tmp, env);
        }
        else {
                tmp.as_object()->cons.cdr = value;
        }
        return cdr(tmp);
}

lisp_value shadow(lisp_value env, lisp_value symbol, lisp_value value) 
{
        auto binding = cons(symbol, value);
        auto shadow_env = cons(binding, env);
        return shadow_env;
}

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
                                result += repr(obj.as_object()->cons.car);
                                if (obj.as_object()->cons.cdr == LISP_NIL) {
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
                                        result += repr(obj.as_object()->cons.cdr);
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

lisp_value map(lisp_value list, lisp_value (func)(lisp_value))
{
        if (list == LISP_NIL)
                return list;
        
        auto head = cons(func(car(list)), LISP_NIL);
        auto current = head;
        list = cdr(list);
        while (list != LISP_NIL) {
                current.as_object()->cons.cdr = cons(func(car(list)), LISP_NIL);
                current = cdr(current);
                list = cdr(list);
        }
        return head;
}

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

int main(int argc, char *argv[])
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
        
#define BIND_PRIM(lisp_name, function) bind_primitive(&LISP_BASE_ENVIRONMENT, lisp_name, function)
        BIND_PRIM("+", lisp_prim_plus);
        BIND_PRIM("PRINT", lisp_prim_print);
        BIND_PRIM("DUMP-ENV", lisp_prim_dump_env);
        BIND_PRIM("-", lisp_prim_minus);
        BIND_PRIM("<", lisp_prim_num_less);
        BIND_PRIM("*", lisp_prim_multiply);
        BIND_PRIM("CAR", lisp_prim_car);
        BIND_PRIM("CDR", lisp_prim_cdr);
        BIND_PRIM("CONS", lisp_prim_cons);
        BIND_PRIM("EQ", lisp_prim_eq);
        BIND_PRIM("PUTCHAR", lisp_prim_putchar);
        BIND_PRIM("DUMP-MACROS", lisp_prim_dump_macros);
        BIND_PRIM("TYPE-OF", lisp_prim_type_of);

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
