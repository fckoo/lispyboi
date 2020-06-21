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
#include <list>

#include "lisp.hpp"
#include "primitives.hpp"
#include "platform.hpp"
#include "backtrace.hpp"

using namespace lisp;

bool LISP_SINGLE_STEP_DEBUGGER = false;

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



struct lisp_exception {
    lisp_value what;
};
struct lisp_unhandleable_exception : lisp_exception {
    const char *msg;
};
struct lisp_signal_exception : lisp_exception {};

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
lisp_value LISP_SYM_TYPE_ERROR;
lisp_value LISP_SYM_INDEX_OUT_OF_BOUNDS_ERROR;
lisp_value LISP_SYM_SYSTEM_POINTER;
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
                    ss << "#:" << obj.as_object()->symbol()->name;
                    result = ss.str();
                }
                else {
                    result = obj.as_object()->symbol()->name;
                }
                break;
            case LAMBDA_TYPE: {
                result += "(LAMBDA (";
                auto const &lambda = *obj.as_object()->lambda();
                auto const &params = lambda.params();
                auto optionals_start_at = lambda.optionals_start_at();
                for (size_t i = 0; i < optionals_start_at; ++i) {
                    if (lambda.has_rest() && i + 1 == params.size()) {
                        result += "&REST ";
                    }
                    result += repr(params[i]);
                    if (i + 1 < optionals_start_at) {
                        result += " ";
                    }
                }
                if (optionals_start_at < params.size()) {
                    result += " &OPTIONAL ";
                    for (size_t i = optionals_start_at; i < params.size()-1; ++i) {
                        result += repr(params[i]);
                        result += " ";
                    }
                    if (lambda.has_rest()) {
                        result += "&REST ";
                    }
                    result += repr(params.back());
                }
                result += ") ";
                auto body = obj.as_object()->lambda()->body();
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
                    auto native = lisp_string_to_native_string(obj);
                    for (auto c : native) {
                        switch (c) {
                            default: result += c; break;
                            case '\t': result += "\\t"; break;
                            case '\r': result += "\\r"; break;
                            case '\n': result += "\\n"; break;
                        }
                    }
                    result += '"';
                }
                else {
                    result += "#(";
                    if (array->length() != 0) {
                        fixnum i = 0;
                        fixnum end = array->length() - 1;
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
                auto f = obj.as_object()->file_stream();
                result += "#<FILE-STREAM ";
                result += f->path();
                result += " :OK ";
                result += f->ok() ? "T" : "NIL";
                result += " :EOF ";
                result += f->eof() ? "T" : "NIL";
                result += ">";
            } break;
            case SYSTEM_POINTER_TYPE: {
                ss << "#<SYSTEM-POINTER 0x" << std::hex << reinterpret_cast<uintptr_t>(obj.as_object()->ptr()) << ">";
                result = ss.str();
            } break;
            case STRUCT_TYPE: {
                result += "#S(";
                result += repr(obj.as_object()->structure()->type_name());
                result += ")";
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
    return ('0' <= c && c <= '9');
}

static FORCE_INLINE
bool is_hex_digit(int c)
{
    return ('0' <= c && c <= '9')
        || ('A' <= c && c <= 'F')
        || ('a' <= c && c <= 'f');
}

static FORCE_INLINE
bool is_oct_digit(int c)
{
    return '0' <= c && c <= '7';
}

static FORCE_INLINE
bool is_bin_digit(int c)
{
    return c == '0' || c == '1';
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
        if (stream.peekc() == '-' || stream.peekc() == '+' || is_digit(stream.peekc())) {
            std::string number_str;
            number_str += stream.getc();
            char firstc = number_str[0];
            while (is_digit(stream.peekc())) {
                number_str += stream.getc();
            }

            bool is_number = !is_symbol_char(stream.peekc());
            while (is_symbol_char(stream.peekc())) {
                number_str += stream.getc();
            }
            if (is_number && (firstc == '-' || firstc == '+') && number_str.size() == 1) {
                is_number = false;
            }

            if (is_number) {
                fixnum result = std::stoll(number_str);
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
            else if (stream.peekc() == 'x' || stream.peekc() == 'X') {
                stream.getc();
                std::string hexnum;
                while (is_hex_digit(stream.peekc())) {
                    hexnum += stream.getc();
                }
                fixnum result = std::stoll(hexnum, 0, 16);
                return lisp_value::wrap_fixnum(result);
            }
            else if (stream.peekc() == 'b' || stream.peekc() == 'B') {
                stream.getc();
                std::string binnum;
                while (is_bin_digit(stream.peekc())) {
                    binnum += stream.getc();
                }
                fixnum result = std::stoll(binnum, 0, 2);
                return lisp_value::wrap_fixnum(result);
            }
            else if (stream.peekc() == 'o' || stream.peekc() == 'O') {
                stream.getc();
                std::string octnum;
                while (is_oct_digit(stream.peekc())) {
                    octnum += stream.getc();
                }
                fixnum result = std::stoll(octnum, 0, 8);
                return lisp_value::wrap_fixnum(result);
            }
        }
        else if (stream.peekc() == '"') {
            stream.getc(); // consume opening "
            std::string str;
            while (stream.peekc() != '"') {
                auto c = stream.getc();
                if (c == '\\') {
                    c = stream.getc();
                    switch (c) {
                        case 'r': c = '\r'; break;
                        case 't': c = '\t'; break;
                        case 'n': c = '\n'; break;
                    }
                }
                str += c;
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


struct bytecode_emitter {

    void emit_push_value(lisp_value val);
    void emit_push_nil();
    void emit_push_fixnum_0();
    void emit_push_fixnum_1();
    void emit_funcall(uint32_t how_many);
    void emit_funcall(lisp_value what, uint32_t how_many);
    void emit_gotocall(uint32_t how_many);
    void emit_gotocall(lisp_value what, uint32_t how_many);
    void emit_return();

    // These branch emitters return the bytecode offset of their 'where' component,
    // this is useful for backfilling labels upon their location discovery
    int32_t emit_jump(int32_t where);
    int32_t emit_jump();
    int32_t emit_pop_jump_if_nil(int32_t where);
    int32_t emit_pop_jump_if_nil();

    void emit_get_value(lisp_value symbol);
    void emit_set_value();
    void emit_set_value(lisp_value symbol, lisp_value val);

    void emit_function_value(lisp_value symbol);
    void emit_define_function(lisp_value symbol, lisp_value val);
    void emit_define_function();

    void emit_pop();

    void emit_instantiate_lambda(lisp_value lambda);
    void emit_instantiate_lambda();

    void emit_cons();
    void emit_car();
    void emit_cdr();
    void emit_halt();

    int32_t emit_push_handler_case(uint32_t how_many);
    void emit_pop_handler_case();
    void emit_raise_signal(uint32_t how_many);

    void emit_eq();
    void emit_rplaca();
    void emit_rplacd();
    void emit_aref();
    void emit_aset();

    void set_raw_8(size_t offset, uint8_t v);
    void set_raw_16(size_t offset, uint16_t v);
    void set_raw_32(size_t offset, uint32_t v);
    void set_raw_s32(size_t offset, int32_t v);
    void set_raw_lisp_value(size_t offset, lisp_value v);

    int32_t position() const;
    const std::vector<uint8_t> &bytecode() const;
    std::vector<uint8_t> &&move_bytecode();

    void push_labels();
    void pop_labels();
    bool get_label(lisp_value tag, int32_t &out_offset);
    int32_t make_label(lisp_value tag);
    void backfill(int32_t offset, lisp_value tag);
  private:
    struct backfill_info { int32_t offs; lisp_value tag; };
    using label_map = std::unordered_map<lisp_value, int32_t>;
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
    std::vector<label_map> m_labels;
    std::list<backfill_info> m_backfills;
};

void bytecode_emitter::push_labels()
{
    m_labels.push_back(label_map());
}
void bytecode_emitter::pop_labels()
{
    assert(m_labels.size() != 0);

    {
        auto it = m_backfills.begin();
        while (it != m_backfills.end()) {
            int32_t label_offs;
            if (get_label(it->tag, label_offs)) {
                auto set_offs = it->offs;

                set_raw_s32(set_offs, label_offs - (set_offs - 1));

                it = m_backfills.erase(it);
            }
            else {
                it++;
            }
        }
    }

    m_labels.pop_back();
    if (m_labels.size() == 0 && !m_backfills.empty()) {
        for (auto const &it : m_backfills) {
            printf("No tag found for: "); pretty_print(it.tag);
        }
        abort();
    }
}
bool bytecode_emitter::get_label(lisp_value tag, int32_t &out_offset)
{
    for (auto it = m_labels.rbegin(); it != m_labels.rend(); ++it) {
        auto found = it->find(tag);
        if (found != it->end()) {
            out_offset = found->second;
            return true;
        }
    }
    return false;
}

int32_t bytecode_emitter::make_label(lisp_value tag)
{
    assert(m_labels.size() != 0);
    assert(m_labels.back().find(tag) == m_labels.back().end());
    auto pos = position();
    m_labels.back()[tag] = pos;
    return pos;
}

void bytecode_emitter::backfill(int32_t offset, lisp_value tag)
{
    assert(m_labels.size() != 0);
    m_backfills.push_back({offset, tag});
}

void compile(bytecode_emitter &e, lisp_value expr, bool toplevel, bool tail_position = false);

struct vm_return_state {
    lisp_value env;
    const uint8_t *address;
};

struct lisp_vm_state {
    lisp_vm_state()
    {
        constexpr int buffer = 16;
        {
            auto p = new lisp_value[0x10000];
            m_params = p;
            for (int i = 0; i < buffer; ++i) {
                p[i] = LISP_NIL;
            }
            param_stack_top = param_stack_bottom = p+buffer;
        }
        {
            auto p = new vm_return_state[0x100000];
            m_returns = p;
            for (int i = 0; i < buffer; ++i) {
                p[i] = { LISP_NIL, nullptr };
            }
            return_stack_top = return_stack_bottom = p+buffer;
        }
    }

    ~lisp_vm_state()
    {
        delete[] m_params;
        delete[] m_returns;
        param_stack_top = param_stack_bottom = nullptr;
        return_stack_top = return_stack_bottom = nullptr;
    }

    lisp_value *param_stack_bottom;
    lisp_value *param_stack_top;

    vm_return_state *return_stack_bottom;
    vm_return_state *return_stack_top;

    const uint8_t *execute(const uint8_t *ip, lisp_value env);

    void debug_dump(std::ostream &out, const std::string &tag, const uint8_t *ip) const;


    FORCE_INLINE
    void push_param(lisp_value val)
    {
        *param_stack_top++ = val;
    }

    FORCE_INLINE
    lisp_value pop_param()
    {
        return *--param_stack_top;
    }

    FORCE_INLINE
    lisp_value &param_top()
    {
        return *(param_stack_top-1);
    }

    FORCE_INLINE
    void push_return(lisp_value env, const uint8_t *addr)
    {
        *return_stack_top++ = { env, addr };
    }

    FORCE_INLINE
    vm_return_state pop_return()
    {
        return *--return_stack_top;
    }

    FORCE_INLINE
    fixnum num_handlers()
    {
        return static_cast<fixnum>(m_handler_cases.size());
    }

    struct handler_case {
        lisp_value env;
        lisp_value *param_stack_top;
        vm_return_state *return_stack_top;
        lisp_value handlers;
    };

  private:

    FORCE_INLINE
    void push_handler_case(lisp_value env, lisp_value handlers)
    {
        m_handler_cases.push_back({ env, param_stack_top, return_stack_top, handlers });
    }

    FORCE_INLINE
    void pop_handler_case()
    {
        if (m_handler_cases.size() != 0) {
            m_handler_cases.pop_back();
        }
    }

    bool find_handler(lisp_value tag, bool auto_pop, handler_case &out_case_state, lisp_value &out_handler);

    lisp_value *m_params;
    vm_return_state *m_returns;
    std::vector<handler_case> m_handler_cases;
};
lisp_vm_state *THE_LISP_VM;

static FORCE_INLINE
lisp_value shadow(lisp_value env, lisp_value symbol, lisp_value value)
{
    auto binding = cons(symbol, value);
    auto shadow_env = cons(binding, env);
    return shadow_env;
}

static
const uint8_t *apply_arguments(lisp_value &shadowed_env, const lisp_lambda *lambda, lisp_value *args, uint32_t nargs)
{
    auto const &params = lambda->params();
    if (params.size() == 0) {
        return lambda->main_entry();
    }
    // @TODO: Handle case where nargs < params.size();
    lisp_value rest_sym = LISP_NIL;
    auto end = params.size();
    if (lambda->has_rest()) {
        end--;
        rest_sym = params.back();
    }
    if (nargs < end) {
        if (lambda->has_optionals()) {
            end = nargs;
        }
        else {
            throw lisp_unhandleable_exception{ {LISP_NIL}, "Argument count mismatch" };
            // @TODO: error, no optionals and not enough args passed
        }
    }
    size_t i = 0;
    for (; i < end; ++i) {
        shadowed_env = shadow(shadowed_env, params[i], args[i]);
    }
    if (rest_sym.is_not_nil() && i == params.size()-1) {
        shadowed_env = shadow(shadowed_env, rest_sym, to_list(args+i, nargs-i));
        i++;
    }
    else {
        // still need to ensure we shadow _all_ of the locals else an optional initializer will set a global var
        for (size_t j = i; j < params.size(); ++j) {
            shadowed_env = shadow(shadowed_env, params[j], LISP_NIL);
        }
    }
    return lambda->begin(i);
}

bool lisp_vm_state::find_handler(lisp_value tag, bool auto_pop, handler_case &out_case_state, lisp_value &out_handler)
{
    bool found = false;
    size_t npop = 0;
    for (auto it = m_handler_cases.rbegin(); !found && (it != m_handler_cases.rend()); ++it) {
        auto handlers = it->handlers;
        npop++;
        while (handlers.is_not_nil()) {
            auto handler = car(handlers);
            auto handler_tag = car(handler);
            if (handler_tag == LISP_T || handler_tag == tag) {
                out_case_state = *it;
                out_handler = handler;
                found = true;
                break;
            }
            handlers = cdr(handlers);
        }
    }
    if (auto_pop) {
        m_handler_cases.resize(m_handler_cases.size() - npop);
    }
    return found;
}

lisp_value lisp::apply(lisp_value env, lisp_value function, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    if (function.is_lisp_primitive()) {
        return function.as_lisp_primitive()(env, args, nargs, raised_signal);
    }
    if (function.is_type(LAMBDA_TYPE)) {
        auto lambda = function.as_object()->lambda();
        auto shadowed = lambda->env();
        lisp_vm_state vm;
        try {
            auto ip = apply_arguments(shadowed, lambda, args, nargs);
            vm.execute(ip, shadowed);
        }
        catch (lisp_signal_exception e) {
            raised_signal = true;
            return e.what;
        }
        auto result = vm.param_top();
        return result;
    }
    throw lisp_unhandleable_exception{ {function}, "Cannot APPLY because not a FUNCTION: " };
}

template<typename Function, typename ...ExtraArgs>
static
lisp_value map(lisp_value list, Function func, ExtraArgs... args)
{
    if (list.is_nil())
        return list;

    auto head = cons(func(car(list), args...), LISP_NIL);
    auto current = head;
    list = cdr(list);
    while (list.is_not_nil()) {
        set_cdr(current, cons(func(car(list), args...), LISP_NIL));
        current = cdr(current);
        list = cdr(list);
    }
    return head;
}

static
lisp_value zip3(lisp_value a, lisp_value b, lisp_value c)
{
    if (a.is_nil())
        return a;

    auto head = list(cons(car(a), cons(car(b), car(c))));
    auto current = head;
    a = cdr(a); b = cdr(b); c = cdr(c);
    while (a.is_not_nil()) {
        auto next = cons(car(a), cons(car(b), car(c)));
        set_cdr(current, cons(next, LISP_NIL));
        current = cdr(current);
        a = cdr(a); b = cdr(b); c = cdr(c);
    }
    return head;
}

static
lisp_value macro_expand_impl(lisp_value obj, lisp_value env)
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
            auto condition = macro_expand_impl(second(obj), env);
            auto consequence = macro_expand_impl(third(obj), env);
            auto alternative = macro_expand_impl(fourth(obj), env);
            return list(LISP_SYM_IF, condition, consequence, alternative);
        }
        if (car == LISP_SYM_DEFMACRO) {
            auto macro_name = second(obj);
            auto params_list = third(obj);
            auto body = map(cdddr(obj), macro_expand_impl, env);
            return cons(LISP_SYM_DEFMACRO, cons(macro_name, cons(params_list, body)));
        }
        if (car == LISP_SYM_LAMBDA) {
            auto args = second(obj);
            auto body = map(cddr(obj), macro_expand_impl, env);
            return cons(LISP_SYM_LAMBDA, cons(args, body));
        }
        if (car == LISP_SYM_SETQ) {
            auto variable_name = second(obj);
            auto value = macro_expand_impl(third(obj), env);
            return list(LISP_SYM_SETQ, variable_name, value);
        }
        if (car == LISP_SYM_HANDLER_CASE) {
            auto form = macro_expand_impl(second(obj), env);
            auto handlers = cddr(obj);
            auto handler_tags = map(handlers, first);
            auto handler_lambda_lists = map(handlers, second);
            auto handler_bodies = map(handlers, cddr);
            auto expanded_bodies = map(handler_bodies, macro_expand_impl, env);
            auto expanded_handlers = zip3(handler_tags, handler_lambda_lists, expanded_bodies);
            return cons(LISP_SYM_HANDLER_CASE, cons(form, expanded_handlers));
        }
        const auto &sym_name = car.as_object()->symbol()->name;
        auto it = LISP_MACROS.find(sym_name);
        if (it != LISP_MACROS.end()) {
            auto function = it->second;
            auto args = rest(obj);
            bool raised_signal = false;
            auto vec = to_vector(args);
            auto expand1 = apply(env, function, vec.data(), vec.size(), raised_signal);
            auto expand_all = macro_expand_impl(expand1, env);
            return expand_all;
        }
    }
    return map(obj, macro_expand_impl, env);
}


lisp_value lisp::macro_expand(lisp_value obj)
{
    return macro_expand_impl(obj, LISP_BASE_ENVIRONMENT);
}


enum class bytecode_op : uint8_t {
#define BYTECODE_DEF(name, opcode, noperands, nargs, size, docstring) op_ ## name = opcode,
#include "bytecode.def"
};

constexpr size_t bytecode_op_size(bytecode_op op)
{
    switch (op) {
#define BYTECODE_DEF(name, opcode, noperands, nargs, size, docstring) \
        case bytecode_op::op_ ## name: return size;
#include "bytecode.def"
    }
    return 1;
}

static
std::string bytecode_op_name(bytecode_op op)
{
    switch (op) {
#define BYTECODE_DEF(name, opcode, noperands, nargs, size, docstring) \
        case bytecode_op::op_ ## name: return #name;
#include "bytecode.def"
    }
    return "??";
}

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

void bytecode_emitter::set_raw_s32(size_t offset, int32_t v)
{
    set_raw<decltype(v)>(offset, v);
}

void bytecode_emitter::set_raw_lisp_value(size_t offset, lisp_value v)
{
    set_raw<decltype(v)>(offset, v);
}

int32_t bytecode_emitter::position() const
{
    return static_cast<int32_t>(m_bytecode.size());
}

const std::vector<uint8_t> &bytecode_emitter::bytecode() const
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

void bytecode_emitter::emit_gotocall(uint32_t how_many)
{
    append(bytecode_op::op_gotocall);
    append(how_many);
}
void bytecode_emitter::emit_gotocall(lisp_value what, uint32_t how_many)
{
    emit_push_value(what);
    emit_gotocall(how_many);
}

void bytecode_emitter::emit_return()
{
    append(bytecode_op::op_return);
}

int32_t bytecode_emitter::emit_jump(int32_t where)
{
    append(bytecode_op::op_jump);
    auto here = position();
    append(where);
    return here;
}
int32_t bytecode_emitter::emit_jump()
{
    return emit_jump(0xdeadbeef);
}
int32_t bytecode_emitter::emit_pop_jump_if_nil(int32_t where)
{
    append(bytecode_op::op_pop_jump_if_nil);
    auto here = position();
    append(where);
    return here;
}
int32_t bytecode_emitter::emit_pop_jump_if_nil()
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


void bytecode_emitter::emit_cons()
{
    append(bytecode_op::op_cons);
}
void bytecode_emitter::emit_car()
{
    append(bytecode_op::op_car);
}
void bytecode_emitter::emit_cdr()
{
    append(bytecode_op::op_cdr);
}

void bytecode_emitter::emit_halt()
{
    append(bytecode_op::op_halt);
}


int32_t bytecode_emitter::emit_push_handler_case(uint32_t how_many)
{
    append(bytecode_op::op_push_handler_case);
    append(how_many);
    auto pos = position();
    append<int32_t>(0xdeadbeef);
    return pos;
}
void bytecode_emitter::emit_pop_handler_case()
{
    append(bytecode_op::op_pop_handler_case);
}

void bytecode_emitter::emit_raise_signal(uint32_t how_many)
{
    append(bytecode_op::op_raise_signal);
    append(how_many);
}

void bytecode_emitter::emit_eq()
{
    append(bytecode_op::op_eq);
}
void bytecode_emitter::emit_rplaca()
{
    append(bytecode_op::op_rplaca);
}
void bytecode_emitter::emit_rplacd()
{
    append(bytecode_op::op_rplacd);
}
void bytecode_emitter::emit_aref()
{
    append(bytecode_op::op_aref);
}
void bytecode_emitter::emit_aset()
{
    append(bytecode_op::op_aset);
}

static
void put_bytes(std::ostream &out, const uint8_t *bytes, size_t nbytes, size_t min_width=10*3)
{
    size_t column = 0;
    for (size_t i = 0; i < nbytes; ++i) {
        out << std::setfill('0') << std::setw(2) << std::hex << (int)bytes[i] << ' ';
        column += 3;
    }

    for (; column < min_width; ++column) {
        out << ' ';
    }
}

static
const uint8_t *disassemble1(std::ostream &out, const uint8_t *ip, bool here)
{
    const auto opcode = static_cast<bytecode_op>(*ip);
    if (here) {
        out << ">> ";
    }
    else {
        out << "   ";
    }
    out << std::setfill('0') << std::setw(8) << std::hex << reinterpret_cast<uintptr_t>(ip) << std::setfill(' ') << "  ";
    auto size = bytecode_op_size(opcode);
    auto name = bytecode_op_name(opcode);
    switch (opcode) {
        default: {
            put_bytes(out, ip, size);
            out << name;
            ip += size;
        } break;

        case bytecode_op::op_raise_signal:
        case bytecode_op::op_funcall:
        case bytecode_op::op_gotocall: {
            auto nargs = *reinterpret_cast<const uint32_t*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << nargs;
            ip += size;
        } break;


        case bytecode_op::op_jump:
        case bytecode_op::op_pop_jump_if_nil: {
            auto offs = *reinterpret_cast<const int32_t*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << offs << " -> " << reinterpret_cast<uintptr_t>(ip+offs);
            ip += size;
        } break;

        case bytecode_op::op_function_value:
        case bytecode_op::op_get_value:
        case bytecode_op::op_push_value: {
            auto obj = *reinterpret_cast<const lisp_value*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << obj.bits();
            auto obj_repr = repr(obj);
            const int n = 25;
            if (obj_repr.size() < n) {
                out << "  [" << obj_repr << "]";
            }
            else {
                out << "  [" << obj_repr.substr(0, n-3) << "... ]";
            }
            ip += size;
        } break;

        case bytecode_op::op_push_handler_case: {
            put_bytes(out, ip, size);
            auto how_many = *reinterpret_cast<const uint32_t*>(ip+1);
            auto branch = *reinterpret_cast<const uint32_t*>(ip+1+sizeof(how_many));
            out << "PUSH_HANDLER_CASE " << how_many << ", " << branch
                << " -> " << reinterpret_cast<uintptr_t>(ip+branch);
            ip += size;
        } break;

        case bytecode_op::op_return:
        case bytecode_op::op_set_value:
        case bytecode_op::op_define_function:
        case bytecode_op::op_pop:
        case bytecode_op::op_push_nil:
        case bytecode_op::op_push_fixnum_0:
        case bytecode_op::op_push_fixnum_1:
        case bytecode_op::op_instantiate_lambda:
        case bytecode_op::op_cons:
        case bytecode_op::op_car:
        case bytecode_op::op_cdr:
        case bytecode_op::op_halt:
        case bytecode_op::op_pop_handler_case:
        case bytecode_op::op_eq:
        case bytecode_op::op_rplaca:
        case bytecode_op::op_rplacd:
        case bytecode_op::op_aref:
        case bytecode_op::op_aset: {
            put_bytes(out, ip, size);
            out << name;
            ip += size;
        } break;
    }
    out << '\n';
    return ip;
}

static
void disassemble(std::ostream &out, const std::string &tag, const uint8_t *ip, bool here = false)
{
    out << "Disassembly for \"" << tag << "\"\n";
    disassemble1(out, ip, here);
}

static
void disassemble(std::ostream &out, const std::string &tag, const uint8_t *start, const uint8_t *end, const uint8_t *ip = nullptr)
{
    out << "Disassembly for \"" << tag << "\"\n";
    for (; start != end;) {
        start = disassemble1(out, start, start == ip);
    }
}

static
void disassemble(std::ostream &out, const std::string &tag, const bytecode_emitter &e)
{
    auto start = e.bytecode().data();
    auto end = start + e.bytecode().size();
    disassemble(out, tag, start, end, nullptr);
}

/* Marked used so compiler doesn't discard this and we can call it inside GDB */
static __attribute__((used))
void disassemble(const char *tag, const uint8_t *start, const uint8_t *end, const uint8_t *ip = nullptr)
{
    std::cout << "Disassembly for \"" << tag << "\"\n";
    for (; start != end;) {
        start = disassemble1(std::cout, start, start == ip);
    }
}


static
void stack_dumps(std::ostream &out, const lisp_value *pb, const lisp_value *pt,
                 const vm_return_state *rb, const vm_return_state *rt,
                 size_t n)
{
    auto r_stack_delta = rt - rb;
    out << std::setfill(' ') << std::dec;
    out << "|R-stack " << std::setw(9) << r_stack_delta << " |         P-stack\n";
    out << "|==================|================\n";
    out << std::setfill('0');
    for (;n != 0; n--) {
        if (reinterpret_cast<uintptr_t>(rt) < reinterpret_cast<uintptr_t>(rb)) {
            out << "| **************** |";
        }
        else {
            out << "| " << std::hex << std::setw(16) << reinterpret_cast<uintptr_t>(rt->address) << " |";
            rt--;
        }

        if (reinterpret_cast<uintptr_t>(pt) < reinterpret_cast<uintptr_t>(pb)) {
            out << " ***";
        }
        else {
            auto obj_repr = repr(*pt);
            const int n = 70;
            if (obj_repr.size() < n) {
                out << " " << obj_repr;
            }
            else {
                out << " " << obj_repr.substr(0, n-3) << "...";
            }
            pt--;
        }
        out << "\n";
    }
}

void lisp_vm_state::debug_dump(std::ostream &out, const std::string &tag, const uint8_t *ip) const
{
    //disassemble_up_to(out, tag, e.bytecode(), ip, -15, 15);
    disassemble1(std::cout, ip, true);
    stack_dumps(out,
                param_stack_bottom, param_stack_top-1,
                return_stack_bottom, return_stack_top-1,
                15);
}

const uint8_t *lisp_vm_state::execute(const uint8_t *ip, lisp_value env)
{

#define TYPE_CHECK(what, typecheck, expected)                           \
    do {                                                                \
        if (!(what).typecheck) {                                        \
            signal_args = list(LISP_SYM_TYPE_ERROR, (expected), (what)); \
            goto raise_signal;                                          \
        }                                                               \
    } while (0)

#define CHECK_FIXNUM(what) TYPE_CHECK(what, is_fixnum(), LISP_SYM_FIXNUM)
#define CHECK_CONS(what) TYPE_CHECK(what, is_cons(), LISP_SYM_CONS)
#define CHECK_CHARACTER(what) TYPE_CHECK(what, is_character(), LISP_SYM_CHARACTER)
#define CHECK_SYMBOL(what) TYPE_CHECK(what, is_type(SYM_TYPE), LISP_SYM_CHARACTER)
#define CHECK_FILE_STREAM(what) TYPE_CHECK(what, is_type(FILE_STREAM_TYPE), LISP_SYM_FILE_STREAM)


    static_assert(sizeof(*ip) == 1, "pointer arithmetic will not work as expected.");
    lisp_value signal_args;
    while (1) {
        if (LISP_SINGLE_STEP_DEBUGGER) {
            debug_dump(std::cout, "VM EXEC", ip);
            if ('c' == getchar()) LISP_SINGLE_STEP_DEBUGGER = false;
        }
        const auto opcode = static_cast<bytecode_op>(*ip);
        switch (opcode) {
            case bytecode_op::op_funcall:
                push_return(env, ip + bytecode_op_size(opcode));
            case bytecode_op::op_gotocall: {
                auto func = pop_param();
                auto ofunc = func;
                auto nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                param_stack_top -= nargs;
                auto args = param_stack_top;
                if (func.is_type(SYM_TYPE)) {
                    func = func.as_object()->symbol()->function;
                }
                if (func.is_lisp_primitive()) {
                    if (opcode == bytecode_op::op_funcall) {
                        pop_return();
                    }
                    bool raised_signal = false;
                    auto result = func.as_lisp_primitive()(env, args, nargs, raised_signal);
                    if (raised_signal) {
                        signal_args = result;
                        goto raise_signal;
                    }
                    else {
                        push_param(result);
                        ip += 1 + sizeof(nargs);
                    }
                    break;
                }
                if (func.is_type(LAMBDA_TYPE)) {
                    auto lambda = func.as_object()->lambda();
                    auto shadowed = lambda->env();
                    ip = apply_arguments(shadowed, lambda, args, nargs);
                    env = shadowed;
                    break;
                }
                throw lisp_unhandleable_exception{ {ofunc}, "Not a callable object: " };
            } break;

            case bytecode_op::op_return: {
                auto ret = pop_return();
                if (ret.address == nullptr) {
                    goto done;
                }
                env = ret.env;
                ip = ret.address;
            } break;

            case bytecode_op::op_jump: {
                auto addr = *reinterpret_cast<const int32_t*>(ip+1);
                ip += addr;
            } break;

            case bytecode_op::op_pop_jump_if_nil: {
                if (pop_param().is_nil()) {
                    auto addr = *reinterpret_cast<const int32_t*>(ip+1);
                    ip += addr;
                }
                else {
                    ip += 5;
                }
            } break;

            case bytecode_op::op_get_value: {
                auto sym = *reinterpret_cast<const lisp_value*>(ip+1);
                if (sym == LISP_T) push_param(sym);
                else if (sym.as_object()->symbol()->is_keyword()) {
                    push_param(sym);
                }
                else {
                    auto val = symbol_lookup(env, sym);
                    if (val.is_nil()) {
                        throw lisp_unhandleable_exception{ {sym}, "Unbound variable: " };
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
                auto obj = *reinterpret_cast<const lisp_value*>(ip+1);
                if (obj.is_type(SYM_TYPE)) {
                    push_param(obj.as_object()->symbol()->function);
                }
                else {
                    push_param(obj);
                }
                ip += 1 + sizeof(obj);
            } break;

            case bytecode_op::op_define_function: {
                auto sym = *reinterpret_cast<const lisp_value*>(ip+1);
                printf("op_define_function NYI\n");
                goto error_and_abort;
                ip += 1 + sizeof(sym);
            } break;

            case bytecode_op::op_pop: {
                pop_param();
                ip += 1;
            } break;

            case bytecode_op::op_push_value: {
                auto val = *reinterpret_cast<const lisp_value*>(ip+1);
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
                auto lambda_copy = obj.as_object()->lambda()->instantiate(env);
                push_param(lisp_obj::create_lambda(lambda_copy));
                ip += 1;
            } break;
            case bytecode_op::op_cons: {
                auto b = pop_param();
                auto a = pop_param();
                push_param(cons(a, b));
                ip += 1;
            } break;
            case bytecode_op::op_car: {
                auto o = pop_param();
                push_param(car(o));
                ip += 1;
            } break;
            case bytecode_op::op_cdr: {
                auto o = pop_param();
                push_param(cdr(o));
                ip += 1;
            } break;
            case bytecode_op::op_halt: {
                goto done;
            } break;
            case bytecode_op::op_push_handler_case: {
                auto how_many = *reinterpret_cast<const uint32_t*>(ip+1);
                auto branch = *reinterpret_cast<const uint32_t*>(ip+1+sizeof(how_many));
                auto handlers = LISP_NIL;
                for (uint32_t i = 0; i < how_many; ++i) {
                    auto tag = pop_param();
                    auto handler = pop_param();
                    handlers = cons(cons(tag, handler), handlers);
                }
                push_return(env, ip+branch);
                push_handler_case(env, handlers);
                ip += 1 + sizeof(how_many) + sizeof(branch);
            } break;
            case bytecode_op::op_pop_handler_case: {
                pop_handler_case();
                auto ret = pop_return();
                if (ret.address == nullptr) {
                    goto done;
                }
                env = ret.env;
                ip = ret.address;
            } break;

            case bytecode_op::op_raise_signal: {
                {
                    auto tag = pop_param();
                    signal_args = LISP_NIL;
                    auto nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                    for (size_t i = 0; i < nargs; ++i) {
                        auto arg = pop_param();
                        signal_args = cons(arg, signal_args);
                    }
                    signal_args = cons(tag, signal_args);
                }
            raise_signal:
                handler_case restore;
                lisp_value handler;
                if (find_handler(first(signal_args), true, restore, handler)) {
                    param_stack_top = restore.param_stack_top;
                    return_stack_top = restore.return_stack_top;
                    auto handler_tag = car(handler);
                    auto handler_func = cdr(handler);
                    auto lambda = handler_func.as_object()->lambda();
                    auto shadowed = restore.env;
                    if (handler_tag == LISP_T) {
                        auto vec = to_vector(signal_args);
                        ip = apply_arguments(shadowed, lambda, vec.data(), vec.size());
                    }
                    else {
                        auto vec = to_vector(rest(signal_args));
                        ip = apply_arguments(shadowed, lambda, vec.data(), vec.size());
                    }
                    env = shadowed;
                }
                else {
                    throw lisp_signal_exception{ {signal_args} };
                }
            } break;

            case bytecode_op::op_eq: {
                auto b = pop_param();
                auto a = pop_param();
                if (a == b) {
                    push_param(LISP_T);
                }
                // @Audit: is this necessary, does it make sense to identity compare objects?
                else if (a.is_object() && b.is_object() &&
                         (a.as_object()->ptr() == b.as_object()->ptr())) {
                    push_param(LISP_T);
                }
                else {
                    push_param(LISP_NIL);
                }
                ip += 1;
            } break;
            case bytecode_op::op_rplaca: {
                auto b = pop_param();
                auto a = param_top();
                CHECK_CONS(a);
                set_car(a, b);
                ip += 1;
            } break;
            case bytecode_op::op_rplacd: {
                auto b = pop_param();
                auto a = param_top();
                CHECK_CONS(a);
                set_cdr(a, b);
                ip += 1;
            } break;
            case bytecode_op::op_aref: {
                auto subscript = pop_param();
                CHECK_FIXNUM(subscript);
                auto array_val = pop_param(); // @TODO: typecheck array_val in AREF primitive
                auto array = array_val.as_object()->simple_array();
                auto index = subscript.as_fixnum();
                if (index < 0 || index >= array->length()) {
                    signal_args = list(LISP_SYM_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
                    goto raise_signal;
                }
                push_param(array->get(index));
                ip += 1;
            } break;
            case bytecode_op::op_aset: {
                auto value = pop_param();
                auto subscript = pop_param();
                CHECK_FIXNUM(subscript);
                auto array_val = pop_param(); // @TODO: typecheck array_val in SET-AREF primitive
                auto array = array_val.as_object()->simple_array();
                auto index = subscript.as_fixnum();
                if (index < 0 || index >= array->length()) {
                    signal_args = list(LISP_SYM_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
                    goto raise_signal;
                }
                auto type = array->type();
                if (type != LISP_T) {
                    if (type == LISP_SYM_FIXNUM && !value.is_fixnum()) {
                        CHECK_FIXNUM(value);
                    }
                    if (type == LISP_SYM_CHARACTER && !value.is_character()) {
                        CHECK_CHARACTER(value);
                    }
                }
                array->set(index, value);
                push_param(value);
                ip += 1;
            } break;
        }
    }
    done:

    return ip;
    error_and_abort:
    std::cout << "IP @ " << std::hex << ip << '\n';
    debug_dump(std::cout, "ERROR", ip);
    abort();
}

lisp_value lisp_prim_disassemble(lisp_value, lisp_value *args, uint32_t nargs, bool &)
{
    if (nargs != 1) {
        return LISP_NIL;
    }
    auto expr = args[0];
    if (expr.is_cons()) {
        auto expanded = macro_expand_impl(expr, LISP_BASE_ENVIRONMENT);
        bytecode_emitter e;
        compile(e, expanded, true);
        disassemble(std::cout, "DISASSEMBLY", e);
    }
    else if (expr.is_fixnum()) {
        auto ptr = expr.as_fixnum();
        auto val = lisp_value(static_cast<lisp_value::underlying_type>(ptr));
        if (val.is_type(LAMBDA_TYPE)) {
            auto lambda = val.as_object()->lambda();
            disassemble(std::cout, "DISASSEMBLY", lambda->earliest_entry(), lambda->end(), lambda->begin(-1));
        }
        else {
            disassemble(std::cout, "DISASSEMBLY", reinterpret_cast<uint8_t*>(ptr));
        }
    }
    else if (expr.is_type(LAMBDA_TYPE)) {
        auto lambda = expr.as_object()->lambda();
        disassemble(std::cout, "DISASSEMBLY", lambda->earliest_entry(), lambda->end(), lambda->begin(-1));
    }
    return LISP_NIL;
}

lisp_value lisp_prim_debugger(lisp_value, lisp_value *args, uint32_t nargs, bool &)
{
    if (nargs == 0 || args[0].is_not_nil()) {
        LISP_SINGLE_STEP_DEBUGGER = true;
        return LISP_T;
    }
    else {
        LISP_SINGLE_STEP_DEBUGGER = false;
        return LISP_NIL;
    }
}

lisp_value lisp_prim_get_num_handlers(lisp_value, lisp_value*, uint32_t, bool &)
{
    return lisp_value::wrap_fixnum(THE_LISP_VM->num_handlers());
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

    LISP_SYM_TYPE_ERROR = intern_symbol("TYPE-ERROR");
    LISP_SYM_INDEX_OUT_OF_BOUNDS_ERROR = intern_symbol("INDEX-OUT-OF-BOUNDS-ERROR");

    LISP_SYM_SYSTEM_POINTER = intern_symbol("SYSTEM-POINTER");

    LISP_BASE_ENVIRONMENT = LISP_NIL;
    primitives::bind_primitives(LISP_BASE_ENVIRONMENT);

    {
        intern_symbol("%%-INTERNAL-GET-NUM-CASE-HANDLERS")
            .as_object()
            ->symbol()
            ->function = lisp_value::wrap_primitive(lisp_prim_get_num_handlers);

        intern_symbol("%DEBUGGER")
            .as_object()
            ->symbol()
            ->function = lisp_value::wrap_primitive(lisp_prim_debugger);

        intern_symbol("DISASSEMBLE")
            .as_object()
            ->symbol()
            ->function = lisp_value::wrap_primitive(lisp_prim_disassemble);
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


bool effect_free(lisp_value expr)
{
    if (!expr.is_cons()) {
        return true;
    }
    if (car(expr) == LISP_SYM_QUOTE) {
        return true;
    }
    return false;
}

void compile_body(bytecode_emitter &e, lisp_value body, bool tail_position)
{
    while (cdr(body).is_not_nil()) {
        if (!effect_free(car(body))) {
            compile(e, car(body), false, false);
            e.emit_pop();
        }
        body = cdr(body);
    }
    compile(e, car(body), false, tail_position);
}

void compile_function(bytecode_emitter &e, lisp_value expr, bool macro, bool toplevel)
{
    auto name = second(expr);
    auto lambda_list = macro ? third(expr) : second(expr);
    auto body = macro ? cdddr(expr) : cddr(expr);
    auto obody = body;

    bytecode_emitter function;
    // Optionals are a little tricky because we allow for any expression to be the default value
    // to an optional, this even means that a default value may refer to an earlier parameter eg:
    //     (defun substring (string start &optional (end (length string))) ...)
    // In this example, end has not only a default value but it's a call to a function using a
    // local variable.
    //
    // We'll solve this by generating the equivalent to a bunch of SETQs for the defaults and
    // storing the address of each one, then at runtime we'll figure out which one of these
    // to jump to.
    auto cur = lambda_list;
    std::vector<lisp_value> params;
    bool has_optionals = false;
    bool has_rest = false;
    size_t optionals_start_at = 0;
    while (cur.is_not_nil()) {
        auto sym = car(cur);
        if (sym == LISP_SYM_AMP_OPTIONAL) {
            cur = cdr(cur);
            has_optionals = true;
            break;
        }
        if (sym == LISP_SYM_AMP_REST || sym == LISP_SYM_AMP_BODY) {
            sym = second(cur);
            cur = LISP_NIL;
            has_rest = true;
        }
        optionals_start_at++;
        params.push_back(sym);
        cur = cdr(cur);
    }
    std::vector<size_t> optional_offsets;
    if (has_optionals) {
        // at this point cur is now pointing to optionals
        while (cur.is_not_nil()) {

            auto param = first(cur);
            if (param == LISP_SYM_AMP_REST || param == LISP_SYM_AMP_BODY) {
                param = second(cur);
                cur = LISP_NIL;
                has_rest = true;
            }

            optional_offsets.push_back(function.position());

            if (param.is_cons()) {
                compile(function, second(param), false);
                function.emit_push_value(first(param));
                function.emit_set_value();
                function.emit_pop();

                params.push_back(first(param));
            }
            // There's no need to generate code for "default to nil" optionals because the
            // argument binder will default them to nil.
            else {
                params.push_back(param);
            }
            cur = cdr(cur);
        }
    }
    else {
        assert(optionals_start_at == params.size());
    }

    auto lambda_offs = function.position();
    compile_body(function, body, true);
    function.emit_return();

    auto size = function.bytecode().size();
    auto p = new uint8_t[size];
    memcpy(p, function.bytecode().data(), size);
    auto main_entry = p + lambda_offs;
    auto endpoint = p + size;
    std::vector<const uint8_t*> optional_initializers;
    for (size_t i = 0; i < optionals_start_at; ++i) {
        optional_initializers.push_back(main_entry);
    }
    for (auto offs : optional_offsets) {
        optional_initializers.push_back(p + offs);
    }

    if (macro) {
        auto macro = lisp_obj::create_lambda(LISP_BASE_ENVIRONMENT,
                                             std::move(params), has_rest, optionals_start_at, obody,
                                             main_entry, endpoint,
                                             std::move(optional_initializers));
        LISP_MACROS[name.as_object()->symbol()->name] = macro;
    }
    else if (toplevel) {
        auto lambda = lisp_obj::create_lambda(LISP_BASE_ENVIRONMENT,
                                              std::move(params), has_rest, optionals_start_at, obody,
                                              main_entry, endpoint,
                                              std::move(optional_initializers));
        e.emit_push_value(lambda);
    }
    else {
        auto lambda_template = lisp_obj::create_lambda(LISP_NIL,
                                                       std::move(params), has_rest, optionals_start_at, obody,
                                                       main_entry, endpoint,
                                                       std::move(optional_initializers));
        e.emit_instantiate_lambda(lambda_template);
    }
}

void compile_function_call(bytecode_emitter &e, lisp_value func, lisp_value args, bool toplevel, bool tail_position, bool funcall)
{
    uint32_t nargs = 0;
    while (args.is_not_nil()) {
        compile(e, car(args), toplevel);
        nargs++;
        args = cdr(args);
    }
    if (func.is_cons() && first(func) == LISP_SYM_LAMBDA) {
        if (second(func).is_nil()) {
            // calling a lambda that takes no arguments is directly inlinable,
            // no call needed... :)
            compile_body(e, cddr(func), tail_position);
            return;
        }
        else {
            compile(e, func, toplevel);
        }
    }
    else if (funcall) {
        if (func.is_type(SYM_TYPE)) {
            e.emit_get_value(func);
        }
        else {
            compile(e, func, toplevel);
        }
    }
    else {
        e.emit_push_value(func);
    }

    if (tail_position) {
        e.emit_gotocall(nargs);
    }
    else {
        e.emit_funcall(nargs);
    }
}

void compile(bytecode_emitter &e, lisp_value expr, bool toplevel, bool tail_position)
{
    static auto perc_SIGNAL = intern_symbol("%SIGNAL");
    static auto perc_CONS = intern_symbol("%CONS");
    static auto perc_CAR = intern_symbol("%CAR");
    static auto perc_CDR = intern_symbol("%CDR");
    static auto perc_EQ = intern_symbol("%EQ");
    static auto perc_RPLACA = intern_symbol("%RPLACA");
    static auto perc_RPLACD = intern_symbol("%RPLACD");
    static auto perc_AREF = intern_symbol("%AREF");
    static auto perc_ASET = intern_symbol("%ASET");
    static auto TAGBODY = intern_symbol("TAGBODY");
    static auto GO = intern_symbol("GO");
    if (expr.is_cons()) {
        auto thing = first(expr);
        if (thing == LISP_SYM_QUOTE) {
            e.emit_push_value(second(expr));
        }
        else if (thing == GO) {
            auto offs = e.emit_jump();
            e.backfill(offs, second(expr));
        }
        else if (thing == TAGBODY) {
            e.push_labels();
            e.emit_push_nil();
            auto body = cdr(expr);
            while (body.is_not_nil()) {
                auto it = car(body);
                if (it.is_cons()) {
                    compile(e, it, toplevel, false);
                    e.emit_pop();
                }
                else {
                    e.make_label(it);
                }
                body = cdr(body);
            };
            e.pop_labels();
        }
        else if (thing == LISP_SYM_IF) {
            auto test = second(expr);
            auto consequence = third(expr);
            auto alternative = fourth(expr);
            compile(e, test, toplevel);
            auto alt_offs = e.emit_pop_jump_if_nil();
            compile(e, consequence, toplevel, tail_position);
            auto out_offs = e.emit_jump();
            auto label_alt = e.position();
            compile(e, alternative, toplevel, tail_position);
            auto label_out = e.position();

            e.set_raw_s32(out_offs, label_out - (out_offs-1));
            e.set_raw_s32(alt_offs, label_alt - (alt_offs-1));
        }
        else if (thing == LISP_SYM_DEFMACRO) {
            compile_function(e, expr, true, true);
        }
        else if (thing == LISP_SYM_LAMBDA) {
            compile_function(e, expr, false, toplevel);
        }
        else if (thing == LISP_SYM_SETQ) {
            compile(e, third(expr), toplevel);
            e.emit_push_value(second(expr));
            e.emit_set_value();
        }
        else if (thing == LISP_SYM_HANDLER_CASE) {
            auto form = second(expr);
            auto handlers = cddr(expr);
            uint32_t nhandlers = 0;
            while (handlers.is_not_nil()) {
                nhandlers++;
                auto handler = car(handlers);
                auto handler_tag = first(handler);
                compile_function(e, handler, false, toplevel);
                e.emit_push_value(handler_tag);
                if (handler_tag == LISP_T) {
                    if (cdr(handlers).is_not_nil()) {
                        fprintf(stderr, "WARNING: Unreachable code in HANDLER-CASE\n");
                    }
                    break;
                }
                handlers = cdr(handlers);
            }
            auto before_form = e.position();
            auto offs = e.emit_push_handler_case(nhandlers);
            compile(e, form, toplevel);
            e.emit_pop_handler_case();
            e.set_raw_32(offs, e.position() - before_form);
        }
        else if (thing == LISP_SYM_FUNCTION) {
            auto thing = second(expr);
            if (thing.is_cons()) {
                if (first(thing) == LISP_SYM_LAMBDA) {
                    compile(e, thing, toplevel);
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
            compile_function_call(e, second(expr), cddr(expr), toplevel, tail_position, true);
        }
        else if (thing == perc_CONS) {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_cons();
        }
        else if (thing == perc_CAR) {
            compile(e, second(expr), toplevel);
            e.emit_car();
        }
        else if (thing == perc_CDR) {
            compile(e, second(expr), toplevel);
            e.emit_cdr();
        }
        else if (thing == perc_SIGNAL) {
            uint32_t nargs = 0;
            auto tag = second(expr);
            auto args = cddr(expr);
            while (args.is_not_nil()) {
                compile(e, car(args), toplevel);
                args = cdr(args);
                nargs++;
            }
            compile(e, tag, toplevel);
            e.emit_raise_signal(nargs);
        }
        else if (thing == perc_EQ) {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_eq();
        }
        else if (thing == perc_RPLACA) {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_rplaca();
        }
        else if (thing == perc_RPLACD) {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_rplacd();
        }
        else if (thing == perc_AREF) {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_aref();
        }
        else if (thing == perc_ASET) {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            compile(e, fourth(expr), toplevel);
            e.emit_aset();
        }
        else {
            compile_function_call(e, first(expr), rest(expr), toplevel, tail_position, false);
        }
    }
    else if (expr.is_type(SYM_TYPE)) {
        e.emit_get_value(expr);
    }
    else if (expr.is_invalid()) {
        //fprintf(stderr, "WARNING: invalid object in compile stream.\n");
    }
    else {
        e.emit_push_value(expr);
    }
}


lisp_value lisp::evaluate(lisp_value env, lisp_value expr)
{
    bytecode_emitter e;
    auto expanded = macro_expand_impl(expr, LISP_BASE_ENVIRONMENT);
    compile(e, expanded, true);
    e.emit_halt();

    lisp_vm_state vm;
    vm.execute(e.bytecode().data(), env);
    return vm.param_top();
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
        auto parsed = parse(stm);
        if (parsed.is_invalid()) {
            consume_whitespace(stm);
            if (stm.eof()) break;
            abort();
        }
        bytecode_emitter e;
        auto expanded = macro_expand_impl(parsed, LISP_BASE_ENVIRONMENT);
        compile(e, expanded, true);
        e.emit_halt();
        auto ip = e.bytecode().data();
        if (ip != nullptr) {
            auto stack_before = vm.param_stack_top;
            vm.execute(ip, LISP_BASE_ENVIRONMENT);
            if (vm.param_stack_top != stack_before) {
                vm.pop_param();
            }
        }

        consume_whitespace(stm);
    }

    std::filesystem::current_path(here_path);
}

void repl_compile_and_execute(lisp_vm_state &vm, bool show_disassembly)
{
    static const char *prompt_lisp = "LISP-NASA> ";
    static const char *prompt_ws   = ".......... ";

    while (1) {
        lisp_value parsed;
        std::string input;
        if (!read_stdin(prompt_lisp, prompt_ws, parsed, &input)) {
            break;
        }
        bytecode_emitter e;
        auto expanded = macro_expand_impl(parsed, LISP_BASE_ENVIRONMENT);
        compile(e, expanded, true);
        e.emit_halt();
        auto ip = e.bytecode().data();
        if (show_disassembly) {
            auto start = e.bytecode().data();
            auto end = start + e.bytecode().size();
            disassemble(std::cout, input, start, end, ip);
        }

        auto stack_before = vm.param_stack_top;
        try {
            vm.execute(ip, LISP_BASE_ENVIRONMENT);
        }
        catch (lisp_unhandleable_exception e) {
            printf("%s\n", e.msg);
            printf("    %s\n", repr(e.what).c_str());
        }
        catch (lisp_signal_exception e) {
            printf("Unhandled signal:\n");
            printf("    %s\n", repr(e.what).c_str());
        }

        if (vm.param_stack_top != stack_before) {
            auto result = vm.pop_param();
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
        else if (strcmp("--debug", arg) == 0) {
            LISP_SINGLE_STEP_DEBUGGER = true;
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
    THE_LISP_VM = &vm;

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
    catch (lisp_signal_exception e) {
        printf("Unhandled signal:\n");
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
