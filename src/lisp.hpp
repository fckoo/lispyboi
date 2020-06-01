#ifndef _LISP_HPP_
#define _LISP_HPP_

#include <string>
#include <stdio.h>
#include <string.h>
#include "backtrace.hpp"

#define XSTR(x) #x
#define STR(x) XSTR(x)
#define ENSURE_VALUE(value, expr) do {                                  \
                if (!(expr)) {                                          \
                        fputs("ENSURE failed: '" STR(expr) "' was false.\n", stderr); \
                        fputs("    " __FILE__ ":" STR(__LINE__) "\n", stderr); \
                        fprintf(stderr, "lisp_value was a: %s\n", lisp::repr(value).c_str()); \
                        bt::trace_and_abort(10);                \
                }                                                       \
        } while (0)

namespace lisp {

        enum LISP_OBJ_TYPE {
                SYM_TYPE = 0,
                LAMBDA_TYPE,
                SIMPLE_ARRAY_TYPE,
        };

        struct lisp_obj;
        struct lisp_cons;
        struct lisp_value;
        std::string repr(const lisp_value *obj);
        typedef lisp_value (*lisp_primitive)(lisp_value env, lisp_value args);
        struct lisp_value {
                /*
                  Do not be fooled into thinking this is another layer of indirection.
                  This struct, or rather _value_, is a mere 8-bytes and allows us to
                  represent several primitive data-types alongside a more general object
                  structure. On 64-bit arch a pointer is 8 bytes so word-aligned
                  addresses have the LSB(3) bits set to 0. We can exploit this and give
                  meaning to those bits.

                  bits 7 6 5 4 3 2 1 0
                  --------------------
                       - - - - - - - 1 -> FIXNUM
                       t t t t t 0 1 0 -> Other immediate; the 5 't' bits are extra tag bits
                                          this gives us 2^5 = 32 distinct tags and 56 bits
                                          worth of storage. We can store things like:
                                          byte literal, utf-8 codepoints, 32-bit floats,
                                          56-bit bit vector, etc.
                       - - - - - 1 0 0 -> Cons
                       - - - - - 1 1 0 -> Primitive Function
                       - - - - - 0 0 0 -> POINTER/OBJECT

                  You'll notice we don't reuse LSB(0) and that is because we want to
                  maximize the range of fixnums. This means when there is a 1 at LSB(0)
                  then the other 63 bits must represent the fixnum.

                  Another side-effect of this decision is that we may represent NIL as
                  all bits set to 0 which allows for more optimized NIL tests.
                */
                static constexpr uint64_t BITS_MASK = 0b111ULL;

                static constexpr uint64_t TAG_OTHER_IMM = 0b010ULL;
                static constexpr uint64_t TAG_CONS      = 0b100ULL;
                static constexpr uint64_t TAG_PRIM_FUNC = 0b110ULL;
                static constexpr uint64_t TAG_POINTER   = 0b000ULL;

                static constexpr uint64_t WTAG_INVALID = (0b00000ULL << 3) | TAG_OTHER_IMM;
                static constexpr uint64_t WTAG_BYTE    = (0b00001ULL << 3) | TAG_OTHER_IMM;
                static constexpr uint64_t WTAG_CHAR    = (0b00010ULL << 3) | TAG_OTHER_IMM;

                // An uninitialized lisp_value will be defaulted to NIL
                inline lisp_value() { u.bits = 0; }

                inline lisp_value(const lisp_value &other)
                {
                        u.bits = other.u.bits;
                }

                inline lisp_value(int64_t fixnum)
                {
                        u.fixnum_layout.tag = 1;
                        u.fixnum_layout.fixnum = fixnum;
                }

                inline lisp_value(lisp_obj *pointer)
                {
                        u.obj = pointer;
                }

                inline lisp_value(lisp_cons *cons)
                {
                        u.cons = cons;
                        u.bits |= TAG_CONS;
                }

                inline lisp_value(lisp_primitive func)
                {
                        u.primitive_func = func;
                        // @HACK: Can we be certain to fit every primitive function in 61 bits?
                        u.bits = (u.bits << 3) | TAG_PRIM_FUNC;
                }

                inline lisp_value(int32_t codepoint)
                {
                        u.bits = WTAG_CHAR;
                        u.character.codepoint = codepoint;
                }

                inline lisp_value(uint8_t byte)
                {
                        u.bits = WTAG_BYTE;
                        u.byte.byte = byte;
                }

                inline bool is_fixnum() const
                {
                        return u.fixnum_layout.tag != 0;
                }

                inline bool is_nil() const
                {
                        return bits() == 0;
                }

                inline bool is_cons() const
                {
                        return tag_bits() == TAG_CONS;
                }

                inline bool is_lisp_primitive() const
                {
                        return tag_bits() == TAG_PRIM_FUNC;
                }

                inline bool is_object() const
                {
                        return tag_bits() == TAG_POINTER;
                }

                inline int64_t as_fixnum() const
                {
                        ENSURE_VALUE(this, is_fixnum());
                        return u.fixnum_layout.fixnum;
                }

                inline int32_t as_character() const
                {
                        ENSURE_VALUE(this, is_character());
                        return u.character.codepoint;
                }

                inline int32_t as_byte() const
                {
                        ENSURE_VALUE(this, is_byte());
                        return u.byte.byte;
                }

                inline lisp_value as_nil() const
                {
                        ENSURE_VALUE(this, is_nil());
                        return *this;
                }

                inline lisp_obj *as_object() const
                {
                        ENSURE_VALUE(this, is_object());
                        return u.obj;
                }

                inline const lisp_obj *as_cobject() const
                {
                        ENSURE_VALUE(this, is_object());
                        return u.obj;
                }

                inline lisp_cons *as_cons() const
                {
                        ENSURE_VALUE(this, is_cons());
                        auto tmp = *this;
                        tmp.u.bits &= ~BITS_MASK;
                        return tmp.u.cons;
                }

                inline lisp_primitive as_lisp_primitive() const
                {
                        ENSURE_VALUE(this, is_lisp_primitive());
                        auto tmp = *this;
                        // @HACK: Can we be certain to fit every primitive function in 61 bits?
                        tmp.u.bits >>= 3;
                        return tmp.u.primitive_func;
                }

                inline const bool is_type(LISP_OBJ_TYPE type) const;

                inline bool operator==(lisp_value other) const
                {
                        return other.u.bits == u.bits;
                }

                inline bool operator!=(lisp_value other) const
                {
                        return other.u.bits != u.bits;
                }

                inline uint64_t bits() const
                {
                        return u.bits;
                }

                inline uint64_t tag_bits() const
                {
                        return bits() & 0b111ULL;
                }

                inline uint64_t wide_tag_bits() const
                {
                        return bits() & 0xFF;
                }

                static inline lisp_value invalid_object()
                {
                        lisp_value invalid;
                        invalid.u.bits = WTAG_INVALID;
                        return invalid;
                }

                inline bool is_invalid() const
                {
                        return wide_tag_bits() == WTAG_INVALID;
                }

                inline bool is_byte() const
                {
                        return wide_tag_bits() == WTAG_BYTE;
                }

                inline bool is_character() const
                {
                        return wide_tag_bits() == WTAG_CHAR;
                }

        private:

                union {
                        uint64_t bits;

                        struct {
                                int32_t _unused; // low bits
                                int32_t codepoint;
                        } character;

                        struct {
                                int32_t _unused; // low bits
                                uint8_t byte;
                        } byte;

                        struct {
                                int64_t tag : 1; // bit 0 == fixnum tag
                                int64_t fixnum : 63;
                        } fixnum_layout;

                        lisp_obj *obj;

                        lisp_cons *cons;

                        lisp_primitive primitive_func;
                } u;


        };

        static_assert(sizeof(lisp_value) == 8);

        extern const lisp_value LISP_NIL;
        extern lisp_value LISP_T;

        /* Commonly used symbols for easier access without having to call intern */
        extern lisp_value LISP_SYM_QUOTE;
        extern lisp_value LISP_SYM_IF;
        extern lisp_value LISP_SYM_LAMBDA;
        extern lisp_value LISP_SYM_SETQ;
        extern lisp_value LISP_SYM_DEFMACRO;
        extern lisp_value LISP_SYM_FIXNUM;
        extern lisp_value LISP_SYM_CONS;
        extern lisp_value LISP_SYM_CHARACTER;
        extern lisp_value LISP_SYM_FUNCTION;
        extern lisp_value LISP_SYM_SYMBOL;
        extern lisp_value LISP_SYM_STRING;
        extern lisp_value LISP_SYM_NULL;
        extern lisp_value LISP_SYM_BOOLEAN;
        extern lisp_value LISP_SYM_QUASIQUOTE;
        extern lisp_value LISP_SYM_UNQUOTE;
        extern lisp_value LISP_SYM_UNQUOTESPLICING;
        extern lisp_value LISP_SYM_SIMPLE_ARRAY;
        extern lisp_value LISP_SYM_ARRAY;

        struct lisp_cons {
                lisp_value car;
                lisp_value cdr;
        };

        struct lisp_lambda {
                lisp_value env;
                lisp_value args;
                lisp_value body;
        };

        struct lisp_simple_array {
                /* A lisp_simple_array is just an array of lisp_values. */

                lisp_simple_array(size_t capacity)
                        : m_values(new lisp_value[capacity]())
                        , m_type(LISP_T)
                        , m_fill_pointer(capacity)
                        , m_capacity(capacity)
                        {}

                lisp_simple_array(size_t capacity, lisp_value type)
                        : m_values(new lisp_value[capacity])
                        , m_type(type)
                        , m_fill_pointer(capacity)
                        , m_capacity(capacity)
                        {}

                ~lisp_simple_array()
                {
                        m_fill_pointer = 0;
                        m_capacity = 0;
                        delete[] m_values;
                }

                inline lisp_value get(int index) const
                {
                        return m_values[index];
                }

                inline void set(int index, lisp_value value)
                {
                        m_values[index] = value;
                }

                inline void set_fill_pointer(size_t new_fill_pointer)
                {
                        m_fill_pointer = new_fill_pointer;
                }

                inline void push_back(lisp_value value)
                {
                        if (m_fill_pointer >= m_capacity) {
                                size_t new_cap = m_fill_pointer * 1.5;
                                auto new_vals = new lisp_value[new_cap];
                                auto old_vals = m_values;
                                memcpy(new_vals, old_vals, sizeof(m_values[0]) * m_fill_pointer);
                                m_values = new_vals;
                                m_capacity = new_cap;
                                delete[] old_vals;
                        }
                        m_values[m_fill_pointer++] = value;
                }

                inline size_t length() const
                {
                        return m_fill_pointer;
                }

                inline lisp_value type() const
                {
                        return m_type;
                }

        private:

                lisp_value *m_values;
                lisp_value m_type;
                size_t m_fill_pointer;
                size_t m_capacity;
        };

        struct lisp_obj {
                lisp_obj() {}
                ~lisp_obj() {}

                inline LISP_OBJ_TYPE type() const
                {
                        return m_type;
                }

                inline const std::string *symbol() const
                {
                        return u.symbol;
                }

                inline const lisp_lambda *lambda() const
                {
                        return u.lambda;
                }

                inline lisp_simple_array *simple_array() const
                {
                        return u.simple_array;
                }

                inline lisp_primitive primitive() const
                {
                        return u.primitive;
                }

                static inline
                lisp_value create_symbol(const std::string &name)
                {
                        // create a symbol that has not been interned.
                        lisp_obj *ret = new lisp_obj();
                        ret->m_type = SYM_TYPE;
                        ret->u.symbol = new std::string(name);
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_lambda(lisp_value env, lisp_value args, lisp_value body)
                {
                        lisp_obj *ret = new lisp_obj();
                        ret->m_type = LAMBDA_TYPE;
                        ret->u.lambda = new lisp_lambda { env, args, body };
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_simple_array(size_t length)
                {
                        lisp_obj *ret = new lisp_obj();
                        ret->m_type = SIMPLE_ARRAY_TYPE;
                        ret->u.simple_array = new lisp_simple_array(length, LISP_T);
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_simple_array(size_t length, lisp_value type)
                {
                        lisp_obj *ret = new lisp_obj();
                        ret->m_type = SIMPLE_ARRAY_TYPE;
                        if (!type.is_type(SYM_TYPE)) {
                                type = LISP_T;
                        }
                        ret->u.simple_array = new lisp_simple_array(length, type);
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_string(const std::string &str)
                {
                        auto array = new lisp_simple_array(8, LISP_SYM_CHARACTER);
                        array->set_fill_pointer(0);
                        // valid utf-8 codepoint enumeration
                        for(size_t i = 0; i < str.length();) {
                                int cplen = 1;

                                if ((str[i] & 0xf8) == 0xf0) {
                                        cplen = 4;
                                }
                                else if ((str[i] & 0xf0) == 0xe0) {
                                        cplen = 3;
                                }
                                else if ((str[i] & 0xe0) == 0xc0) {
                                        cplen = 2;
                                }

                                if ((i + cplen) > str.length()) {
                                        cplen = 1;
                                }
                                auto character = str.substr(i, cplen).c_str();
                                auto codepoint = *reinterpret_cast<const int32_t*>(character);
                                array->push_back(codepoint);
                                i += cplen;
                        }
                        auto ret = new lisp_obj();
                        ret->m_type = SIMPLE_ARRAY_TYPE;
                        ret->u.simple_array = array;
                        return lisp_value(ret);
                }

        private:
                LISP_OBJ_TYPE m_type;
                union {
                        std::string *symbol;
                        lisp_lambda *lambda;
                        lisp_simple_array *simple_array;
                        lisp_primitive primitive;
                } u;
        };

        inline const bool lisp_value::is_type(LISP_OBJ_TYPE type) const
        {
                return !is_nil() &&
                        is_object() &&
                        as_object()->type() == type;
        }

        struct lisp_stream {
                static const int end_of_file = -1;
                virtual int getc() = 0;
                virtual int peekc() = 0;
                virtual bool eof() = 0;
        };


        lisp_value intern_symbol(const std::string &symbol_name);
        std::string pretty_print(lisp_value obj);
        std::string repr(lisp_value obj);
        // @Audit: Should lisp::parse return a bool like read_stdin and store the parsed value in a passed reference?
        lisp_value parse(lisp_stream &stream);
        bool read_stdin(const char *prompt_top_level, const char *prompt_continued, lisp_value &out_value);
        lisp_value macro_expand(lisp_value obj);
        lisp_value evaluate(lisp_value env, lisp_value obj);
        lisp_value apply(lisp_value env, lisp_value function, lisp_value obj);

        static inline void set_car(lisp_value cons, lisp_value val)
        {
                // @TODO: Throw error if not cons.
                cons.as_cons()->car = val;
        }

        static inline void set_cdr(lisp_value cons, lisp_value val)
        {
                // @TODO: Throw error if not cons.
                cons.as_cons()->cdr = val;
        }

        static inline lisp_value car(lisp_value obj)
        {
                // @TODO: Throw error if not nil or cons.
                if (obj.is_nil()) return obj;
                return obj.as_cons()->car;
        }

        static inline lisp_value cdr(lisp_value obj)
        {
                // @TODO: Throw error if not nil or cons.
                if (obj.is_nil()) return obj;
                return obj.as_cons()->cdr;
        }

        static inline lisp_value cddr(lisp_value obj)    { return cdr(cdr(obj)); }
        static inline lisp_value cdddr(lisp_value obj)   { return cdr(cdr(cdr(obj))); }
        static inline lisp_value cadr(lisp_value obj)    { return car(cdr(obj)); }
        static inline lisp_value caddr(lisp_value obj)   { return car(cdr(cdr(obj))); }
        static inline lisp_value cadddr(lisp_value obj)  { return car(cdr(cdr(cdr(obj)))); }
        static inline lisp_value caar(lisp_value obj)    { return car(car(obj)); }
        static inline lisp_value cdar(lisp_value obj)    { return cdr(car(obj)); }
        static inline lisp_value first(lisp_value obj)   { return car(obj); }
        static inline lisp_value rest(lisp_value obj)    { return cdr(obj); }
        static inline lisp_value second(lisp_value obj)  { return cadr(obj); }
        static inline lisp_value third(lisp_value obj)   { return caddr(obj); }
        static inline lisp_value fourth(lisp_value obj)  { return cadddr(obj); }

        static inline lisp_value cons(lisp_value car, lisp_value cdr)
        {
                lisp_cons *ret = new lisp_cons();
                ret->car = car;
                ret->cdr = cdr;
                return lisp_value(ret);
        }

        static inline lisp_value list()
        {
                return LISP_NIL;
        }

        template<class tlast>
        static inline lisp_value list(tlast e)
        {
                return cons(e, list());
        }

        template<class tfirst, class ...trest>
        static inline lisp_value list(tfirst first, trest... rest)
        {
                return cons(first, list(rest...));
        }
}

#endif
