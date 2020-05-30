#ifndef _LISP_HPP_
#define _LISP_HPP_

#include <string>
#include <stdio.h>
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
                CHAR_TYPE,
                LAMBDA_TYPE,
                ARRAY_TYPE,
        };

        struct lisp_obj;
        struct lisp_cons;
        struct lisp_value;
        std::string repr(const lisp_value *obj);
        typedef lisp_value (*primitive_function)(lisp_value env, lisp_value args);
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

                inline lisp_value(primitive_function func)
                {
                        u.primitive_func = func;
                        // @HACK: Can we be certain to fit every primitive function in 61 bits?
                        u.bits = (u.bits << 3) | TAG_PRIM_FUNC;
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

                inline bool is_primitive_function() const
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

                inline primitive_function as_primitive_function() const
                {
                        ENSURE_VALUE(this, is_primitive_function());
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
                                int64_t tag : 1; // bit 0 == fixnum tag
                                int64_t fixnum : 63;
                        } fixnum_layout;
                        lisp_obj *obj;
                        lisp_cons *cons;
                        primitive_function primitive_func;
                } u;


        };

        struct lisp_cons {
                lisp_value car;
                lisp_value cdr;
        };

        static_assert(sizeof(lisp_value) == 8);


        struct lisp_obj {
                lisp_obj() {}
                ~lisp_obj() {}

                LISP_OBJ_TYPE type;
                union {
                        std::string *symbol;
                        char character;
                        struct {
                                lisp_value env;
                                lisp_value args;
                                lisp_value body;
                        } lambda;
                        primitive_function primitive;
                };
        };

        inline const bool lisp_value::is_type(LISP_OBJ_TYPE type) const
        {
                return !is_nil() &&
                        is_object() &&
                        as_object()->type == type;
        }

        struct lisp_stream {
                static const int end_of_file = -1;
                virtual int getc() = 0;
                virtual int peekc() = 0;
                virtual bool eof() = 0;
        };

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
        extern lisp_value LISP_SYM_NULL;
        extern lisp_value LISP_SYM_BOOLEAN;
        extern lisp_value LISP_SYM_QUASIQUOTE;
        extern lisp_value LISP_SYM_UNQUOTE;
        extern lisp_value LISP_SYM_UNQUOTESPLICING;


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

        static inline lisp_value create_lisp_obj_symbol(const std::string &name)
        {
                // create a symbol that has not been interned.
                lisp_obj *ret = new lisp_obj();
                ret->type = SYM_TYPE;
                ret->symbol = new std::string(name);
                return lisp_value(ret);
        }

        static inline lisp_value create_lisp_obj_character(char in)
        {
                lisp_obj *ret = new lisp_obj();
                ret->type = CHAR_TYPE;
                ret->character = in;
                return lisp_value(ret);
        }

        static inline lisp_value create_lisp_obj_fixnum(int64_t fixnum)
        {
                return lisp_value(fixnum);
        }

        static inline lisp_value create_lisp_obj_lambda(lisp_value env, lisp_value args, lisp_value body)
        {
                lisp_obj *ret = new lisp_obj();
                ret->type = LAMBDA_TYPE;
                ret->lambda.env = env;
                ret->lambda.args = args;
                ret->lambda.body = body;
                return lisp_value(ret);
        }
}

#endif
