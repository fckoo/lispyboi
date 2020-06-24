#ifndef _LISP_HPP_
#define _LISP_HPP_

#include <type_traits>
#include <string>
#include <vector>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "backtrace.hpp"

#if !defined(DEBUG)
#define DEBUG 0
#endif

#define XSTR(x) #x
#define STR(x) XSTR(x)
#if DEBUG > 1
#define ENSURE_VALUE(value, expr) do {                                  \
        if (!(expr)) {                                                  \
            fputs("ENSURE failed: '" STR(expr) "' was false.\n", stderr); \
            fputs("    " __FILE__ ":" STR(__LINE__) "\n", stderr);      \
            fprintf(stderr, "lisp_value was a [0x%zx]: %s\n", value->bits(), lisp::repr(value).c_str()); \
            bt::trace_and_abort(10);                                    \
        }                                                               \
    } while (0)

#else
#define ENSURE_VALUE(value, expr) ((void)value)
#endif

#if DEBUG > 2
#define FORCE_INLINE inline __attribute__((noinline))
#define FLATTEN __attribute__((noinline))
#elif DEBUG > 1
#define FORCE_INLINE inline
#define FLATTEN
#else
#define FORCE_INLINE inline __attribute__((always_inline))
#define FLATTEN __attribute__((flatten))
#endif

#define LISP_NIL (lisp::lisp_value::nil())

namespace lisp {

using fixnum = int64_t;

enum LISP_OBJ_TYPE {
    SYM_TYPE = 0,
    LAMBDA_TYPE,
    SIMPLE_ARRAY_TYPE,
    FILE_STREAM_TYPE,
    SYSTEM_POINTER_TYPE,
    STRUCT_TYPE,
};

struct lisp_obj;
struct lisp_cons;
struct lisp_value;
std::string repr(const lisp_value *obj);
typedef lisp_value (*lisp_primitive)(lisp_value *args, uint32_t nargs, bool &raised_signal);
struct lisp_value {

    using underlying_type = uint64_t;

    FORCE_INLINE lisp_value() = default;

    constexpr explicit FORCE_INLINE
    lisp_value(underlying_type bits) noexcept
        : v(bits)
    {}

    FORCE_INLINE
    bool operator==(lisp_value other) const noexcept
    {
        return v == other.v;
    }

    FORCE_INLINE
    bool operator!=(lisp_value other) const noexcept
    {
        return v != other.v;
    }

    static FORCE_INLINE
    lisp_value wrap_fixnum(fixnum fixnum) noexcept
    {
        union {
            struct {
                lisp::fixnum tag : 1;
                lisp::fixnum value : 63;
            } fixnum;
            underlying_type bits;
        } u;
        u.fixnum.tag = 1;
        u.fixnum.value = fixnum;
        return lisp_value(u.bits);
    }

    static FORCE_INLINE
    lisp_value wrap_object(lisp_obj *object) noexcept
    {
        union {
            lisp_obj *obj;
            underlying_type bits;
        } u;
        u.obj = object;
        return lisp_value(u.bits);
    }

    static FORCE_INLINE
    lisp_value wrap_cons(lisp_cons *cons) noexcept
    {
        union {
            lisp_cons *cons;
            underlying_type bits;
        } u;
        u.cons = cons;
        u.bits |= TAG_CONS;
        return lisp_value(u.bits);
    }

    static FORCE_INLINE
    lisp_value wrap_primitive(lisp_primitive func) noexcept
    {
        union {
            lisp_primitive primitive_func;
            underlying_type bits;
        } u;
        u.primitive_func = func;
        // @HACK: Can we be certain to fit every primitive function in 61 bits?
        u.bits <<= 3;
        u.bits |= TAG_PRIM_FUNC;
        return lisp_value(u.bits);
    }

    static FORCE_INLINE
    lisp_value wrap_character(int32_t codepoint) noexcept
    {
        union {
            struct {
                int32_t _unused;
                int32_t codepoint;
            } character;
            underlying_type bits;
        } u;
        u.bits = WTAG_CHAR;
        u.character.codepoint = codepoint;
        return lisp_value(u.bits);
    }

    FORCE_INLINE
    bool is_fixnum() const noexcept
    {
        return bits() & 1;
    }

    FORCE_INLINE
    bool is_character() const noexcept
    {
        return wide_tag_bits() == WTAG_CHAR;
    }

    FORCE_INLINE
    bool is_nil() const noexcept
    {
        return bits() == 0;
    }

    FORCE_INLINE
    bool is_not_nil() const noexcept
    {
        return bits() != 0;
    }

    FORCE_INLINE
    bool is_cons() const noexcept
    {
        return tag_bits() == TAG_CONS;
    }

    FORCE_INLINE
    bool is_lisp_primitive() const noexcept
    {
        return tag_bits() == TAG_PRIM_FUNC;
    }

    FORCE_INLINE
    bool is_object() const noexcept
    {
        return is_not_nil() && (tag_bits() == TAG_POINTER);
    }

    FORCE_INLINE
    fixnum as_fixnum() const noexcept
    {
        ENSURE_VALUE(this, is_fixnum());
        union {
            struct {
                lisp::fixnum tag : 1;
                lisp::fixnum value : 63;
            } fixnum;
            underlying_type bits;
        } u;
        u.bits = bits();
        return u.fixnum.value;
    }

    FORCE_INLINE
    int32_t as_character() const noexcept
    {
        ENSURE_VALUE(this, is_character());
        union {
            struct {
                int32_t _unused;
                int32_t codepoint;
            } character;
            underlying_type bits;
        } u;
        u.bits = bits();
        return u.character.codepoint;
    }

    FORCE_INLINE
    lisp_obj *as_object() const noexcept
    {
        ENSURE_VALUE(this, is_object());
        union {
            lisp_obj *obj;
            underlying_type bits;
        } u;
        u.bits = bits();
        return u.obj;
    }

    FORCE_INLINE
    const lisp_obj *as_cobject() const noexcept
    {
        ENSURE_VALUE(this, is_object());
        union {
            lisp_obj *obj;
            underlying_type bits;
        } u;
        u.bits = bits();
        return u.obj;
    }

    FORCE_INLINE
    lisp_cons *as_cons() const noexcept
    {
        ENSURE_VALUE(this, is_cons());
        union {
            lisp_cons *cons;
            underlying_type bits;
        } u;
        u.bits = bits();
        u.bits &= ~BITS_MASK;
        return u.cons;
    }

    FORCE_INLINE
    lisp_primitive as_lisp_primitive() const noexcept
    {
        ENSURE_VALUE(this, is_lisp_primitive());
        // @HACK: Can we be certain to fit every primitive function in 61 bits?
        union {
            lisp_primitive primitive_func;
            underlying_type bits;
        } u;
        u.bits = bits();
        u.bits >>= 3;
        return u.primitive_func;
    }

    FORCE_INLINE
    const bool is_type(LISP_OBJ_TYPE type) const;

    FORCE_INLINE
    underlying_type bits() const noexcept
    {
        return v;
    }

    FORCE_INLINE
    underlying_type tag_bits() const noexcept
    {
        return bits() & 0b111ULL;
    }

    FORCE_INLINE
    underlying_type wide_tag_bits() const noexcept
    {
        return bits() & 0xFF;
    }

    static constexpr
    lisp_value nil() noexcept
    {
        return lisp_value(0);
    }

    static FORCE_INLINE
    lisp_value invalid_object() noexcept
    {
        lisp_value invalid;
        invalid.v = WTAG_INVALID;
        return invalid;
    }

    FORCE_INLINE bool
    is_invalid() const noexcept
    {
        return wide_tag_bits() == WTAG_INVALID;
    }

  private:
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
    static constexpr underlying_type BITS_MASK = 0b111ULL;

    static constexpr underlying_type TAG_OTHER_IMM = 0b010ULL;
    static constexpr underlying_type TAG_CONS      = 0b100ULL;
    static constexpr underlying_type TAG_PRIM_FUNC = 0b110ULL;
    static constexpr underlying_type TAG_POINTER   = 0b000ULL;

    static constexpr underlying_type WTAG_INVALID = (0b00000ULL << 3) | TAG_OTHER_IMM;
    static constexpr underlying_type WTAG_CHAR    = (0b00001ULL << 3) | TAG_OTHER_IMM;

    underlying_type v;
};

static_assert(sizeof(lisp_value) == sizeof(void*), "lisp_value size wrong.");
static_assert(std::is_trivial<lisp_value>::value, "lisp_value not trivial type.");

extern lisp_value LISP_BASE_ENVIRONMENT;
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
extern lisp_value LISP_SYM_AMP_REST;
extern lisp_value LISP_SYM_AMP_BODY;
extern lisp_value LISP_SYM_AMP_OPTIONAL;
extern lisp_value LISP_SYM_HANDLER_CASE;
extern lisp_value LISP_SYM_FILE_STREAM;
extern lisp_value LISP_SYM_SYSTEM_POINTER;
extern lisp_value LISP_SYM_TYPE_ERROR;
extern lisp_value LISP_SYM_INDEX_OUT_OF_BOUNDS_ERROR;

struct lisp_cons {
    lisp_value car;
    lisp_value cdr;
};

struct lisp_lambda {
    lisp_lambda(lisp_value env, std::vector<lisp_value> &&params, bool has_rest, size_t optionals_start_at,
                lisp_value body, const uint8_t *main_entry, const uint8_t *end,
                std::vector<const uint8_t*> &&optional_initializers)
        : m_env(env)
        , m_params(new std::vector<lisp_value>(params))
        , m_has_rest(has_rest)
        , m_optionals_start_at(optionals_start_at)
        , m_body(body)
        , m_main_entry(main_entry)
        , m_endpoint(end)
        , m_optional_initializers(new std::vector<const uint8_t*>(optional_initializers))
    {}

    lisp_lambda *instantiate(lisp_value env) const
    {
        return new lisp_lambda(env,
                               m_params, m_has_rest, m_optionals_start_at,
                               m_body, m_main_entry, m_endpoint,
                               m_optional_initializers);
    }

    const uint8_t *earliest_entry() const
    {
        if (!has_optionals()) {
            return m_main_entry;
        }
        return m_optional_initializers->at(m_optionals_start_at);
    }

    const uint8_t *main_entry() const
    {
        return m_main_entry;
    }

    const uint8_t *begin(size_t idx) const
    {
        if (idx < m_optional_initializers->size())
            return m_optional_initializers->at(idx);
        return m_main_entry;
    }

    const uint8_t *end() const
    {
        return m_endpoint;
    }

    lisp_value env() const
    {
        return m_env;
    }

    const std::vector<lisp_value> &params() const
    {
        return *m_params;
    }

    bool has_rest() const
    {
        return m_has_rest;
    }

    bool has_optionals() const
    {
        return m_optionals_start_at != m_params->size();
    }

    size_t optionals_start_at() const
    {
        return m_optionals_start_at;
    }

    lisp_value body() const
    {
        return m_body;
    }

    const std::vector<const uint8_t*> &optional_initializers() const
    {
        return *m_optional_initializers;
    }
  private:

    lisp_lambda(lisp_value env, std::vector<lisp_value> *params, bool has_rest, size_t optionals_start_at,
                lisp_value body, const uint8_t *main_entry, const uint8_t *end,
                std::vector<const uint8_t*> *optional_initializers)
        : m_env(env)
        , m_params(params)
        , m_has_rest(has_rest)
        , m_optionals_start_at(optionals_start_at)
        , m_body(body)
        , m_main_entry(main_entry)
        , m_endpoint(end)
        , m_optional_initializers(optional_initializers)
    {}

    lisp_value m_env;
    std::vector<lisp_value> *m_params;
    bool m_has_rest;
    size_t m_optionals_start_at;
    lisp_value m_body;
    const uint8_t *m_main_entry;
    const uint8_t *m_endpoint;
    std::vector<const uint8_t*> *m_optional_initializers;
};

struct lisp_symbol {
    std::string name;
    lisp_value function;
    bool interned;
    bool is_keyword() const
    {
        return name[0] == ':';
    }
};

struct lisp_simple_array {
    /* A lisp_simple_array is just an array of lisp_values. */

    lisp_simple_array(fixnum capacity)
        : m_values(capacity ? new lisp_value[capacity]() : nullptr)
        , m_type(LISP_T)
        , m_fill_pointer(capacity)
        , m_capacity(capacity)
    {}

    lisp_simple_array(fixnum capacity, lisp_value type)
        : m_values(capacity ? new lisp_value[capacity]() : nullptr)
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

    FORCE_INLINE lisp_value get(fixnum index) const
    {
        return m_values[index];
    }

    FORCE_INLINE void set(fixnum index, lisp_value value)
    {
        m_values[index] = value;
    }

    FORCE_INLINE void set_fill_pointer(fixnum new_fill_pointer)
    {
        m_fill_pointer = new_fill_pointer;
    }

    FORCE_INLINE void push_back(lisp_value value)
    {
        if (m_fill_pointer >= m_capacity) {
            fixnum new_cap = m_fill_pointer * 1.5;
            auto new_vals = new lisp_value[new_cap];
            auto old_vals = m_values;
            memcpy(new_vals, old_vals, sizeof(m_values[0]) * m_fill_pointer);
            m_values = new_vals;
            m_capacity = new_cap;
            delete[] old_vals;
        }
        m_values[m_fill_pointer++] = value;
    }

    FORCE_INLINE fixnum length() const
    {
        return m_fill_pointer;
    }

    FORCE_INLINE lisp_value type() const
    {
        return m_type;
    }

  private:

    lisp_value *m_values;
    lisp_value m_type;
    fixnum m_fill_pointer;
    fixnum m_capacity;
};

struct lisp_stream {
    static const int end_of_file = 0;
    virtual ~lisp_stream() = default;
    virtual int getc() = 0;
    virtual int peekc() = 0;
    virtual bool eof() = 0;
};

struct lisp_file_stream : lisp_stream {
    enum io_mode {
        invalid = 0,
        read = 1 << 0,
        overwrite = 1 << 1,
        append = 1 << 2,
        read_overwrite = read | overwrite,
        read_append = read | append
    };

    lisp_file_stream()
        : m_fp(nullptr)
        , m_path()
        , m_mode()
    {}

    inline const std::string &path() const {
        return m_path;
    }

    inline bool ok() const
    {
        return m_fp != nullptr;
    }

    inline bool eof()
    {
        if (!ok()) return true;
        return feof(m_fp);
    }

    inline void flush()
    {
        if (ok())
            fflush(m_fp);
    }

    inline fixnum length()
    {
        if (!ok()) return 0;
        auto original_pos = ftell(m_fp);
        fseek(m_fp, 0, SEEK_END);
        auto size = ftell(m_fp);
        fseek(m_fp, original_pos, SEEK_SET);
        return size;
    }

    inline io_mode mode() const
    {
        return m_mode;
    }

    void open(const std::string &path, io_mode mode)
    {
        if (ok())
            close();
        m_path = path;
        m_mode = mode;
        const char *mode_str = "rb+";
        if (mode & io_mode::read) {
            if (mode & io_mode::append) {
                mode_str = "rab+";
            }
            else if (mode & io_mode::overwrite) {
                mode_str = "rwb+";
            }
        }
        else if (mode & io_mode::append) {
            mode_str = "ab+";
        }
        else if (mode & io_mode::overwrite) {
            mode_str = "wb+";
        }

        m_fp = fopen(path.c_str(), mode_str);
        //if (!m_fp) {
        //        int x = errno;
        //        printf("%s\n\t%s\n", strerror(x), path.c_str());
        //}
    }

    void close()
    {
        if (ok()) {
            fclose(m_fp);
            m_fp = nullptr;
        }
    }

    int getc()
    {
        return read_byte();
    }

    int peekc()
    {
        return peek_byte();
    }

    uint8_t peek_byte()
    {
        if (eof()) return end_of_file;
        int c = fgetc(m_fp);
        ungetc(c, m_fp);
        return c;
    }

    uint8_t read_byte()
    {
        if (eof()) return end_of_file;
        return fgetc(m_fp);
    }

    int32_t peek_utf8()
    {
        if (eof()) return end_of_file;
        if (m_ungetted.size() != 0) {
            auto utf8 = m_ungetted.back();
            return utf8;
        }
        else {
            auto utf8 = read_utf8();
            m_ungetted.push_back(utf8);
            return utf8;
        }
    }

    int32_t read_utf8()
    {
        if (eof()) return end_of_file;
        if (m_ungetted.size() != 0) {
            auto utf8 = m_ungetted.back();
            m_ungetted.pop_back();
            return utf8;
        }
        else {
            int32_t c = fgetc(m_fp) & 0xff;
            if ((c & 0xf8) == 0xf0) {
                c |= (fgetc(m_fp) & 0xff) << 8;
                c |= (fgetc(m_fp) & 0xff) << 16;
                c |= (fgetc(m_fp) & 0xff) << 24;
            }
            else if ((c & 0xf0) == 0xe0) {
                c |= (fgetc(m_fp) & 0xff) << 8;
                c |= (fgetc(m_fp) & 0xff) << 16;
            }
            else if ((c & 0xe0) == 0xc0) {
                c |= (fgetc(m_fp) & 0xff) << 8;
            }
            return c;
        }
    }

    fixnum write(const std::string &str)
    {
        if (ok())
            return fwrite(str.data(), 1, str.size(), m_fp);
        return 0;
    }

    fixnum write_byte(uint8_t b)
    {
        if (ok())
            return fputc(b, m_fp) != EOF;
        return 0;
    }

    fixnum write_utf8(int32_t c)
    {
        if (ok()) {
            /* unsure if the manual unrolling is better than
             * one call to fwrite but here is the manually
             * unrolled putter
             if ((c & 0xf8) == 0xf0) {
             fputc((c >>  0) & 0xff, m_fp);
             fputc((c >>  8) & 0xff, m_fp);
             fputc((c >> 16) & 0xff, m_fp);
             fputc((c >> 24) & 0xff, m_fp);
             }
             else if ((c & 0xf0) == 0xe0) {
             fputc((c >>  0) & 0xff, m_fp);
             fputc((c >>  8) & 0xff, m_fp);
             fputc((c >> 16) & 0xff, m_fp);
             }
             else if ((c & 0xe0) == 0xc0) {
             fputc((c >>  0) & 0xff, m_fp);
             fputc((c >>  8) & 0xff, m_fp);
             }
            */
            if ((c & 0xf8) == 0xf0) {
                return fwrite(&c, 1, 4, m_fp);
            }
            else if ((c & 0xf0) == 0xe0) {
                return fwrite(&c, 1, 3, m_fp);
            }
            else if ((c & 0xe0) == 0xc0) {
                return fwrite(&c, 1, 2, m_fp);
            }
            else {
                return fputc(c & 0xff, m_fp) != EOF;
            }
        }
        return 0;
    }


    static lisp_file_stream *new_stdin()
    {
        return new lisp_file_stream(stdin, "<stdin>", io_mode::read);
    }
    static lisp_file_stream *new_stdout()
    {
        return new lisp_file_stream(stdout, "<stdout>", io_mode::append);
    }
    static lisp_file_stream *new_stderr()
    {
        return new lisp_file_stream(stderr, "<stderr>", io_mode::append);
    }
  private:
    lisp_file_stream(FILE *fp, const std::string &path, io_mode mode)
        : m_fp(fp)
        , m_path(path)
        , m_mode(mode)
    {}
    FILE *m_fp;
    std::string m_path;
    io_mode m_mode;
    std::vector<int32_t> m_ungetted;
};

struct lisp_struct {
    lisp_struct(lisp_value type, fixnum num_slots)
        : m_type(type)
        , m_slots(num_slots)
    {}
    lisp_value type() const
    {
        return m_type;
    }

    lisp_value type_name() const;

    lisp_simple_array &slots()
    {
        return m_slots;
    }

  private:
    lisp_value m_type;
    lisp_simple_array m_slots;
};

struct lisp_obj {
    lisp_obj() {}
    ~lisp_obj() {}

    inline LISP_OBJ_TYPE type() const
    {
        return m_type;
    }

    inline lisp_symbol *symbol() const
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

    inline lisp_file_stream *file_stream() const
    {
        return u.file_stream;
    }

    inline void *ptr()
    {
        return u.ptr;
    }

    inline void *ptr_ref()
    {
        return &u.ptr;
    }

    inline void ptr(void *p)
    {
        u.ptr = p;
    }

    inline lisp_struct *structure()
    {
        return u.structure;
    }

    static inline
    lisp_value wrap_pointer(void *ptr)
    {
        auto ret = new lisp_obj();
        ret->m_type = SYSTEM_POINTER_TYPE;
        ret->u.ptr = ptr;
        return lisp_value::wrap_object(ret);
    }

    static inline
    lisp_value create_file_stream(lisp_file_stream *fs)
    {
        auto ret = new lisp_obj();
        ret->m_type = FILE_STREAM_TYPE;
        ret->u.file_stream = fs;
        return lisp_value::wrap_object(ret);
    }

    static inline
    lisp_value create_symbol(const std::string &name)
    {
        // create a symbol that has not been interned.
        auto ret = new lisp_obj();
        ret->m_type = SYM_TYPE;
        ret->u.symbol = new lisp_symbol{ name, LISP_NIL, false };
        return lisp_value::wrap_object(ret);
    }

    static inline
    lisp_value create_lambda(lisp_lambda *lambda)
    {
        auto ret = new lisp_obj();
        ret->m_type = LAMBDA_TYPE;
        ret->u.lambda = lambda;
        return lisp_value::wrap_object(ret);
    }

    static inline
    lisp_value create_lambda(lisp_value env,
                             std::vector<lisp_value> &&params, bool has_rest, int optionals_start_at,
                             lisp_value body, const uint8_t *main_entry, const uint8_t *end,
                             std::vector<const uint8_t*> &&optionals)
    {
        return create_lambda(new lisp_lambda(env,
                                             std::move(params), has_rest, optionals_start_at,
                                             body, main_entry, end,
                                             std::move(optionals)));
    }

    static inline
    lisp_value create_simple_array(fixnum length)
    {
        auto ret = new lisp_obj();
        ret->m_type = SIMPLE_ARRAY_TYPE;
        ret->u.simple_array = new lisp_simple_array(length, LISP_T);
        return lisp_value::wrap_object(ret);
    }

    static inline
    lisp_value create_simple_array(fixnum length, lisp_value type)
    {
        auto ret = new lisp_obj();
        ret->m_type = SIMPLE_ARRAY_TYPE;
        if (!type.is_type(SYM_TYPE)) {
            type = LISP_T;
        }
        ret->u.simple_array = new lisp_simple_array(length, type);
        return lisp_value::wrap_object(ret);
    }

    static
    lisp_value create_string(const char *str, fixnum len)
    {
        auto array = new lisp_simple_array(8, LISP_SYM_CHARACTER);
        array->set_fill_pointer(0);
        // valid utf-8 codepoint enumeration
        for(fixnum i = 0; i < len;) {
            int cp_len = 1;
            if ((str[i] & 0xf8) == 0xf0) {
                cp_len = 4;
            }
            else if ((str[i] & 0xf0) == 0xe0) {
                cp_len = 3;
            }
            else if ((str[i] & 0xe0) == 0xc0) {
                cp_len = 2;
            }
            if ((i + cp_len) > len) {
                cp_len = 1;
            }

            int32_t codepoint = 0;
            switch (cp_len) {
                // neat use of a fallthrough.
                case 4: codepoint |= (str[i+3] & 0xff) << 24;
                case 3: codepoint |= (str[i+2] & 0xff) << 16;
                case 2: codepoint |= (str[i+1] & 0xff) <<  8;
                case 1: codepoint |= (str[i+0] & 0xff) <<  0;
            }
            array->push_back(lisp_value::wrap_character(codepoint));
            i += cp_len;
        }
        auto ret = new lisp_obj();
        ret->m_type = SIMPLE_ARRAY_TYPE;
        ret->u.simple_array = array;
        return lisp_value::wrap_object(ret);
    }

    static
    lisp_value create_string(const std::string &str)
    {
        return create_string(str.data(), str.size());
    }

    static
    lisp_value create_struct(lisp_value type, fixnum num_slots)
    {
        auto ret = new lisp_obj();
        ret->m_type = STRUCT_TYPE;
        ret->u.structure = new lisp_struct(type, num_slots);
        return lisp_value::wrap_object(ret);
    }

    static
    lisp_value standard_input_stream()
    {
        static auto lfs = lisp_file_stream::new_stdin();
        static auto obj = create_file_stream(lfs);
        return obj;
    }

    static
    lisp_value standard_output_stream()
    {
        static auto lfs = lisp_file_stream::new_stdout();
        static auto obj = create_file_stream(lfs);
        return obj;
    }

    static
    lisp_value standard_error_stream()
    {
        static auto lfs = lisp_file_stream::new_stderr();
        static auto obj = create_file_stream(lfs);
        return obj;
    }

  private:
    LISP_OBJ_TYPE m_type;
    union {
        lisp_symbol *symbol;
        lisp_lambda *lambda;
        lisp_simple_array *simple_array;
        lisp_file_stream *file_stream;
        lisp_struct *structure;
        void *ptr;
    } u;
};

static inline std::string lisp_string_to_native_string(lisp_value str)
{
    std::string ret;
    auto array = str.as_object()->simple_array();
    for (fixnum i = 0; i < array->length(); ++i) {
        auto obj = array->get(i);
        if (!obj.is_character()) break;
        auto c = obj.as_character();
        if ((c & 0xf8) == 0xf0) {
            ret.push_back(static_cast<char>((c >>  0) & 0xff));
            ret.push_back(static_cast<char>((c >>  8) & 0xff));
            ret.push_back(static_cast<char>((c >> 16) & 0xff));
            ret.push_back(static_cast<char>((c >> 24) & 0xff));
        }
        else if ((c & 0xf0) == 0xe0) {
            ret.push_back(static_cast<char>((c >>  0) & 0xff));
            ret.push_back(static_cast<char>((c >>  8) & 0xff));
            ret.push_back(static_cast<char>((c >> 16) & 0xff));
        }
        else if ((c & 0xe0) == 0xc0) {
            ret.push_back(static_cast<char>((c >>  0) & 0xff));
            ret.push_back(static_cast<char>((c >>  8) & 0xff));
        }
        else {
            ret.push_back(static_cast<char>(c & 0xff));
        }
    }
    return ret;
}

inline const bool lisp_value::is_type(LISP_OBJ_TYPE type) const
{
    return is_object() && as_object()->type() == type;
}

lisp_value intern_symbol(const std::string &symbol_name);
void pretty_print(lisp_value obj, int depth=0);
std::string repr(lisp_value obj);
// @Audit: Should lisp::parse return a bool like read_stdin and store the parsed value in a passed reference?
lisp_value parse(lisp_stream &stream);
bool read_stdin(const char *prompt_top_level, const char *prompt_continued, lisp_value &out_value, std::string *out_input = nullptr);
lisp_value macro_expand(lisp_value obj);
lisp_value evaluate(lisp_value obj);
lisp_value apply(lisp_value function, lisp_value *args, uint32_t nargs, bool &raised_signal);

static FORCE_INLINE void set_car(lisp_value cons, lisp_value val)
{
    cons.as_cons()->car = val;
}

static FORCE_INLINE void set_cdr(lisp_value cons, lisp_value val)
{
    cons.as_cons()->cdr = val;
}

static FORCE_INLINE lisp_value car(lisp_value obj)
{
    if (obj.is_nil()) return obj;
    return obj.as_cons()->car;
}

static FORCE_INLINE lisp_value cdr(lisp_value obj)
{
    if (obj.is_nil()) return obj;
    return obj.as_cons()->cdr;
}

FLATTEN static inline
lisp_value cddr(lisp_value obj)    { return cdr(cdr(obj)); }
FLATTEN static inline
lisp_value cdddr(lisp_value obj)   { return cdr(cdr(cdr(obj))); }
FLATTEN static inline
lisp_value cadr(lisp_value obj)    { return car(cdr(obj)); }
FLATTEN static inline
lisp_value caddr(lisp_value obj)   { return car(cdr(cdr(obj))); }
FLATTEN static inline
lisp_value cadddr(lisp_value obj)  { return car(cdr(cdr(cdr(obj)))); }
FLATTEN static inline
lisp_value caar(lisp_value obj)    { return car(car(obj)); }
FLATTEN static inline
lisp_value cdar(lisp_value obj)    { return cdr(car(obj)); }
FLATTEN static inline
lisp_value first(lisp_value obj)   { return car(obj); }
FLATTEN static inline
lisp_value rest(lisp_value obj)    { return cdr(obj); }
FLATTEN static inline
lisp_value second(lisp_value obj)  { return cadr(obj); }
FLATTEN static inline
lisp_value third(lisp_value obj)   { return caddr(obj); }
FLATTEN static inline
lisp_value fourth(lisp_value obj)  { return cadddr(obj); }

lisp_cons *cons_pool_pop();
FLATTEN static FORCE_INLINE lisp_value cons(lisp_value car, lisp_value cdr)
{
    lisp_cons *ret = cons_pool_pop();
    //lisp_cons *ret = new lisp_cons();
    ret->car = car;
    ret->cdr = cdr;
    return lisp_value::wrap_cons(ret);
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


static FORCE_INLINE
lisp_value to_list(const lisp_value *vals, uint32_t nvals)
{
    switch (nvals) {
        case 0: return LISP_NIL;
        case 1: return list(vals[0]);
        case 2: return list(vals[0], vals[1]);
        case 3: return list(vals[0], vals[1], vals[2]);
        case 4: return list(vals[0], vals[1], vals[2], vals[3]);
        case 5: return list(vals[0], vals[1], vals[2], vals[3], vals[4]);
        case 6: return list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5]);
        case 7: return list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]);
        case 8: return list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]);
    }
    auto head = list(vals[0]);
    auto current = head;
    for (uint32_t i = 1; i < nvals; ++i) {
        set_cdr(current, cons(vals[i], LISP_NIL));
        current = cdr(current);
    }
    return head;
}

static FORCE_INLINE
lisp_value to_list(const std::vector<lisp_value> &vals)
{
    return to_list(vals.data(), vals.size());
}

static FORCE_INLINE
std::vector<lisp_value> to_vector(lisp_value list)
{
    std::vector<lisp_value> v;
    while (list.is_not_nil()) {
        v.push_back(car(list));
        list = cdr(list);
    }
    return v;
}


static FORCE_INLINE
lisp_value push(lisp_value item, lisp_value place)
{
    if (place.is_nil()) {
        return cons(item, LISP_NIL);
    }
    auto original = car(place);
    set_car(place, item);
    set_cdr(place, cons(original, cdr(place)));
    return place;
}

static FORCE_INLINE
lisp_value symbol_lookup(lisp_value env, lisp_value symbol)
{
    /* env is a list of pairs mapping symbols to their corresponding value in the form of
     * ((symbol . value) (symbol . value) (symbol . value))
     */
    if (env.is_cons()) {
        while (env.is_not_nil()) {
            auto pair = car(env);
            auto s = car(pair);
            if (s == symbol)
                return pair;
            env = cdr(env);
        }
    }
    return LISP_NIL;
}

FORCE_INLINE
lisp_value lisp_struct::type_name() const
{
    return car(m_type);
}


}

namespace std {

template <>
struct hash<lisp::lisp_value>
{
    std::size_t operator()(const lisp::lisp_value& v) const
    {
        return std::hash<lisp::lisp_value::underlying_type>()(v.bits());
    }
};

}


#endif
