#ifndef _LISP_HPP_
#define _LISP_HPP_

#include <string>
#include <stdio.h>
#include <string.h>
#include "backtrace.hpp"

#define XSTR(x) #x
#define STR(x) XSTR(x)
#define ENSURE_VALUE(value, expr) do {                                \
                if (!(expr)) {                                          \
                        fputs("ENSURE failed: '" STR(expr) "' was false.\n", stderr); \
                        fputs("    " __FILE__ ":" STR(__LINE__) "\n", stderr); \
                        fprintf(stderr, "lisp_value was a [0x%x]: %s\n", value->bits(), lisp::repr(value).c_str()); \
                        bt::trace_and_abort(10);                \
                }                                                       \
        } while (0)


#define FORCE_INLINE inline __attribute__((always_inline))
#define FLATTEN __attribute__((flatten))

namespace lisp {

        enum LISP_OBJ_TYPE {
                SYM_TYPE = 0,
                LAMBDA_TYPE,
                SIMPLE_ARRAY_TYPE,
                FILE_STREAM_TYPE
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
                FORCE_INLINE lisp_value() { u.bits = 0; }

                FORCE_INLINE lisp_value(const lisp_value &other)
                {
                        u.bits = other.u.bits;
                }

                FORCE_INLINE lisp_value(int64_t fixnum)
                {
                        u.fixnum_layout.tag = 1;
                        u.fixnum_layout.fixnum = fixnum;
                }

                FORCE_INLINE lisp_value(lisp_obj *pointer)
                {
                        u.obj = pointer;
                }

                FORCE_INLINE lisp_value(lisp_cons *cons)
                {
                        u.cons = cons;
                        u.bits |= TAG_CONS;
                }

                FORCE_INLINE lisp_value(lisp_primitive func)
                {
                        u.primitive_func = func;
                        // @HACK: Can we be certain to fit every primitive function in 61 bits?
                        u.bits = (u.bits << 3) | TAG_PRIM_FUNC;
                }

                FORCE_INLINE lisp_value(int32_t codepoint)
                {
                        u.bits = WTAG_CHAR;
                        u.character.codepoint = codepoint;
                }

                FORCE_INLINE lisp_value(uint8_t byte)
                {
                        u.bits = WTAG_BYTE;
                        u.byte.byte = byte;
                }

                FORCE_INLINE bool is_fixnum() const
                {
                        return u.fixnum_layout.tag != 0;
                }

                FORCE_INLINE bool is_nil() const
                {
                        return bits() == 0;
                }

                FORCE_INLINE bool is_cons() const
                {
                        return tag_bits() == TAG_CONS;
                }

                FORCE_INLINE bool is_lisp_primitive() const
                {
                        return tag_bits() == TAG_PRIM_FUNC;
                }

                FORCE_INLINE bool is_object() const
                {
                        return tag_bits() == TAG_POINTER;
                }

                FORCE_INLINE int64_t as_fixnum() const
                {
                        ENSURE_VALUE(this, is_fixnum());
                        return u.fixnum_layout.fixnum;
                }

                FORCE_INLINE int32_t as_character() const
                {
                        ENSURE_VALUE(this, is_character());
                        return u.character.codepoint;
                }

                FORCE_INLINE uint8_t as_byte() const
                {
                        ENSURE_VALUE(this, is_byte());
                        return u.byte.byte;
                }

                FORCE_INLINE lisp_value as_nil() const
                {
                        ENSURE_VALUE(this, is_nil());
                        return *this;
                }

                FORCE_INLINE lisp_obj *as_object() const
                {
                        ENSURE_VALUE(this, is_object());
                        return u.obj;
                }

                FORCE_INLINE const lisp_obj *as_cobject() const
                {
                        ENSURE_VALUE(this, is_object());
                        return u.obj;
                }

                FORCE_INLINE lisp_cons *as_cons() const
                {
                        ENSURE_VALUE(this, is_cons());
                        auto tmp = *this;
                        tmp.u.bits &= ~BITS_MASK;
                        return tmp.u.cons;
                }

                FORCE_INLINE lisp_primitive as_lisp_primitive() const
                {
                        ENSURE_VALUE(this, is_lisp_primitive());
                        auto tmp = *this;
                        // @HACK: Can we be certain to fit every primitive function in 61 bits?
                        tmp.u.bits >>= 3;
                        return tmp.u.primitive_func;
                }

                FORCE_INLINE const bool is_type(LISP_OBJ_TYPE type) const;

                FORCE_INLINE bool operator==(lisp_value other) const
                {
                        return other.u.bits == u.bits;
                }

                FORCE_INLINE bool operator!=(lisp_value other) const
                {
                        return other.u.bits != u.bits;
                }

                FORCE_INLINE uint64_t bits() const
                {
                        return u.bits;
                }

                FORCE_INLINE uint64_t tag_bits() const
                {
                        return bits() & 0b111ULL;
                }

                FORCE_INLINE uint64_t wide_tag_bits() const
                {
                        return bits() & 0xFF;
                }

                static FORCE_INLINE lisp_value invalid_object()
                {
                        lisp_value invalid;
                        invalid.u.bits = WTAG_INVALID;
                        return invalid;
                }

                FORCE_INLINE bool is_invalid() const
                {
                        return wide_tag_bits() == WTAG_INVALID;
                }

                FORCE_INLINE bool is_byte() const
                {
                        return wide_tag_bits() == WTAG_BYTE;
                }

                FORCE_INLINE bool is_character() const
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
        extern lisp_value LISP_SYM_AMP_REST;
        extern lisp_value LISP_SYM_AMP_BODY;
        extern lisp_value LISP_SYM_AMP_OPTIONAL;

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
                        : m_values(capacity ? new lisp_value[capacity]() : nullptr)
                        , m_type(LISP_T)
                        , m_fill_pointer(capacity)
                        , m_capacity(capacity)
                        {}

                lisp_simple_array(size_t capacity, lisp_value type)
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

                FORCE_INLINE lisp_value get(int index) const
                {
                        return m_values[index];
                }

                FORCE_INLINE void set(int index, lisp_value value)
                {
                        m_values[index] = value;
                }

                FORCE_INLINE void set_fill_pointer(size_t new_fill_pointer)
                {
                        m_fill_pointer = new_fill_pointer;
                }

                FORCE_INLINE void push_back(lisp_value value)
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

                FORCE_INLINE size_t length() const
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
                size_t m_fill_pointer;
                size_t m_capacity;
        };

        struct lisp_stream {
                static const int end_of_file = 0;
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
                
                inline size_t length()
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

                int32_t read_utf8()
                {
                        if (eof()) return end_of_file;
                        int c = fgetc(m_fp) & 0xff;
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
                
                size_t write(const std::string &str) 
                {
                        if (ok())
                                return fwrite(str.data(), 1, str.size(), m_fp);
                        return 0;
                }

                size_t write_byte(uint8_t b)
                {
                        if (ok())
                                return fputc(b, m_fp) != EOF;
                        return 0;
                }

                size_t write_utf8(int32_t c)
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

                inline lisp_file_stream *file_stream() const
                {
                        return u.file_stream;
                }

                static inline
                lisp_value create_file_stream(lisp_file_stream *fs)
                {
                        auto ret = new lisp_obj();
                        ret->m_type = FILE_STREAM_TYPE;
                        ret->u.file_stream = fs;
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_symbol(const std::string &name)
                {
                        // create a symbol that has not been interned.
                        auto ret = new lisp_obj();
                        ret->m_type = SYM_TYPE;
                        ret->u.symbol = new std::string(name);
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_lambda(lisp_value env, lisp_value args, lisp_value body)
                {
                        auto ret = new lisp_obj();
                        ret->m_type = LAMBDA_TYPE;
                        ret->u.lambda = new lisp_lambda { env, args, body };
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_simple_array(size_t length)
                {
                        auto ret = new lisp_obj();
                        ret->m_type = SIMPLE_ARRAY_TYPE;
                        ret->u.simple_array = new lisp_simple_array(length, LISP_T);
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_simple_array(size_t length, lisp_value type)
                {
                        auto ret = new lisp_obj();
                        ret->m_type = SIMPLE_ARRAY_TYPE;
                        if (!type.is_type(SYM_TYPE)) {
                                type = LISP_T;
                        }
                        ret->u.simple_array = new lisp_simple_array(length, type);
                        return lisp_value(ret);
                }
                
                static inline
                lisp_value create_string(const char *str, size_t len)
                {
                        auto array = new lisp_simple_array(8, LISP_SYM_CHARACTER);
                        array->set_fill_pointer(0);
                        // valid utf-8 codepoint enumeration
                        for(size_t i = 0; i < len;) {
                                int cp_len = 1;
                                int32_t cp_mask = 0x000000ff;

                                if ((str[i] & 0xf8) == 0xf0) {
                                        cp_mask = 0xffffffff;
                                        cp_len = 4;
                                }
                                else if ((str[i] & 0xf0) == 0xe0) {
                                        cp_mask = 0x00ffffff;
                                        cp_len = 3;
                                }
                                else if ((str[i] & 0xe0) == 0xc0) {
                                        cp_mask = 0x0000ffff;
                                        cp_len = 2;
                                }

                                if ((i + cp_len) > len) {
                                        cp_mask = 0x000000ff;
                                        cp_len = 1;
                                }
                                auto codepoint = (*reinterpret_cast<const int32_t*>(str + i)) & cp_mask;
                                array->push_back(codepoint);
                                i += cp_len;
                        }
                        auto ret = new lisp_obj();
                        ret->m_type = SIMPLE_ARRAY_TYPE;
                        ret->u.simple_array = array;
                        return lisp_value(ret);
                }

                static inline
                lisp_value create_string(const std::string &str)
                {
                        return create_string(str.data(), str.size());
                }

                static lisp_value standard_input_stream()
                {
                        static auto lfs = lisp_file_stream::new_stdin();
                        static auto obj = create_file_stream(lfs);
                        return lisp_value(obj);
                }

                static lisp_value standard_output_stream()
                {
                        static auto lfs = lisp_file_stream::new_stdout();
                        static auto obj = create_file_stream(lfs);
                        return lisp_value(obj);
                }

                static lisp_value standard_error_stream()
                {
                        static auto lfs = lisp_file_stream::new_stderr();
                        static auto obj = create_file_stream(lfs);
                        return lisp_value(obj);
                }

        private:
                LISP_OBJ_TYPE m_type;
                union {
                        std::string *symbol;
                        lisp_lambda *lambda;
                        lisp_simple_array *simple_array;
                        lisp_file_stream *file_stream;
                } u;
        };
        
        static inline std::string lisp_string_to_native_string(lisp_value str)
        {
                std::string ret;
                auto array = str.as_object()->simple_array();
                for (size_t i = 0; i < array->length(); ++i) {
                        auto c = array->get(i).as_character();
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
                return !is_nil() &&
                        is_object() &&
                        as_object()->type() == type;
        }

        lisp_value intern_symbol(const std::string &symbol_name);
        std::string pretty_print(lisp_value obj);
        std::string repr(lisp_value obj);
        // @Audit: Should lisp::parse return a bool like read_stdin and store the parsed value in a passed reference?
        lisp_value parse(lisp_stream &stream);
        bool read_stdin(const char *prompt_top_level, const char *prompt_continued, lisp_value &out_value);
        lisp_value macro_expand(lisp_value obj);
        lisp_value evaluate(lisp_value env, lisp_value obj);
        lisp_value apply(lisp_value env, lisp_value function, lisp_value obj);
        
        static FORCE_INLINE void set_car(lisp_value cons, lisp_value val)
        {
                // @TODO: Throw error if not cons.
                cons.as_cons()->car = val;
        }

        static FORCE_INLINE void set_cdr(lisp_value cons, lisp_value val)
        {
                // @TODO: Throw error if not cons.
                cons.as_cons()->cdr = val;
        }

        static FORCE_INLINE lisp_value car(lisp_value obj)
        {
                // @TODO: Throw error if not nil or cons.
                if (obj.is_nil()) return obj;
                return obj.as_cons()->car;
        }

        static FORCE_INLINE lisp_value cdr(lisp_value obj)
        {
                // @TODO: Throw error if not nil or cons.
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

        FLATTEN static FORCE_INLINE lisp_value cons(lisp_value car, lisp_value cdr)
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

        static FORCE_INLINE
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

        static FORCE_INLINE
        lisp_value symbol_lookup(lisp_value env, lisp_value symbol)
        {
                /* env is a list of pairs mapping symbols to their corresponding value in the form of
                 * ((symbol . value) (symbol . value) (symbol . value))
                 */
                if (env.is_cons()) {
                        while (env != LISP_NIL) {
                                auto pair = car(env);
                                auto s = car(pair);
                                auto v = cdr(pair);
                                if (s == symbol)
                                        return pair;
                                env = cdr(env);
                        }
                }
                return lisp_value::invalid_object();
        }

}

#endif
