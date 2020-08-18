#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <type_traits>
#include <string>
#include <vector>
#include <list>
#include <array>
#include <unordered_map>
#include <unordered_set>
#include <functional>
#include <algorithm>
#include <chrono>

#include <filesystem>
#include <iostream>
#include <sstream>
#include <fstream>

#include "ffi.hpp"
#include "platform.hpp"

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
            abort();                                    \
        }                                                               \
    } while (0)

#else
#define ENSURE_VALUE(value, expr) ((void)value)
#endif


#if DEBUG > 2
#if !defined(GC_DIAGNOSTICS)
#define GC_DIAGNOSTICS 1
#endif
#define FORCE_INLINE inline __attribute__((noinline))
#define FLATTEN __attribute__((noinline))

#elif DEBUG == 2
#define FORCE_INLINE inline __attribute__((noinline))
#define FLATTEN __attribute__((noinline))

#elif DEBUG == 1
#define FORCE_INLINE inline
#define FLATTEN

#elif DEBUG == 0
#define FORCE_INLINE inline __attribute__((always_inline))
#define FLATTEN __attribute__((flatten))
#define GC_NO_OPT 0
#define USE_COMPUTED_GOTOS 1

#else
#define FORCE_INLINE
#define FLATTEN
#error "Unknown DEBUG value"
#endif

#if !defined GC_DIAGNOSTICS
#define GC_DIAGNOSTICS 0
#endif

#if !defined(USE_COMPUTED_GOTOS)
#define USE_COMPUTED_GOTOS 0
#endif

#if !defined(GC_NO_OPT)
#define GC_NO_OPT 1
#endif

#if USE_COMPUTED_GOTOS == 0 && DEBUG > 1
#define PROFILE_OPCODE_PAIRS 1
#warning "Profiling opcode pairs is enabled."
#else
#define PROFILE_OPCODE_PAIRS 0
#endif


struct Opcode_Pair
{
    int a;
    int b;

    bool operator==(const Opcode_Pair &other) const noexcept
    {
        return a == other.a && b == other.b;
    }

    bool operator!=(const Opcode_Pair &other) const noexcept
    {
        return !(*this == other);
    }
};

namespace std
{

template <>
struct hash<Opcode_Pair>
{
    std::size_t operator()(const Opcode_Pair& p) const
    {
        return p.a << 8 | p.b;
    }
};

}

template <class...> constexpr std::false_type always_false{};

namespace lisp
{

using Fixnum = int64_t;

struct Cons;
struct Object;
struct Value;

struct Symbol;
struct Closure;
struct Package;
struct File_Stream;
struct Simple_Array;
struct Structure;
struct Function;

using System_Pointer = void*;

using Primitive = Value (*)(Value *args, uint32_t nargs, bool &raised_signal);

enum class Object_Type
{
    Symbol,
    Closure,
    Package,
    File_Stream,
    Simple_Array,
    System_Pointer,
    Structure,
};

struct Value
{
    using Bits_Type = uint64_t;

    FORCE_INLINE
    Value() = default;

    constexpr explicit FORCE_INLINE
    Value(Bits_Type bits) noexcept
        : m_bits(bits)
    {}

    FORCE_INLINE
    bool operator==(const Value &other) const noexcept
    {
        return m_bits == other.m_bits;
    }

    FORCE_INLINE
    bool operator!=(const Value &other) const noexcept
    {
        return m_bits != other.m_bits;
    }

    // The operators (+, -, +=, and -=) are implemented in a way that allows us to abuse
    // the underlying representation where LSB(0) is 0.
    FORCE_INLINE
    Value operator+(const Value &other) const noexcept
    {
        return Value(m_bits + other.m_bits);
    }

    FORCE_INLINE
    Value operator-(const Value &other) const noexcept
    {
        return Value(m_bits - other.m_bits);
    }

    FORCE_INLINE
    Value &operator+=(const Value &other) noexcept
    {
        m_bits += other.m_bits;
        return *this;
    }

    FORCE_INLINE
    Value &operator-=(const Value &other) noexcept
    {
        m_bits -= other.m_bits;
        return *this;
    }

    FORCE_INLINE
    Value operator|(const Value &other) const noexcept
    {
        return Value(m_bits | other.m_bits);
    }

    FORCE_INLINE
    Value &operator|=(const Value &other) noexcept
    {
        m_bits |= other.m_bits;
        return *this;
    }

    FORCE_INLINE
    Bits_Type bits() const noexcept
    {
        return m_bits;
    }

    FORCE_INLINE
    Bits_Type tag_bits() const noexcept
    {
        return m_bits & 0b111;
    }

    FORCE_INLINE
    Bits_Type wide_tag_bits() const noexcept
    {
        return m_bits & 0xFF;
    }

    static FORCE_INLINE
    Value wrap_fixnum(Fixnum fixnum) noexcept
    {
        union
        {
            struct
            {
                Fixnum tag : 1;
                Fixnum value : 63;
            } f;
            Bits_Type bits;
        } u;
        u.f.tag = 0;
        u.f.value = fixnum;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value wrap_cons(Cons *cons) noexcept
    {
        union
        {
            Cons *cons;
            Bits_Type bits;
        } u;
        u.cons = cons;
        u.bits |= TAG_CONS;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value wrap_object(Object *object) noexcept
    {
        union
        {
            Object *object;
            Bits_Type bits;
        } u;
        u.object = object;
        u.bits |= TAG_OBJECT;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value wrap_primitive(Primitive func) noexcept
    {
        union
        {
            Primitive func;
            Bits_Type bits;
        } u;
        u.func = func;
        u.bits <<= 3;
        u.bits |= TAG_PRIM_FUNC;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value wrap_character(int32_t codepoint) noexcept
    {
        union
        {
            struct
            {
                int32_t _unused;
                int32_t codepoint;
            } c;
            Bits_Type bits;
        } u;
        u.bits = WTAG_CHAR;
        u.c.codepoint = codepoint;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value nil() noexcept
    {
        return Value(WTAG_NIL);
    }

    FORCE_INLINE
    bool is_fixnum() const noexcept
    {
        return (bits() & 1) == 0;
    }

    FORCE_INLINE
    bool is_character() const noexcept
    {
        return wide_tag_bits() == WTAG_CHAR;
    }

    FORCE_INLINE
    bool is_nil() const noexcept
    {
        return nil() == *this;
    }

    FORCE_INLINE
    bool is_cons() const noexcept
    {
        return tag_bits() == TAG_CONS;
    }

    FORCE_INLINE
    bool is_list() const noexcept
    {
        return is_nil() || is_cons();
    }

    FORCE_INLINE
    bool is_garbage_collected() const noexcept
    {
        return is_cons() || is_object();
    }

    FORCE_INLINE
    bool is_lisp_primitive() const noexcept
    {
        return tag_bits() == TAG_PRIM_FUNC;
    }

    FORCE_INLINE
    bool is_object() const noexcept
    {
        return tag_bits() == TAG_OBJECT;
    }

    bool is_type(Object_Type type) const noexcept;

    FORCE_INLINE
    Fixnum as_fixnum() const noexcept
    {
        ENSURE_VALUE(this, is_fixnum());
        union
        {
            struct
            {
                Fixnum tag : 1;
                Fixnum value : 63;
            } f;
            Bits_Type bits;
        } u;
        u.bits = m_bits;
        return u.f.value;
    }

    FORCE_INLINE
    Cons *as_cons() const noexcept
    {
        ENSURE_VALUE(this, is_cons());
        union
        {
            Cons *cons;
            Bits_Type bits;
        } u;
        u.bits = m_bits & ~BITS_MASK;
        return u.cons;
    }

    FORCE_INLINE
    Object *as_object() const noexcept
    {
        ENSURE_VALUE(this, is_object());
        union
        {
            Object *object;
            Bits_Type bits;
        } u;
        u.bits = m_bits & ~BITS_MASK;
        return u.object;
    }

    FORCE_INLINE
    Primitive as_lisp_primitive() const noexcept
    {
        ENSURE_VALUE(this, is_lisp_primitive());
        union
        {
            Primitive func;
            Bits_Type bits;
        } u;
        u.bits = m_bits;
        u.bits >>= 3;
        return u.func;
    }

    FORCE_INLINE
    int32_t as_character() const noexcept
    {
        ENSURE_VALUE(this, is_character());
        union
        {
            struct
            {
                int32_t _unused;
                int32_t codepoint;
            } c;
            Bits_Type bits;
        } u;
        u.bits = m_bits;
        return u.c.codepoint;
    }

    FORCE_INLINE
    void *unwrap_pointer() const noexcept
    {
        return reinterpret_cast<void*>(bits() & ~BITS_MASK);
    }

  private:
    // Do not be fooled into thinking this is another layer of indirection.
    // This struct, or rather _value_, is a mere 8-bytes and allows us to
    // represent several primitive data-types alongside a more general object
    // structure. On 64-bit arch a pointer is 8 bytes so word-aligned
    // addresses have the LSB(3) bits set to 0. We can exploit this and give
    // meaning to those bits.

    // bits 7 6 5 4 3 2 1 0
    // --------------------
    // - - - - - - - 0 -> FIXNUM
    // t t t t t 0 1 1 -> Other immediate; the 5 't' bits are extra tag bits
    //                    this gives us 2^5 = 32 distinct tags and 56 bits
    //                    worth of storage. We can store things like:
    //                    byte literal, utf-8 codepoints, 32-bit floats,
    //                    56-bit bit vector, etc.
    // - - - - - 1 0 1 -> Cons
    // - - - - - 1 1 1 -> Primitive Function
    // - - - - - 0 0 1 -> POINTER/OBJECT

    // You'll notice we don't reuse LSB(0) and that is because we want to
    // maximize the range of fixnums. This means when there is a 0 at LSB(0)
    // then the other 63 bits must represent the fixnum.

    // Another side-effect of this decision is that fixnum adds and subracts
    // do not need to be unwrapped because a LSB(0) of 0 means the number
    // is even and adding two even integers always results in an even integer,
    // effectively meaning LSB(0) has no impact to the underlying fixnum.
    static constexpr Bits_Type BITS_MASK = 0b111ULL;

    static constexpr Bits_Type TAG_OBJECT    = 0b001ULL;
    static constexpr Bits_Type TAG_OTHER_IMM = 0b011ULL;
    static constexpr Bits_Type TAG_CONS      = 0b101ULL;
    static constexpr Bits_Type TAG_PRIM_FUNC = 0b111ULL;

    static constexpr Bits_Type WTAG_NIL       = (0b00000ULL << 3) | TAG_OTHER_IMM;
    static constexpr Bits_Type WTAG_CHAR      = (0b00001ULL << 3) | TAG_OTHER_IMM;

    Bits_Type m_bits;
};

static_assert(std::is_trivial<Value>::value, "Value not trivial type.");

struct Closure_Reference
{
    Closure_Reference()
        : next(nullptr)
        , m_location(nullptr)
        , m_closed_value(0)
    {}

    Closure_Reference(Value *location)
        : next(nullptr)
        , m_location(location)
        , m_closed_value(0)
    {}

    FORCE_INLINE
    void close()
    {
        m_closed_value = *m_location;
        m_location = &m_closed_value;
    }

    Value &value()
    {
        return *m_location;
    }

    const Value *location() const
    {
        return m_location;
    }

    Closure_Reference *next;
  private:
    Value *m_location;
    Value m_closed_value;
};

static bool read_gc_paused(std::istream &source, Value &out_result);
static std::string repr(Value value);
namespace bytecode
{
static
int disassemble(std::ostream &out, const std::string &tag, const Function *function, const uint8_t *ip = nullptr);
}

}

namespace std
{

template <>
struct hash<lisp::Value>
{
    std::size_t operator()(const lisp::Value& v) const
    {
        return std::hash<lisp::Value::Bits_Type>()(v.bits());
    }
};

}

namespace lisp
{

struct Cons
{
    Value car;
    Value cdr;
};

FORCE_INLINE
void set_car(Value cons, Value val)
{
    cons.as_cons()->car = val;
}

FORCE_INLINE
void set_cdr(Value cons, Value val)
{
    cons.as_cons()->cdr = val;
}

FORCE_INLINE
Value car(Value obj)
{
    if (obj.is_nil()) return obj;
    return obj.as_cons()->car;
}

FORCE_INLINE
Value cdr(Value obj)
{
    if (obj.is_nil()) return obj;
    return obj.as_cons()->cdr;
}

FLATTEN inline Value cddr(Value obj)   { return cdr(cdr(obj)); }
FLATTEN inline Value cdddr(Value obj)  { return cdr(cdr(cdr(obj))); }
FLATTEN inline Value cadr(Value obj)   { return car(cdr(obj)); }
FLATTEN inline Value caddr(Value obj)  { return car(cdr(cdr(obj))); }
FLATTEN inline Value cadddr(Value obj) { return car(cdr(cdr(cdr(obj)))); }
FLATTEN inline Value caar(Value obj)   { return car(car(obj)); }
FLATTEN inline Value cdar(Value obj)   { return cdr(car(obj)); }
FLATTEN inline Value first(Value obj)  { return car(obj); }
FLATTEN inline Value rest(Value obj)   { return cdr(obj); }
FLATTEN inline Value second(Value obj) { return cadr(obj); }
FLATTEN inline Value third(Value obj)  { return caddr(obj); }
FLATTEN inline Value fourth(Value obj) { return cadddr(obj); }

struct Symbol
{
    Symbol(const std::string &name)
        : m_name(name)
        , m_function(Value::nil())
        , m_package(nullptr)
    {}

    Symbol(const std::string &name, Package *package)
        : m_name(name)
        , m_function(Value::nil())
        , m_package(package)
    {}

    Symbol(const std::string &name, Value function, Package *package)
        : m_name(name)
        , m_function(function)
        , m_package(package)
    {}

    bool is_keyword() const;
    std::string qualified_name() const;
    const std::string &name() const
    {
        return m_name;
    }

    Value function() const
    {
        return m_function;
    }

    void function(Value new_func)
    {
        m_function = new_func;
    }

    Package *package() const
    {
        return m_package;
    }
  private:
    friend struct GC;
    std::string m_name;
    Value m_function;
    Package *m_package;
};

struct Structure
{
    Structure(Value type, Fixnum num_slots)
        : m_type(type)
        , m_num_slots(num_slots)
        , m_slots(new Value[num_slots])
    {}

    ~Structure()
    {
        delete[] m_slots;
    }

    Value type() const
    {
        return m_type;
    }

    Fixnum num_slots() const
    {
        return m_num_slots;
    }

    Value &slot_value(Fixnum slot)
    {
        return m_slots[slot];
    }

    Value type_name() const
    {
        return car(m_type);
    }

  private:
    friend struct GC;
    Value m_type;
    Fixnum m_num_slots;
    Value *m_slots;
};

struct Simple_Array
{
    Simple_Array(Value element_type, Fixnum capacity)
        : m_element_type(element_type)
        , m_capacity(capacity)
        , m_fill_pointer(capacity)
        , m_buffer(capacity <= 0 ? nullptr : new Value[capacity]{Value(0)})
    {}

    Simple_Array(Value element_type, Fixnum capacity, Fixnum fill_pointer)
        : m_element_type(element_type)
        , m_capacity(capacity)
        , m_fill_pointer(fill_pointer)
        , m_buffer(capacity <= 0 ? nullptr : new Value[capacity]{Value(0)})
    {}

    ~Simple_Array()
    {
        delete[] m_buffer;
    }

    Value element_type() const
    {
        return m_element_type;
    }

    Fixnum size() const
    {
        return m_fill_pointer;
    }

    Fixnum capacity() const
    {
        return m_capacity;
    }

    Value &at(Fixnum n) const
    {
        return m_buffer[n];
    }

    void push_back(Value value)
    {
        if (m_capacity == 0)
        {
            m_capacity = 8;
            m_buffer = new Value[m_capacity]{Value(0)};
        }
        else if (m_fill_pointer == m_capacity)
        {
            Fixnum new_capacity = m_fill_pointer * 1.5;
            auto new_buffer = new Value[new_capacity];
            memcpy(new_buffer, m_buffer, sizeof(Value) * m_fill_pointer);
            delete[] m_buffer;
            m_buffer = new_buffer;
            m_capacity = new_capacity;
        }
        m_buffer[m_fill_pointer++] = value;
    }

  private:
    friend struct GC;
    Value m_element_type;
    Fixnum m_capacity;
    Fixnum m_fill_pointer;
    Value *m_buffer;
};

struct File_Stream
{
    File_Stream(const std::string &path, std::ios_base::openmode mode)
        : m_stream(path, mode)
        , m_path(path)
        , m_mode(mode)
    {}

    ~File_Stream()
    {
        m_stream.flush();
    }

    std::fstream &stream()
    {
        return m_stream;
    }

    const std::string &path() const
    {
        return m_path;
    }

    std::ios_base::openmode mode() const
    {
        return m_mode;
    }

    int32_t peek_character()
    {
        if (m_ungetted.size() != 0)
        {
            return m_ungetted.back();
        }
        else
        {
            auto c = read_character();
            m_ungetted.push_back(c);
            return c;
        }
    }

    int32_t read_character()
    {
        if (m_ungetted.size() != 0)
        {
            auto c = m_ungetted.back();
            m_ungetted.pop_back();
            return c;
        }
        else {
            int32_t c = m_stream.get() & 0xff;
            if ((c & 0xf8) == 0)
            {
                c |= (m_stream.get() & 0xff) << 8;
                c |= (m_stream.get() & 0xff) << 16;
                c |= (m_stream.get() & 0xff) << 24;
            }
            else if ((c & 0xf0) == 0xe0)
            {
                c |= (m_stream.get() & 0xff) << 8;
                c |= (m_stream.get() & 0xff) << 16;
            }
            else if ((c & 0xe0) == 0xc0)
            {
                c |= (m_stream.get() & 0xff) << 8;
            }

            return c;
        }
    }

    Fixnum write_character(int32_t c)
    {
        if (m_stream.good())
        {
            auto p = reinterpret_cast<const char*>(&c);
            if ((c & 0xf8) == 0xf0)
            {
                m_stream.write(p, 4);
                return 4;
            }
            else if ((c & 0xf0) == 0xe0)
            {
                m_stream.write(p, 3);
                return 3;
            }
            else if ((c & 0xe0) == 0xc0)
            {
                m_stream.write(p, 2);
                return 2;
            }
            else
            {
                m_stream.write(p, 1);
                return 1;
            }
        }
        return 0;
    }

    void flush()
    {
        m_stream.flush();
    }

    auto tellg()
    {
        return m_stream.tellg();
    }

    void seekg(Fixnum offset, std::ios_base::seekdir dir)
    {
        m_stream.seekg(offset, dir);
    }

  private:
    std::fstream m_stream;
    std::vector<int32_t> m_ungetted;
    std::string m_path;
    std::ios_base::openmode m_mode;
};

struct Function
{
    struct Capture_Offset
    {
        uint32_t index;
        bool is_local;
        std::string name;
    };

    Function(std::vector<uint8_t> &&code,
             std::vector<uint32_t> &&entrypoints,
             std::vector<const Symbol*> &&parameters,
             std::vector<Capture_Offset> &&capture_offsets,
             uint32_t main_entry,
             uint32_t optionals_start_at,
             uint32_t num_locals,
             bool has_rest,
             bool has_optionals)
        : m_code(std::move(code))
        , m_entrypoints(std::move(entrypoints))
        , m_parameters(std::move(parameters))
        , m_capture_offsets(std::move(capture_offsets))
        , m_main_entry(main_entry)
        , m_optionals_start_at(optionals_start_at)
        , m_num_locals(num_locals)
        , m_has_rest(has_rest)
        , m_has_optionals(has_optionals)
    {}

    size_t arity() const
    {
        return m_parameters.size();
    }

    const uint8_t *main_entry() const
    {
        return &m_code[m_main_entry];
    }

    const uint8_t *entrypoint(uint32_t nth_param) const
    {
        if (nth_param >= arity())
        {
            return &m_code[m_main_entry];
        }
        auto ep = m_entrypoints[nth_param];
        return &m_code[ep];
    }

    const uint8_t *begin() const
    {
        return m_code.data();
    }

    const uint8_t *end() const
    {
        return m_code.data() + m_code.size();
    }

    const std::vector<const Symbol*> &parameters() const
    {
        return m_parameters;
    }

    const std::vector<Capture_Offset> &capture_offsets() const
    {
        return m_capture_offsets;
    }

    bool has_rest() const
    {
        return m_has_rest;
    }

    uint32_t rest_index() const
    {
        return m_parameters.size() - 1;
    }

    bool has_captures() const
    {
        return m_capture_offsets.size() != 0;
    }

    uint32_t optionals_start_at() const
    {
        return m_optionals_start_at;
    }

    bool has_optionals() const
    {
        return m_has_optionals;
    }

    bool is_too_many_args(uint32_t n) const
    {
        return !has_rest() && n > arity();
    }

    bool is_too_few_args(uint32_t n) const
    {
        if (has_optionals())
        {
            return n < optionals_start_at();
        }
        if (has_rest())
        {
            return n < rest_index();
        }
        return n < arity();
    }

    uint32_t num_locals() const
    {
        return m_num_locals;
    }

    // for gdb
    __attribute__((used, noinline))
    void ez_disassemble() const
    {
        bytecode::disassemble(std::cout, "Function", this, main_entry());
    }

  private:
    std::vector<uint8_t> m_code;
    std::vector<uint32_t> m_entrypoints;
    std::vector<const Symbol*> m_parameters;
    std::vector<Capture_Offset> m_capture_offsets;
    uint32_t m_main_entry;
    uint32_t m_optionals_start_at;
    uint32_t m_num_locals;
    bool m_has_rest;
    bool m_has_optionals;
};


struct Closure
{
    Closure(const Function *function)
        : m_function(function)
    {
        m_captures.resize(function->capture_offsets().size(), nullptr);
    }

    void capture_reference(size_t idx, Closure_Reference *p)
    {
        m_captures[idx] = p;
    }

    Value get_capture(size_t idx) const
    {
        return m_captures[idx]->value();
    }

    void set_capture(size_t idx, Value new_val)
    {
        m_captures[idx]->value() = new_val;
    }

    Closure_Reference *get_reference(size_t idx)
    {
        if (idx >= m_captures.size())
        {
            return nullptr;
        }
        return m_captures[idx];
    }

    const Function *function() const
    {
        return m_function;
    }

    const std::vector<Closure_Reference*> &captures() const
    {
        return m_captures;
    }

  private:
    std::vector<Closure_Reference*> m_captures;
    const Function *m_function;
};

struct Object
{
    Object_Type type() const
    {
        return m_type;
    }

    Symbol *symbol()
    {
        return as<Symbol>();
    }

    Closure *closure()
    {
        return as<Closure>();
    }

    Package *package()
    {
        return as<Package>();
    }

    File_Stream *file_stream()
    {
        return as<File_Stream>();
    }

    Simple_Array *simple_array()
    {
        return as<Simple_Array>();
    }

    Structure *structure()
    {
        return as<Structure>();
    }

    System_Pointer system_pointer()
    {
        return *as<System_Pointer>();
    }

    void system_pointer(System_Pointer new_val)
    {
        *as<System_Pointer>() = new_val;
    }

    System_Pointer pointer_ref()
    {
        return as<System_Pointer>();
    }

  private:
    friend struct GC;

    template<typename T>
    FORCE_INLINE
    T *as()
    {
        return reinterpret_cast<T*>(&m_data[0]);
    }

    template<typename T, typename... Args>
    FORCE_INLINE
    void construct_data(Args... args)
    {
        if constexpr (std::is_same<T, Symbol>::value)
        {
            m_type = Object_Type::Symbol;
        }
        else if constexpr (std::is_same<T, Closure>::value)
        {
            m_type = Object_Type::Closure;
        }
        else if constexpr (std::is_same<T, Package>::value)
        {
            m_type = Object_Type::Package;
        }
        else if constexpr (std::is_same<T, File_Stream>::value)
        {
            m_type = Object_Type::File_Stream;
        }
        else if constexpr (std::is_same<T, Simple_Array>::value)
        {
            m_type = Object_Type::Simple_Array;
        }
        else if constexpr (std::is_same<T, System_Pointer>::value)
        {
            m_type = Object_Type::System_Pointer;
        }
        else if constexpr (std::is_same<T, Structure>::value)
        {
            m_type = Object_Type::Structure;
        }
        else
        {
            static_assert(always_false<T>);
        }

        new (&m_data[0]) T{args...};
    }

    void destruct_data()
    {
        switch (m_type)
        {
            case Object_Type::Symbol: symbol()->~Symbol(); break;
            case Object_Type::Closure: closure()->~Closure(); break;
            case Object_Type::Package: break;
            case Object_Type::File_Stream: file_stream()->~File_Stream(); break;
            case Object_Type::Simple_Array: simple_array()->~Simple_Array(); break;
            case Object_Type::System_Pointer: break;
            case Object_Type::Structure: structure()->~Structure(); break;
        }
    }

    Object_Type m_type;
    char m_data[0];
};

static
std::string lisp_string_to_native_string(Value str)
{
    std::string ret;
    auto array = str.as_object()->simple_array();
    for (Fixnum i = 0; i < array->size(); ++i) {
        auto obj = array->at(i);
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


FORCE_INLINE
bool Value::is_type(Object_Type type) const noexcept
{
    return is_object() && as_object()->type() == type;
}

FORCE_INLINE
bool symbolp(Value v)
{
    return v.is_type(Object_Type::Symbol);
}

bool stringp(Value v);

struct GC
{
    GC()
        : m_marked(0)
        , m_total_consed(0)
        , m_time_spent_in_gc(0)
        , m_times_gc_has_run(0)
        , m_is_paused(true)
        , m_is_warmed_up(false)
    {
        m_free_small_bins.resize(SMALL_BINS_SIZE);
    }

    ~GC()
    {
        // Not the most elegant way to GC everything at exit but it works.
        for (auto gen : m_generations)
        {
            for (auto ref : *gen)
            {
                ref->marked = false;
                m_recent_allocations.push_back(ref);
            }
        }
        sweep();
    }

    struct Reference
    {
        enum Type : uint32_t
        {
            Cons,
            Object,
            Closure_Reference,
        };

        #if GC_NO_OPT
        static constexpr uint32_t MAGIC_CONSTANT = 0xFEEDFACE;
        uint32_t magic;
        #endif

        uint32_t size;

        uint32_t collections_survived : 20;
        uint32_t type : 10;
        uint32_t marking : 1;
        uint32_t marked : 1;

        #if GC_NO_OPT
        // This padding is required because we need data to be 8-byte aligned so that Value
        // is guaranteed to have 3 bits of tag storage.
        uint32_t _pad;
        #endif

        char data[0];

        template<typename T>
        T *as()
        {
            return reinterpret_cast<T*>(&data[0]);
        }
    };
    #if GC_NO_OPT
    static_assert(offsetof(Reference, magic) == 0);
    static_assert(offsetof(Reference, size) == 4);
    static_assert(offsetof(Reference, data) == 16);
    #else
    static_assert(offsetof(Reference, size) == 0);
    static_assert(offsetof(Reference, data) == 8);
    #endif


    Closure_Reference *make_closure_reference(Value *v)
    {
        auto ref = allocate<true>(sizeof(Closure_Reference));
        ref->type = Reference::Type::Closure_Reference;
        auto clos_ref = ref->as<Closure_Reference>();
        new (clos_ref) Closure_Reference{v};
        return clos_ref;
    }

    template<typename T, typename... Args>
    Value alloc_object(Args... args)
    {
        auto ref = allocate<true>(offsetof(Object, m_data) + sizeof(T));
        ref->type = Reference::Type::Object;
        auto obj = ref->as<Object>();
        obj->construct_data<T>(args...);
        return Value::wrap_object(obj);
    }

    template<typename T, typename... Args>
    Value alloc_object_unmanaged(Args... args)
    {
        auto ref = allocate<false>(offsetof(Object, m_data) + sizeof(T));
        ref->type = Reference::Type::Object;
        auto obj = ref->as<Object>();
        obj->construct_data<T>(args...);
        return Value::wrap_object(obj);
    }

    Value alloc_string(const char *str, Fixnum len);
    Value alloc_string(const std::string &str);

    Value cons(Value car, Value cdr)
    {
        auto ref = allocate<true>(sizeof(Cons));
        ref->type = Reference::Type::Cons;
        auto cons = ref->as<Cons>();
        new (cons) Cons{car, cdr};
        return Value::wrap_cons(cons);
    }

    bool paused() const
    {
        return m_is_paused;
    }

    bool pause()
    {
        auto tmp = m_is_paused;
        m_is_paused = true;
        return tmp;
    }

    void set_paused(bool new_val)
    {
        m_is_paused = new_val;
    }

    using Mark_Function = std::function<void(GC&)>;

    void register_marking_function(Mark_Function func)
    {
        m_mark_functions.push_back(func);
    }

    void mark()
    {
        auto curr = m_pinned_values.head();
        while (curr)
        {
            mark_value(curr->value);
            curr = curr->next;
        }

        for (auto it : m_mark_functions)
        {
            it(*this);
        }
    }

    inline void mark_symbol(Symbol *symbol)
    {
        mark_value(symbol->m_function);
    }

    void mark_closure_reference(Closure_Reference *clos_ref)
    {
        while (clos_ref)
        {
            auto ref = ptr_to_ref(clos_ref);
            if (ref && !ref->marking)
            {
                ref->marking = true;

                mark_value(clos_ref->value());

                ref->collections_survived++;
                ref->marked = true;
                ref->marking = false;
                m_marked++;
            }
            clos_ref = clos_ref->next;
        }
    }

    inline void mark_closure(Closure *closure)
    {
        for (auto clos_ref : closure->captures())
        {
            mark_closure_reference(clos_ref);
        }
    }

    inline void mark_simple_array(Simple_Array *simple_array);

    inline void mark_structure(Structure *structure)
    {
        for (Fixnum i = 0; i < structure->num_slots(); ++i)
        {
            mark_value(structure->slot_value(i));
        }
    }

    void mark_value(Value value)
    {
        auto ref = value_to_ref(value);
        if (!ref)
        {
            return;
        }

        if (ref->marking ||
            (ref->marked && ref->collections_survived <= GENERATIONAL_SURVIVOR_THRESHOLD))
        {
            return;
        }

        ref->marking = true;

        #if GC_DIAGNOSTICS > 1
        printf("Marking: %s: %p\n", repr(value).c_str(), (void*)value.bits());
        #endif
        if (value.is_cons())
        {
            auto cur = value;
            while (!cur.is_nil())
            {
                // This appears to be horribly recursive, but this marking function doesn't try to mark
                // values that are already being marked or have been marked already. This makes the
                // usual list case fairly linear and the only real risk of exceeding the recursion limit
                // would be from a very heavily left-leaning tree.
                mark_value(car(cur));
                auto cur_cons_ref = value_to_ref(cur);
                cur_cons_ref->marked = true;
                cur_cons_ref->collections_survived++;
                //mark_value(cur);
                cur = cdr(cur);
                if (!cur.is_cons() && !cur.is_nil())
                {
                    mark_value(cur);
                    break;
                }
            }
        }
        else
        {
            auto obj = value.as_object();
            switch (obj->type())
            {
                case Object_Type::Symbol:
                    mark_symbol(obj->symbol());
                    break;
                case Object_Type::Closure:
                    mark_closure(obj->closure());
                    break;
                case Object_Type::Package:
                    break;
                case Object_Type::File_Stream:
                    break;
                case Object_Type::Simple_Array:
                    // @TODO: Optimize scanning array if it's a string?
                    mark_simple_array(obj->simple_array());
                    break;
                case Object_Type::System_Pointer:
                    break;
                case Object_Type::Structure:
                    mark_structure(obj->structure());
                    break;
            }
        }

        ref->collections_survived++;
        ref->marked = true;
        ref->marking = false;
        m_marked++;
    }

    size_t sweep()
    {
        #if GC_DIAGNOSTICS > 0
        fprintf(stderr, "Running sweep... %zu ", m_recent_allocations.size());
        #endif
        auto &current_gen = current_generation();

        size_t moved_to_generation = 0;
        size_t freed = 0;
        for (size_t i = 0; i < m_recent_allocations.size();)
        {
            auto r = m_recent_allocations[i];
            if (r->marked == false)
            {
                switch (r->type)
                {
                    case Reference::Type::Cons:
                        break;
                    case Reference::Type::Object:
                        r->as<Object>()->~Object();
                        break;
                    case Reference::Type::Closure_Reference:
                        break;
                }
                #if DEBUG > 1
                memset(r->data, 0xCC, r->size);
                #endif
                get_bin(r->size).push_back(r);
                m_recent_allocations[i] = m_recent_allocations.back();
                m_recent_allocations.pop_back();
                freed++;
            }
            else if (r->collections_survived >= GENERATIONAL_SURVIVOR_THRESHOLD)
            {
                r->marked = false;
                current_gen.push_back(r);
                m_recent_allocations[i] = m_recent_allocations.back();
                m_recent_allocations.pop_back();
                moved_to_generation++;
            }
            else
            {
                r->marked = false;
                i++;
            }
        }

        #if GC_DIAGNOSTICS > 0
        fprintf(stderr, " Moved: %zu, Freed: %zu, Generation (%zu) Size: %zu\n",
                moved_to_generation, freed, m_generations.size(), current_gen.size());
        #endif

        if (current_gen.size() >= NEW_GENERATION_THRESHOLD)
        {
            make_new_generation();
        }
        return freed;
    }

    size_t mark_and_sweep()
    {
        #if GC_DIAGNOSTICS > 0
        fprintf(stderr, "New mark started.\n");
        #endif
        m_marked = 0;
        mark();
        #if GC_DIAGNOSTICS > 0
        fprintf(stderr, "Mark phase finished.\n");
        fprintf(stderr, "Marked %zu\n", m_marked);
        #endif
        return sweep();
    }

    template<typename... Args>
    Value list(Args... args)
    {
        if constexpr (sizeof...(args) == 0)
        {
            return Value::nil();
        }

        std::array<Ptr_Handle, sizeof...(args)> arg_handles = {pin_value(args)...};
        std::array<Value, sizeof...(args)> arg_values = {args...};

        auto head = cons(arg_values[0], Value::nil());
        auto head_handle = pin_value(head);
        unpin_value(arg_handles[0]);
        auto curr = head;

        for (size_t i = 1; i < arg_values.size(); ++i)
        {
            set_cdr(curr, cons(arg_values[i], Value::nil()));
            unpin_value(arg_handles[i]);
            curr = cdr(curr);
        }

        unpin_value(head_handle);
        return head;
    }

    struct Ptr_Handle
    {
        void *ptr;
    };

    Ptr_Handle pin_value(Value value)
    {
        if (value.is_garbage_collected())
        {
            return {m_pinned_values.push_back(value)};
        }
        return {nullptr};
    }

    void unpin_value(Ptr_Handle &handle)
    {
        if (handle.ptr)
        {
            auto node = static_cast<Pinned_Values_List::Node*>(handle.ptr);
            handle.ptr = nullptr;
            m_pinned_values.unlink_and_free(node);
        }
    }

    size_t get_consed() const
    {
        return m_total_consed;
    }

    size_t get_time_spent_in_gc() const
    {
        return m_time_spent_in_gc;
    }

    size_t get_times_gc_has_run() const
    {
        return m_times_gc_has_run;
    }

  private:

    Reference *ptr_to_ref(void *ptr)
    {
        if (!ptr)
        {
            return nullptr;
        }

        auto p = reinterpret_cast<uint8_t*>(ptr) - offsetof(Reference, data);

        auto ref = reinterpret_cast<Reference*>(p);
        #if GC_NO_OPT
        if (ref->magic != Reference::MAGIC_CONSTANT)
        {
            return nullptr;
        }
        #endif
        return ref;
    }

    Reference *value_to_ref(Value val)
    {
        if (!val.is_garbage_collected())
        {
            return nullptr;
        }

        auto ptr = val.unwrap_pointer();
        return ptr_to_ref(ptr);
    }

    FORCE_INLINE
    void maybe_mark_and_sweep()
    {
        if (!m_is_paused && m_is_warmed_up)
        {
            // If a sweep doesn't free enough then to minimize successive garbage collections
            // we'll revert to a no longer warmed-up state which will only switch back when
            // there are enough recent allocations.
            auto start_time = std::chrono::high_resolution_clock::now();
            m_is_warmed_up = mark_and_sweep() > GC_COOLDOWN_THRESHOLD;
            auto end_time = std::chrono::high_resolution_clock::now();
            auto duration = std::chrono::duration_cast<std::chrono::microseconds>( end_time - start_time ).count();
            m_time_spent_in_gc += duration;
            m_times_gc_has_run++;
        }
    }

    template<bool is_managed>
    Reference *allocate(size_t size)
    {
        if constexpr (is_managed)
        {
            m_total_consed += size;
            maybe_mark_and_sweep();
        }

        Reference *ref = nullptr;
        if constexpr (is_managed)
        {
            auto &bin = get_bin(size);
            for (size_t i = 0; i < bin.size(); ++i)
            {
                if (bin[i]->size >= size)
                {
                    ref = bin[i];
                    bin[i] = bin.back();
                    bin.pop_back();
                    break;
                }
            }
        }

        if (!ref)
        {
            ref = static_cast<Reference*>(::operator new(offsetof(Reference, data) + size));
            ref->size = size;
        }

        #if GC_NO_OPT
        ref->magic = Reference::MAGIC_CONSTANT;
        #endif
        ref->collections_survived = 0;
        ref->marking = false;
        ref->marked = false;

        if constexpr (is_managed)
        {
            m_recent_allocations.push_back(ref);
            m_is_warmed_up |= m_recent_allocations.size() >= GC_WARMUP_THRESHOLD;
        }

        return ref;
    }

    using Generation = std::vector<Reference*>;

    Generation &current_generation()
    {
        if (m_generations.size() == 0)
        {
            make_new_generation();
        }
        return *m_generations.back();
    }

    void make_new_generation()
    {
        m_generations.push_back(new Generation);
    }

    std::vector<Reference*> &get_bin(size_t size)
    {
        if (size > SMALL_BINS_SIZE)
        {
            return m_free_large_bin;
        }
        return m_free_small_bins[size];
    }

    static constexpr auto GC_WARMUP_THRESHOLD = 1000000;
    // attempt to keep GC from running many times in succession
    static constexpr auto GC_COOLDOWN_THRESHOLD = GC_WARMUP_THRESHOLD * 0.06;
    static constexpr auto NEW_GENERATION_THRESHOLD = GC_WARMUP_THRESHOLD * 1.5;
    // how many GC runs does a Reference need to survive before being moved to the current generation?
    static constexpr auto GENERATIONAL_SURVIVOR_THRESHOLD = 2;
    static constexpr auto SMALL_BINS_SIZE = 256;

    std::vector<Generation*> m_generations;
    std::vector<Reference*> m_recent_allocations;
    //std::vector<Reference*> m_free_references;
    std::vector<std::vector<Reference*>> m_free_small_bins;
    std::vector<Reference*> m_free_large_bin;
    struct Pinned_Values_List
    {
        struct Node
        {
            Value value;
            Node *next;
            Node *prev;
        };
        Pinned_Values_List()
            : m_head(nullptr)
            , m_tail(nullptr)
        {
            constexpr size_t n = 1000;
            m_free_node_pool.reserve(n);
            for (size_t i = 0; i < n; ++i)
            {
                m_free_node_pool.push_back(new Node {Value::wrap_fixnum(0), nullptr, nullptr});
            }
        }

        Node *head()
        {
            return m_head;
        }

        Node *tail()
        {
            return m_tail;
        }

        Node *push_back(Value value)
        {
            if (m_head)
            {
                m_tail->next = alloc_node(value, m_tail);
                m_tail = m_tail->next;
            }
            else
            {
                m_head = alloc_node(value, nullptr);
                m_tail = m_head;
            }
            return m_tail;
        }

        void unlink_and_free(Node *node)
        {
            auto prev = node->prev;
            auto next = node->next;
            if (prev)
            {
                prev->next = next;
            }
            if (next)
            {
                next->prev = prev;
            }
            if (node == m_head)
            {
                m_head = next;
            }
            else if (node == m_tail)
            {
                m_tail = prev;
            }
            free_node(node);
        }

      private:
        inline Node *alloc_node(Value value, Node *prev)
        {
            if (m_free_node_pool.empty())
            {
                return new Node {value, nullptr, prev};
            }
            auto node = m_free_node_pool.back();
            new (node) Node{value, nullptr, prev};
            m_free_node_pool.pop_back();
            return node;
        }

        inline void free_node(Node *node)
        {
            m_free_node_pool.push_back(node);
        }

        Node *m_head;
        Node *m_tail;
        std::vector<Node*> m_free_node_pool;
    } m_pinned_values;
    std::vector<Mark_Function> m_mark_functions;
    size_t m_marked;
    size_t m_total_consed;
    size_t m_time_spent_in_gc;
    size_t m_times_gc_has_run;
    bool m_is_paused;
    bool m_is_warmed_up;
} gc;

#define GC_GUARD()                              \
    auto __gc_guard__paused = lisp::gc.pause()  \

#define GC_UNGUARD() lisp::gc.set_paused(__gc_guard__paused)

static FORCE_INLINE
Value to_list(const Value *vals, uint32_t nvals)
{
    switch (nvals)
    {
        case 0: return Value::nil();
        case 1: return gc.list(vals[0]);
        case 2: return gc.list(vals[0], vals[1]);
        case 3: return gc.list(vals[0], vals[1], vals[2]);
        case 4: return gc.list(vals[0], vals[1], vals[2], vals[3]);
        case 5: return gc.list(vals[0], vals[1], vals[2], vals[3], vals[4]);
        case 6: return gc.list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5]);
        case 7: return gc.list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]);
        case 8: return gc.list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]);
    }
    GC_GUARD();
    auto head = gc.list(vals[0]);
    auto current = head;
    for (uint32_t i = 1; i < nvals; ++i)
    {
        set_cdr(current, gc.cons(vals[i], Value::nil()));
        current = cdr(current);
    }
    GC_UNGUARD();
    return head;
}

static FORCE_INLINE
Value to_list(const std::vector<Value> &vals)
{
    return to_list(vals.data(), vals.size());
}

static FORCE_INLINE
std::vector<Value> to_vector(Value list)
{
    std::vector<Value> v;
    while (!list.is_nil())
    {
        v.push_back(car(list));
        list = cdr(list);
    }
    return v;
}

struct Package
{
    enum class Symbol_Location_Type
    {
        Internal,
        External,
        Inherited
    };

    using Name_Symbol_Map = std::unordered_map<std::string, Value>;

    const std::string &name() const
    {
        return m_name;
    }

    const Name_Symbol_Map &symbols() const
    {
        return m_symbols;
    }

    const Name_Symbol_Map &exported_symbols() const
    {
        return m_exported_symbols;
    }

    void inherit(Package *pkg)
    {
        if (pkg)
        {
            m_inherit_from.push_back(pkg);
        }
    }

    Value intern_symbol(const std::string &name)
    {
        Value inherited;
        if (find_inherited(name, inherited))
        {
            return inherited;
        }

        auto it = m_symbols.find(name);
        if (it != m_symbols.end())
        {
            return it->second;
        }

        auto value = gc.alloc_object<Symbol>(name, Value::nil(), this);
        m_symbols[name] = value;
        return value;
    }

    bool import_symbol(Value symbol_val)
    {
        auto symbol = symbol_val.as_object()->symbol();
        if (symbol->package() == this)
        {
            return true;
        }

        auto it = m_symbols.find(symbol->name());
        if (it != m_symbols.end())
        {
            return false;
        }

        m_symbols[symbol->name()] = symbol_val;
        return true;
    }

    Value export_symbol(const std::string &name)
    {
        auto value = intern_symbol(name);
        m_exported_symbols[name] = value;
        return value;
    }

    bool find_inherited(const std::string &name, Value &out_value)
    {
        for (auto it = m_inherit_from.rbegin();
             it != m_inherit_from.rend();
             ++it)
        {
            if ((*it)->is_exported(name, &out_value))
            {
                return true;
            }
        }
        return false;
    }

    bool is_exported(const std::string &name, Value *opt_out = nullptr) const
    {
        auto it = m_exported_symbols.find(name);
        if (it != m_exported_symbols.end())
        {
            if (opt_out != nullptr)
            {
                *opt_out = it->second;
            }
            return true;
        }
        return false;
    }


    bool find_symbol(const std::string &name, Value &out_value, Symbol_Location_Type *out_loc = nullptr)
    {
        if (find_inherited(name, out_value))
        {
            if (out_loc)
            {
                *out_loc = Symbol_Location_Type::Inherited;
            }
            return true;
        }
        auto it = m_symbols.find(name);
        if (it != m_symbols.end())
        {
            out_value = it->second;
            if (out_loc)
            {
                *out_loc = is_exported(name)
                    ? Symbol_Location_Type::External
                    : Symbol_Location_Type::Internal;
            }
            return true;
        }
        return false;
    }

    Value find_or_intern_symbol(const std::string &name)
    {
        Value res;
        if (find_symbol(name, res))
        {
            return res;
        }
        return intern_symbol(name);
    }

    Value as_lisp_value()
    {
        return m_this_package;
    }

    Package(const std::string &name) : m_name(name) {}
  private:
    friend struct Package_Registry;

    void gc_mark(GC &gc)
    {
        for (auto &[k, v] : m_symbols)
        {
            gc.mark_value(v);
        }
    }

    std::string m_name;
    Name_Symbol_Map m_symbols;
    Name_Symbol_Map m_exported_symbols;
    std::vector<Package*> m_inherit_from;
    Value m_this_package;
};


std::string Symbol::qualified_name() const
{
    std::string res;
    if (m_package)
    {
        if (is_keyword())
        {
            res += ":";
        }
        else {
            res += m_package->name();
            if (m_package->is_exported(m_name))
            {
                res += ":";
            }
            else
            {
                res += "::";
            }
        }
        res += m_name;
    }
    else
    {
        res += "#:";
        res += m_name;
    }
    return res;
}

struct Package_Registry
{
    Package_Registry() : m_current_package(nullptr)
    {
        gc.register_marking_function([this](GC &gc) {this->gc_mark(gc);});
    }

    Package *find(const std::string &name) const
    {
        auto it = m_packages.find(name);
        if (it != m_packages.end())
        {
            return it->second;
        }
        return nullptr;
    }

    Package *find_or_create(const std::string &name)
    {
        auto package = find(name);
        if (package)
        {
            return package;
        }

        auto package_val = gc.alloc_object_unmanaged<Package>(name);
        package = package_val.as_object()->package();
        m_packages[name] = package;
        package->m_this_package = package_val;
        return package;
    }

    Package *current() const
    {
        return m_current_package;
    }

    void in_package(Package *p)
    {
        m_current_package = p;
    }

    bool package_exists(const std::string &name) const
    {
        return m_packages.find(name) != m_packages.end();
    }

    void alias_package(const std::string &alias, Package *to)
    {
        auto it = m_packages.find(alias);
        if (it != m_packages.end() && it->second != to)
        {
            fprintf(stderr, "WARNING: Overwriting package %s with alias %s -> %s\n",
                    it->second->name().c_str(), alias.c_str(), to->name().c_str());
        }
        m_packages[alias] = to;
    }

  private:
    void gc_mark(GC &gc)
    {
        for (auto &[k, pkg] : m_packages)
        {
            pkg->gc_mark(gc);
        }
    }
    std::unordered_map<std::string, Package*> m_packages;
    Package *m_current_package;
};

namespace bytecode
{
struct Emitter;
}

struct VM_State
{
    struct Call_Frame
    {
        const uint8_t *ip;
        Value current_closure;
        Value *locals;
        Value *stack_top;
    };

    struct Exception
    {
        Exception(Value what) : what(what) {}

        Value what;
    };

    struct Unhandleable_Exception : Exception
    {
        const char *msg;
    };

    struct Signal_Exception : Exception
    {
        Signal_Exception(Value what)
            : Exception(what)
            , ip(nullptr)
            , stack_trace_top(nullptr)
            , stack_trace_bottom(nullptr)
        {}

        Signal_Exception(Value what, const uint8_t *ip, Call_Frame *stack_top, Call_Frame *stack_bottom)
            : Exception(what)
            , ip(ip)
            , stack_trace_top(stack_top)
            , stack_trace_bottom(stack_bottom)
        {}

        const uint8_t *ip;
        const Call_Frame *stack_trace_top;
        const Call_Frame *stack_trace_bottom;
    };


    VM_State()
        : m_current_closure(Value::nil())
        , m_open_closure_references(nullptr)
    {
        m_locals = m_stack_top = m_stack_bottom = new Value[0x100000];
        m_call_frame_top = m_call_frame_bottom = new Call_Frame[0x10000];
        gc.register_marking_function([this](GC &gc) { gc_mark(gc); });
    }

    Value &param_top()
    {
        return *(m_stack_top - 1);
    }

    Value &param_top(int32_t n)
    {
        return *(m_stack_top - 1 + n);
    }

    void push_param(Value v)
    {
        *m_stack_top++ = v;
    }

    Value pop_param()
    {
        return *--m_stack_top;
    }

    void pop_params(uint32_t n)
    {
        m_stack_top -= n;
    }

    void push_frame(const uint8_t *ip, uint32_t nargs)
    {
        Call_Frame frame {
            ip,
            m_current_closure,
            m_locals,
            m_stack_top - nargs
        };
        *m_call_frame_top++ = frame;
    }

    Call_Frame pop_frame()
    {
        return *--m_call_frame_top;
    }

    Call_Frame &frame_top()
    {
        return *(m_call_frame_top - 1);
    }

    void set_frame(const Call_Frame &frame)
    {
        m_current_closure = frame.current_closure;
        m_locals = frame.locals;
        m_stack_top = frame.stack_top;
    }

    Call_Frame &get_frame(uint32_t idx)
    {
        return *(m_call_frame_top - idx);
    }

    const Value *stack_top() const
    {
        return m_stack_top;
    }

    const Value *stack_bottom() const
    {
        return m_stack_bottom;
    }

    const Call_Frame *call_frame_top() const
    {
        return m_call_frame_top;
    }

    const Call_Frame *call_frame_bottom() const
    {
        return m_call_frame_bottom;
    }


    template<bool debuggable>
    const uint8_t *execute_impl(const uint8_t *ip);

    inline const uint8_t *execute(const uint8_t *ip);

    Value call_lisp_function(Value function_or_symbol, Value *args, uint32_t nargs);

    void debug_dump(std::ostream &out, const std::string &tag, const uint8_t *ip, bool full = false) const;
    int stack_dump(std::ostream &out, size_t max_size = 15) const;

    struct Signal_Handler
    {
        Value tag;
        Value handler;
    };

    struct Handler_Case
    {
        Value *stack;
        Call_Frame *frame;
        std::vector<Signal_Handler> handlers;
    };

    struct Save_State
    {
        Value current_closure;
        Value *locals;
        Value *stack_top, *stack_bottom;
        Call_Frame *call_frame_top, *call_frame_bottom;
        Closure_Reference *open_closure_references;
        std::vector<Handler_Case> handler_cases;
    };

    Save_State save()
    {
        return {
            m_current_closure,
            m_locals,
            m_stack_top, m_stack_bottom,
            m_call_frame_top, m_call_frame_bottom,
            m_open_closure_references,
            m_handler_cases
        };
    }

    void restore(Save_State &state)
    {
        m_current_closure = state.current_closure;
        m_locals = state.locals;
        m_stack_top = state.stack_top;
        m_stack_bottom = state.stack_bottom;
        m_call_frame_top = state.call_frame_top;
        m_call_frame_bottom = state.call_frame_bottom;
        m_open_closure_references = state.open_closure_references;
        m_handler_cases = state.handler_cases;
    }

    __attribute__((used))
    __attribute__((noinline))
    void ez_debug(const uint8_t *ip)
    {
        debug_dump(std::cout, "EZ DEBUG", ip, true);
    }

    #if PROFILE_OPCODE_PAIRS
    const std::unordered_map<Opcode_Pair, int> &opcode_pairs() const
    {
        return m_opcode_pairs;
    }
    #endif

  private:

    void push_handler_case(std::vector<Signal_Handler> &&handlers)
    {
        m_handler_cases.push_back({m_stack_top, m_call_frame_top, std::move(handlers)});
    }

    void pop_handler_case()
    {
        m_handler_cases.pop_back();
    }

    bool find_handler(Value tag, bool auto_pop, Handler_Case &out_case_state, Signal_Handler &out_handler);

    void gc_mark(GC &gc)
    {
        for (auto p = m_stack_bottom; p != m_stack_top; ++p)
        {
            gc.mark_value(*p);
        }

        // Ensure our stack of closures don't accidently get GC
        for (auto p = m_call_frame_bottom; p != m_call_frame_top; ++p)
        {
            gc.mark_value(p->current_closure);
        }

        gc.mark_value(m_current_closure);

        gc.mark_closure_reference(m_open_closure_references);

        for (auto &handler_case : m_handler_cases)
        {
            for (auto &handler : handler_case.handlers)
            {
                gc.mark_value(handler.tag);
                gc.mark_value(handler.handler);
            }
        }
    }

    Closure_Reference *capture_closure_reference(Value *local)
    {
        Closure_Reference* prev_ref = nullptr;
        auto curr_ref = m_open_closure_references;

        while (curr_ref != nullptr && std::greater()(curr_ref->location(), local))
        {
            prev_ref = curr_ref;
            curr_ref = curr_ref->next;
        }

        if (curr_ref != nullptr && curr_ref->location() == local) return curr_ref;

        auto new_ref = gc.make_closure_reference(local);
        new_ref->next = curr_ref;

        if (prev_ref == nullptr)
        {
            m_open_closure_references = new_ref;
        }
        else
        {
            prev_ref->next = new_ref;
        }

        return new_ref;
    }

    void close_values(Value *end)
    {
        while (m_open_closure_references != nullptr
               && m_open_closure_references->location() >= end)
        {
            m_open_closure_references->close();
            m_open_closure_references = m_open_closure_references->next;
        }
    }


    Value m_current_closure;
    Value *m_locals;
    Value *m_stack_top, *m_stack_bottom;
    Call_Frame *m_call_frame_top, *m_call_frame_bottom;
    Closure_Reference *m_open_closure_references;
    std::vector<Handler_Case> m_handler_cases;

    struct Call_Lisp_From_Native_Stub
    {
        Call_Lisp_From_Native_Stub()
            : emitter(nullptr)
            , nargs_offset(0)
            , function_offset(0)
        {}
        bytecode::Emitter *emitter;
        uint32_t nargs_offset;
        uint32_t function_offset;
    } m_stub;

    #if PROFILE_OPCODE_PAIRS
    std::unordered_map<Opcode_Pair, int> m_opcode_pairs;
    #endif

};
static VM_State *THE_LISP_VM;
static Value macro_expand_impl(Value obj, VM_State &vm);

struct Runtime_Globals
{
    Runtime_Globals()
    {
        gc.register_marking_function([this](GC &gc) {this->gc_mark(gc);});
    }

    void gc_mark(GC &gc)
    {
        for (auto it : global_value_slots)
        {
            gc.mark_value(it);
        }

        for (auto it : literal_object_slots)
        {
            gc.mark_value(it);
        }

        for (auto &[k, val] : macros)
        {
            gc.mark_value(val);
        }
    }

    Package *kernel()
    {
        return packages.find_or_create("KERNEL");
    }

    Package *keyword()
    {
        return packages.find_or_create("KEYWORD");
    }

    Package *core()
    {
        return packages.find_or_create("LISPYBOI");
    }

    Package *user()
    {
        return packages.find_or_create("LISPYBOI-USER");
    }

    Value get_keyword(const std::string &symbol_name)
    {
        return keyword()->intern_symbol(symbol_name);
    }

    Value get_symbol(const std::string &symbol_name)
    {
        return packages.current()->find_or_intern_symbol(symbol_name);
    }

    void resize_globals(size_t newsize)
    {
        global_value_slots.resize(newsize);
    }

    Value s_T;
    Value s_IF;
    Value s_OR;
    Value s_FIXNUM;
    Value s_CONS;
    Value s_LIST;
    Value s_CHARACTER;
    Value s_SYMBOL;
    Value s_STRING;
    Value s_FUNCTION;
    Value s_BOOLEAN;
    Value s_STRUCTURE;
    Value s_PACKAGE;
    Value s_FILE_STREAM;
    Value s_SIMPLE_ARRAY;
    Value s_SYSTEM_POINTER;
    Value s_QUOTE;
    Value s_QUASIQUOTE;
    Value s_UNQUOTE;
    Value s_UNQUOTE_SPLICING;
    Value s_TYPE_ERROR;
    Value s_SIMPLE_ERROR;
    Value s_aOPTIONAL;
    Value s_aREST;
    Value s_aBODY;

    Value s_NULL;
    Value s_DIVIDE_BY_ZERO_ERROR;
    Value s_INDEX_OUT_OF_BOUNDS_ERROR;
    Value s_END_OF_FILE;
    Value s_BIT;
    Value s_OVERWRITE;
    Value s_APPEND;
    Value s_READ;
    Value s_MARSHAL_ERROR;
    Value s_BEGINNING;
    Value s_END;
    Value s_CURRENT;

    Value s_pLAMBDA;
    Value s_pCAR;
    Value s_pCDR;
    Value s_pCONS;
    Value s_pEQ;
    Value s_pRPLACA;
    Value s_pRPLACD;
    Value s_pSETQ;
    Value s_pAREF;
    Value s_pASET;
    Value s_pDEBUGGER;
    Value s_pAPPLY;
    Value s_pFUNCALL;
    Value s_pTAGBODY;
    Value s_pGO;
    Value s_pSIGNAL;
    Value s_pHANDLER_CASE;
    Value s_pDEFINE_MACRO;

    Package_Registry packages;
    std::vector<Value> global_value_slots;
    std::vector<Value> literal_object_slots;
    std::unordered_map<Symbol*, Value> macros;

    struct Debugger
    {
        enum Command
        {
            Continue,
            Step_Into,
            Step_Over,
        };

        Debugger()
            : addr0(nullptr)
            , addr1(nullptr)
            , command(Command::Continue)
            , breaking(false)
        {}

        const uint8_t *addr0;
        const uint8_t *addr1;
        Command command;
        bool breaking;
    } debugger;
} g;

inline const uint8_t *VM_State::execute(const uint8_t *ip)
{
    #if USE_COMPUTED_GOTOS
    if (g.debugger.breaking)
    {
        return execute_impl<true>(ip);
    }
    return execute_impl<false>(ip);
    #else
    return execute_impl<true>(ip);
    #endif
}

inline void GC::mark_simple_array(Simple_Array *simple_array)
{
    const auto t = simple_array->element_type();
    if (t != g.s_CHARACTER && t != g.s_FIXNUM)
    {
        for (Fixnum i = 0; i < simple_array->size(); ++i)
        {
            mark_value(simple_array->at(i));
        }
    }
}

static
std::string repr(Value value)
{
    if (value.is_fixnum())
    {
        return std::to_string(value.as_fixnum());
    }

    if (value.is_nil())
    {
        return "NIL";
    }

    if (value.is_cons())
    {
        std::string res = "(";
        res += repr(car(value));
        value = cdr(value);
        while (value.is_cons() && !value.is_nil())
        {
            res += " ";
            res += repr(car(value));
            value = cdr(value);
        }
        if (!value.is_cons() && !value.is_nil())
        {
            res += " . ";
            res += repr(value);
        }
        res += ")";
        return res;
    }

    if (value.is_character())
    {
        auto codepoint = value.as_character();
        switch (codepoint)
        {
            default:   return std::string("#\\") + reinterpret_cast<const char*>(&codepoint);
            case ' ':  return "#\\Space";
            case '\t': return "#\\Tab";
            case '\n': return "#\\Newline";
            case '\r': return "#\\Return";
        }
    }

    if (value.is_object())
    {
        auto obj = value.as_object();
        switch (obj->type())
        {
            case Object_Type::Symbol:
            {
                auto symbol = obj->symbol();
                //return symbol->qualified_name();
                std::string str;
                if (symbol->is_keyword())
                {
                    str += ":";
                }
                else if (symbol->package() == nullptr)
                {
                    str += "#:";
                }
                return str + obj->symbol()->name();
            }
            case Object_Type::Closure:
            {
                std::stringstream ss;
                auto clos = obj->closure();
                ss << "#<LAMBDA " << std::hex << reinterpret_cast<uintptr_t>(clos)
                   << " -> {" << reinterpret_cast<uintptr_t>(clos->function()->begin()) << "}>";
                return ss.str();
            }
            case Object_Type::Package:
            {
                std::string res;
                res += "#<PACKAGE ";
                res += obj->package()->name();
                res += ">";
                return res;
            }
            case Object_Type::File_Stream: return "#<FILE-STREAM>";
            case Object_Type::Simple_Array:
            {
                auto array = obj->simple_array();
                if (array->element_type() == g.s_CHARACTER)
                {
                    auto str = lisp_string_to_native_string(value);
                    std::string res;
                    res += "\"";
                    for (auto c : str)
                    {
                        switch (c)
                        {
                            default:
                                res += c;
                                break;
                            case '"':
                                res += "\\\"";
                                break;
                            case '\r':
                                res += "\\r";
                                break;
                            case '\t':
                                res += "\\t";
                                break;
                            case '\n':
                                res += "\\n";
                                break;
                        }
                    }
                    res += "\"";
                    return res;
                }
                else
                {
                    std::string res;
                    res += "#(";
                    if (array->size() > 0)
                    {
                        res += repr(array->at(0));
                    }
                    for (Fixnum i = 1; i < array->size(); ++i)
                    {
                        res += " ";
                        res += repr(array->at(i));
                    }
                    res += ")";
                    return res;
                }
            }
            case Object_Type::System_Pointer:
            {
                std::stringstream ss;
                ss << "#<SYSTEM-POINTER 0x"
                   << std::hex << reinterpret_cast<uintptr_t>(obj->system_pointer())
                   << ">";
                return ss.str();
            }
            case Object_Type::Structure: return "#<STRUCTURE>";
        }
    }

    if (value.is_lisp_primitive())
    {
        return "#<PRIMITIVE>";
    }
    return "#<!REPR WTF!>";
}

bool Symbol::is_keyword() const
{
    return m_package == g.keyword();
}

FORCE_INLINE
bool stringp(Value v)
{
    return v.is_type(Object_Type::Simple_Array) &&
        v.as_object()->simple_array()->element_type() == g.s_CHARACTER;
}


Value GC::alloc_string(const char *str, Fixnum len)
{
    std::vector<uint32_t> codepoints;
    // valid utf-8 codepoint enumeration
    for(Fixnum i = 0; i < len;)
    {
        int cp_len = 1;
        if ((str[i] & 0xf8) == 0xf0)
        {
            cp_len = 4;
        }
        else if ((str[i] & 0xf0) == 0xe0)
        {
            cp_len = 3;
        }
        else if ((str[i] & 0xe0) == 0xc0)
        {
            cp_len = 2;
        }
        if ((i + cp_len) > len)
        {
            cp_len = 1;
        }

        int32_t codepoint = 0;
        switch (cp_len)
        {
            // neat use of a fallthrough.
            case 4: codepoint |= (str[i+3] & 0xff) << 24;
            case 3: codepoint |= (str[i+2] & 0xff) << 16;
            case 2: codepoint |= (str[i+1] & 0xff) <<  8;
            case 1: codepoint |= (str[i+0] & 0xff) <<  0;
        }
        codepoints.push_back(codepoint);
        i += cp_len;
    }
    auto obj = alloc_object<Simple_Array>(g.s_CHARACTER, static_cast<Fixnum>(codepoints.size()));
    auto array = obj.as_object()->simple_array();
    for (size_t i = 0; i < codepoints.size(); ++i)
    {
        array->at(i) = Value::wrap_character(codepoints[i]);
    }
    return obj;
}

Value GC::alloc_string(const std::string &str)
{
    return alloc_string(str.data(), str.size());
}

namespace compiler
{

struct Scope;

struct Variable
{
    const Symbol *symbol() const
    {
        return m_symbol;
    }

    bool is_captured() const
    {
        return m_is_captured;
    }

    bool is_global() const;

    Scope *scope()
    {
        return m_scope;
    }

  private:
    friend struct Scope;
    Variable(const Symbol *symbol, Scope *scope)
        : m_symbol(symbol)
        , m_scope(scope)
    {}
    const Symbol *m_symbol;
    Scope *m_scope;
    bool m_is_captured;
};

struct Scope
{
    struct Capture_Info
    {
        Symbol *symbol;
        uint32_t index;
        bool is_local;
    };

    Scope()
        : m_parent(nullptr)
    {}

    Scope(Scope *parent)
        : m_parent(parent)
        , m_stack_space_needed(0)
    {}

    bool is_root() const
    {
        return m_parent == nullptr;
    }

    Scope *get_root()
    {
        auto p = this;
        while (p->m_parent)
        {
            p = p->m_parent;
        }
        return p;
    }

    Scope *parent()
    {
        return m_parent;
    }

    Scope *push_scope()
    {
        return new Scope(this);
    }

    const std::vector<Capture_Info> &capture_info() const
    {
        return m_captures;
    }

    const std::vector<Variable*> &locals() const
    {
        return m_locals;
    }

    const size_t stack_space_needed() const
    {
        return m_stack_space_needed;
    }

    Variable *create_variable(const Symbol *symbol, uint32_t *opt_out_idx = nullptr)
    {
        auto var = new Variable(symbol, this);
        if (opt_out_idx != nullptr)
        {
            *opt_out_idx = m_locals.size();
        }
        m_locals.push_back(var);
        m_stack_space_needed = std::max(m_stack_space_needed, m_locals.size());
        return var;
    }

    void pop_variables(size_t n)
    {
        m_locals.resize(m_locals.size() - n);
    }

    bool resolve_local(Symbol *symbol, uint32_t &out_idx)
    {
        if (m_locals.size() != 0)
        {
            for (uint32_t i = m_locals.size(); i > 0; --i)
            {
                auto local = m_locals[i - 1];
                if (local->symbol() == symbol)
                {
                    out_idx = i - 1;
                    return true;
                }
            }
        }
        return false;
    }

    bool resolve_capture(Symbol *symbol, uint32_t &out_idx)
    {
        if (m_parent == nullptr || m_parent->is_root())
        {
            return false;
        }

        uint32_t idx;
        if (m_parent->resolve_local(symbol, idx))
        {
            return capture(symbol, idx, true, out_idx);
        }

        if (m_parent->resolve_capture(symbol, idx))
        {
            return capture(symbol, idx, false, out_idx);
        }

        return false;
    }


  private:
    friend struct Variable;

    bool capture(Symbol *symbol, uint32_t index, bool is_local, uint32_t &out_idx)
    {
        if (is_root())
        {
            return false;
        }

        for (uint32_t i = 0; i < m_captures.size(); ++i)
        {
            if (m_captures[i].symbol == symbol)
            {
                out_idx = i;
                return true;
            }
        }
        out_idx = m_captures.size();
        m_captures.push_back({symbol, index, is_local});
        return true;
    }

    Scope *m_parent;
    std::vector<Variable*> m_locals;
    std::vector<Capture_Info> m_captures;
    size_t m_stack_space_needed;
};

Scope *THE_ROOT_SCOPE;

bool Variable::is_global() const
{
    return m_scope->is_root();
}

}

namespace bytecode
{
enum class Opcode : uint8_t
{
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) op_ ## name,
#include "bytecode.def"
#undef BYTECODE_DEF
};

static
std::string opcode_name(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return #name;
#include "bytecode.def"
#undef BYTECODE_DEF
    }
    return "???";
}

static
size_t opcode_size(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return size;
#include "bytecode.def"
#undef BYTECODE_DEF
    }
    return 1;
}

struct Debug_Info
{
    struct Region
    {
        Region() = default;
        Region(const void *start, size_t size)
            : m_start(start)
            , m_size(size)
        {}

        bool contains(const void *ptr) const
        {
            auto begin = reinterpret_cast<uintptr_t>(m_start);
            auto end = begin + m_size;
            auto p = reinterpret_cast<uintptr_t>(ptr);

            return begin <= p && p < end;
        }

        const void *begin() const
        {
            return m_start;
        }

        const void *end() const
        {
            return reinterpret_cast<void*>(reinterpret_cast<uintptr_t>(m_start) + m_size);
        }

        size_t size() const
        {
            return m_size;
        }

      private:
        const void *m_start;
        size_t m_size;
    };

    static bool find(const void *address, Value &out_expr)
    {
        size_t size = ~0ull;
        bool found = false;
        for (auto it = m_bytecode_address_expr_map.rbegin();
             it != m_bytecode_address_expr_map.rend();
             ++it)
        {
            if (it->first.contains(address))
            {
                found = true;
                if (it->first.size() < size)
                {
                    size = it->first.size();
                    out_expr = it->second;
                }
            }
        }
        return found;
    }

    static bool find_function(const void *address, const Function **out_function)
    {
        for (auto it : m_functions)
        {
            Region r(it->begin(), it->end() - it->begin());
            if (r.contains(address))
            {
                *out_function = it;
                return true;
            }
        }
        return false;
    }

    static void push(const void *start, size_t size, Value expr)
    {
        Region r(start, size);
        m_bytecode_address_expr_map.push_back({r, expr});
    }

    static void track_function(const Function *function)
    {
        if (function)
        {
            m_functions.push_back(function);
        }
    }

  private:
    static std::vector<std::pair<Region, Value>> m_bytecode_address_expr_map;
    static std::vector<const Function*> m_functions;
};
std::vector<std::pair<Debug_Info::Region, Value>> Debug_Info::m_bytecode_address_expr_map;
std::vector<const Function*> Debug_Info::m_functions;

struct Emitter
{
    Emitter(compiler::Scope *scope)
        : m_scope(scope)
        , m_locked(false)
    {}

    void emit_push_literal(Value value);
    void emit_push_nil();
    void emit_push_fixnum_0();
    void emit_push_fixnum_1();

    void emit_funcall(uint32_t argc);
    void emit_gotocall(uint32_t argc);
    void emit_apply(uint32_t argc);

    void emit_return();

    int32_t emit_jump();
    int32_t emit_pop_jump_if_nil();

    void emit_get_value(Value value);
    void emit_set_value(Value value);

    void emit_function_value(Value func_val);

    void emit_instantiate_closure(const Function *function);
    void emit_close_values(uint32_t num_values);

    void emit_pop();

    void emit_halt();

    int32_t emit_push_handler_case(uint32_t num_handlers);
    void emit_pop_handler_case();

    void emit_raise_signal(uint32_t argc);

    void emit_cons();
    void emit_car();
    void emit_cdr();
    void emit_eq();
    void emit_rplaca();
    void emit_rplacd();
    void emit_aref();
    void emit_aset();

    void emit_debug_trap();

    template<typename T>
    void set_raw(size_t offset, T value)
    {
        assert(offset + sizeof(value) <= m_bytecode.size());
        T *slot = reinterpret_cast<T*>(m_bytecode.data() + offset);
        *slot = value;
    }

    void lock();
    void map_range_to(size_t begin, size_t end, Value expr);
    int32_t position() const;
    const std::vector<uint8_t> &bytecode() const;
    std::vector<uint8_t> &&move_bytecode();

    void push_labels();
    void pop_labels();
    bool get_label(Value tag, int32_t &out_offset);
    int32_t make_label(Value tag);
    void backfill_label(int32_t offset, Value tag);

    compiler::Scope *scope() const;

  private:
    void emit_get_value(Symbol *symbol);
    void emit_set_value(Symbol *symbol);
    struct Backfill_Info
    {
        Value tag;
        int32_t offset;
    };

    using Label_Map = std::unordered_map<Value, int32_t>;

    template<typename T>
    void append(T value)
    {
        if (m_locked)
        {
            fprintf(stderr, "cannot write to locked bytecode emitter!\n");
            abort();
        }
        m_bytecode.resize(m_bytecode.size() + sizeof(T), 0xCC);
        auto end = m_bytecode.data() + m_bytecode.size();
        T *slot = reinterpret_cast<T*>(end - sizeof(T));
        *slot = value;
    }

    struct Range_Value_Pair
    {
        size_t begin;
        size_t end;
        Value value;
    };

    std::vector<Range_Value_Pair> m_debug_map;

    std::vector<uint8_t> m_bytecode;
    std::vector<Label_Map> m_label_stack;
    std::list<Backfill_Info> m_backfills;
    compiler::Scope *m_scope;

    bool m_locked;
};

void Emitter::emit_push_literal(Value value)
{
    if (value.is_garbage_collected())
    {
        g.literal_object_slots.push_back(value);
    }

    if (value.is_nil())
    {
        emit_push_nil();
    }
    else if (value == Value::wrap_fixnum(0))
    {
        emit_push_fixnum_0();
    }
    else if (value == Value::wrap_fixnum(1))
    {
        emit_push_fixnum_1();
    }
    else
    {
        append(Opcode::op_push_value);
        append(value);
    }
}

void Emitter::emit_push_nil()
{
    append(Opcode::op_push_nil);
}

void Emitter::emit_push_fixnum_0()
{
    append(Opcode::op_push_fixnum_0);
}

void Emitter::emit_push_fixnum_1()
{
    append(Opcode::op_push_fixnum_1);
}

void Emitter::emit_funcall(uint32_t argc)
{
    append(Opcode::op_funcall);
    append(argc);
}

void Emitter::emit_gotocall(uint32_t argc)
{
    append(Opcode::op_gotocall);
    append(argc);
}

void Emitter::emit_apply(uint32_t argc)
{
    append(Opcode::op_apply);
    append(argc);
}

void Emitter::emit_return()
{
    append(Opcode::op_return);
}

int32_t Emitter::emit_jump()
{
    append(Opcode::op_jump);
    auto offset = position();
    append<uint32_t>(0xDEADBEEF);
    return offset;
}
int32_t Emitter::emit_pop_jump_if_nil()
{
    append(Opcode::op_pop_jump_if_nil);
    auto offset = position();
    append<uint32_t>(0xDEADBEEF);
    return offset;
}

void Emitter::emit_get_value(Symbol *symbol)
{
    uint32_t idx = ~0u;
    Opcode opcode;

    if (m_scope->resolve_local(symbol, idx))
    {
        if (m_scope->is_root())
        {
            opcode = Opcode::op_get_global;
        }
        else
        {
            opcode = Opcode::op_get_local;
        }
    }
    else if (m_scope->resolve_capture(symbol, idx))
    {
        opcode = Opcode::op_get_capture;
    }
    else if (m_scope->get_root()->resolve_local(symbol, idx))
    {
        opcode = Opcode::op_get_global;
    }
    else
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Undefined symbol"),
                                   gc.alloc_string(symbol->qualified_name()));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }

    assert(idx != ~0u);

    append(opcode);
    append(idx);
}

void Emitter::emit_get_value(Value value)
{
    if (!symbolp(value))
    {
        fprintf(stderr, "Cannot get_value non-symbol value: %s\n", repr(value).c_str());
        abort();
    }
    else if (value == g.s_T || value.as_object()->symbol()->is_keyword())
    {
        emit_push_literal(value);
    }
    else
    {
        emit_get_value(value.as_object()->symbol());
    }
}

void Emitter::emit_set_value(Symbol *symbol)
{
    uint32_t idx = ~0u;
    Opcode opcode;

    if (m_scope->resolve_local(symbol, idx))
    {
        if (m_scope->is_root())
        {
            opcode = Opcode::op_set_global;
        }
        else
        {
            opcode = Opcode::op_set_local;
        }
    }
    else if (m_scope->resolve_capture(symbol, idx))
    {
        opcode = Opcode::op_set_capture;
    }
    else
    {
        opcode = Opcode::op_set_global;
        auto root = m_scope->get_root();
        if (!root->resolve_local(symbol, idx))
        {
            root->create_variable(symbol, &idx);
        }
    }

    assert(idx != ~0u);

    append(opcode);
    append(idx);
}

void Emitter::emit_set_value(Value value)
{
    if (!symbolp(value))
    {
        fprintf(stderr, "Cannot set_value non-symbol value: %s\n", repr(value).c_str());
        abort();
    }
    else
    {
        emit_set_value(value.as_object()->symbol());
    }
}

void Emitter::emit_function_value(Value func_val)
{
    append(Opcode::op_function_value);
    append(func_val);
}

void Emitter::emit_instantiate_closure(const Function *function)
{
    append(Opcode::op_instantiate_closure);
    append(function);
}

void Emitter::emit_close_values(uint32_t num_values)
{
    append(Opcode::op_close_values);
    append(num_values);
}

void Emitter::emit_pop()
{
    append(Opcode::op_pop);
}

void Emitter::emit_halt()
{
    append(Opcode::op_halt);
}

int32_t Emitter::emit_push_handler_case(uint32_t num_handlers)
{
    append(Opcode::op_push_handler_case);
    append(num_handlers);
    auto offset = position();
    append<uint32_t>(0xDEADBEEF);
    return offset;
}

void Emitter::emit_pop_handler_case()
{
    append(Opcode::op_pop_handler_case);
}

void Emitter::emit_raise_signal(uint32_t argc)
{
    append(Opcode::op_raise_signal);
    append(argc);
}

void Emitter::emit_cons()
{
    append(Opcode::op_cons);
}

void Emitter::emit_car()
{
    append(Opcode::op_car);
}

void Emitter::emit_cdr()
{
    append(Opcode::op_cdr);
}

void Emitter::emit_eq()
{
    append(Opcode::op_eq);
}

void Emitter::emit_rplaca()
{
    append(Opcode::op_rplaca);
}

void Emitter::emit_rplacd()
{
    append(Opcode::op_rplacd);
}

void Emitter::emit_aref()
{
    append(Opcode::op_aref);
}

void Emitter::emit_aset()
{
    append(Opcode::op_aset);
}

void Emitter::emit_debug_trap()
{
    append(Opcode::op_debug_trap);
}

int32_t Emitter::position() const
{
    return m_bytecode.size();
}

void Emitter::lock()
{
    if (m_locked)
    {
        return;
    }

    for (const auto &it : m_debug_map)
    {
        auto size = it.end - it.begin;
        auto addr = m_bytecode.data() + it.begin;
        Debug_Info::push(addr, size, it.value);
    }

    m_locked = true;
}

void Emitter::map_range_to(size_t begin, size_t end, Value expr)
{
    m_debug_map.push_back({begin, end, expr});
}

const std::vector<uint8_t> &Emitter::bytecode() const
{
    return m_bytecode;
}

std::vector<uint8_t> &&Emitter::move_bytecode()
{
    return std::move(m_bytecode);
}

void Emitter::push_labels()
{
    m_label_stack.push_back(Label_Map());
}

void Emitter::pop_labels()
{
    assert(m_label_stack.size() != 0);

    {
        auto it = m_backfills.begin();
        while (it != m_backfills.end())
        {
            int32_t label_offs;
            if (get_label(it->tag, label_offs))
            {
                auto set_offs = it->offset;
                set_raw<uint32_t>(set_offs, label_offs - (set_offs - 1));
                it = m_backfills.erase(it);
            }
            else
            {
                it++;
            }
        }
    }

    m_label_stack.pop_back();
    if (m_label_stack.size() == 0 && !m_backfills.empty())
    {
        std::vector<Value> vals;
        for (auto const &it : m_backfills)
        {
            vals.push_back(it.tag);
        }
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("No label tag found"),
                                   to_list(vals));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }
}

bool Emitter::get_label(Value tag, int32_t &out_offset)
{
    for (auto it = m_label_stack.rbegin(); it != m_label_stack.rend(); ++it)
    {
        auto found = it->find(tag);
        if (found != it->end())
        {
            out_offset = found->second;
            return true;
        }
    }
    return false;
}

int32_t Emitter::make_label(Value tag)
{
    assert(m_label_stack.size() != 0);
    if (m_label_stack.back().find(tag) != m_label_stack.back().end())
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Label with tag already exists"),
                                   tag);
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }
    auto pos = position();
    m_label_stack.back()[tag] = pos;
    return pos;
}

void Emitter::backfill_label(int32_t offset, Value tag)
{
    assert(m_label_stack.size() != 0);
    m_backfills.push_back({tag, offset});
}

compiler::Scope *Emitter::scope() const
{
    return m_scope;
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
    const auto opcode = static_cast<Opcode>(*ip);
    if (here) {
        out << ">> ";
    }
    else {
        out << "   ";
    }
    out << std::setfill('0') << std::setw(8) << std::hex << reinterpret_cast<uintptr_t>(ip) << std::setfill(' ') << "  ";
    auto size = opcode_size(opcode);
    auto name = opcode_name(opcode);
    switch (opcode) {
        default:
        {
            put_bytes(out, ip, size);
            out << name;
            ip += size;
        } break;

        case Opcode::op_get_global:
        case Opcode::op_set_global:

        case Opcode::op_get_local:
        case Opcode::op_set_local:

        case Opcode::op_get_capture:
        case Opcode::op_set_capture:

        case Opcode::op_close_values:

        case Opcode::op_raise_signal:

        case Opcode::op_apply:
        case Opcode::op_funcall:
        case Opcode::op_gotocall:
        {
            auto nargs = *reinterpret_cast<const uint32_t*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << nargs;
            ip += size;
        } break;


        case Opcode::op_jump:
        case Opcode::op_pop_jump_if_nil:
        {
            auto offs = *reinterpret_cast<const int32_t*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << offs << " -> " << reinterpret_cast<uintptr_t>(ip+offs);
            ip += size;
        } break;

        case Opcode::op_function_value:
        case Opcode::op_push_value:
        {
            auto obj = *reinterpret_cast<const Value*>(ip+1);
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

        case Opcode::op_push_handler_case:
        {
            put_bytes(out, ip, size);
            auto how_many = *reinterpret_cast<const uint32_t*>(ip+1);
            auto branch = *reinterpret_cast<const uint32_t*>(ip+1+sizeof(how_many));
            out << name << " " << how_many << ", " << branch
                << " -> " << reinterpret_cast<uintptr_t>(ip+branch);
            ip += size;
        } break;

        case Opcode::op_instantiate_closure:
        {
            auto function = *reinterpret_cast<const Function* const*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << reinterpret_cast<uintptr_t>(function)
                << " -> {" << reinterpret_cast<uintptr_t>(function->begin()) << "}";
            ip += size;
        } break;

        case Opcode::op_return:
        case Opcode::op_pop:
        case Opcode::op_push_nil:
        case Opcode::op_push_fixnum_0:
        case Opcode::op_push_fixnum_1:
        case Opcode::op_cons:
        case Opcode::op_car:
        case Opcode::op_cdr:
        case Opcode::op_halt:
        case Opcode::op_pop_handler_case:
        case Opcode::op_eq:
        case Opcode::op_rplaca:
        case Opcode::op_rplacd:
        case Opcode::op_aref:
        case Opcode::op_aset:
        case Opcode::op_debug_trap:
        {
            put_bytes(out, ip, size);
            out << name;
            ip += size;
        } break;
    }
    out << '\n';
    return ip;
}

static int put_disassembly_tag(std::ostream &out, const std::string &tag)
{
    out << "Disassembly for \"" << tag << "\"\n";
    return 1;
}

static
int disassemble(std::ostream &out, const std::string &tag, const uint8_t *ip, bool here = false)
{
    put_disassembly_tag(out, tag);
    disassemble1(out, ip, here);
    return 2;
}

static
int disassemble(std::ostream &out, const std::string &tag, const uint8_t *start, const uint8_t *end, const uint8_t *ip = nullptr)
{
    int lines_printed = put_disassembly_tag(out, tag);
    for (; start != end;) {
        start = disassemble1(out, start, start == ip);
        lines_printed++;
    }
    return lines_printed;
}

static
int disassemble(std::ostream &out, const std::string &tag, const Emitter &e)
{
    auto start = e.bytecode().data();
    auto end = start + e.bytecode().size();
    return disassemble(out, tag, start, end, nullptr);
}

static
int disassemble(std::ostream &out, const std::string &tag, const Function *function, const uint8_t *ip)
{
    int lines_printed = put_disassembly_tag(out, tag);
    for (auto p = function->begin(); p != function->end();)
    {
        p = disassemble1(out, p, p == ip);
        lines_printed++;
    }
    return lines_printed;
}

static
int disassemble_maybe_function(std::ostream &out, const std::string &tag, const uint8_t *ip, bool here = false)
{
    const Function *func;
    if (Debug_Info::find_function(ip, &func))
    {
        return disassemble(out, tag, func, ip);
    }

    put_disassembly_tag(out, tag);
    disassemble1(out, ip, here);
    return 2;
}

}

namespace compiler
{

static void compile(bytecode::Emitter &e, Value expr, bool toplevel, bool tail_position = false);

static
bool constantp(Value expr)
{
    if (expr.is_type(Object_Type::Symbol))
    {
        if (expr == g.s_T)
        {
            return true;
        }
        if (expr.as_object()->symbol()->is_keyword())
        {
            return true;
        }
        return false;
    }
    if (expr.is_cons())
    {
        auto f = car(expr);
        if (f == g.s_QUOTE ||
            f == g.s_FUNCTION ||
            f == g.s_pLAMBDA)
        {
            return true;
        }
        return false;
    }
    return true;
}

static
bool effect_free(Value expr)
{
    return expr.is_type(Object_Type::Symbol)
        || constantp(expr);
}

static
void compile_body(bytecode::Emitter &e, Value body, bool tail_position)
{
    while (!cdr(body).is_nil())
    {
        if (!effect_free(car(body)))
        {
            compile(e, car(body), false, false);
            e.emit_pop();
        }
        body = cdr(body);
    }
    compile(e, car(body), false, tail_position);
}

static
void compile_function(bytecode::Emitter &e, Value expr, bool macro, bool toplevel)
{
    auto name = second(expr);
    auto lambda_list = macro ? third(expr) : second(expr);
    auto body = macro ? cdddr(expr) : cddr(expr);

    bytecode::Emitter function_emitter(e.scope()->push_scope());
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
    std::vector<const Symbol*> params;
    bool has_optionals = false;
    bool has_rest = false;
    size_t optionals_start_at = 0;
    while (!cur.is_nil())
    {
        auto sym = car(cur);
        if (sym == g.s_aOPTIONAL)
        {
            cur = cdr(cur);
            has_optionals = true;
            break;
        }
        if (sym == g.s_aREST || sym == g.s_aBODY)
        {
            has_rest = true;
            break;
        }
        optionals_start_at++;
        if (!symbolp(sym))
        {
            GC_GUARD();
            auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                       gc.alloc_string("Non-symbol parameter in lambda list"),
                                       sym);
            GC_UNGUARD();
            throw VM_State::Signal_Exception(signal_args);
        }
        params.push_back(sym.as_object()->symbol());
        cur = cdr(cur);
    }

    for (auto const symbol : params)
    {
        function_emitter.scope()->create_variable(symbol);
    }


    std::vector<uint32_t> optional_offsets;
    for (size_t i = 0; i < optionals_start_at; ++i)
    {
        optional_offsets.push_back(0);
    }

    if (has_optionals || has_rest)
    {
        // at this point cur is now pointing to optionals
        while (!cur.is_nil())
        {
            auto param = first(cur);
            if (param == g.s_aREST || param == g.s_aBODY)
            {
                param = second(cur);
                cur = Value::nil();
                has_rest = true;
            }

            optional_offsets.push_back(function_emitter.position());

            if (param.is_cons())
            {
                auto symbol = first(param).as_object()->symbol();
                function_emitter.scope()->create_variable(symbol);
                params.push_back(symbol);

                compile(function_emitter, second(param), false);
                function_emitter.emit_set_value(first(param));
                function_emitter.emit_pop();
            }
            else
            {
                auto symbol = param.as_object()->symbol();
                function_emitter.scope()->create_variable(symbol);
                params.push_back(symbol);

                function_emitter.emit_push_nil();
                function_emitter.emit_set_value(param);
                function_emitter.emit_pop();
            }
            cur = cdr(cur);
        }
    }
    else
    {
        assert(optionals_start_at == params.size());
    }

    auto main_entry_offset = function_emitter.position();
    compile_body(function_emitter, body, true);
    function_emitter.emit_return();
    function_emitter.lock(); // the function MUST be locked before we can resolve captures or move bytecode.
    std::vector<Function::Capture_Offset> capture_offsets;
    for (auto const &cap : function_emitter.scope()->capture_info())
    {
        capture_offsets.push_back({
                cap.index,
                cap.is_local,
                cap.symbol->qualified_name()
            });
    }

    //disassemble(std::cout, "LAMBDA", function_emitter); // @DELETE-ME

    //{
    //    auto locs = function_emitter.scope()->locals();
    //    printf("Num locals: %zu\n", locs.size());
    //    for (size_t i = 0; i < locs.size(); ++i)
    //    {
    //        printf(" [%zu]: %s\n", i, locs[i]->symbol()->qualified_name().c_str());
    //    }
    //}

    auto const *function = new Function(std::move(function_emitter.move_bytecode()),
                                        std::move(optional_offsets),
                                        std::move(params),
                                        std::move(capture_offsets),
                                        main_entry_offset,
                                        optionals_start_at,
                                        // using the max number of locals instead of the function's arity
                                        // because we may inline immediate lambda calls which expand the
                                        // number of local variables but not change the arity of the
                                        // outer function.
                                        function_emitter.scope()->stack_space_needed(),
                                        has_rest,
                                        has_optionals);

    if (macro)
    {
        auto obj = gc.alloc_object_unmanaged<Closure>(function);
        g.macros[name.as_object()->symbol()] = obj;
    }
    else
    {
        e.emit_instantiate_closure(function);
    }
    bytecode::Debug_Info::track_function(function);
    // We created it in this function, so needs to be deleted here.
    delete function_emitter.scope();
}


static
void compile_function_call(bytecode::Emitter &e, Value func, Value args, bool toplevel, bool tail_position, bool funcall)
{
    if (func.is_cons() && first(func) == g.s_pLAMBDA)
    {
        if (second(func).is_nil())
        {
            // calling a lambda that takes no arguments is directly inlinable,
            // no call needed... :)
            compile_body(e, cddr(func), tail_position);
            return;
        }
        else if (!toplevel)
        {
            // and in the case of a lambda instead of instantiate + funcall we just inline into the current
            // stack frame.
            // @TODO: although it's unlikely to have &optional and &rest here, we should still support it
            // but for now we'll just not do the optimization in that case.
            bool do_opt = true;
            auto params = second(func);
            std::vector<Value> symbols;
            while (!params.is_nil())
            {
                if (symbolp(car(params)))
                {
                    symbols.push_back(car(params));
                }
                else
                {
                    do_opt = false;
                    break;
                }
                params = cdr(params);
            }

            if (do_opt)
            {
                size_t i = 0;
                while (!args.is_nil())
                {
                    i++;
                    compile(e, car(args), false);
                    args = cdr(args);
                }
                for (; i < symbols.size(); ++i)
                {
                    e.emit_push_nil();
                }
                std::vector<Variable*> vars;
                for (auto symbol_value : symbols)
                {
                    auto symbol = symbol_value.as_object()->symbol();
                    auto var = e.scope()->create_variable(symbol);
                    vars.push_back(var);
                }
                for (auto it = symbols.rbegin(); it != symbols.rend(); ++it)
                {
                    e.emit_set_value(*it);
                    e.emit_pop();
                }
                compile_body(e, cddr(func), tail_position);
                // If we're in the tail position, then the gotocall will handle cleaning up the stack
                // so there's no need to emit these instructions.
                if (!tail_position)
                {
                    if (e.scope()->capture_info().size() != 0)
                    {
                        e.emit_close_values(symbols.size());
                    }
                }
                e.scope()->pop_variables(symbols.size());
                return;
            }
        }
    }

    uint32_t nargs = 0;
    while (!args.is_nil())
    {
        compile(e, car(args), toplevel);
        nargs++;
        args = cdr(args);
    }
    if (func.is_cons() && first(func) == g.s_pLAMBDA)
    {
        compile(e, func, toplevel);
    }
    else if (funcall)
    {
        if (symbolp(func))
        {
            e.emit_get_value(func);
        }
        else
        {
            compile(e, func, toplevel);
        }
    }
    else
    {
        e.emit_push_literal(func);
    }

    if (tail_position)
    {
        e.emit_gotocall(nargs);
    }
    else
    {
        e.emit_funcall(nargs);
    }
}


static
void compile(bytecode::Emitter &e, Value expr, bool toplevel, bool tail_position)
{
    if (expr.is_cons())
    {
        auto thing = first(expr);
        auto begin = e.position();
        auto saved_expr = expr;
        if (thing == g.s_QUOTE)
        {
            e.emit_push_literal(second(expr));
        }
        else if (thing == g.s_pGO)
        {
            auto offs = e.emit_jump();
            e.backfill_label(offs, second(expr));
        }
        else if (thing == g.s_pTAGBODY)
        {
            e.push_labels();
            e.emit_push_nil();
            auto body = cdr(expr);
            while (!body.is_nil())
            {
                auto it = car(body);
                if (it.is_cons())
                {
                    compile(e, it, toplevel, false);
                    e.emit_pop();
                }
                else
                {
                    e.make_label(it);
                }
                body = cdr(body);
            };
            e.pop_labels();
        }
        else if (thing == g.s_IF)
        {
            auto test = second(expr);
            auto consequence = third(expr);
            auto alternative = fourth(expr);
            if (constantp(test))
            {
                if (test.is_nil())
                {
                    compile(e, alternative, toplevel, tail_position);
                }
                else
                {
                    compile(e, consequence, toplevel, tail_position);
                }
            }
            else
            {
                compile(e, test, toplevel);
                auto alt_offs = e.emit_pop_jump_if_nil();
                compile(e, consequence, toplevel, tail_position);
                if (tail_position)
                {
                    e.emit_return();
                    auto label_alt = e.position();
                    compile(e, alternative, toplevel, tail_position);

                    e.set_raw<int32_t>(alt_offs, label_alt - (alt_offs-1));
                }
                else
                {
                    auto out_offs = e.emit_jump();
                    auto label_alt = e.position();
                    compile(e, alternative, toplevel, tail_position);
                    auto label_out = e.position();

                    e.set_raw<int32_t>(out_offs, label_out - (out_offs-1));
                    e.set_raw<int32_t>(alt_offs, label_alt - (alt_offs-1));
                }
            }
        }
        else if (thing == g.s_pDEFINE_MACRO)
        {
            compile_function(e, expr, true, true);
        }
        else if (thing == g.s_pLAMBDA)
        {
            compile_function(e, expr, false, toplevel);
        }
        else if (thing == g.s_pSETQ)
        {
            compile(e, third(expr), toplevel);
            if (symbolp(second(expr)))
            {
                e.emit_set_value(second(expr));
            }
            else
            {
                fprintf(stderr, "Cannot %%SETQ a non-symbol: %s\n", repr(second(expr)).c_str());
            }
        }
        else if (thing == g.s_pHANDLER_CASE)
        {
            auto form = second(expr);
            auto handlers = cddr(expr);
            uint32_t nhandlers = 0;
            while (!handlers.is_nil())
            {
                nhandlers++;
                auto handler = car(handlers);
                auto handler_tag = first(handler);
                compile_function(e, handler, false, toplevel);
                e.emit_push_literal(handler_tag);
                if (handler_tag == g.s_T)
                {
                    if (!cdr(handlers).is_nil())
                    {
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
            e.set_raw<uint32_t>(offs, e.position() - before_form);
        }
        else if (thing == g.s_FUNCTION)
        {
            auto thing = second(expr);
            if (thing.is_cons())
            {
                if (first(thing) == g.s_pLAMBDA)
                {
                    compile(e, thing, toplevel);
                }
                else
                {
                    // ???
                }
            }
            else
            {
                e.emit_function_value(second(expr));
            }
        }
        else if (thing == g.s_pFUNCALL)
        {
            compile_function_call(e, second(expr), cddr(expr), toplevel, tail_position, true);
        }
        else if (thing == g.s_pCONS)
        {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_cons();
        }
        else if (thing == g.s_pCAR)
        {
            compile(e, second(expr), toplevel);
            e.emit_car();
        }
        else if (thing == g.s_pCDR)
        {
            compile(e, second(expr), toplevel);
            e.emit_cdr();
        }
        else if (thing == g.s_pSIGNAL)
        {
            uint32_t nargs = 0;
            auto tag = second(expr);
            auto args = cddr(expr);
            while (!args.is_nil())
            {
                compile(e, car(args), toplevel);
                args = cdr(args);
                nargs++;
            }
            compile(e, tag, toplevel);
            e.emit_raise_signal(nargs);
        }
        else if (thing == g.s_pEQ)
        {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_eq();
        }
        else if (thing == g.s_pRPLACA)
        {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_rplaca();
        }
        else if (thing == g.s_pRPLACD)
        {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_rplacd();
        }
        else if (thing == g.s_pAREF)
        {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            e.emit_aref();
        }
        else if (thing == g.s_pASET)
        {
            compile(e, second(expr), toplevel);
            compile(e, third(expr), toplevel);
            compile(e, fourth(expr), toplevel);
            e.emit_aset();
        }
        else if (thing == g.s_pDEBUGGER)
        {
            compile(e, second(expr), toplevel);
            e.emit_debug_trap();
        }
        else if (thing == g.s_pAPPLY)
        {
            uint32_t nargs = 0;
            auto args = cddr(expr);
            while (!args.is_nil())
            {
                compile(e, car(args), toplevel);
                nargs++;
                args = cdr(args);
            }
            compile(e, second(expr), toplevel);
            e.emit_apply(nargs);
        }
        else
        {
            compile_function_call(e, first(expr), rest(expr), toplevel, tail_position, false);
        }
        if (thing != g.s_QUOTE && thing != g.s_FUNCTION)
        {
            auto end = e.position();
            e.map_range_to(begin, end, saved_expr);
        }
    }
    else if (symbolp(expr))
    {
        e.emit_get_value(expr);
    }
    else
    {
        e.emit_push_literal(expr);
    }
}
}

int VM_State::stack_dump(std::ostream &out, size_t max_size) const
{
    if (max_size == ~0u)
    {
        auto a = (m_call_frame_top - m_call_frame_bottom);
        auto b = (m_stack_top - m_stack_bottom);
        max_size = std::max(a, b);
    }
    auto rt = m_call_frame_top;
    auto rb = m_call_frame_bottom;

    auto pt = m_stack_top;
    auto pb = m_stack_bottom;

    auto r_stack_delta = rt - rb;
    out << std::setfill(' ') << std::dec;
    out << "|R-stack " << std::setw(9) << r_stack_delta << " |         P-stack\n";
    out << "|==================|================\n";
    out << std::setfill('0');
    for (;max_size != 0; max_size--)
    {
        rt--;
        pt--;
        if (reinterpret_cast<uintptr_t>(rt) < reinterpret_cast<uintptr_t>(rb))
        {
            out << "| **************** |";
        }
        else
        {
            out << "| " << std::hex << std::setw(16) << reinterpret_cast<uintptr_t>(rt->ip) << " |";
        }

        if (reinterpret_cast<uintptr_t>(pt) < reinterpret_cast<uintptr_t>(pb))
        {
            out << "                ";
        }
        else
        {
            out << std::hex << std::setw(16) << reinterpret_cast<uintptr_t>(pt);
        }
        if (pt == m_locals)
        {
            out << " -> ";
        }
        else
        {
            out << "    ";
        }

        if (reinterpret_cast<uintptr_t>(pt) < reinterpret_cast<uintptr_t>(pb))
        {
            out << " ***";
        }
        else
        {
            auto obj_repr = repr(*pt);
            const int n = 70;
            if (obj_repr.size() < n)
            {
                out << " " << obj_repr;
            }
            else
            {
                out << " " << obj_repr.substr(0, n-3) << "...";
            }
        }
        out << "\n";
    }
    int lines_printed = max_size;
    if (!m_current_closure.is_nil())
    {
        auto cc = m_current_closure.as_object()->closure();
        auto func_caps = cc->function()->capture_offsets();
        out << "Captures for current closure:\n";
        lines_printed++;
        auto &caps = cc->captures();
        for (size_t i = 0; i < caps.size(); ++i)
        {
            auto &offs = func_caps[i];
            out << offs.name << " "
                << std::hex << reinterpret_cast<uintptr_t>(caps[i]->location()) << " "
                << (caps[i] ? repr(caps[i]->value()) : "#<nullptr>") << "\n";
            lines_printed++;
        }
    }
    return lines_printed;
}

void VM_State::debug_dump(std::ostream &out, const std::string &tag, const uint8_t *ip, bool full) const
{
    //plat::clear_console();
    const Function *function;
    int lines_printed = 0;
    if (bytecode::Debug_Info::find_function(ip, &function))
    {
        std::vector<const uint8_t*> instructions;
        int ip_at = -1;
        for (auto it = function->begin(); it != function->end();)
        {
            if (it == ip)
            {
                ip_at = instructions.size();
            }
            instructions.push_back(it);
            it += bytecode::opcode_size(static_cast<bytecode::Opcode>(*it));
        }
        instructions.push_back(function->end());

        if (ip_at != -1 && instructions.size() >= 32)
        {
            out << "Disassemble for \"" << tag << "\"\n";
            lines_printed++;

            auto first = std::max(ip_at - 15, 0);
            auto instr = instructions[first];
            bool disassembling = true;
            for (; lines_printed < 32; ++lines_printed)
            {
                if (instr == function->end())
                {
                    disassembling = false;
                }

                if (disassembling)
                {
                    instr = bytecode::disassemble1(out, instr, instr == ip);
                }
                else
                {
                    out << "\n";
                }
            }
        }
        else
        {
            lines_printed = bytecode::disassemble(out, tag, function, ip);
        }
    }
    else
    {
        lines_printed = bytecode::disassemble(out, tag, ip, true);
    }

    for (; lines_printed <= 32; lines_printed++)
    {
        out << "\n";
    }

    if (full)
    {
        stack_dump(out, ~0u);
    }
    else
    {
        stack_dump(out);
    }
}

template<bool debuggable>
const uint8_t *VM_State::execute_impl(const uint8_t *ip)
{

#define BYTECODE_DEF(name, noperands, nargs, size, docstring) &&opcode_ ## name,
    void *computed_gotos[256] =
    {
        #include "bytecode.def"
    };

#define DISPATCH(name) case bytecode::Opcode::op_ ## name: opcode_ ## name:

#define DISPATCH_NEXT if constexpr (debuggable) break; else goto *computed_gotos[*ip];
#define EXEC switch(static_cast<bytecode::Opcode>(*ip))
#define DISPATCH_LOOP for (;;)

#if PROFILE_OPCODE_PAIRS
#define PREDICTED(name) //empty
#define PREDICT(name) //empty
#else
#define PREDICTED(name) predicted_ ## name:
#define PREDICT(name)                                                   \
    do {                                                                \
        if (static_cast<bytecode::Opcode>(*ip) == bytecode::Opcode::op_ ## name) \
        {                                                               \
            goto predicted_ ## name;                                       \
        }                                                               \
    } while (0)
#endif


#define TYPE_CHECK(what, typecheck, expected)                           \
    do {                                                                \
        if (!(what).typecheck) {                                        \
            signal_args = gc.list(g.s_TYPE_ERROR, (expected), (what));  \
            goto raise_signal;                                          \
        }                                                               \
    } while (0)

#define CHECK_FIXNUM(what) TYPE_CHECK(what, is_fixnum(), g.s_FIXNUM)
#define CHECK_CONS(what) TYPE_CHECK(what, is_cons(), g.s_CONS)
#define CHECK_LIST(what) TYPE_CHECK(what, is_list(), g.s_LIST)
#define CHECK_CHARACTER(what) TYPE_CHECK(what, is_character(), g.s_CHARACTER)
#define CHECK_SYMBOL(what) TYPE_CHECK(what, is_type(Object_Type::Symbol), g.s_SYMBOL)
#define CHECK_FILE_STREAM(what) TYPE_CHECK(what, is_type(Object_Type::File_Stream), g.s_FILE_STREAM)
#define CHECK_SIMPLE_ARRAY(what) TYPE_CHECK(what, is_type(Object_Type::Simple_Array), g.s_SIMPLE_ARRAY)
#define CHECK_SYSTEM_POINTER(what) TYPE_CHECK(what, is_type(Object_Type::System_Pointer), g.s_SYSTEM_POINTER)
#define CHECK_STRUCT(what) TYPE_CHECK(what, is_type(Object_Type::Structure), g.s_STRUCTURE)


    static_assert(sizeof(*ip) == 1, "pointer arithmetic will not work as expected.");
    Value signal_args;
    Value func;
    uint32_t nargs;
    DISPATCH_LOOP
    {
        if constexpr (debuggable)
        {
            const auto opcode = static_cast<bytecode::Opcode>(*ip);
            if (g.debugger.breaking)
            {
                // @TODO: Fix debugger step over:
                // currently it just goes to the next instruction address which is ok in the case of
                // op_apply or op_funcall but it is incorrect in the case off op_jump and op_pop_jump_if_nil
                if ((g.debugger.command == Runtime_Globals::Debugger::Command::Step_Over &&
                     (g.debugger.addr0 == ip || g.debugger.addr1 == ip))
                    || g.debugger.command == Runtime_Globals::Debugger::Command::Step_Into)
                {
                    debug_dump(std::cout, "VM EXEC", ip);
                    auto &in = std::cin;
                    bool eat_newline = true;
                    switch (in.peek())
                    {
                        case 'c':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Continue;
                            in.get();
                            break;
                        case 's':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Step_Into;
                            in.get();
                            break;
                        case 'n':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Step_Over;
                            in.get();
                            break;
                        case '\n':
                            in.get();
                            eat_newline = false;
                            break;
                    }

                    if (eat_newline && in.peek() == '\n')
                    {
                        in.get();
                    }

                    switch (g.debugger.command)
                    {
                        case Runtime_Globals::Debugger::Command::Continue:
                            g.debugger.breaking = false;
                            break;
                        case Runtime_Globals::Debugger::Command::Step_Into:
                            break;
                        case Runtime_Globals::Debugger::Command::Step_Over:
                            g.debugger.addr0 = ip + bytecode::opcode_size(opcode);
                            g.debugger.addr1 = nullptr;
                            if (opcode == bytecode::Opcode::op_pop_jump_if_nil)
                            {
                                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                                g.debugger.addr1 = ip + offset;
                            }
                            else if (opcode == bytecode::Opcode::op_jump)
                            {
                                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                                g.debugger.addr0 = ip + offset;
                            }
                            break;
                    }
                }
            }
#if PROFILE_OPCODE_PAIRS
            auto this_opcode = static_cast<bytecode::Opcode>(*ip);
            switch (this_opcode)
            {
                case bytecode::Opcode::op_halt:
                case bytecode::Opcode::op_return:
                case bytecode::Opcode::op_gotocall:
                case bytecode::Opcode::op_jump:
                case bytecode::Opcode::op_pop_jump_if_nil:
                    break;
                default: {
                    auto next_opcode = *(ip + bytecode::opcode_size(this_opcode));
                    m_opcode_pairs[Opcode_Pair{static_cast<short>(this_opcode), next_opcode}]++;
                } break;
            }
#endif
        } // if constexpr (debuggable)

        EXEC
        {
            DISPATCH(apply)
            {
                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                if (nargs == 0)
                {
                    GC_GUARD();
                    signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Too few arguments!"));
                    GC_UNGUARD();
                    goto raise_signal;
                }

                auto last_arg = pop_param();
                CHECK_LIST(last_arg);
                nargs--;
                while (!last_arg.is_nil())
                {
                    push_param(car(last_arg));
                    last_arg = cdr(last_arg);
                    nargs++;
                }
                goto do_funcall;
            }

            DISPATCH(funcall)
            {
                PREDICTED(funcall);
                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);

                do_funcall:
                auto ofunc = func;
                if (symbolp(func))
                {
                    func = func.as_object()->symbol()->function();
                }

                if (func.is_lisp_primitive())
                {
                    bool raised_signal = false;
                    auto primitive = func.as_lisp_primitive();
                    auto result = primitive(m_stack_top - nargs, nargs, raised_signal);
                    m_stack_top -= nargs;
                    if (raised_signal)
                    {
                        signal_args = result;
                        goto raise_signal;
                    }
                    else
                    {
                        push_param(result);
                        ip += 1 + sizeof(nargs);
                    }
                    DISPATCH_NEXT;
                }

                if (func.is_type(Object_Type::Closure))
                {
                    // Although op_raise_signal also jumps here in a "funcall"-like way, it has
                    // already setup the stack in the state the closure expects to execute under
                    // so there is no need to push a frame for it here.
                    if (static_cast<bytecode::Opcode>(*ip) == bytecode::Opcode::op_funcall ||
                        static_cast<bytecode::Opcode>(*ip) == bytecode::Opcode::op_apply)
                    {
                        push_frame(ip+5, nargs);
                    }

                    m_current_closure = func;
                    auto closure = func.as_object()->closure();
                    auto function = closure->function();

                    // locals always start at first argument
                    m_locals = m_stack_top - nargs;

                    if (function->is_too_many_args(nargs))
                    {
                        GC_GUARD();
                        signal_args = gc.list(g.s_SIMPLE_ERROR,
                                              gc.alloc_string("Too many arguments!"),
                                              func,
                                              Value::wrap_fixnum(function->arity()),
                                              Value::wrap_fixnum(nargs));
                        GC_UNGUARD();
                        goto raise_signal;
                    }

                    if (function->is_too_few_args(nargs))
                    {
                        GC_GUARD();
                        signal_args = gc.list(g.s_SIMPLE_ERROR,
                                              gc.alloc_string("Too few arguments!"),
                                              func,
                                              Value::wrap_fixnum(function->arity()),
                                              Value::wrap_fixnum(nargs));
                        GC_UNGUARD();
                        goto raise_signal;
                    }

                    if (function->has_rest() && nargs > function->rest_index())
                    {
                        auto rest = to_list(m_locals+function->rest_index(),
                                            nargs - function->rest_index());
                        m_locals[function->rest_index()] = rest;
                    }

                    m_stack_top = m_locals + function->num_locals();
                    #if DEBUG > 1
                    {
                        assert(function->arity() <= function->num_locals());
                        auto start = m_locals + function->arity();
                        auto end = m_locals + function->num_locals();
                        for (; start != end; ++start)
                        {
                            *start = Value::nil();
                        }
                    }
                    #endif

                    ip = function->entrypoint(nargs);
                    DISPATCH_NEXT;
                }

                // error
                GC_GUARD();
                signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Not a callable object"), ofunc, func);
                GC_UNGUARD();
                goto raise_signal;
            }

            DISPATCH(gotocall)
            {

                close_values(m_locals);

                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                auto begin = m_stack_top - nargs;
                auto end = m_stack_top;
                m_stack_top = m_locals;
                std::copy(begin, end, m_stack_top);
                m_stack_top += nargs;

                goto do_funcall;
            }

            DISPATCH(pop_handler_case)
                pop_handler_case();
                // fallthrough
            DISPATCH(return)
            {
                if (m_call_frame_top == m_call_frame_bottom)
                {
                    goto done;
                }

                close_values(m_locals);

                auto val = param_top();
                auto frame = pop_frame();
                set_frame(frame);
                push_param(val);
                if (frame.ip == nullptr)
                {
                    goto done;
                }
                ip = frame.ip;
                DISPATCH_NEXT;
            }

            DISPATCH(jump)
            {
                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                ip += offset;
                DISPATCH_NEXT;
            }

            DISPATCH(pop_jump_if_nil)
            {
                if (pop_param().is_nil())
                {
                    auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                    ip += offset;
                }
                else
                {
                    ip += 5;
                }
                DISPATCH_NEXT;
            }

            DISPATCH(get_global)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(g.global_value_slots[index]);
                ip += 5;
                DISPATCH_NEXT;
            }

            DISPATCH(set_global)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                g.global_value_slots[index] = param_top();
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(get_local)
            {
                PREDICTED(get_local);
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(m_locals[index]);
                ip += 5;
                PREDICT(push_value);
                PREDICT(get_local);
                DISPATCH_NEXT;
            }

            DISPATCH(set_local)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                m_locals[index] = param_top();
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(get_capture)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(m_current_closure.as_object()->closure()->get_capture(index));
                ip += 5;
                DISPATCH_NEXT;
            }

            DISPATCH(set_capture)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                m_current_closure.as_object()->closure()->set_capture(index, param_top());
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(function_value)
            {
                auto obj = *reinterpret_cast<const Value*>(ip+1);
                if (symbolp(obj))
                {
                    push_param(obj.as_object()->symbol()->function());
                }
                else
                {
                    push_param(obj);
                }
                ip += 1 + sizeof(obj);
                DISPATCH_NEXT;
            }

            DISPATCH(pop)
            {
                PREDICTED(pop);
                pop_param();
                ip += 1;
                PREDICT(get_local);
                DISPATCH_NEXT;
            }

            DISPATCH(push_value)
            {
                PREDICTED(push_value);
                auto val = *reinterpret_cast<const Value*>(ip+1);
                push_param(val);
                ip += 1 + sizeof(val);
                PREDICT(funcall);
                DISPATCH_NEXT;
            }

            DISPATCH(push_nil)
            {
                push_param(Value::nil());
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(push_fixnum_0)
            {
                push_param(Value::wrap_fixnum(0));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(push_fixnum_1)
            {
                push_param(Value::wrap_fixnum(1));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(instantiate_closure)
            {
                auto function = *reinterpret_cast<const Function* const*>(ip+1);
                auto instance = gc.alloc_object<Closure>(function);
                push_param(instance); // push this first so GC won't free it from under us
                auto closure = instance.as_object()->closure();
                if (function->has_captures())
                {

                    auto const &cap_offsets = function->capture_offsets();
                    for (size_t i = 0; i < cap_offsets.size(); ++i)
                    {
                        auto const &offs = cap_offsets[i];
                        Closure_Reference *ref;
                        if (offs.is_local)
                        {
                            ref = capture_closure_reference(m_locals + offs.index);
                        }
                        else
                        {
                            ref = m_current_closure.as_object()->closure()->get_reference(offs.index);
                        }
                        //printf("[%s] %d %p %s\n",
                        //       offs.name.c_str(),
                        //       offs.index,
                        //       ref->location(),
                        //       (ref->location() ? repr(ref->value()).c_str() : "#<nullptr>"));
                        closure->capture_reference(i, ref);
                    }
                }
                ip += 1 + sizeof(function);
                DISPATCH_NEXT;
            }

            DISPATCH(close_values)
            {
                auto n = *reinterpret_cast<const uint32_t*>(ip+1);

                close_values(m_stack_top-n);

                ip += 1 + sizeof(n);
                DISPATCH_NEXT;
            }

            DISPATCH(cons)
            {
                // Instead of popping twice, we use param_top(n) like this to serve as
                // a GC guard because before the GC may trigger a run before the cons
                // is allocated which may cause these values to be collected.
                auto val = gc.cons(param_top(-1), param_top(0));
                // THEN we pop the values after the cons is allocated.
                pop_params(2);
                push_param(val);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(car)
            {
                auto o = pop_param();
                if (o.is_nil())
                {
                    push_param(o);
                }
                else
                {
                    CHECK_CONS(o);
                    push_param(car(o));
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(cdr)
            {
                auto o = pop_param();
                if (o.is_nil())
                {
                    push_param(o);
                }
                else
                {
                    CHECK_CONS(o);
                    push_param(cdr(o));
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(halt)
            {
                goto done;
            }

            DISPATCH(push_handler_case)
            {
                {
                    auto how_many = *reinterpret_cast<const uint32_t*>(ip+1);
                    auto branch = *reinterpret_cast<const uint32_t*>(ip+1+sizeof(how_many));
                    std::vector<Signal_Handler> handlers;
                    for (uint32_t i = 0; i < how_many; ++i)
                    {
                        auto tag = pop_param();
                        auto handler = pop_param();
                        handlers.push_back({tag, handler});
                    }
                    push_frame(ip + branch, 0);
                    push_handler_case(std::move(handlers));
                    ip += 1 + sizeof(how_many) + sizeof(branch);
                }
                DISPATCH_NEXT;
            }

            DISPATCH(raise_signal)
            {
                {
                    GC_GUARD();
                    // @Design, should we move the tag to be the first thing pushed since this just gets
                    // turned into a FUNCALL? The only reason to have the tag here is for easy access.
                    auto tag = pop_param();
                    auto nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                    signal_args = to_list(m_stack_top - nargs, nargs);
                    signal_args = gc.cons(tag, signal_args);
                    GC_UNGUARD();
                }
                raise_signal:
                //bytecode::disassemble_maybe_function(std::cout, "SIGNAL", ip);
                Handler_Case restore;
                Signal_Handler handler;
                if (find_handler(first(signal_args), true, restore, handler))
                {
                    m_stack_top = restore.stack;
                    m_call_frame_top = restore.frame;
                    // By default signal_args includes the handler tag and a specific handler knows its
                    // own tag because it is labeled as such. In the case of a handler with the T tag it
                    // is unknown so we leave it, otherwise it is removed.
                    if (handler.tag != g.s_T)
                    {
                        signal_args = cdr(signal_args);
                    }
                    func = handler.handler;
                    nargs = 0;
                    while (!signal_args.is_nil())
                    {
                        ++nargs;
                        push_param(car(signal_args));
                        signal_args = cdr(signal_args);
                    }
                    goto do_funcall;
                }
                else
                {
                    auto top = m_call_frame_top;
                    auto bottom = m_call_frame_bottom;
                    m_call_frame_top = m_call_frame_bottom;
                    m_stack_top = m_stack_bottom;
                    throw Signal_Exception(signal_args, ip, top, bottom);
                }
            }

            DISPATCH(eq)
            {
                auto b = pop_param();
                auto a = pop_param();
                if (a == b)
                {
                    push_param(g.s_T);
                }
                else if (a.is_type(Object_Type::System_Pointer) &&
                         b.is_type(Object_Type::System_Pointer) &&
                         (a.as_object()->system_pointer() == b.as_object()->system_pointer()))
                {
                    push_param(g.s_T);
                }
                else
                {
                    push_param(Value::nil());
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(rplaca)
            {
                auto b = pop_param();
                auto a = param_top();
                CHECK_CONS(a);
                set_car(a, b);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(rplacd)
            {
                auto b = pop_param();
                auto a = param_top();
                CHECK_CONS(a);
                set_cdr(a, b);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(aref)
            {
                auto subscript = pop_param();
                CHECK_FIXNUM(subscript);
                auto array_val = pop_param();
                CHECK_SIMPLE_ARRAY(array_val);
                auto array = array_val.as_object()->simple_array();
                auto index = subscript.as_fixnum();
                if (index < 0 || index >= array->size())
                {
                    signal_args = gc.list(g.s_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
                    goto raise_signal;
                }
                push_param(array->at(index));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(aset)
            {
                auto value = pop_param();
                auto subscript = pop_param();
                CHECK_FIXNUM(subscript);
                auto array_val = pop_param();
                CHECK_SIMPLE_ARRAY(array_val);
                auto array = array_val.as_object()->simple_array();
                auto index = subscript.as_fixnum();
                if (index < 0 || index >= array->size())
                {
                    signal_args = gc.list(g.s_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
                    goto raise_signal;
                }
                auto type = array->element_type();
                if (type != g.s_T)
                {
                    if (type == g.s_FIXNUM && !value.is_fixnum())
                    {
                        CHECK_FIXNUM(value);
                    }
                    else if (type == g.s_CHARACTER && !value.is_character())
                    {
                        CHECK_CHARACTER(value);
                    }
                }
                array->at(index) = value;
                push_param(value);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(debug_trap)
            {
                g.debugger.breaking = !param_top().is_nil();
                if (g.debugger.breaking)
                {
                    g.debugger.command = Runtime_Globals::Debugger::Command::Step_Into;
                }
                else
                {
                    g.debugger.command = Runtime_Globals::Debugger::Command::Continue;
                }
                ip += 1;
                DISPATCH_NEXT;
            }
        }
    }
    done:

    return ip;
}

#undef TYPE_CHECK
#define TYPE_CHECK(what, typecheck, expected)                   \
    do {                                                        \
        if (!(what).typecheck) {                                \
            raised_signal = true;                               \
            return gc.list(g.s_TYPE_ERROR, (expected), (what)); \
        }                                                       \
    } while (0)

#define CHECK_STRING(what)                                      \
    do {                                                        \
        if (!stringp(what)) {                                   \
            raised_signal = true;                               \
            return gc.list(g.s_TYPE_ERROR, g.s_STRING, (what)); \
        }                                                       \
    } while (0)

#define CHECK_AT_LEAST_N(what, n)                                       \
    do {                                                                \
        if ((what) < (n)) {                                             \
            raised_signal = true;                                       \
            return gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Argument count mismatch"), Value::wrap_fixnum(n), Value::wrap_fixnum(what)); \
        }                                                               \
    } while (0)

#define CHECK_EXACTLY_N(what, n)                                        \
    do {                                                                \
        if ((what) != (n)) {                                            \
            raised_signal = true;                                       \
            return gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Argument count mismatch"), Value::wrap_fixnum(n), Value::wrap_fixnum(what)); \
        }                                                               \
    } while (0)


static void set_global(compiler::Scope *scope, Value symbol_value, Value value);

struct Function_Initializer
{
    using Cpp_Function = Value (*)(Value *args, uint32_t nargs, bool &raised_signal);

    std::string lisp_name;
    std::string cpp_name;
    Package *package;
    Cpp_Function cpp_function;
    bool is_exported;
};
static std::vector<Function_Initializer> g_function_initializers;

struct Function_Initializer_With_Defun
{
    Function_Initializer_With_Defun(const std::string &lisp_name,
                                    const std::string &cpp_name,
                                    const std::string &package_name,
                                    Function_Initializer::Cpp_Function cpp_function,
                                    bool is_exported)
    {
        auto package = g.packages.find_or_create(package_name);
        g_function_initializers.push_back({lisp_name, cpp_name, package, cpp_function, is_exported});
    }

    Function_Initializer_With_Defun(const std::string &lisp_name,
                                    const std::string &cpp_name,
                                    Package *package,
                                    Function_Initializer::Cpp_Function cpp_function,
                                    bool is_exported)
    {
        g_function_initializers.push_back({lisp_name, cpp_name, package, cpp_function, is_exported});
    }
};

namespace primitives
{

#define CONCAT_IMPL(x, y) x ## y
#define MACRO_CONCAT(x, y) CONCAT_IMPL(x, y)
#define GENSYM(pfx) MACRO_CONCAT(pfx, __COUNTER__)

#define DEFUN(lisp_name, cpp_name, package, export)                 \
    Value cpp_name(Value *args, uint32_t nargs, bool &raised_signal); \
    static Function_Initializer_With_Defun GENSYM(pfx) (lisp_name, #cpp_name, package, cpp_name, export); \
    Value cpp_name(Value *args, uint32_t nargs, bool &raised_signal)

///////////////////////////////////////////////////////////////////////
// Internal Functions

DEFUN("%PRINT", func_print, g.core(), false)
{
    for (uint32_t i = 0; i < nargs; ++i)
    {
        printf("%s ", repr(args[i]).c_str());
    }
    printf("\n");
    return Value::nil();
}

DEFUN("%DISASSEMBLE", func_disassemble, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 1);
    auto expr = args[0];
    if (expr.is_cons())
    {
        try
        {
            auto expanded = macro_expand_impl(expr, *THE_LISP_VM);
            bytecode::Emitter e(compiler::THE_ROOT_SCOPE);
            compiler::compile(e, expanded, true);
            e.lock();
            bytecode::disassemble(std::cout, "DISASSEMBLY", e);
        }
        catch (VM_State::Signal_Exception ex)
        {
            raised_signal = true;
            return ex.what;
        }
    }
    else if (expr.is_fixnum())
    {
        auto ptr = expr.as_fixnum();
        auto val = Value(static_cast<Value::Bits_Type>(ptr));
        if (val.is_type(Object_Type::Closure))
        {
            auto closure = expr.as_object()->closure();
            bytecode::disassemble(std::cout, "DISASSEMBLY", closure->function(), closure->function()->main_entry());
        }
        else
        {
            bytecode::disassemble(std::cout, "DISASSEMBLY", reinterpret_cast<uint8_t*>(ptr));
        }
    }
    else if (expr.is_type(Object_Type::Closure))
    {
        auto closure = expr.as_object()->closure();
        bytecode::disassemble(std::cout, "DISASSEMBLY", closure->function(), closure->function()->main_entry());
    }
    return Value::nil();
}

DEFUN("%DEFINE-FUNCTION", func_define_function, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 2);
    auto sym = args[0];
    CHECK_SYMBOL(sym);
    auto func = args[1];
    if (!func.is_lisp_primitive() && !func.is_type(Object_Type::Closure))
    {
        raised_signal = true;
        return gc.list(g.s_TYPE_ERROR, g.s_FUNCTION, func);
    }
    sym.as_object()->symbol()->function(func);
    return sym;
}

static
bool check_string_like(Value &arg, Value &out_signal_args, std::string &out_string)
{
    // Helper to check if arg is a string-like, storing the string representation in out_string.
    // Returns true if a signal should be raised and stores the signal arguments in out_signal_args.
    if (symbolp(arg))
    {
        out_string = arg.as_object()->symbol()->name();
    }
    else if (stringp(arg))
    {
        out_string = lisp_string_to_native_string(arg);
    }
    else
    {
        GC_GUARD();
        out_signal_args = gc.list(g.s_TYPE_ERROR,
                           gc.list(g.get_symbol("OR"),
                                   g.get_symbol("STRING"),
                                   g.get_symbol("SYMBOL"),
                                   g.get_symbol("KEYWORD")),
                           arg);
        GC_UNGUARD();
        return true;
    }
    return false;
}

static
bool check_package(Value &arg, Package **out_pkg, Value &out_signal_args)
{
    // Helper to check type of arg as a package or package designator: i.e.
    // symbol, string, keyword, or package.
    // returns true to raise signal and false if arg is a package designator.
    // stores the found package in out_pkg.
    if (symbolp(arg))
    {
        *out_pkg = g.packages.find(arg.as_object()->symbol()->name());
    }
    else if (stringp(arg))
    {
        *out_pkg = g.packages.find(lisp_string_to_native_string(arg));
    }
    else if (arg.is_type(Object_Type::Package))
    {
        *out_pkg = arg.as_object()->package();
    }
    else
    {
        GC_GUARD();
        out_signal_args = gc.list(g.s_TYPE_ERROR,
                           gc.list(g.get_symbol("OR"),
                                   g.get_symbol("STRING"),
                                   g.get_symbol("SYMBOL"),
                                   g.get_symbol("KEYWORD"),
                                   g.get_symbol("PACKAGE")),
                           arg);
        GC_UNGUARD();
        return true;
    }
    return false;
}

DEFUN("%PACKAGE-NAME", func_package_name, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 1);
    Package *package = nullptr;
    {
        Value res;
        raised_signal = check_package(args[0], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[0]);
        GC_UNGUARD();
        return res;
    }

    return gc.alloc_string(args[0].as_object()->package()->name());
}

DEFUN("%MAKE-PACKAGE", func_make_package, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 1);
    std::string package_name;
    {
        Value signal_args;
        raised_signal = check_string_like(args[0], signal_args, package_name);
        if (raised_signal)
        {
            return signal_args;
        }
    }

    if (g.packages.find(package_name))
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR,
                           gc.alloc_string("A package with the same name already exists."),
                           args[0]);
        GC_UNGUARD();
        return res;
    }

    auto package = g.packages.find_or_create(package_name);
    return package->as_lisp_value();
}

DEFUN("%FIND-PACKAGE", func_find_package, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 1);
    std::string package_name;
    {
        Value signal_args;
        raised_signal = check_string_like(args[0], signal_args, package_name);
        if (raised_signal)
        {
            return signal_args;
        }
    }

    auto pkg = g.packages.find(package_name);
    return pkg ? pkg->as_lisp_value() : Value::nil();
}

DEFUN("%USE-PACKAGE", func_use_package, g.kernel(), false)
{
    CHECK_AT_LEAST_N(nargs, 1);

    std::string package_name;
    {
        Value signal_args;
        raised_signal = check_string_like(args[0], signal_args, package_name);
        if (raised_signal)
        {
            return signal_args;
        }
    }
    Package *package_to_use = g.packages.find(package_name);

    if (package_to_use == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR,
                           gc.alloc_string("Package does not exist"),
                           args[0]);
        GC_UNGUARD();
        return res;
    }

    auto in_package = g.packages.current();
    if (nargs > 1)
    {
        Value signal_args;
        raised_signal = check_package(args[1], &in_package, signal_args);
        if (raised_signal)
        {
            return signal_args;
        }
    }

    if (in_package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    in_package->inherit(package_to_use);
    return g.s_T;
}

DEFUN("%IN-PACKAGE", func_in_package, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 1);


    Package *package_to_use;
    if (args[0].is_type(Object_Type::Package))
    {
        package_to_use = args[0].as_object()->package();
    }
    else
    {
        std::string package_name;
        {
            Value signal_args;
            raised_signal = check_string_like(args[0], signal_args, package_name);
            if (raised_signal)
            {
                return signal_args;
            }
        }
        package_to_use = g.packages.find(package_name);
    }

    if (package_to_use == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR,
                           gc.alloc_string("Package does not exist"),
                           args[0]);
        GC_UNGUARD();
        return res;
    }

    g.packages.in_package(package_to_use);

    set_global(compiler::THE_ROOT_SCOPE,
               g.get_symbol("*PACKAGE*"),
               package_to_use->as_lisp_value());
    return package_to_use->as_lisp_value();
}

DEFUN("%+", func_plus, g.kernel(), false)
{
    /***
        (+ &rest fixnums)
    */
    auto result = Value::wrap_fixnum(0);
    for (uint32_t i = 0; i < nargs; ++i)
    {
        auto tmp = args[i];
        CHECK_FIXNUM(tmp);
        result += tmp;
    }
    return result;
}

DEFUN("%-", func_minus, g.kernel(), false)
{
    /***
        (- &rest fixnums)
    */
    auto result = Value::wrap_fixnum(0);
    if (nargs == 0)
    {
        ;
    }
    else if (nargs == 1)
    {
        CHECK_FIXNUM(args[0]);
        result -= args[0];
    }
    else
    {
        CHECK_FIXNUM(args[0]);
        result = args[0];
        for (uint32_t i = 1; i < nargs; ++i)
        {
            CHECK_FIXNUM(args[i]);
            result -= args[i];
        }
    }
    return result;
}

DEFUN("%*", func_multiply, g.kernel(), false)
{
    /***
        (* &rest fixnums)
    */
    int64_t result = 1;
    for (uint32_t i = 0; i < nargs; ++i)
    {
        auto tmp = args[i];
        CHECK_FIXNUM(tmp);
        result *= tmp.as_fixnum();
    }
    return Value::wrap_fixnum(result);
}

DEFUN("%/", func_divide, g.kernel(), false)
{
    /***
        (/ x y)
    */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);
    auto x = args[0].as_fixnum();
    auto y = args[1].as_fixnum();
    if (y == 0)
    {
        raised_signal = true;
        return gc.list(g.s_DIVIDE_BY_ZERO_ERROR, args[0], args[1]);
    }
    return Value::wrap_fixnum(x / y);
}

DEFUN("%<", func_num_less, g.kernel(), false)
{
    /***
        (< a b &rest more-fixnums)
    */
    CHECK_AT_LEAST_N(nargs, 2);
    auto a = args[0];
    CHECK_FIXNUM(a);
    auto b = args[1];
    CHECK_FIXNUM(b);
    bool result = a.as_fixnum() < b.as_fixnum();
    if (result)
    {
        a = b;
        for (uint32_t i = 2; i < nargs; ++i)
        {
            b = args[i];
            CHECK_FIXNUM(b);
            result = a.as_fixnum() < b.as_fixnum();
            if (result == false)
            {
                break;
            }
            a = b;
        }
    }
    return result ? g.s_T : Value::nil();
}

DEFUN("%=", func_num_equal, g.kernel(), false)
{
    /***
        (= n &rest more-fixnums)
    */
    CHECK_AT_LEAST_N(nargs, 1);
    auto n = args[0];
    CHECK_FIXNUM(n);
    for (uint32_t i = 1; i < nargs; ++i)
    {
        CHECK_FIXNUM(args[i]);
        if (args[i] != n)
        {
            return Value::nil();
        }
    }
    return g.s_T;
}

DEFUN("%>", func_num_greater, g.kernel(), false)
{
    /***
        (> a b &rest more-fixnums)
    */
    CHECK_AT_LEAST_N(nargs, 2);
    auto a = args[0];
    CHECK_FIXNUM(a);
    auto b = args[1];
    CHECK_FIXNUM(b);
    bool result = a.as_fixnum() > b.as_fixnum();
    if (result)
    {
        a = b;
        for (uint32_t i = 2; i < nargs; ++i)
        {
            b = args[i];
            CHECK_FIXNUM(b);
            result = a.as_fixnum() > b.as_fixnum();
            if (result == false)
            {
                break;
            }
            a = b;
        }
    }
    return result ? g.s_T : Value::nil();
}

DEFUN("%FILE-TELLG", func_file_tellg, g.kernel(), false)
{
    /***
        (file-tellg stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_FILE_STREAM(args[0]);
    auto &stm = args[0].as_object()->file_stream()->stream();
    auto pos = stm.tellg();
    return Value::wrap_fixnum(pos);
}

DEFUN("%FILE-SEEKG", func_file_seekg, g.kernel(), false)
{
    /***
        (file-seekg stream offset dir)
    */
    CHECK_EXACTLY_N(nargs, 3);
    CHECK_FILE_STREAM(args[0]);
    CHECK_FIXNUM(args[1]);
    CHECK_SYMBOL(args[2]);
    auto &stm = args[0].as_object()->file_stream()->stream();
    auto pos = std::ios_base::cur;
    if (args[2] == g.s_BEGINNING)
    {
        pos = std::ios_base::beg;
    }
    else if (args[2] == g.s_END)
    {
        pos = std::ios_base::end;
    }
    else if (args[2] == g.s_CURRENT)
    {
        pos = std::ios_base::cur;
    }
    else
    {
        raised_signal = true;
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Expected one of"),
                                   gc.list(g.s_BEGINNING, g.s_CURRENT, g.s_END),
                                   args[2]);
        GC_UNGUARD();
        return signal_args;
    }
    stm.seekg(args[1].as_fixnum(), pos);
    return Value::nil();
}

DEFUN("%FILE-WRITE", func_file_write, g.kernel(), false)
{
    /***
        (file-write stream object)
    */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_FILE_STREAM(args[0]);
    auto &stm = args[0].as_object()->file_stream()->stream();
    auto obj = args[1];
    auto pos = stm.tellg();
    stm << repr(obj);
    auto bytes_written = stm.tellg() - pos;
    stm.flush();
    return Value::wrap_fixnum(bytes_written);
}

DEFUN("%FILE-PUTCHAR", func_file_putchar, g.kernel(), false)
{
    /***
        (file-putchar stream character)
    */

    CHECK_EXACTLY_N(nargs, 2);
    CHECK_FILE_STREAM(args[0]);
    auto stm = args[0].as_object()->file_stream();
    CHECK_CHARACTER(args[1]);
    auto codepoint = args[1].as_character();
    auto bytes_written = stm->write_character(codepoint);
    if (codepoint == '\n')
    {
        stm->flush();
    }
    return Value::wrap_fixnum(bytes_written);
}

DEFUN("%FILE-PUTS", func_file_puts, g.kernel(), false)
{
    /***
        (file-puts stream string)
    */

    CHECK_EXACTLY_N(nargs, 2);
    CHECK_FILE_STREAM(args[0]);
    CHECK_STRING(args[1]);
    auto &stm = args[0].as_object()->file_stream()->stream();
    auto pos = stm.tellg();
    stm << lisp_string_to_native_string(args[1]);
    auto bytes_written = stm.tellg() - pos;
    stm.flush();
    return Value::wrap_fixnum(bytes_written);
}

DEFUN("%TYPE-OF", func_type_of, g.kernel(), false)
{
    /***
        (type-of object)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    if (it.is_fixnum())
    {
        return g.s_FIXNUM;
    }
    if (it.is_nil())
    {
        return g.s_NULL;
    }
    if (it.is_cons())
    {
        return g.s_CONS;
    }
    if (it.is_character())
    {
        return g.s_CHARACTER;
    }
    if (it.is_object())
    {
        switch (it.as_object()->type())
        {
            case Object_Type::Symbol: return g.s_SYMBOL;
            case Object_Type::Closure: return g.s_FUNCTION;
            case Object_Type::Simple_Array:
            {
                auto array = it.as_object()->simple_array();
                return gc.list(g.s_SIMPLE_ARRAY,
                               array->element_type(),
                               Value::wrap_fixnum(array->size()));
            };
            case Object_Type::File_Stream: return g.s_FILE_STREAM;
            case Object_Type::System_Pointer: return g.s_SYSTEM_POINTER;
            case Object_Type::Structure: return it.as_object()->structure()->type_name();
            case Object_Type::Package: return g.s_PACKAGE;
        }
        return Value::nil();
    }
    if (it.is_lisp_primitive())
    {
        return g.s_FUNCTION;
    }
    if (it == g.s_T)
    {
        return g.s_BOOLEAN;
    }
    raised_signal = true;
    GC_GUARD();
    auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Cannot determine type of object."), it);
    GC_UNGUARD();
    return res;
}

DEFUN("%READ", func_read, g.kernel(), false)
{
    /***
        (read &optional file-stream eof-error-p eof-value)
    */

    bool eof_error_p = true;
    if (nargs > 1)
    {
        eof_error_p = !args[1].is_nil();
    }
    auto eof_value = Value::nil();
    if (nargs > 2)
    {
        eof_value = args[2];
    }

    if (nargs == 0)
    {
        Value result;
        if (!read_gc_paused(std::cin, result))
        {
            // @FIXME: This should be checking for errors, not assuming EOF
            if (eof_error_p)
            {
                raised_signal = true;
                return gc.list(g.s_END_OF_FILE);
            }
            return eof_value;
        }
        return result;
    }
    else
    {
        CHECK_FILE_STREAM(args[0]);
        Value result;
        if (!read_gc_paused(args[0].as_object()->file_stream()->stream(), result))
        {
            // @FIXME: This should be checking for errors, not assuming EOF
            if (eof_error_p)
            {
                raised_signal = true;
                return gc.list(g.s_END_OF_FILE);
            }
            return eof_value;
        }
        return result;
    }
}

DEFUN("%MACRO-EXPAND", func_macro_expand, g.kernel(), false)
{
    /***
        (macro-expand expr)
    */
    CHECK_EXACTLY_N(nargs, 1);
    return macro_expand_impl(args[0], *THE_LISP_VM);
}

DEFUN("%EVAL", func_eval, g.kernel(), false)
{
    /***
        (eval expr)
    */
    auto vm = THE_LISP_VM;
    auto save = vm->save();

    CHECK_EXACTLY_N(nargs, 1);
    auto expr = args[0];
    auto expr_handle = gc.pin_value(expr);

    try
    {
        bytecode::Emitter e(compiler::THE_ROOT_SCOPE);

        auto expanded = macro_expand_impl(expr, *vm);
        gc.unpin_value(expr_handle);

        compiler::compile(e, expanded, true, false);
        e.emit_halt();
        e.lock();

        g.resize_globals(compiler::THE_ROOT_SCOPE->locals().size());

        vm->push_frame(nullptr, 0);

        vm->execute(e.bytecode().data());
    }
    catch (VM_State::Signal_Exception e)
    {
        vm->restore(save);
        raised_signal = true;
        return e.what;
    }

    auto result = vm->pop_param();
    vm->restore(save);
    return result;
}

DEFUN("%GENSYM", func_gensym, g.kernel(), false)
{
    /***
        (gensym &optional hint)
    */
    static unsigned int counter = 0;
    std::string sym_name;
    if (nargs != 0)
    {
        auto hint = args[0];
        CHECK_STRING(hint);
        sym_name = lisp_string_to_native_string(hint);
    }
    else
    {
        sym_name = "G";
    }

    sym_name += std::to_string(counter++);
    return gc.alloc_object<Symbol>(sym_name);
}

DEFUN("%MAKE-SYMBOL", func_make_symbol, g.kernel(), false)
{
    /***
        (make-symbol symbol-name)
    */
    CHECK_EXACTLY_N(nargs, 1);
    std::string name;
    CHECK_STRING(args[0]);
    auto array = args[0].as_object()->simple_array();
    for (Fixnum i = 0; i < array->size(); ++i)
    {
        auto codepoint = array->at(i).as_character();
        name += reinterpret_cast<const char*>(&codepoint);
    }
    return gc.alloc_object<Symbol>(name);
}

DEFUN("%SYMBOL-NAME", func_symbol_name, g.kernel(), false)
{
    /***
        (symbol-name symbol)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYMBOL(args[0]);
    return gc.alloc_string(args[0].as_object()->symbol()->name());
}

DEFUN("%SYMBOL-PACKAGE", func_symbol_package, g.kernel(), false)
{
    /***
        (symbol-name symbol)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYMBOL(args[0]);
    auto pkg = args[0].as_object()->symbol()->package();
    return pkg ? pkg->as_lisp_value() : Value::nil();
}

DEFUN("%EXPORT", func_export, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 2);
    auto package = g.packages.current();
    {
        Value res;
        raised_signal = check_package(args[1], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    auto list_of_symbols = args[0];
    CHECK_LIST(list_of_symbols);
    while (!list_of_symbols.is_nil())
    {
        auto sym = car(list_of_symbols);
        CHECK_SYMBOL(sym);
        package->export_symbol(sym.as_object()->symbol()->name());
        list_of_symbols = cdr(list_of_symbols);
    }
    return g.s_T;
}

DEFUN("%IMPORT", func_import, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 2);
    Package *package = nullptr;
    {
        Value res;
        raised_signal = check_package(args[1], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    auto list_of_symbols = args[0];
    CHECK_LIST(list_of_symbols);
    while (!list_of_symbols.is_nil())
    {
        auto sym = car(list_of_symbols);
        CHECK_SYMBOL(sym);
        package->import_symbol(sym);
        list_of_symbols = cdr(list_of_symbols);
    }
    return g.s_T;
}

DEFUN("%INTERN", func_intern, g.kernel(), false)
{
    /***
        (intern symbol-name &optional package)
    */
    CHECK_EXACTLY_N(nargs, 2);
    std::string name;
    CHECK_STRING(args[0]);
    Package *package = nullptr;

    {
        Value res;
        raised_signal = check_package(args[1], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    auto array = args[0].as_object()->simple_array();
    for (Fixnum i = 0; i < array->size(); ++i)
    {
        auto codepoint = array->at(i).as_character();
        name += reinterpret_cast<const char*>(&codepoint);
    }

    auto sym = package->intern_symbol(name);
    return sym;
}

DEFUN("%FIND-SYMBOL", func_find_symbol, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 2);
    Package *package = nullptr;
    {
        Value res;
        raised_signal = check_package(args[1], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    CHECK_STRING(args[0]);
    auto str = lisp_string_to_native_string(args[0]);
    Package::Symbol_Location_Type location;
    Value symbol;
    if (package->find_symbol(str, symbol, &location))
    {
        Value status;
        switch (location)
        {
            case Package::Symbol_Location_Type::Internal: status = g.get_keyword("INTERNAL"); break;
            case Package::Symbol_Location_Type::External: status = g.get_keyword("EXTERNAL"); break;
            case Package::Symbol_Location_Type::Inherited: status = g.get_keyword("INHERITED"); break;
        }
        return gc.list(symbol, status);
    }
    return gc.list(Value::nil(), Value::nil());
}


DEFUN("%EXIT", func_exit, g.kernel(), false)
{
    /***
        (exit &optional n)
    */
    int code = 0;
    if (nargs != 0)
    {
        CHECK_FIXNUM(args[0]);
        code = args[0].as_fixnum();
    }
    exit(code);
}

DEFUN("%SIGNAL", func_signal, g.kernel(), false)
{
    /***
        (signal tag &rest args)
    */
    CHECK_AT_LEAST_N(nargs, 1);
    raised_signal = true;
    return to_list(args, nargs);
}

DEFUN("%MAKE-ARRAY", func_make_array, g.kernel(), false)
{
    /***
        (make-array length type fill-pointer)
    */
    CHECK_EXACTLY_N(nargs, 3);
    auto length = args[0];
    CHECK_FIXNUM(length);
    auto fill_pointer = args[2];
    CHECK_FIXNUM(fill_pointer);
    auto type = args[1];
    if (type == g.s_CHARACTER || type == g.s_FIXNUM)
    {
        return gc.alloc_object<Simple_Array>(type, length.as_fixnum(), fill_pointer.as_fixnum());
    }
    return gc.alloc_object<Simple_Array>(g.s_T, length.as_fixnum(), fill_pointer.as_fixnum());
}

DEFUN("%ARRAY-PUSH-BACK", func_array_push_back, g.kernel(), false)
{
    /***
        (array-push-back array value)
    */
    CHECK_EXACTLY_N(nargs, 2);
    auto array_val = args[0];
    CHECK_SIMPLE_ARRAY(array_val);

    auto value = args[1];
    auto array = array_val.as_object()->simple_array();
    auto type = array->element_type();
    if (type != g.s_T)
    {
        if (type == g.s_FIXNUM)
        {
            CHECK_FIXNUM(value);
        }
        else if (type == g.s_CHARACTER)
        {
            CHECK_CHARACTER(value);
        }
    }
    array->push_back(value);
    return value;
}

DEFUN("%ARRAY-CAPACITY", func_array_capacity, g.kernel(), false)
{
    /***
        (array-capacity array)
    */
    CHECK_EXACTLY_N(nargs, 1);

    auto array = args[0];
    CHECK_SIMPLE_ARRAY(array);
    return Value::wrap_fixnum(array.as_object()->simple_array()->capacity());
}

DEFUN("%ARRAY-LENGTH", func_array_length, g.kernel(), false)
{
    /***
        (array-length array)
    */
    CHECK_EXACTLY_N(nargs, 1);

    auto array = args[0];
    CHECK_SIMPLE_ARRAY(array);
    return Value::wrap_fixnum(array.as_object()->simple_array()->size());
}

DEFUN("%ARRAY-TYPE", func_array_type, g.kernel(), false)
{
    /***
        (array-type array)
    */
    CHECK_EXACTLY_N(nargs, 1);

    auto array = args[0];
    CHECK_SIMPLE_ARRAY(array);
    return array.as_object()->simple_array()->element_type();
}

DEFUN("%BITS-OF", func_bits_of, g.kernel(), false)
{
    /***
        (bits-of object)
    */
    CHECK_EXACTLY_N(nargs, 1);

    auto obj = args[0];
    auto ret = gc.alloc_object<Simple_Array>(g.s_BIT, 64);
    auto bits = obj.bits();
    auto array = ret.as_object()->simple_array();
    for (int i = 0; i < 64; ++i)
    {
        array->at(i) = Value::wrap_fixnum(bits & 1);
        bits >>= 1;
    }
    return ret;
}

DEFUN("%CODE-CHAR", func_code_char, g.kernel(), false)
{
    /***
        (code-char integer)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_FIXNUM(args[0]);
    auto char_code = args[0].as_fixnum();
    return Value::wrap_character(char_code);
}

DEFUN("%CHAR-CODE", func_char_code, g.kernel(), false)
{
    /***
        (char-code character)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_CHARACTER(args[0]);
    auto character = args[0].as_character();
    return Value::wrap_fixnum(character);
}

static
std::ios_base::openmode get_mode(Value v)
{
    if (v == g.s_OVERWRITE)
    {
        return std::ios_base::trunc;
    }
    else if (v == g.s_READ)
    {
        return std::ios_base::in;
    }
    else if (v == g.s_APPEND)
    {
        return std::ios_base::app;
    }
    return static_cast<std::ios_base::openmode>(0);
}

DEFUN("%OPEN", func_open, g.kernel(), false)
{
    /***
        (open file-path direction)
    */
    CHECK_EXACTLY_N(nargs, 2);

    CHECK_STRING(args[0]);

    auto path = lisp_string_to_native_string(args[0]);
    auto direction = args[1];
    auto mode = std::ios_base::binary;
    if (direction.is_cons())
    {
        auto p = direction;
        while (!p.is_nil())
        {
            CHECK_SYMBOL(car(p));
            mode |= get_mode(car(p));
            p = cdr(p);
        }
    }
    else
    {
        CHECK_SYMBOL(direction);
        mode = get_mode(direction);
    }
    if (mode != std::ios_base::binary)
    {
        return gc.alloc_object<File_Stream>(path, mode);
    }
    return Value::nil();
}

DEFUN("%CLOSE", func_close, g.kernel(), false)
{
    /***
        (close file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    it.as_object()->file_stream()->stream().close();
    return g.s_T;
}

DEFUN("%FILE-PATH", func_file_path, g.kernel(), false)
{
    /***
        (file-path file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    return gc.alloc_string(it.as_object()->file_stream()->path());
}

DEFUN("%FILE-OK-P", func_file_ok_p, g.kernel(), false)
{
    /***
        (file-ok-p file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    if (it.is_nil()) return it;
    CHECK_FILE_STREAM(it);
    return it.as_object()->file_stream()->stream().good() ? g.s_T : Value::nil();
}

DEFUN("%FILE-EOF-P", func_file_eof_p, g.kernel(), false)
{
    /***
        (file-eof-p file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    if (it.is_nil()) return it;
    CHECK_FILE_STREAM(it);
    return it.as_object()->file_stream()->stream().eof() ? g.s_T : Value::nil();
}

DEFUN("%FILE-MODE", func_file_mode, g.kernel(), false)
{
    /***
        (file-mode file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto mode = it.as_object()->file_stream()->mode();
    std::vector<Value> vals;
    if (mode | std::ios_base::app)
    {
        vals.push_back(g.s_APPEND);
    }
    if (mode | std::ios_base::trunc)
    {
        vals.push_back(g.s_OVERWRITE);
    }
    if (mode | std::ios_base::in)
    {
        vals.push_back(g.s_READ);
    }
    return to_list(vals);
}

DEFUN("%FILE-FLUSH", func_file_flush, g.kernel(), false)
{
    /***
        (file-flush file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    it.as_object()->file_stream()->stream().flush();
    return g.s_T;
}

DEFUN("%FILE-READ-BYTE", func_file_read_byte, g.kernel(), false)
{
    /***
        (file-read-byte file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto &stm = it.as_object()->file_stream()->stream();
    if (stm.eof())
    {
        raised_signal = true;
        return gc.list(g.s_END_OF_FILE);
    }
    return Value::wrap_fixnum(stm.get());
}

DEFUN("%FILE-PEEK-BYTE", func_file_peek_byte, g.kernel(), false)
{
    /***
        (file-peek-byte file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto &stm = it.as_object()->file_stream()->stream();
    if (stm.eof())
    {
        raised_signal = true;
        return gc.list(g.s_END_OF_FILE);
    }
    return Value::wrap_fixnum(stm.peek());
}

DEFUN("%FILE-PEEK-CHARACTER", func_file_peek_character, g.kernel(), false)
{
    /***
        (file-peek-character file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto fs = it.as_object()->file_stream();
    if (fs->stream().eof())
    {
        raised_signal = true;
        return gc.list(g.s_END_OF_FILE);
    }
    return Value::wrap_character(fs->peek_character());
}

DEFUN("%FILE-READ-CHARACTER", func_file_read_character, g.kernel(), false)
{
    /***
        (file-read-character file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto fs = it.as_object()->file_stream();
    if (fs->stream().eof())
    {
        raised_signal = true;
        return gc.list(g.s_END_OF_FILE);
    }
    return Value::wrap_character(fs->read_character());
}

DEFUN("%FUNCTION-DEFINITION", func_function_definition, g.kernel(), false)
{
    /***
        (function-definition symbol)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto sym = args[0];
    CHECK_SYMBOL(sym);
    return sym.as_object()->symbol()->function();
}

DEFUN("%STRUCTURE-DEFINITION", func_structure_definition, g.kernel(), false)
{
    /***
        (structure-definition object)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_STRUCT(args[0]);
    return args[0].as_object()->structure()->type();
}

DEFUN("%CREATE-INSTANCE", func_create_instance, g.kernel(), false)
{
    /***
        (create-instance type &rest slots)
     */
    CHECK_AT_LEAST_N(nargs, 1);
    auto instance_val = gc.alloc_object<Structure>(args[0], nargs-1);
    if (nargs > 1)
    {
        auto inst = instance_val.as_object()->structure();
        for (uint32_t i = 1; i < nargs; ++i)
        {
            inst->slot_value(i-1) = args[i];
        }
    }
    return instance_val;
}

DEFUN("%GET-SLOT", func_get_slot, g.kernel(), false)
{
    /***
        (get-slot object n)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_STRUCT(args[0]);
    CHECK_FIXNUM(args[1]);
    auto index = args[1].as_fixnum();
    return args[0].as_object()->structure()->slot_value(index);
}

DEFUN("%SET-SLOT", func_set_slot, g.kernel(), false)
{
    /***
        (set-slot object n value)
     */
    CHECK_EXACTLY_N(nargs, 3);
    CHECK_STRUCT(args[0]);
    CHECK_FIXNUM(args[1]);
    auto index = args[1].as_fixnum();
    auto value = args[2];
    args[0].as_object()->structure()->slot_value(index) = value;
    return value;
}

DEFUN("%GC-PAUSE", func_gc_pause, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 0);
    return gc.pause() ? g.s_T : Value::nil();
}

DEFUN("%GC-PAUSED-P", func_gc_paused_p, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 0);
    return gc.paused() ? g.s_T : Value::nil();
}

DEFUN("%GC-SET-PAUSED", func_gc_set_paused, g.kernel(), false)
{
    CHECK_EXACTLY_N(nargs, 1);
    gc.set_paused(!args[0].is_nil());
    return args[0];
}

DEFUN("%GC-COLLECT", func_gc_collect, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.mark_and_sweep());
}

DEFUN("%GC-GET-CONSED", func_gc_get_consed, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.get_consed());
}

DEFUN("%GC-GET-TIME-SPENT-IN-GC", func_gc_get_time_spent_in_gc, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.get_time_spent_in_gc());
}

DEFUN("%GC-GET-TIMES-GC-HAS-RUN", func_gc_get_times_gc_has_run, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.get_times_gc_has_run());
}

///////////////////////////////////////////////////////////////////////
// Exported Functions

DEFUN("BIT-NOT", func_bit_not, g.kernel(), true)
{
    /***
        (bit-not x)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_FIXNUM(args[0]);
    auto x = args[0].as_fixnum();
    return Value::wrap_fixnum(~x);
}

DEFUN("BIT-AND", func_bit_and, g.kernel(), true)
{
    /***
        (bit-and x y)
    */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);
    auto x = args[0].as_fixnum();
    auto y = args[1].as_fixnum();
    return Value::wrap_fixnum(x & y);
}

DEFUN("BIT-IOR", func_bit_ior, g.kernel(), true)
{
    /***
        (bit-or x y &rest z)
    */
    CHECK_AT_LEAST_N(nargs, 2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);

    auto res = args[0] | args[1];
    for (uint32_t i = 2; i < nargs; ++i)
    {
        CHECK_FIXNUM(args[i]);
        res |= args[i];
    }
    return res;
}

DEFUN("BIT-XOR", func_bit_xor, g.kernel(), true)
{
    /***
        (bit-xor x y)
    */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);
    auto x = args[0].as_fixnum();
    auto y = args[1].as_fixnum();
    return Value::wrap_fixnum(x ^ y);
}

DEFUN("BIT-SHIFT", func_bit_shift, g.kernel(), true)
{
    /***
        (bit-shift integer count)
    */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);
    auto integer = args[0].as_fixnum();
    auto count = args[1].as_fixnum();
    if (count > 0)
    {
        return Value::wrap_fixnum(integer << count);
    }
    return Value::wrap_fixnum(integer >> -count);
}

DEFUN("GET-WORKING-DIRECTORY", func_get_working_directory, g.kernel(), true)
{
    /***
        (get-working-directory)
    */
    std::error_code error;
    auto current_path = plat::get_working_directory(error);
    return error.value() != 0 ? Value::nil() : gc.alloc_string(current_path);
}

DEFUN("CHANGE-DIRECTORY", func_change_directory, g.kernel(), true)
{
    /***
        (change-directory path)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_STRING(args[0]);
    auto new_path = lisp_string_to_native_string(args[0]);
    std::error_code error;
    plat::change_directory(new_path, error);
    if (error.value() != 0)
    {
        return Value::nil();
    }

    error.clear();
    auto current_path = plat::get_working_directory(error);
    return error.value() != 0 ? Value::nil() : gc.alloc_string(current_path);
}

DEFUN("GET-EXECUTABLE-PATH", func_get_executable_path, g.kernel(), true)
{
    /***
        (get-executable-path)
    */
    CHECK_EXACTLY_N(nargs, 0);
    return gc.alloc_string(plat::get_executable_path());
}

DEFUN("GET-CLOCK-TICKS", func_get_clock_ticks, g.kernel(), true)
{
    /***
        (get-clock-ticks)
    */
    CHECK_EXACTLY_N(nargs, 0);
    auto now = std::chrono::high_resolution_clock::now();
    auto duration = now.time_since_epoch();
    auto microseconds = std::chrono::duration_cast<std::chrono::microseconds>(duration);
    return Value::wrap_fixnum(microseconds.count());
}

DEFUN("CLOCKS-PER-SECOND", func_clocks_per_second, g.kernel(), true)
{
    /***
        (clocks-per-second)
    */
    CHECK_EXACTLY_N(nargs, 0);
    return Value::wrap_fixnum(1000000); // @FIXME use something better than costant number
}

DEFUN("OPERATING-SYSTEM", func_operating_system, g.kernel(), true)
{
    /***
        (operating-system)
    */
#if defined(_WIN32) || defined(_WIN64)
    return g.core()->export_symbol("WINDOWS");
#elif defined(__linux__)
    return g.core()->export_symbol("LINUX");
#elif defined(__APPLE__) || defined(__MACH__)
    return g.core()->export_symbol("MAC-OS-X");
#else
    return g.core()->export_symbol("UNKNOWN-OS");
#endif
}

static
bool ffi_try_marshal(Value val, void **out_ptr)
{
    if (val.is_type(Object_Type::System_Pointer))
    {
        *out_ptr = val.as_object()->system_pointer();
        return true;
    }
    if (val.is_fixnum())
    {
        *out_ptr = reinterpret_cast<void*>(val.as_fixnum());
        return true;
    }
    if (val.is_character())
    {
        *out_ptr = reinterpret_cast<void*>(val.as_character());
        return true;
    }
    if (val.is_object())
    {
        if (val.is_type(Object_Type::Simple_Array))
        {
            auto array = val.as_object()->simple_array();
            if (array->element_type() == g.s_CHARACTER)
            {
                auto str = lisp_string_to_native_string(val);
                auto buffer = (char*)ffi::alloc_mem(str.size()+1); // @LEAK
                memcpy(buffer, str.data(), str.size());
                buffer[str.size()] = 0;
                *out_ptr = buffer;
                return true;
            }
        }
    }
    return false;
}

DEFUN("ERRNO", func_errno, g.kernel(), true)
{
    /***
        (errno)
     */
    return Value::wrap_fixnum(errno);
}

DEFUN("ERRNO-STR", func_errno_str, g.kernel(), true)
{
    /***
        (errno-str)
     */
    if (nargs != 0)
    {
        CHECK_FIXNUM(args[0]);
        return gc.alloc_string(strerror(args[0].as_fixnum()));
    }
    return gc.alloc_string(strerror(errno));
}

DEFUN("FFI-MACHINE-POINTER-SIZE", func_ffi_machine_pointer_size, g.kernel(), true)
{
    /***
        (ffi-machine-pointer-size)
     */
    return Value::wrap_fixnum(sizeof(void*));
}

DEFUN("FFI-OPEN", func_ffi_open, g.kernel(), true)
{
    /***
        (ffi-open dll-path)
     */
    CHECK_EXACTLY_N(nargs, 1);
    auto lib = args[0];
    CHECK_STRING(lib);
    auto lib_str = lisp_string_to_native_string(lib);
    auto handle = ffi::open(lib_str.c_str());
    return handle ? gc.alloc_object<System_Pointer>(handle) : Value::nil();
}

DEFUN("FFI-CLOSE", func_ffi_close, g.kernel(), true)
{
    /***
        (ffi-close dll-handle)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    ffi::close(args[0].as_object()->system_pointer());
    return Value::nil();
}

DEFUN("FFI-GET-SYMBOL", func_ffi_get_symbol, g.kernel(), true)
{
    /***
        (ffi-get-symbol dll-handle symbol-name)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    CHECK_STRING(args[1]);

    auto handle = args[0].as_object()->system_pointer();
    auto symbol = args[1];
    auto symbol_str = lisp_string_to_native_string(symbol);

    auto ptr = ffi::getsym(handle, symbol_str.c_str());
    return ptr ? gc.alloc_object<System_Pointer>(ptr) : Value::nil();
}

DEFUN("FFI-CALL", func_ffi_call, g.kernel(), true)
{
    /***
        (ffi-call c-function &rest args)
     */
    CHECK_AT_LEAST_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto func = args[0].as_object()->system_pointer();
    std::vector<void *> marshalled;
    for (uint32_t i = 1; i < nargs; ++i)
    {
        void *m = nullptr;
        if (ffi_try_marshal(args[i], &m))
        {
            marshalled.push_back(m);
        }
        else
        {
            raised_signal = true;
            GC_GUARD();
            auto res = gc.list(g.s_MARSHAL_ERROR, gc.alloc_string("Cannot marshal object"), args[i]);
            GC_UNGUARD();
            return res;
        }
    }
    auto result = ffi::call(func, marshalled.data(), marshalled.size());
    return gc.alloc_object<System_Pointer>(result);
}

DEFUN("FFI-NULLPTR", func_ffi_nullptr, g.kernel(), true)
{
    /***
        (ffi-nullptr)
     */
    CHECK_EXACTLY_N(nargs, 0);
    return gc.alloc_object<System_Pointer>(nullptr);
}

DEFUN("FFI-ALLOC", func_ffi_alloc, g.kernel(), true)
{
    /***
        (ffi-alloc size)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_FIXNUM(args[0]);
    auto size = args[0].as_fixnum();
    return gc.alloc_object<System_Pointer>(ffi::alloc_mem(size));
}

DEFUN("FFI-ZERO-ALLOC", func_ffi_zero_alloc, g.kernel(), true)
{
    /***
        (ffi-zero-alloc size)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_FIXNUM(args[0]);
    auto size = args[0].as_fixnum();
    return gc.alloc_object<System_Pointer>(ffi::calloc_mem(size));
}

DEFUN("FFI-FREE", func_ffi_free, g.kernel(), true)
{
    /***
        (ffi-free pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = args[0].as_object()->system_pointer();
    ffi::free_mem(ptr);
    args[0].as_object()->system_pointer(nullptr);
    return g.s_T;
}

DEFUN("FFI-REF", func_ffi_ref, g.kernel(), true)
{
    /***
        (ffi-ref pointer &optional offset)
     */
    CHECK_AT_LEAST_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    if (nargs == 1)
    {
        return gc.alloc_object<System_Pointer>(args[0].as_object()->pointer_ref());
    }
    else
    {
        auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->system_pointer());
        CHECK_FIXNUM(args[1]);
        auto offset = args[1].as_fixnum();

        return gc.alloc_object<System_Pointer>(ptr + offset);
    }
}

DEFUN("FFI-REF-8", func_ffi_ref_8, g.kernel(), true)
{
    /***
        (ffi-ref-8 pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(*ptr);
}

DEFUN("FFI-REF-16", func_ffi_ref_16, g.kernel(), true)
{
    /***
        (ffi-ref-16 pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint16_t*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(*ptr);
}

DEFUN("FFI-REF-32", func_ffi_ref_32, g.kernel(), true)
{
    /***
        (ffi-ref-32 pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint32_t*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(*ptr);
}

DEFUN("FFI-REF-64", func_ffi_ref_64, g.kernel(), true)
{
    /***
        (ffi-ref-64 pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint64_t*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(*ptr);
}

DEFUN("FFI-SET-REF", func_ffi_set_ref, g.kernel(), true)
{
    /***
        (ffi-set-ref pointer value value-size)
     */
    CHECK_EXACTLY_N(nargs, 3);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->system_pointer());
    CHECK_SYSTEM_POINTER(args[1]);
    auto value = reinterpret_cast<uint8_t*>(args[1].as_object()->system_pointer());
    CHECK_FIXNUM(args[2]);
    auto value_size = args[2].as_fixnum();

    memcpy(ptr, value, value_size);
    return args[2];
}

DEFUN("FFI-SET-REF-8", func_ffi_set_ref_8, g.kernel(), true)
{
    /***
        (ffi-set-ref-8 pointer value)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    uint8_t value;
    auto ptr = reinterpret_cast<decltype(value)*>(args[0].as_object()->system_pointer());
    if (args[1].is_type(Object_Type::System_Pointer))
    {
        value = static_cast<decltype(value)>(
            reinterpret_cast<uintptr_t>(args[1].as_object()->system_pointer()));
    }
    else
    {
        CHECK_FIXNUM(args[1]);
        value = args[1].as_fixnum() & 0xff;
    }
    *ptr = value;
    return Value::wrap_fixnum(value);
}

DEFUN("FFI-SET-REF-16", func_ffi_set_16, g.kernel(), true)
{
    /***
        (ffi-set-ref-16 pointer value)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    uint16_t value;
    auto ptr = reinterpret_cast<decltype(value)*>(args[0].as_object()->system_pointer());
    if (args[1].is_type(Object_Type::System_Pointer))
    {
        value = static_cast<decltype(value)>(
            reinterpret_cast<uintptr_t>(args[1].as_object()->system_pointer()));
    }
    else
    {
        CHECK_FIXNUM(args[1]);
        value = args[1].as_fixnum() & 0xffff;
    }
    *ptr = value;
    return Value::wrap_fixnum(value);
}

DEFUN("FFI-SET-REF-32", func_ffi_set_32, g.kernel(), true)
{
    /***
        (ffi-set-ref-32 pointer value)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    uint32_t value;
    auto ptr = reinterpret_cast<decltype(value)*>(args[0].as_object()->system_pointer());
    if (args[1].is_type(Object_Type::System_Pointer))
    {
        value = static_cast<decltype(value)>(
            reinterpret_cast<uintptr_t>(args[1].as_object()->system_pointer()));
    }
    else
    {
        CHECK_FIXNUM(args[1]);
        value = args[1].as_fixnum() & 0xffffffff;
    }
    *ptr = value;
    return Value::wrap_fixnum(value);
}

DEFUN("FFI-SET-REF-64", func_ffi_set_64, g.kernel(), true)
{
    /***
        (ffi-set-ref-64 pointer value)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    uint64_t value;
    auto ptr = reinterpret_cast<decltype(value)*>(args[0].as_object()->system_pointer());
    if (args[1].is_type(Object_Type::System_Pointer))
    {
        value = static_cast<decltype(value)>(
            reinterpret_cast<uintptr_t>(args[1].as_object()->system_pointer()));
    }
    else
    {
        CHECK_FIXNUM(args[1]);
        value = args[1].as_fixnum();
    }
    *ptr = value;
    return Value::wrap_fixnum(value);
}

DEFUN("FFI-MARSHAL", func_ffi_marshal, g.kernel(), true)
{
    /***
        (ffi-marshal object)
     */
    CHECK_EXACTLY_N(nargs, 1);
    void *result = nullptr;
    if (ffi_try_marshal(args[0], &result))
    {
        return gc.alloc_object<System_Pointer>(result);
    }
    raised_signal = true;
    GC_GUARD();
    auto res = gc.list(g.s_MARSHAL_ERROR, gc.alloc_string("Cannot marshal object"), args[0]);
    GC_UNGUARD();
    return res;
}

DEFUN("FFI-STRLEN", func_ffi_strlen, g.kernel(), true)
{
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<const char*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(strlen(ptr));
}

DEFUN("FFI-COERCE-FIXNUM", func_ffi_coerce_fixnum, g.kernel(), true)
{
    /***
        (ffi-coerce-fixnum system-pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<Fixnum>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(ptr);
}

DEFUN("FFI-COERCE-INT", func_ffi_coerce_int, g.kernel(), true)
{
    /***
        (ffi-coerce-int system-pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = static_cast<int>(reinterpret_cast<uintptr_t>(args[0].as_object()->system_pointer()));
    return Value::wrap_fixnum(ptr);
}

DEFUN("FFI-COERCE-STRING", func_ffi_coerce_string, g.kernel(), true)
{
    /***
        (ffi-coerce-string system-pointer &optional length)
     */
    CHECK_AT_LEAST_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<const char*>(args[0].as_object()->system_pointer());
    if (nargs == 1)
    {
        return gc.alloc_string(ptr);
    }
    CHECK_FIXNUM(args[1]);
    auto len = args[1].as_fixnum();
    return gc.alloc_string(ptr, len);
}

}

static
void export_function(Package *package, const std::string &name, Primitive func)
{
    auto symbol = package->export_symbol(name);
    symbol.as_object()->symbol()->function(Value::wrap_primitive(func));
}

static
void internal_function(Package *package, const std::string &name, Primitive func)
{
    auto symbol = package->intern_symbol(name);
    symbol.as_object()->symbol()->function(Value::wrap_primitive(func));
}

static
void set_global(compiler::Scope *scope, Value symbol_value, Value value)
{
    if (!symbolp(symbol_value))
    {
        fprintf(stderr, "Tried to set global with non-symbol: %s\n", repr(symbol_value).c_str());
        return;
    }
    auto symbol = symbol_value.as_object()->symbol();
    auto root = scope->get_root();
    uint32_t global_idx;
    if (!root->resolve_local(symbol, global_idx))
    {
        root->create_variable(symbol, &global_idx);
    }
    g.resize_globals(root->locals().size());
    g.global_value_slots[global_idx] = value;
}

static
void initialize_globals(compiler::Scope *root_scope, char **argv)
{
    auto kernel = g.kernel();
    auto core = g.core();
    auto user = g.user();

    core->inherit(kernel);
    kernel->inherit(core);
    user->inherit(core);

    g.s_pCAR             = kernel->export_symbol("%CAR");
    g.s_pCDR             = kernel->export_symbol("%CDR");
    g.s_pCONS            = kernel->export_symbol("%CONS");
    g.s_pEQ              = kernel->export_symbol("%EQ");
    g.s_pRPLACA          = kernel->export_symbol("%RPLACA");
    g.s_pRPLACD          = kernel->export_symbol("%RPLACD");
    g.s_pSETQ            = kernel->export_symbol("%SETQ");
    g.s_pAREF            = kernel->export_symbol("%AREF");
    g.s_pASET            = kernel->export_symbol("%ASET");
    g.s_pDEBUGGER        = kernel->export_symbol("%DEBUGGER");
    g.s_pAPPLY           = kernel->export_symbol("%APPLY");
    g.s_pFUNCALL         = kernel->export_symbol("FUNCALL");
    g.s_pTAGBODY         = kernel->export_symbol("%TAGBODY");
    g.s_pGO              = kernel->export_symbol("%GO");
    g.s_pSIGNAL          = kernel->export_symbol("%SIGNAL");
    g.s_pHANDLER_CASE    = kernel->export_symbol("%HANDLER-CASE");
    g.s_pDEFINE_MACRO    = kernel->export_symbol("%DEFINE-MACRO");
    g.s_pLAMBDA           = kernel->export_symbol("%LAMBDA");

    g.s_T                = core->export_symbol("T");
    g.s_IF               = core->export_symbol("IF");
    g.s_OR               = core->export_symbol("OR");
    g.s_FIXNUM           = core->export_symbol("FIXNUM");
    g.s_CONS             = core->export_symbol("CONS");
    g.s_NULL             = core->export_symbol("NULL");
    g.s_LIST             = core->export_symbol("LIST");
    g.s_CHARACTER        = core->export_symbol("CHARACTER");
    g.s_SYMBOL           = core->export_symbol("SYMBOL");
    g.s_STRING           = core->export_symbol("STRING");
    g.s_FUNCTION         = core->export_symbol("FUNCTION");
    g.s_BOOLEAN          = core->export_symbol("BOOLEAN");
    g.s_STRUCTURE        = core->export_symbol("STRUCTURE");
    g.s_PACKAGE          = core->export_symbol("PACKAGE");
    g.s_FILE_STREAM      = core->export_symbol("FILE-STREAM");
    g.s_SYSTEM_POINTER   = core->export_symbol("SYSTEM-POINTER");
    g.s_SIMPLE_ARRAY     = core->export_symbol("SIMPLE-ARRAY");
    g.s_QUOTE            = core->export_symbol("QUOTE");
    g.s_QUASIQUOTE       = core->export_symbol("QUASIQUOTE");
    g.s_UNQUOTE          = core->export_symbol("UNQUOTE");
    g.s_UNQUOTE_SPLICING = core->export_symbol("UNQUOTE-SPLICING");
    g.s_TYPE_ERROR       = core->export_symbol("TYPE-ERROR");
    g.s_SIMPLE_ERROR     = core->export_symbol("SIMPLE-ERROR");
    g.s_aOPTIONAL        = core->export_symbol("&OPTIONAL");
    g.s_aREST            = core->export_symbol("&REST");
    g.s_aBODY            = core->export_symbol("&BODY");

    g.s_DIVIDE_BY_ZERO_ERROR = core->export_symbol("DIVIDE-BY-ZERO-ERROR");
    g.s_INDEX_OUT_OF_BOUNDS_ERROR = core->export_symbol("INDEX-OUT-OF-BOUNDS-ERROR");
    g.s_END_OF_FILE      = core->export_symbol("END-OF-FILE");
    g.s_BIT              = core->export_symbol("BIT");

    g.s_OVERWRITE        = core->export_symbol("OVERWRITE");
    g.s_APPEND           = core->export_symbol("APPEND");
    g.s_READ             = core->export_symbol("READ");

    g.s_MARSHAL_ERROR    = core->export_symbol("MARSHAL-ERROR");

    g.s_BEGINNING        = core->export_symbol("BEGINNING");
    g.s_END              = core->export_symbol("END");
    g.s_CURRENT          = core->export_symbol("CURRENT");

    for (auto &init : g_function_initializers)
    {
        if (init.is_exported)
        {
            export_function(init.package, init.lisp_name, init.cpp_function);
        }
        else
        {
            internal_function(init.package, init.lisp_name, init.cpp_function);
        }
    }

#if defined(__unix__) || defined(__unix)
    set_global(root_scope,
               core->export_symbol("*STANDARD-OUTPUT*"),
               gc.alloc_object<File_Stream>("/dev/stdout", std::ios_base::binary | std::ios_base::app));
    set_global(root_scope,
               core->export_symbol("*STANDARD-ERROR*"),
               gc.alloc_object<File_Stream>("/dev/stderr", std::ios_base::binary | std::ios_base::app));
    set_global(root_scope,
               core->export_symbol("*STANDARD-INPUT*"),
               gc.alloc_object<File_Stream>("/dev/stdin", std::ios_base::binary | std::ios_base::in));
#else
    #error "Need to set stdio globals"
#endif
    set_global(root_scope,
               core->export_symbol("*PACKAGE*"),
               Value::nil());

    std::vector<Value> script_args;
    for(; *argv; ++argv)
    {
        script_args.push_back(gc.alloc_string(*argv));
    }

    set_global(root_scope,
               core->export_symbol("*COMMAND-LINE*"),
               to_list(script_args));

    {
        // call this function so the correct symbol is automatically exported
        bool b;
        primitives::func_operating_system(nullptr, 0, b);
    }

}

bool VM_State::find_handler(Value tag, bool auto_pop, Handler_Case &out_case_state, Signal_Handler &out_handler)
{
    bool found = false;
    size_t npop = 0;
    for (auto it = m_handler_cases.rbegin(); !found && it != m_handler_cases.rend(); it++)
    {
        npop++;
        auto &handlers = it->handlers;
        for (auto h = handlers.rbegin(); h != handlers.rend(); ++h)
        {
            if (h->tag == g.s_T || h->tag == tag)
            {
                out_case_state = *it;
                out_handler = *h;
                found = true;
                break;
            }
        }
    }

    if (auto_pop)
    {
        m_handler_cases.resize(m_handler_cases.size() - npop);
    }
    return found;
}

Value VM_State::call_lisp_function(Value function_or_symbol, Value *args, uint32_t nargs)
{
    auto function = symbolp(function_or_symbol)
        ? function_or_symbol.as_object()->symbol()->function()
        : function_or_symbol;

    // What better way to reduce code sync bugs than by just letting the VM handle dispatching the call?
    if (m_stub.emitter == nullptr)
    {
        m_stub.emitter = new bytecode::Emitter(nullptr);
        m_stub.function_offset = m_stub.emitter->position() + 1;
        m_stub.emitter->emit_push_literal(function);
        m_stub.nargs_offset = m_stub.emitter->position() + 1;
        m_stub.emitter->emit_funcall(nargs);
        m_stub.emitter->emit_halt();
        m_stub.emitter->lock();
    }
    else
    {
        m_stub.emitter->set_raw(m_stub.function_offset, function);
        m_stub.emitter->set_raw(m_stub.nargs_offset, nargs);
    }
    for (uint32_t i = 0; i < nargs; ++i)
    {
        push_param(args[i]);
    }
    execute(m_stub.emitter->bytecode().data());
    return pop_param();
}

template<typename Function, typename ...ExtraArgs>
static
Value map(Value list, Function func, ExtraArgs&... args)
{
    if (list.is_nil())
    {
        return list;
    }

    GC_GUARD();
    auto head = gc.list(func(car(list), args...));
    auto current = head;
    list = cdr(list);
    while (!list.is_nil())
    {
        set_cdr(current, gc.list(func(car(list), args...)));
        current = cdr(current);
        list = cdr(list);
    }
    GC_UNGUARD();
    return head;
}

static
Value zip3(Value a, Value b, Value c)
{
    if (a.is_nil())
    {
        return a;
    }

    GC_GUARD();
    auto head = gc.list(gc.cons(car(a), gc.cons(car(b), car(c))));
    auto current = head;
    a = cdr(a); b = cdr(b); c = cdr(c);
    while (!a.is_nil())
    {
        auto next = gc.cons(car(a), gc.cons(car(b), car(c)));
        set_cdr(current, gc.list(next));
        current = cdr(current);
        a = cdr(a); b = cdr(b); c = cdr(c);
    }
    GC_UNGUARD();
    return head;
}

static
Value macro_expand_impl(Value obj, VM_State &vm)
{
    if (!obj.is_cons())
    {
        return obj;
    }
    auto car = first(obj);
    if (symbolp(car))
    {
        if (car == g.s_QUOTE)
        {
            return obj;
        }
        if (car == g.s_IF)
        {
            auto condition = macro_expand_impl(second(obj), vm);
            auto consequence = macro_expand_impl(third(obj), vm);
            auto alternative = macro_expand_impl(fourth(obj), vm);
            return gc.list(car, condition, consequence, alternative);
        }
        if (car == g.s_pDEFINE_MACRO)
        {
            auto macro_name = second(obj);
            auto params_list = third(obj);
            auto body = map(cdddr(obj), macro_expand_impl, vm);
            GC_GUARD();
            auto res = gc.cons(car, gc.cons(macro_name, gc.cons(params_list, body)));
            GC_UNGUARD();
            return res;
        }
        if (car == g.s_pLAMBDA)
        {
            auto lambda_list = second(obj); // @TODO: macroexpand &optional default expressions
            auto body = map(cddr(obj), macro_expand_impl, vm);
            GC_GUARD();
            auto res = gc.cons(car, gc.cons(lambda_list, body));
            GC_UNGUARD();
            return res;
        }
        if (car == g.s_pSETQ)
        {
            auto variable_name = second(obj);
            auto value = macro_expand_impl(third(obj), vm);
            return gc.list(car, variable_name, value);
        }
        if (car == g.s_pHANDLER_CASE)
        {
            auto form = macro_expand_impl(second(obj), vm);
            auto handlers = cddr(obj);
            auto handler_tags = map(handlers, first);
            auto handler_lambda_lists = map(handlers, second);
            auto handler_bodies = map(handlers, cddr);
            auto expanded_bodies = map(handler_bodies, macro_expand_impl, vm);
            auto expanded_handlers = zip3(handler_tags, handler_lambda_lists, expanded_bodies);
            GC_GUARD();
            auto res = gc.cons(car, gc.cons(form, expanded_handlers));
            GC_UNGUARD();
            return res;
        }
        auto it = g.macros.find(car.as_object()->symbol());
        if (it != g.macros.end())
        {
            auto function = it->second;
            auto args = rest(obj);
            auto vec = to_vector(args);
            auto expand1 = vm.call_lisp_function(function, vec.data(), vec.size());
            auto expand_all = macro_expand_impl(expand1, vm);
            return expand_all;
        }
    }
    return map(obj, macro_expand_impl, vm);
}


static FORCE_INLINE
bool is_whitespace(int c)
{
    return (c == ' ' || c == '\n' || c == '\r' || c == '\t');
}

static FORCE_INLINE
bool is_digit(int c)
{
    return ('0' <= c && c <= '9');
}

static FORCE_INLINE
bool is_symbol_start_char(int c)
{
    if (c == '(' || c == ')'
        || c == '\'' || c == '"' || c == '`' || c == ','
        || is_whitespace(c)
        || is_digit(c))
    {
        return false;
    }
    else
    {
        return true;
    }
}

static FORCE_INLINE
bool is_symbol_char(int c)
{
    return is_symbol_start_char(c) || is_digit(c);
}

static FORCE_INLINE
std::string str_upper(std::string in)
{
    std::transform(in.begin(), in.end(), in.begin(), [](unsigned char c){ return std::toupper(c) ; } );
    return in;
}

static FORCE_INLINE
int peek_consuming(std::istream &stream)
{
    while (is_whitespace(stream.peek()))
    {
        stream.get();
    }
    return stream.peek();
}

static
Value make_symbol(const std::string &str)
{
    auto package = g.packages.current();
    std::string symbol_name;
    std::string package_name;
    if (str.size() > 2 && str[0] == ':' && str[1] == ':')
    {
        package = g.keyword();
        symbol_name = str.substr(2);
    }
    else if (str.size() > 1 && str[0] == ':')
    {
        package = g.keyword();
        symbol_name = str.substr(1);
    }
    else
    {
        auto i = str.find(':');
        if (i == std::string::npos) // no colons
        {
            package = g.packages.current();
            symbol_name = str;
        }
        else if (i+1 < str.size() && str[i+1] == ':') // two colons
        {
            package_name = str.substr(0, i);
            package = g.packages.find(package_name);
            symbol_name = str.substr(i+2);
        }
        else // one colon
        {
            package_name = str.substr(0, i);
            package = g.packages.find(package_name);
            symbol_name = str.substr(i+1);
        }
    }

    if (package == nullptr)
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Package does not exist"),
                                   gc.alloc_string(package_name));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }

    if (symbol_name.find(':') != std::string::npos)
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Too many colons in symbol name"),
                                   gc.alloc_string(str));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }

    if (symbol_name.size() == 0)
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("A package was specifed with no symbol"),
                                   gc.alloc_string(str));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }

    return package->intern_symbol(symbol_name);
}

static
bool read(std::istream &source, Value &out_result)
{
    auto c = peek_consuming(source);

    while ((c = peek_consuming(source)) == ';')
    {
        while (!source.eof() && source.peek() != '\n')
        {
            source.get();
        }
    }

    if (source.eof())
    {
        return false;
    }

    if (is_digit(c) || c == '-' || c == '+')
    {
        std::string str;
        str += c;
        source.get();
        while (!source.eof() && is_digit(source.peek()))
        {
            str += source.get();
        }
        bool is_number = source.eof() || !is_symbol_char(source.peek());
        while (!source.eof() && is_symbol_char(source.peek()))
        {
            str += source.get();
        }
        if (is_number && str.size() == 1 && (str[0] == '-' || str[0] == '+'))
        {
            is_number = false;
        }
        if (is_number)
        {
            out_result = Value::wrap_fixnum(std::stoll(str));
            return true;
        }
        out_result = make_symbol(str_upper(str));
        return true;
    }

    if (c == '#')
    {
        source.get();
        if (source.peek() == '\'')
        {
            source.get();
            Value func;
            if (read(source, func))
            {
                out_result = gc.list(g.s_FUNCTION, func);
                return true;
            }
            return false;
        }
        if (source.peek() == '\\')
        {
            source.get();
            std::string character;
            while (!source.eof() && is_symbol_char(source.peek()))
            {
                character += source.get();
            }
            if (character.size() == 0)
            {
                if (source.eof())
                {
                    return false;
                }
                character += source.get();
            }

            if (character.size() == 1)
            {
                out_result = Value::wrap_character(character[0]);
                return true;
            }

            character = str_upper(character);

            if (character == "SPACE")
            {
                out_result = Value::wrap_character(' ');
            }
            else if (character == "RETURN")
            {
                out_result = Value::wrap_character('\r');
            }
            else if (character == "NEWLINE")
            {
                out_result = Value::wrap_character('\n');
            }
            else if (character == "TAB")
            {
                out_result = Value::wrap_character('\t');
            }
            else
            {
                auto it = *reinterpret_cast<const int32_t*>(character.c_str());
                out_result = Value::wrap_character(it);
            }
            return true;
        }
        return false;
    }

    if (c == '"')
    {
        source.get();
        std::string val;
        while (!source.eof() && source.peek() != '"')
        {
            auto c = source.get();
            if (c == '\\')
            {
                c = source.get();
                switch (c)
                {
                    case 'r': c = '\r'; break;
                    case 't': c = '\t'; break;
                    case 'n': c = '\n'; break;
                }
            }
            val += c;
        }
        source.get();
        out_result = gc.alloc_string(val);
        return true;
    }

    if (c == '\'')
    {
        source.get();
        Value quoted;
        if (read(source, quoted))
        {
            out_result = gc.list(g.s_QUOTE, quoted);
            return true;
        }
        return false;
    }

    if (c == '`')
    {
        source.get();
        Value quoted;
        if (read(source, quoted))
        {
            out_result = gc.list(g.s_QUASIQUOTE, quoted);
            return true;
        }
        return false;
    }

    if (c == ',')
    {
        source.get();
        auto symbol = g.s_UNQUOTE;
        if (source.peek() == '@')
        {
            source.get();
            symbol = g.s_UNQUOTE_SPLICING;
        }
        Value value;
        if (read(source, value))
        {
            out_result = gc.list(symbol, value);
            return true;
        }
        return false;
    }

    if (is_symbol_start_char(c))
    {
        std::string str;
        while (!source.eof() && is_symbol_char(source.peek()))
        {
            str += source.get();
        }
        str = str_upper(str);
        if (str == "NIL")
        {
            out_result = Value::nil();
        }
        else
        {
            out_result = make_symbol(str);
        }
        return true;
    }

    if (c == '(')
    {
        source.get();
        if (peek_consuming(source) == ')')
        {
            source.get();
            out_result = Value::nil();
            return true;
        }

        Value car_obj;
        if (!read(source, car_obj))
        {
            return false;
        }
        auto head = gc.list(car_obj);
        car_obj = head;

        while (!source.eof() && peek_consuming(source) != ')')
        {
            if (source.peek() == '.')
            {
                source.get();
                Value cdr_obj;
                if (!read(source, cdr_obj))
                {
                    return false;
                }
                set_cdr(car_obj, cdr_obj);
                break;
            }

            Value elem;
            if (!read(source, elem))
            {
                return false;
            }
            set_cdr(car_obj, gc.list(elem));
            car_obj = cdr(car_obj);
        }

        if (source.eof() || peek_consuming(source) != ')')
        {
            return false;
        }
        source.get();

        out_result = head;
        return true;
    }

    return false;
}

static bool
read_gc_paused(std::istream &source, Value &out_result)
{
    GC_GUARD();
    auto result = read(source, out_result);
    GC_UNGUARD();
    return result;
}


void trace_signal_exception(VM_State &vm, const VM_State::Signal_Exception &e)
{
    if (e.stack_trace_top != e.stack_trace_bottom)
    {
        auto p = e.stack_trace_bottom;
        for (; p != e.stack_trace_top; ++p)
        {
            if (p->ip)
            {
                bytecode::disassemble_maybe_function(std::cout, "WHOOPS", p->ip);
            }
        }
    }
    else if (e.ip)
    {
        vm.debug_dump(std::cout, "WHOOPS", e.ip, true);
    }
    printf("ERROR: %s\n", repr(e.what).c_str());
    //auto signal = car(e.what);
    //auto signal_args = cdr(e.what);
}

bool eval_file(VM_State &vm, compiler::Scope &root_scope, const std::filesystem::path &path)
{
    static auto load_sym = g.core()->export_symbol("LOAD").as_object()->symbol();
    if (!load_sym->function().is_nil())
    {
        auto path_str = gc.alloc_string(path);
        try
        {
            vm.call_lisp_function(load_sym->function(), &path_str, 1);
        }
        catch (VM_State::Signal_Exception e)
        {
            trace_signal_exception(vm, e);
            return false;
        }
        return true;
    }
    else
    {
        std::ifstream stm(path, std::ios::binary);
        if (!stm.is_open())
        {
            fprintf(stderr, "cannot open file: %s\n", path.c_str());
            return false;
        }

        set_global(&root_scope,
                   g.core()->export_symbol("*FILE-PATH*"),
                   gc.alloc_string(path));

        auto here_path = std::filesystem::current_path();
        auto there_path = path.parent_path();
        if (there_path != "")
        {
            std::filesystem::current_path(there_path);
        }

        while (!stm.eof())
        {
            Value out;
            if (read_gc_paused(stm, out))
            {
                auto out_handle = gc.pin_value(out);
                try
                {
                    auto expanded = macro_expand_impl(out, vm);
                    gc.unpin_value(out_handle);

                    bytecode::Emitter e(&root_scope);
                    compiler::compile(e, expanded, true, false);
                    e.emit_halt();
                    e.lock();

                    g.resize_globals(root_scope.locals().size());

                    vm.push_frame(nullptr, 0);
                    vm.execute(e.bytecode().data());
                }
                catch (VM_State::Signal_Exception e)
                {
                    trace_signal_exception(vm, e);
                    return false;
                }
                if (vm.stack_top() != vm.stack_bottom())
                {
                    vm.pop_param();
                }

                if (vm.call_frame_top() != vm.call_frame_bottom())
                {
                    vm.set_frame(vm.pop_frame());
                }
            }
            else if (stm.eof())
            {
                break;
            }
        }
        std::filesystem::current_path(here_path);
        return true;
    }
}

void run_repl(VM_State &vm, compiler::Scope &root_scope)
{
    set_global(&root_scope,
               g.core()->export_symbol("*FILE-PATH*"),
               gc.alloc_string("<stdin>"));


    g.packages.in_package(g.user());
    set_global(&root_scope,
               g.core()->export_symbol("*PACKAGE*"),
               g.packages.current()->as_lisp_value());

    auto &stm = std::cin;
    while (!stm.eof())
    {
        printf("%s> ", g.packages.current()->name().c_str());
        Value out;
        if (read_gc_paused(stm, out))
        {
            auto out_handle = gc.pin_value(out);
            try
            {
                auto expanded = macro_expand_impl(out, vm);
                gc.unpin_value(out_handle);

                bytecode::Emitter e(&root_scope);
                compiler::compile(e, expanded, true, false);
                e.emit_halt();
                e.lock();

                g.resize_globals(root_scope.locals().size());

                vm.push_frame(nullptr, 0);
                vm.execute(e.bytecode().data());
            }
            catch (VM_State::Signal_Exception e)
            {
                gc.unpin_value(out_handle);
                trace_signal_exception(vm, e);
                continue;
            }
            if (vm.stack_top() != vm.stack_bottom())
            {
                auto result = vm.pop_param();
                printf("==> %s\n", repr(result).c_str());
            }

            if (vm.call_frame_top() != vm.call_frame_bottom())
            {
                vm.set_frame(vm.pop_frame());
            }
        }
        else if (stm.eof())
        {
            break;
        }
    }
}

}


int main(int argc, char **argv)
{
    using namespace lisp;
    bool use_boot = true;
    bool repl = false;
    char *file = nullptr;

    int i = 1;
    for (; i < argc; ++i)
    {
        if (argv[i][0] == '-')
        {
            if (strcmp("--no-boot", argv[i]) == 0)
            {
                use_boot = false;
            }
            else if (strcmp("--boot", argv[i]) == 0)
            {
                use_boot = true;
            }
            else if (strcmp("-i", argv[i]) == 0)
            {
                repl = true;
            }
            else if (strcmp("-", argv[i]) == 0)
            {
                i++;
                break;
            }
        }
        else
        {
            file = argv[i];
            break;
        }
    }

    repl = repl || !file;

    compiler::THE_ROOT_SCOPE = new compiler::Scope;
    initialize_globals(compiler::THE_ROOT_SCOPE, argv+i);

    g.packages.in_package(g.user());

    auto vm = THE_LISP_VM = new VM_State;

    gc.set_paused(false);
    if (use_boot)
    {
        auto exe_dir = plat::get_executable_path().parent_path();
        auto boot_path = exe_dir/"lib"/"boot.lisp";
        eval_file(*vm, *compiler::THE_ROOT_SCOPE, std::filesystem::absolute(boot_path));
    }

    if (file)
    {
        eval_file(*vm, *compiler::THE_ROOT_SCOPE, file);
    }

    if (repl)
    {
        run_repl(*vm, *compiler::THE_ROOT_SCOPE);
    }

#if PROFILE_OPCODE_PAIRS
    std::vector<std::pair<Opcode_Pair, int>> results;
    for (auto &[k, v] : vm->opcode_pairs())
    {
        results.push_back({k, v});
    }
    std::sort(results.begin(), results.end(), [](const std::pair<Opcode_Pair, int> &a,
                                                 const std::pair<Opcode_Pair, int> &b) {
                                                  return a.second > b.second;
                                              });
    int count = 10;
    for (auto &[pair, times] : results)
    {
        std::cout << bytecode::opcode_name(static_cast<bytecode::Opcode>(pair.a)) << " : "
                  << bytecode::opcode_name(static_cast<bytecode::Opcode>(pair.b)) << " -> "
                  << times << "\n";
        if (count-- == 0)
        {
            break;
        }
    }
#endif

    return 0;
}
