#include <stdio.h>
#include <chrono>
#include "platform.hpp"
#include "primitives.hpp"
#include "ffi.hpp"

using namespace lisp;

#define NYI(fmt, ...)                                                   \
    do {                                                                \
        fprintf(stderr, "NYI: " __FILE__ ":" STR(__LINE__) "\n\tin %s\n", __PRETTY_FUNCTION__); \
        fprintf(stderr, fmt "\n", ##__VA_ARGS__);                       \
        abort();                                                        \
    } while (0)


#define TYPE_CHECK(what, typecheck, expected)                       \
    do {                                                            \
        if (!(what).typecheck) {                                    \
            bt::trace();                                            \
            raised_signal = true;                                   \
            return list(LISP_SYM_TYPE_ERROR, (expected), (what));   \
        }                                                           \
    } while (0)

#define CHECK_FIXNUM(what) TYPE_CHECK(what, is_fixnum(), LISP_SYM_FIXNUM)
#define CHECK_CONS(what) TYPE_CHECK(what, is_cons(), LISP_SYM_CONS)
#define CHECK_CHARACTER(what) TYPE_CHECK(what, is_character(), LISP_SYM_CHARACTER)
#define CHECK_SYMBOL(what) TYPE_CHECK(what, is_type(SYM_TYPE), LISP_SYM_SYMBOL)
#define CHECK_FILE_STREAM(what) TYPE_CHECK(what, is_type(FILE_STREAM_TYPE), LISP_SYM_FILE_STREAM)
#define CHECK_SYSTEM_POINTER(what) TYPE_CHECK(what, is_type(SYSTEM_POINTER_TYPE), LISP_SYM_SYSTEM_POINTER)

#define CHECK_FUNCTION(what)                                            \
    do {                                                                \
        if (!(what).is_lisp_primitive() && !(what).is_type(LAMBDA_TYPE)) { \
            bt::trace();                                                \
            raised_signal = true;                                       \
            return list(LISP_SYM_TYPE_ERROR, LISP_SYM_FUNCTION, (what)); \
        }                                                               \
    } while (0)

#define CHECK_AT_LEAST_N(what, n)                                       \
    do {                                                                \
        if ((what) < (n)) {                                             \
            bt::trace();                                                \
            raised_signal = true;                                       \
            return list(intern_symbol("ARGUMENT-COUNT-MISMATCH"), lisp_value::wrap_fixnum(n), lisp_value::wrap_fixnum(what)); \
        }                                                               \
    } while (0)

#define CHECK_EXACTLY_N(what, n)                                        \
    do {                                                                \
        if ((what) != (n)) {                                            \
            bt::trace();                                                \
            raised_signal = true;                                       \
            return list(intern_symbol("ARGUMENT-COUNT-MISMATCH"), lisp_value::wrap_fixnum(n), lisp_value::wrap_fixnum(what)); \
        }                                                               \
    } while (0)





lisp_value lisp_prim_plus(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (+ &rest fixnums)
    */
    int64_t result = 0;
    for (uint32_t i = 0; i < nargs; ++i) {
        auto tmp = args[i];
        CHECK_FIXNUM(tmp);
        result += tmp.as_fixnum();
    }
    return lisp_value::wrap_fixnum(result);
}

lisp_value lisp_prim_minus(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (- &rest fixnums)
    */
    int64_t result = 0;
    if (nargs == 0) {
        ;
    }
    else if (nargs == 1) {
        CHECK_FIXNUM(args[0]);
        result = -args[0].as_fixnum();
    }
    else {
        CHECK_FIXNUM(args[0]);
        result = args[0].as_fixnum();
        for (uint32_t i = 1; i < nargs; ++i) {
            CHECK_FIXNUM(args[i]);
            result -= args[i].as_fixnum();
        }
    }
    return lisp_value::wrap_fixnum(result);
}

lisp_value lisp_prim_multiply(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (* &rest fixnums)
    */
    int64_t result = 1;
    for (uint32_t i = 0; i < nargs; ++i) {
        auto tmp = args[i];
        CHECK_FIXNUM(tmp);
        result *= tmp.as_fixnum();
    }
    return lisp_value::wrap_fixnum(result);
}

lisp_value lisp_prim_divide(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (/ x y)
    */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);
    auto x = args[0].as_fixnum();
    auto y = args[1].as_fixnum();
    if (y == 0) {
        static auto DIVIDE_BY_ZERO_ERROR = intern_symbol("DIVIDE-BY-ZERO-ERROR");
        raised_signal = true;
        return list(DIVIDE_BY_ZERO_ERROR, args[0], args[1]);
    }
    return lisp_value::wrap_fixnum(x / y);
}

lisp_value lisp_prim_print(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (print obj &optional stream)
    */
    CHECK_AT_LEAST_N(nargs, 1);
    lisp_file_stream *stream = nullptr;
    auto obj = args[0];
    if (nargs == 1 || args[1] == LISP_T) {
        auto _stdout = cdr(symbol_lookup(env, intern_symbol("*STANDARD-OUTPUT*")));
        if (_stdout.is_not_nil()) {
            CHECK_FILE_STREAM(_stdout);
            stream = _stdout.as_object()->file_stream();
        }
    }
    else {
        CHECK_FILE_STREAM(args[1]);
        stream = args[1].as_object()->file_stream();
    }
    if (stream) {
        auto s = repr(obj);
        stream->write(s);
    }
    return obj;
}

lisp_value lisp_prim_num_less(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
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
    if (result) {
        a = b;
        for (uint32_t i = 2; i < nargs; ++i) {
            b = args[i];
            CHECK_FIXNUM(b);
            result = a.as_fixnum() < b.as_fixnum();
            if (result == false) {
                break;
            }
            a = b;
        }
    }
    return result ? LISP_T : LISP_NIL;
}

lisp_value lisp_prim_num_equal(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (= n &rest more-fixnums)
    */
    CHECK_AT_LEAST_N(nargs, 1);
    auto n = args[0];
    CHECK_FIXNUM(n);
    for (uint32_t i = 1; i < nargs; ++i) {
        CHECK_FIXNUM(args[i]);
        if (args[i] != n)
            return LISP_NIL;
    }
    return LISP_T;
}

lisp_value lisp_prim_num_greater(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
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
    if (result) {
        a = b;
        for (uint32_t i = 2; i < nargs; ++i) {
            b = args[i];
            CHECK_FIXNUM(b);
            result = a.as_fixnum() > b.as_fixnum();
            if (result == false) {
                break;
            }
            a = b;
        }
    }
    return result ? LISP_T : LISP_NIL;
}

lisp_value lisp_prim_putchar(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (putchar character &optional stm)
    */

    CHECK_AT_LEAST_N(nargs, 1);
    CHECK_CHARACTER(args[0]);
    lisp_file_stream *stm = nullptr;
    if (nargs == 1 || args[1] == LISP_T) {
        auto _stdout = cdr(symbol_lookup(env, intern_symbol("*STANDARD-OUTPUT*")));
        if (_stdout.is_not_nil()) {
            CHECK_FILE_STREAM(_stdout);
            stm = _stdout.as_object()->file_stream();
        }
    }
    else {
        CHECK_FILE_STREAM(args[1]);
        stm = args[1].as_object()->file_stream();
    }
    int64_t bytes_written = 0;
    if (stm) {
        auto codepoint = args[0].as_character();
        bytes_written = stm->write_utf8(codepoint);
        if (codepoint == '\n')
            stm->flush();
    }
    return lisp_value::wrap_fixnum(bytes_written);
}

lisp_value lisp_prim_type_of(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (type-of object)
    */
    CHECK_AT_LEAST_N(nargs, 1);
    auto it = args[0];
    if (it.is_fixnum()) {
        return LISP_SYM_FIXNUM;
    }
    if (it.is_nil()) {
        return LISP_SYM_NULL;
    }
    if (it.is_cons()) {
        return LISP_SYM_CONS;
    }
    if (it.is_character()) {
        return LISP_SYM_CHARACTER;
    }
    if (it.is_object()) {
        switch (it.as_object()->type()) {
            case SYM_TYPE: return LISP_SYM_SYMBOL;
            case LAMBDA_TYPE: return LISP_SYM_FUNCTION;
            case SIMPLE_ARRAY_TYPE: {
                auto array = it.as_object()->simple_array();
                return list(LISP_SYM_SIMPLE_ARRAY,
                            array->type(),
                            lisp_value::wrap_fixnum(array->length()));
            };
            case FILE_STREAM_TYPE: return LISP_SYM_FILE_STREAM;
            case SYSTEM_POINTER_TYPE: return LISP_SYM_SYSTEM_POINTER;
        }
        return LISP_NIL;
    }
    if (it.is_lisp_primitive()) {
        return LISP_SYM_FUNCTION;
    }
    if (it == LISP_T) {
        return LISP_SYM_BOOLEAN;
    }
    raised_signal = true;
    return list(intern_symbol("UNKNOWN-TYPE"), it);
}

lisp_value lisp_prim_read(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (read &optional file-stream)
    */

    if (nargs == 0) {
        lisp_value result;
        if (!read_stdin(">>> ", "... ", result))
            return LISP_NIL;
        return result;
    }
    else {
        CHECK_FILE_STREAM(args[0]);
        return parse(*args[0].as_object()->file_stream());
    }
    return LISP_NIL;
}

lisp_value lisp_prim_macro_expand(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (macro-expand expr)
    */
    CHECK_EXACTLY_N(nargs, 1);
    return macro_expand(args[0]);
}

lisp_value lisp_prim_eval(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (eval expr)
    */
    CHECK_EXACTLY_N(nargs, 1);
    return lisp::evaluate(LISP_BASE_ENVIRONMENT, args[0]);
}

lisp_value lisp_prim_apply(lisp_value env, lisp_value *real_args, uint32_t nargs, bool &raised_signal)
{
    /***
        (apply func &rest args args-list)
    */
    CHECK_AT_LEAST_N(nargs, 1);

    auto function = real_args[0];
    if (nargs == 1) {
        return lisp::apply(env, function, nullptr, 0, raised_signal);
    }
    
    auto args = to_list(real_args+1, nargs-1);
    auto head = LISP_NIL;
    auto current = head;
    while (args.is_not_nil()) {
        if (rest(args).is_nil()) break;
        if (head.is_nil()) {
            head = list(first(args));
            current = head;
        }
        else {
            set_cdr(current, cons(first(args), LISP_NIL));
            current = rest(current);
        }
        args = rest(args);
    }
    if (head.is_nil()) {
        head = first(args);
    }
    else {
        set_cdr(current, first(args));
    }
    auto vec = to_vector(head);
    return lisp::apply(env, function, vec.data(), vec.size(), raised_signal);   
}

lisp_value lisp_prim_get_env(lisp_value env, lisp_value*, uint32_t nargs, bool &raised_signal)
{
    /***
        (get-env)
    */
    CHECK_EXACTLY_N(nargs, 0);
    return env;
}

lisp_value lisp_prim_gensym(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (gensym &optional hint)
    */
    static unsigned int counter = 0;
    std::string sym_name;
    if (nargs != 0) {
        // @TODO: typecheck for string in GENSYM primitive
        auto hint = args[0];
        sym_name = lisp_string_to_native_string(hint);
    }
    else {
        sym_name = "G";
    }

    sym_name += std::to_string(counter++);
    return lisp_obj::create_symbol(sym_name);
}

lisp_value lisp_prim_make_symbol(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (make-symbol symbol-name)
    */
    CHECK_EXACTLY_N(nargs, 1);
    // @TODO: typecheck for string in MAKE-SYMBOL primitive
    std::string name;
    auto array = args[0].as_object()->simple_array();
    for (fixnum i = 0; i < array->length(); ++i) {
        auto codepoint = array->get(i).as_character();
        name += reinterpret_cast<const char*>(&codepoint);
    }
    return lisp_obj::create_symbol(name);
}

lisp_value lisp_prim_symbol_name(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (symbol-name symbol)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYMBOL(args[0]);
    return lisp_obj::create_string(args[0].as_object()->symbol()->name);
}

lisp_value lisp_prim_intern(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (intern symbol-name)
    */
    CHECK_EXACTLY_N(nargs, 1);
    // @TODO: typecheck for string in INTERN primitive
    std::string name;
    auto array = args[0].as_object()->simple_array();
    for (fixnum i = 0; i < array->length(); ++i) {
        auto codepoint = array->get(i).as_character();
        name += reinterpret_cast<const char*>(&codepoint);
    }
    return intern_symbol(name);
}

lisp_value lisp_prim_exit(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (exit &optional n)
    */
    int code = 0;
    if (nargs != 0) {
        CHECK_FIXNUM(args[0]);
        code = args[0].as_fixnum();
    }
    exit(code);
}

lisp_value lisp_prim_signal(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (signal tag &rest args)
    */
    CHECK_AT_LEAST_N(nargs, 1);
    raised_signal = true;
    return to_list(args, nargs);
}

lisp_value lisp_prim_make_array(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (make-array length &optional type)
    */
    CHECK_AT_LEAST_N(nargs, 1);
    auto length = args[0];
    CHECK_FIXNUM(length);
    // @TODO: Array operations still need type checking
    if (nargs != 1) {
        auto type = args[1];
        if (type == LISP_SYM_CHARACTER || type == LISP_SYM_FIXNUM)
            return lisp_obj::create_simple_array(length.as_fixnum(), type);
    }
    return lisp_obj::create_simple_array(length.as_fixnum());

}

lisp_value lisp_prim_array_length(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (array-length array)
    */
    CHECK_EXACTLY_N(nargs, 1);

    // @TODO: typecheck array in ARRAY-LENGTH primitive
    auto array = args[0];
    if (array.is_type(SIMPLE_ARRAY_TYPE)) {
        return lisp_value::wrap_fixnum(array.as_object()->simple_array()->length());
    }
    return LISP_NIL;
}

lisp_value lisp_prim_array_type(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (array-type array)
    */
    CHECK_EXACTLY_N(nargs, 1);

    // @TODO: typecheck array in ARRAY-TYPE primitive
    auto array = args[0];
    if (array.is_type(SIMPLE_ARRAY_TYPE)) {
        return array.as_object()->simple_array()->type();
    }
    return LISP_NIL;
}

lisp_value lisp_prim_bits_of(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (bits-of object)
    */
    CHECK_EXACTLY_N(nargs, 1);

    auto obj = args[0];
    auto ret = lisp_obj::create_simple_array(64, intern_symbol("BIT"));
    auto bits = obj.bits();
    auto array = ret.as_object()->simple_array();
    for (int i = 0; i < 64; ++i) {
        array->set(i, lisp_value::wrap_fixnum(bits & 1));
        bits >>= 1;
    }
    return ret;
}

lisp_value lisp_prim_code_char(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (code-char integer)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_FIXNUM(args[0]);
    auto char_code = args[0].as_fixnum();
    return lisp_value::wrap_character(char_code);
}

lisp_value lisp_prim_char_code(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (char-code character)
    */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_CHARACTER(args[0]);
    auto character = args[0].as_character();
    return lisp_value::wrap_fixnum(character);
}


static
lisp_file_stream::io_mode get_mode(lisp_value mode_sym)
{
    static auto OVERWRITE = intern_symbol("OVERWRITE");
    static auto READ = intern_symbol("READ");
    static auto APPEND = intern_symbol("APPEND");
    if (mode_sym == OVERWRITE)
        return lisp_file_stream::io_mode::overwrite;
    if (mode_sym == READ)
        return lisp_file_stream::io_mode::read;
    if (mode_sym == APPEND)
        return lisp_file_stream::io_mode::append;
    return lisp_file_stream::io_mode::invalid;
}

lisp_value lisp_prim_open(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (open file-path direction)
    */
    CHECK_EXACTLY_N(nargs, 2);

    // @TODO: typecheck FILE-PATH string in OPEN primitive

    auto path = lisp_string_to_native_string(args[0]);
    auto direction = args[1];
    lisp_file_stream *fs = nullptr;
    int mode = lisp_file_stream::io_mode::invalid;
    if (direction.is_cons()) {
        auto p = direction;
        while (p.is_not_nil()) {
            CHECK_SYMBOL(car(p));
            mode |= static_cast<int>(get_mode(car(p)));
            p = cdr(p);
        }
    }
    else {
        CHECK_SYMBOL(direction);
        mode = get_mode(direction);
    }
    if (mode != lisp_file_stream::io_mode::invalid) {
        fs = new lisp_file_stream();
        fs->open(path, static_cast<lisp_file_stream::io_mode>(mode));
        return lisp_obj::create_file_stream(fs);
    }
    return LISP_NIL;
}

lisp_value lisp_prim_close(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (close file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    it.as_object()->file_stream()->close();
    return LISP_T;
}

lisp_value lisp_prim_file_length(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (file-length file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto size = it.as_object()->file_stream()->length();
    return lisp_value::wrap_fixnum(size);
}

lisp_value lisp_prim_file_ok(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (file-ok file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    if (it.is_nil()) return it;
    CHECK_FILE_STREAM(it);
    return it.as_object()->file_stream()->ok() ? LISP_T : LISP_NIL;
}

lisp_value lisp_prim_file_eof(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (file-eof-p file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    if (it.is_nil()) return it;
    CHECK_FILE_STREAM(it);
    return it.as_object()->file_stream()->eof() ? LISP_T : LISP_NIL;
}

lisp_value lisp_prim_file_mode(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (file-mode file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    int64_t mode = it.as_object()->file_stream()->mode();
    return lisp_value::wrap_fixnum(mode);
}

lisp_value lisp_prim_file_flush(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (file-flush file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    it.as_object()->file_stream()->flush();
    return LISP_T;
}

lisp_value lisp_prim_file_read_byte(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (file-read-byte file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    return lisp_value::wrap_fixnum(it.as_object()->file_stream()->read_byte());
}

lisp_value lisp_prim_file_peek_byte(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (file-peek-byte file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    return lisp_value::wrap_fixnum(it.as_object()->file_stream()->peek_byte());
}

lisp_value lisp_prim_file_read_characater(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (file-read-character file-stream)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    return lisp_value::wrap_character(it.as_object()->file_stream()->read_utf8());
}

lisp_value lisp_prim_get_working_directory(lisp_value, lisp_value*, uint32_t, bool &raised_signal)
{
    /***
        (get-working-directory)
    */
    std::error_code error;
    auto current_path = plat::get_working_directory(error);
    return error.value() != 0 ? LISP_NIL : lisp_obj::create_string(current_path);
}

lisp_value lisp_prim_change_directory(lisp_value env, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (change-directory path)
    */
    CHECK_EXACTLY_N(nargs, 1);
    // @TODO: typecheck PATH for string in CHANGE-DIRECTORY-PRIMITIVE
    auto new_path = lisp_string_to_native_string(args[0]);
    std::error_code error;
    plat::change_directory(new_path, error);
    if (error.value() != 0) {
        return LISP_NIL;
    }

    error.clear();
    auto current_path = plat::get_working_directory(error);
    return error.value() != 0 ? LISP_NIL : lisp_obj::create_string(current_path);
}

lisp_value lisp_prim_get_executable_path(lisp_value, lisp_value*, uint32_t, bool &raised_signal)
{
    /***
        (get-executable-path)
    */
    static auto ret = lisp_obj::create_string(plat::get_executable_path());
    return ret;
}

lisp_value lisp_prim_get_clock_ticks(lisp_value, lisp_value*, uint32_t, bool &raised_signal)
{
    /***
        (get-clock-ticks)
    */
    auto now = std::chrono::high_resolution_clock::now();
    auto duration = now.time_since_epoch();
    auto microseconds = std::chrono::duration_cast<std::chrono::microseconds>(duration);
    return lisp_value::wrap_fixnum(microseconds.count());
}

lisp_value lisp_prim_clocks_per_second(lisp_value, lisp_value*, uint32_t, bool &raised_signal)
{
    /***
        (clocks-per-second)
    */
    return lisp_value::wrap_fixnum(1000000);
}

lisp_value lisp_prim_define_function(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (define-function symbol function)
    */
    CHECK_EXACTLY_N(nargs, 2);
    auto sym = args[0];
    CHECK_SYMBOL(sym);
    auto func = args[1];
    CHECK_FUNCTION(func);
    sym.as_object()->symbol()->function = func;
    return sym;
}

lisp_value lisp_prim_function_definition(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (function-definition symbol)
    */
    CHECK_EXACTLY_N(nargs, 1);
    auto sym = args[0];
    CHECK_SYMBOL(sym);
    return sym.as_object()->symbol()->function;
}

lisp_value lisp_prim_ffi_open(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-open dll-path)
     */
    CHECK_EXACTLY_N(nargs, 1);
    auto lib = args[0];
    if (!lib.is_type(SIMPLE_ARRAY_TYPE)) {
        raised_signal = true;
        return list(LISP_SYM_TYPE_ERROR, intern_symbol("STRING"), lib);
    }
    auto array = lib.as_object()->simple_array();
    if (array->type() != LISP_SYM_CHARACTER) {
        raised_signal = true;
        return list(LISP_SYM_TYPE_ERROR, intern_symbol("STRING"), lib);
    }
    
    auto lib_str = lisp_string_to_native_string(lib);
    auto handle = ffi::open(lib_str.c_str());
    
    return lisp_obj::wrap_pointer(handle);
}

lisp_value lisp_prim_ffi_close(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-close dll-handle)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    ffi::close(args[0].as_object()->ptr());
    return LISP_NIL;
}

lisp_value lisp_prim_ffi_get_symbol(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-get-symbol dll-handle symbol-name)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    auto symbol = args[0];
    if (!symbol.is_type(SIMPLE_ARRAY_TYPE)) {
        raised_signal = true;
        return list(LISP_SYM_TYPE_ERROR, intern_symbol("STRING"), symbol);
    }
    auto array = symbol.as_object()->simple_array();
    if (array->type() != LISP_SYM_CHARACTER) {
        raised_signal = true;
        return list(LISP_SYM_TYPE_ERROR, intern_symbol("STRING"), symbol);
    }
    auto handle = args[0].as_object()->ptr();
    auto symbol_str = lisp_string_to_native_string(symbol);

    auto func = ffi::getsym(handle, symbol_str.c_str());
    return lisp_obj::wrap_pointer(func);
}

lisp_value lisp_prim_ffi_call(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-call c-function &rest args)
     */
    CHECK_AT_LEAST_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto func = args[0].as_object()->ptr();
    auto result = ffi::call(func, args+1, nargs-1);
    return lisp_obj::wrap_pointer(result);
}

lisp_value lisp_prim_ffi_nullptr(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-nullptr)
     */
    CHECK_EXACTLY_N(nargs, 0);
    return lisp_obj::wrap_pointer(nullptr);
}

lisp_value lisp_prim_ffi_alloc(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-alloc size)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_FIXNUM(args[0]);
    auto size = args[0].as_fixnum();
    return lisp_obj::wrap_pointer(ffi::alloc_mem(size));
}

lisp_value lisp_prim_ffi_zero_alloc(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-zero-alloc size)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_FIXNUM(args[0]);
    auto size = args[0].as_fixnum();
    return lisp_obj::wrap_pointer(ffi::calloc_mem(size));
}

lisp_value lisp_prim_ffi_free(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-free pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = args[0].as_object()->ptr();
    ffi::free_mem(ptr);
    args[0].as_object()->ptr(nullptr);
    return LISP_T;
}

lisp_value lisp_prim_ffi_ref(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-ref pointer &optional offset)
     */
    CHECK_AT_LEAST_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    if (nargs == 1) {
        return lisp_obj::wrap_pointer(args[0].as_object()->ptr_ref());
    }
    else {
        auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->ptr());
        CHECK_FIXNUM(args[1]);
        auto offset = args[1].as_fixnum();
        return lisp_obj::wrap_pointer(ptr + offset);
    }
}

lisp_value lisp_prim_ffi_ref_8(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-ref-8 pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->ptr());
    return lisp_value::wrap_fixnum(*ptr);
}

lisp_value lisp_prim_ffi_ref_16(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-ref-16 pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint16_t*>(args[0].as_object()->ptr());
    return lisp_value::wrap_fixnum(*ptr);
}

lisp_value lisp_prim_ffi_ref_32(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-ref-32 pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint32_t*>(args[0].as_object()->ptr());
    return lisp_value::wrap_fixnum(*ptr);
}

lisp_value lisp_prim_ffi_ref_64(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-ref-64 pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint64_t*>(args[0].as_object()->ptr());
    return lisp_value::wrap_fixnum(*ptr);
}

lisp_value lisp_prim_ffi_set_ref(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-set-ref pointer offset value value-size)
     */
    CHECK_EXACTLY_N(nargs, 4);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->ptr());
    CHECK_FIXNUM(args[1]);
    auto offset = args[1].as_fixnum();
    CHECK_SYSTEM_POINTER(args[2]);
    auto value = reinterpret_cast<uint8_t*>(args[2].as_object()->ptr());
    CHECK_FIXNUM(args[3]);
    auto value_size = args[3].as_fixnum();
    
    memcpy(ptr + offset, value, value_size);
    return args[2];
}

lisp_value lisp_prim_ffi_set_ref_8(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-set-ref-8 pointer value)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->ptr());
    CHECK_FIXNUM(args[1]);
    auto value = args[1].as_fixnum() & 0xff;
    *ptr = value;
    return lisp_value::wrap_fixnum(value);
}

lisp_value lisp_prim_ffi_set_ref_16(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-set-ref-16 pointer value)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint16_t*>(args[0].as_object()->ptr());
    CHECK_FIXNUM(args[1]);
    auto value = args[1].as_fixnum() & 0xffff;
    *ptr = value;
    return lisp_value::wrap_fixnum(value);
}

lisp_value lisp_prim_ffi_set_ref_32(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-set-ref-32 pointer value)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint32_t*>(args[0].as_object()->ptr());
    CHECK_FIXNUM(args[1]);
    auto value = args[1].as_fixnum() & 0xffffffff;
    *ptr = value;
    return lisp_value::wrap_fixnum(value);
}

lisp_value lisp_prim_ffi_set_ref_64(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-set-ref-64 pointer value)
     */
    CHECK_EXACTLY_N(nargs, 2);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint64_t*>(args[0].as_object()->ptr());
    CHECK_FIXNUM(args[1]);
    auto value = args[1].as_fixnum();
    *ptr = value;
    return lisp_value::wrap_fixnum(value);
}

lisp_value lisp_prim_ffi_marshal(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-marshal object)
     */
    CHECK_EXACTLY_N(nargs, 1);
    return lisp_obj::wrap_pointer(ffi::marshal(args[0]));
}

lisp_value lisp_prim_ffi_coerce_fixnum(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-coerce-fixnum system-pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<fixnum>(args[0].as_object()->ptr());
    return lisp_value::wrap_fixnum(ptr);
}

lisp_value lisp_prim_ffi_coerce_int(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-coerce-int system-pointer)
     */
    CHECK_EXACTLY_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = static_cast<int>(reinterpret_cast<uintptr_t>(args[0].as_object()->ptr()));
    return lisp_value::wrap_fixnum(ptr);
}

lisp_value lisp_prim_ffi_coerce_string(lisp_value, lisp_value *args, uint32_t nargs, bool &raised_signal)
{
    /***
        (ffi-coerce-string system-pointer &optional length)
     */
    CHECK_AT_LEAST_N(nargs, 1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<const char*>(args[0].as_object()->ptr());
    if (nargs == 1) {
        return lisp_obj::create_string(ptr);
    }
    CHECK_FIXNUM(args[1]);
    auto len = args[1].as_fixnum();
    return lisp_obj::create_string(ptr, len);
}


static inline
void bind_primitive(const std::string &symbol_name, lisp_primitive primitive)
{
    auto symbol = intern_symbol(symbol_name);
    auto prim_object = lisp_value::wrap_primitive(primitive);
    symbol.as_object()->symbol()->function = prim_object;
}

static inline
void bind_value(lisp_value &environment, const std::string &symbol_name, lisp_value value)
{
    auto symbol = intern_symbol(symbol_name);
    auto binding = cons(symbol, value);
    environment = cons(binding, environment);
}

void primitives::bind_primitives(lisp_value &environment)
{
    bind_primitive("%PRINT", lisp_prim_print);
    bind_primitive("%+", lisp_prim_plus);
    bind_primitive("%-", lisp_prim_minus);
    bind_primitive("%*", lisp_prim_multiply);
    bind_primitive("%/", lisp_prim_divide);
    bind_primitive("%<", lisp_prim_num_less);
    bind_primitive("%=", lisp_prim_num_equal);
    bind_primitive("%>", lisp_prim_num_greater);
    bind_primitive("%PUTCHAR", lisp_prim_putchar);
    bind_primitive("%TYPE-OF", lisp_prim_type_of);
    bind_primitive("%READ", lisp_prim_read);
    bind_primitive("%MACRO-EXPAND", lisp_prim_macro_expand);
    bind_primitive("%EVAL", lisp_prim_eval);
    bind_primitive("%APPLY", lisp_prim_apply);
    bind_primitive("%GET-ENV", lisp_prim_get_env);
    bind_primitive("%GENSYM", lisp_prim_gensym);
    bind_primitive("%MAKE-SYMBOL", lisp_prim_make_symbol);
    bind_primitive("%SYMBOL-NAME", lisp_prim_symbol_name);
    bind_primitive("%INTERN", lisp_prim_intern);
    bind_primitive("%EXIT", lisp_prim_exit);
    bind_primitive("%SIGNAL", lisp_prim_signal);
    bind_primitive("%MAKE-ARRAY", lisp_prim_make_array);
    bind_primitive("%ARRAY-LENGTH", lisp_prim_array_length);
    bind_primitive("%ARRAY-TYPE", lisp_prim_array_type);
    bind_primitive("%CHAR-CODE", lisp_prim_char_code);
    bind_primitive("%CODE-CHAR", lisp_prim_code_char);
    bind_primitive("%BITS-OF", lisp_prim_bits_of);
    bind_primitive("%OPEN", lisp_prim_open);
    bind_primitive("%CLOSE", lisp_prim_close);
    bind_primitive("%FILE-LENGTH", lisp_prim_file_length);
    bind_primitive("%FILE-MODE", lisp_prim_file_mode);
    bind_primitive("%FILE-EOF-P", lisp_prim_file_eof);
    bind_primitive("%FILE-OK-P", lisp_prim_file_ok);
    bind_primitive("%FILE-FLUSH", lisp_prim_file_flush);
    bind_primitive("%FILE-READ-BYTE", lisp_prim_file_read_byte);
    bind_primitive("%FILE-PEEK-BYTE", lisp_prim_file_peek_byte);
    bind_primitive("%FILE-READ-CHARACTER", lisp_prim_file_read_characater);

    bind_primitive("%DEFINE-FUNCTION", lisp_prim_define_function);
    bind_primitive("%FUNCTION-DEFINITION", lisp_prim_function_definition);

    bind_primitive("GET-WORKING-DIRECTORY", lisp_prim_get_working_directory);
    bind_primitive("CHANGE-DIRECTORY", lisp_prim_change_directory);
    bind_primitive("GET-EXECUTABLE-PATH", lisp_prim_get_executable_path);
    bind_primitive("GET-CLOCK-TICKS", lisp_prim_get_clock_ticks);
    bind_primitive("CLOCKS-PER-SECOND", lisp_prim_clocks_per_second);
    
    bind_primitive("FFI-OPEN", lisp_prim_ffi_open);
    bind_primitive("FFI-CLOSE", lisp_prim_ffi_close);
    bind_primitive("FFI-GET-SYMBOL", lisp_prim_ffi_get_symbol);
    bind_primitive("FFI-CALL", lisp_prim_ffi_call);
    bind_primitive("FFI-NULLPTR", lisp_prim_ffi_nullptr);
    bind_primitive("FFI-ALLOC", lisp_prim_ffi_alloc);
    bind_primitive("FFI-ZERO-ALLOC", lisp_prim_ffi_zero_alloc);
    bind_primitive("FFI-FREE", lisp_prim_ffi_free);
    bind_primitive("FFI-MARSHAL", lisp_prim_ffi_marshal);
    bind_primitive("FFI-COERCE-FIXNUM", lisp_prim_ffi_coerce_fixnum);
    bind_primitive("FFI-COERCE-INT", lisp_prim_ffi_coerce_int);
    bind_primitive("FFI-COERCE-STRING", lisp_prim_ffi_coerce_string);
    bind_primitive("FFI-REF", lisp_prim_ffi_ref);
    bind_primitive("FFI-REF-8", lisp_prim_ffi_ref_8);
    bind_primitive("FFI-REF-16", lisp_prim_ffi_ref_16);
    bind_primitive("FFI-REF-32", lisp_prim_ffi_ref_32);
    bind_primitive("FFI-REF-64", lisp_prim_ffi_ref_64);
    bind_primitive("FFI-SET-REF", lisp_prim_ffi_set_ref);
    bind_primitive("FFI-SET-REF-8", lisp_prim_ffi_set_ref_8);
    bind_primitive("FFI-SET-REF-16", lisp_prim_ffi_set_ref_16);
    bind_primitive("FFI-SET-REF-32", lisp_prim_ffi_set_ref_32);
    bind_primitive("FFI-SET-REF-64", lisp_prim_ffi_set_ref_64);

    bind_value(environment, "*STANDARD-INPUT*", lisp_obj::standard_input_stream());
    bind_value(environment, "*STANDARD-OUTPUT*", lisp_obj::standard_output_stream());
    bind_value(environment, "*STANDARD-ERROR*", lisp_obj::standard_error_stream());

}
