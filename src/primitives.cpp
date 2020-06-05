#include <stdio.h>
#include <chrono>
#include "platform.hpp"
#include "primitives.hpp"

using namespace lisp;

#define NYI(fmt, ...) do {                                              \
                fprintf(stderr, "NYI: " __FILE__ ":" STR(__LINE__) "\n\tin %s\n", __PRETTY_FUNCTION__); \
                fprintf(stderr, fmt "\n", ##__VA_ARGS__);               \
                abort();                                                \
        } while (0)



lisp_value lisp_prim_plus(lisp_value env, lisp_value args)
{
        /***
            (+ &rest fixnums)
        */
        int64_t result = 0;
        while (args != LISP_NIL) {
                auto tmp = car(args);
                result += tmp.as_fixnum();
                args = cdr(args);
        }
        return lisp_value(result);
}

lisp_value lisp_prim_minus(lisp_value env, lisp_value args)
{
        /***
            (- &rest fixnums)
        */
        int64_t result = 0;
        if (args == LISP_NIL) {
                ;
        }
        else if (cdr(args) == LISP_NIL) {
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
        return lisp_value(result);
}

lisp_value lisp_prim_multiply(lisp_value env, lisp_value args)
{
        /***
            (* &rest fixnums)
        */
        int64_t result = 1;
        while (args != LISP_NIL) {
                auto tmp = car(args);
                result *= tmp.as_fixnum();
                args = cdr(args);
        }
        return lisp_value(result);
}

lisp_value lisp_prim_divide(lisp_value env, lisp_value args)
{
        /***
            (/ x y)
        */
        auto x = first(args).as_fixnum();
        auto y = second(args).as_fixnum();
        return lisp_value(x / y);
}

lisp_value lisp_prim_print(lisp_value env, lisp_value args)
{
        /***
            (print obj &optional stream)
        */
        lisp_file_stream *stream = nullptr;
        auto obj = first(args);
        if (cdr(args) == LISP_NIL || second(args) == LISP_T) {
                auto _stdout = cdr(symbol_lookup(env, intern_symbol("*STANDARD-OUTPUT*")));
                if (_stdout != LISP_NIL) {
                        stream = _stdout.as_object()->file_stream();
                }
        }
        else {
                stream = second(args).as_object()->file_stream();
        }
        auto s = repr(obj);
        if (stream) {
                stream->write(s);
        }
        return obj;
}

lisp_value lisp_prim_num_less(lisp_value env, lisp_value args)
{
        /***
            (< a b &rest more-fixnums)
        */
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

lisp_value lisp_prim_num_equal(lisp_value env, lisp_value args)
{
        /***
            (= a b &rest more-fixnums)
        */
        auto a = first(args);
        auto b = second(args);
        bool result = a.as_fixnum() == b.as_fixnum();
        if (result) {
                args = cddr(args);
                a = b;
                while (args != LISP_NIL) {
                        b = car(args);
                        result = a.as_fixnum() == b.as_fixnum();
                        if (result == false) {
                                break;
                        }
                        a = b;
                        args = cdr(args);
                }
        }
        return result ? LISP_T : LISP_NIL;
}

lisp_value lisp_prim_num_greater(lisp_value env, lisp_value args)
{
        /***
            (> a b &rest more-fixnums)
        */
        auto a = first(args);
        auto b = second(args);
        bool result = a.as_fixnum() > b.as_fixnum();
        if (result) {
                args = cddr(args);
                a = b;
                while (args != LISP_NIL) {
                        b = car(args);
                        result = a.as_fixnum() > b.as_fixnum();
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
        /***
            (car obj)
        */
        return caar(args);
}

lisp_value lisp_prim_cdr(lisp_value env, lisp_value args)
{
        /***
            (cdr obj)
        */
        return cdar(args);
}

lisp_value lisp_prim_cons(lisp_value env, lisp_value args)
{
        /***
            (cons x y)
        */
        return cons(first(args), second(args));
}

lisp_value lisp_prim_eq(lisp_value env, lisp_value args)
{
        /***
            (eq x y &rest more-objects)
        */

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
        /***
            (putchar character &optional stm)
        */

        lisp_file_stream *stm = nullptr;
        if (cdr(args) == LISP_NIL || second(args) == LISP_T) {
                auto _stdout = cdr(symbol_lookup(env, intern_symbol("*STANDARD-OUTPUT*")));
                if (_stdout != LISP_NIL) {
                        stm = _stdout.as_object()->file_stream();
                }
        }
        else {
                stm = second(args).as_object()->file_stream();
        }
        auto codepoint = car(args).as_character();
        int64_t bytes_written = 0;
        if (stm) {
                bytes_written = stm->write_utf8(codepoint);
                if (codepoint == '\n')
                        stm->flush();
        }
        return lisp_value(bytes_written);
}

lisp_value lisp_prim_type_of(lisp_value, lisp_value args)
{
        /***
            (type-of object)
        */
        auto it = car(args);
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
                                            lisp_value(static_cast<int64_t>(array->length())));
                        } break;

                }
                return LISP_NIL;
        }
        if (it.is_lisp_primitive()) {
                return LISP_SYM_FUNCTION;
        }
        if (it == LISP_T) {
                return LISP_SYM_BOOLEAN;
        }
        return LISP_NIL;
}

lisp_value lisp_prim_read(lisp_value, lisp_value args)
{
        /***
            (read &optional file-stream)
        */

        if (args.is_nil()) {
                lisp_value result;
                if (!read_stdin(">>> ", "... ", result))
                        return LISP_NIL;
                return result;
        }
        else {
                return parse(*car(args).as_object()->file_stream());
        }
        return LISP_NIL;
}

lisp_value lisp_prim_macro_expand(lisp_value, lisp_value args)
{
        /***
            (macro-expand expr)
        */

        return macro_expand(car(args));
}

lisp_value lisp_prim_eval(lisp_value env, lisp_value args)
{
        /***
            (eval expr &optional environment)
        */
        
        if (cdr(args) != LISP_NIL)
                env = second(args);

        return lisp::evaluate(env, car(args));
}

lisp_value lisp_prim_apply(lisp_value env, lisp_value args)
{
        /***
            (apply func &rest args args-list)
        */

        auto function = first(args);
        args = rest(args);
        if (args == LISP_NIL)
                return lisp::apply(env, function, LISP_NIL);

        auto head = LISP_NIL;
        auto current = head;
        while (args != LISP_NIL) {
                if (rest(args) == LISP_NIL) break;
                if (head == LISP_NIL) {
                        head = list(first(args));
                        current = head;
                }
                else {
                        set_cdr(current, cons(first(args), LISP_NIL));
                        current = rest(current);
                }
                args = rest(args);
        }
        if (head == LISP_NIL) {
                head = first(args);
        }
        else {
                set_cdr(current, first(args));
        }
        return lisp::apply(env, function, head);
}

lisp_value lisp_prim_set_car(lisp_value env, lisp_value args)
{
        /***
            (set-car cons value)
        */

        auto obj = first(args);
        auto val = second(args);
        set_car(obj, val);
        return val;
}

lisp_value lisp_prim_set_cdr(lisp_value env, lisp_value args)
{
        /***
            (set-cdr cons value)
        */
        auto obj = first(args);
        auto val = second(args);
        set_cdr(obj, val);
        return val;
}

lisp_value lisp_prim_get_env(lisp_value env, lisp_value)
{
        /***
            (get-env)
        */
        return env;
}

lisp_value lisp_prim_gensym(lisp_value, lisp_value args)
{
        /***
            (gensym &optional hint)
        */
        static unsigned int counter = 0;
        auto hint = first(args);
        std::string sym_name;
        if (hint != LISP_NIL) {
                sym_name = lisp_string_to_native_string(hint);
        }
        else {
                sym_name = "G";
        }

        sym_name += std::to_string(counter++);
        return lisp_obj::create_symbol(sym_name);
}

lisp_value lisp_prim_make_symbol(lisp_value, lisp_value args)
{
        /***
            (make-symbol symbol-name)
        */
        auto symbol_name = first(args);
        std::string name;
        auto array = first(args).as_object()->simple_array();
        for (size_t i = 0; i < array->length(); ++i) {
                auto codepoint = array->get(i).as_character();
                name += reinterpret_cast<const char*>(&codepoint);
        }
        return lisp_obj::create_symbol(name);
}

lisp_value lisp_prim_symbol_name(lisp_value, lisp_value args)
{
        /***
            (symbol-name symbol)
        */
        return lisp_obj::create_string(*first(args).as_object()->symbol());
}

lisp_value lisp_prim_intern(lisp_value, lisp_value args)
{
        /***
            (intern symbol-name)
        */
        auto symbol_name = first(args);
        std::string name;
        auto array = first(args).as_object()->simple_array();
        for (size_t i = 0; i < array->length(); ++i) {
                auto codepoint = array->get(i).as_character();
                name += reinterpret_cast<const char*>(&codepoint);
        }
        return intern_symbol(name);
}

lisp_value lisp_prim_exit(lisp_value, lisp_value args)
{
        /***
            (exit n)
        */
        int code = 0;
        if (car(args) != LISP_NIL) {
                code = car(args).as_fixnum();
        }
        exit(code);
}

lisp_value lisp_prim_make_array(lisp_value, lisp_value args)
{
        /***
            (make-array length &optional type)
        */
        auto type = second(args);
        if (type != LISP_NIL) {
                return lisp_obj::create_simple_array(first(args).as_fixnum(), type);
        }
        return lisp_obj::create_simple_array(first(args).as_fixnum());

}

lisp_value lisp_prim_aref(lisp_value, lisp_value args)
{
        /***
            (aref array subscript)
        */

        auto array = first(args);
        auto subscript = second(args);

        return array.as_object()->simple_array()->get(subscript.as_fixnum());
}

lisp_value lisp_prim_set_aref(lisp_value, lisp_value args)
{
        /***
            (set-aref array subscript value)
        */

        auto array = first(args);
        auto subscript = second(args);
        auto value = third(args);

        array.as_object()->simple_array()->set(subscript.as_fixnum(), value);
        return value;
}

lisp_value lisp_prim_array_length(lisp_value, lisp_value args)
{
        /***
            (array-length array)
        */

        auto array = first(args);
        if (array.is_type(SIMPLE_ARRAY_TYPE)) {
                return lisp_value(static_cast<int64_t>(array.as_object()->simple_array()->length()));
        }
        return LISP_NIL;
}

lisp_value lisp_prim_array_type(lisp_value, lisp_value args)
{
        /***
            (array-type array)
        */
        auto array = first(args);
        if (array.is_type(SIMPLE_ARRAY_TYPE)) {
                return array.as_object()->simple_array()->type();
        }
        return LISP_NIL;
}

lisp_value lisp_prim_bits_of(lisp_value, lisp_value args)
{
        /***
            (bits-of object)
        */
        auto obj = car(args);
        auto ret = lisp_obj::create_simple_array(64, intern_symbol("BIT"));
        auto bits = obj.bits();
        auto array = ret.as_object()->simple_array();
        for (int i = 0; i < 64; ++i) {
                array->set(i, lisp_value(static_cast<int64_t>(bits & 1)));
                bits >>= 1;
        }
        return ret;
}

lisp_value lisp_prim_code_char(lisp_value, lisp_value args)
{
        /***
            (code-char integer)
        */
        auto char_code = car(args).as_fixnum();
        return lisp_value(static_cast<int32_t>(char_code));
}

lisp_value lisp_prim_char_code(lisp_value, lisp_value args)
{
        /***
            (char-code character)
        */
        auto character = car(args).as_character();
        return lisp_value(static_cast<int64_t>(character));
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

lisp_value lisp_prim_open(lisp_value, lisp_value args)
{
        /***
            (open file-path direction)
         */

        auto path = lisp_string_to_native_string(first(args));
        auto direction = second(args);
        lisp_file_stream *fs = nullptr;
        int mode = lisp_file_stream::io_mode::invalid;
        if (direction.is_cons()) {
                auto p = direction;
                while (p != LISP_NIL) {
                        mode |= static_cast<int>(get_mode(car(p)));
                        p = cdr(p);
                }
        }
        else {
                mode = get_mode(direction);
        }
        if (mode != lisp_file_stream::io_mode::invalid) {
                fs = new lisp_file_stream();
                fs->open(path, static_cast<lisp_file_stream::io_mode>(mode));
                return lisp_obj::create_file_stream(fs);
        }
        return LISP_NIL;
}

lisp_value lisp_prim_close(lisp_value, lisp_value args)
{
        /***
            (close file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                it.as_object()->file_stream()->close();
                return LISP_T;
        }
        return LISP_NIL;
}

lisp_value lisp_prim_file_length(lisp_value, lisp_value args)
{
        /***
            (file-length file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                auto size = it.as_object()->file_stream()->length();
                return lisp_value(static_cast<int64_t>(size));
        }
        return LISP_NIL;
}

lisp_value lisp_prim_file_ok(lisp_value, lisp_value args)
{
        /***
            (file-ok file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                return it.as_object()->file_stream()->ok() ? LISP_T : LISP_NIL;
        }
        return LISP_NIL;
}

lisp_value lisp_prim_file_eof(lisp_value, lisp_value args)
{
        /***
            (file-eof-p file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                return it.as_object()->file_stream()->eof() ? LISP_T : LISP_NIL;
        }
        return LISP_NIL;
}

lisp_value lisp_prim_file_mode(lisp_value, lisp_value args)
{
        /***
            (file-mode file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                int64_t mode = it.as_object()->file_stream()->mode();
                return lisp_value(mode);
        }
        return LISP_NIL;
}

lisp_value lisp_prim_file_flush(lisp_value, lisp_value args)
{
        /***
            (file-flush file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                it.as_object()->file_stream()->flush();
        }
        return LISP_NIL;
}

lisp_value lisp_prim_file_read_byte(lisp_value, lisp_value args)
{
        /***
            (file-read-byte file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                return lisp_value(it.as_object()->file_stream()->read_byte());
        }
        return LISP_NIL;
}

lisp_value lisp_prim_file_peek_byte(lisp_value, lisp_value args)
{
        /***
            (file-peek-byte file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                return lisp_value(it.as_object()->file_stream()->peek_byte());
        }
        return LISP_NIL;
}

lisp_value lisp_prim_file_read_characater(lisp_value, lisp_value args)
{
        /***
            (file-read-character file-stream)
         */
        auto it = car(args);
        if (it.is_type(FILE_STREAM_TYPE)) {
                return lisp_value(it.as_object()->file_stream()->read_utf8());
        }
        return LISP_NIL;
}

lisp_value lisp_prim_get_working_directory(lisp_value, lisp_value)
{
        /***
            (get-working-directory)
         */
        std::error_code error;
        auto current_path = plat::get_working_directory(error);
        return error.value() != 0 ? LISP_NIL : lisp_obj::create_string(current_path);
}

lisp_value lisp_prim_change_directory(lisp_value env, lisp_value args)
{
        /***
            (change-directory path)
         */
        auto new_path = lisp_string_to_native_string(car(args));
        std::error_code error;
        plat::change_directory(new_path, error);
        if (error.value() != 0) {
                return LISP_NIL;
        }
        
        error.clear();
        auto current_path = plat::get_working_directory(error);
        return error.value() != 0 ? LISP_NIL : lisp_obj::create_string(current_path);
}

lisp_value lisp_prim_get_executable_path(lisp_value, lisp_value)
{
        /***
            (get-executable-path)
         */
        auto ret = lisp_obj::create_string(plat::get_executable_path());
        return ret;
}

lisp_value lisp_prim_get_clock_ticks(lisp_value, lisp_value)
{
        auto now = std::chrono::high_resolution_clock::now();
        auto duration = now.time_since_epoch();
        auto microseconds = std::chrono::duration_cast<std::chrono::microseconds>(duration);
        return lisp_value(static_cast<int64_t>(microseconds.count()));
}

lisp_value lisp_prim_clocks_per_second(lisp_value, lisp_value)
{
        return lisp_value(static_cast<int64_t>(1000000));
}

static inline
void bind_primitive(lisp_value &environment, const std::string &symbol_name, lisp_primitive primitive)
{
        auto prim_object = lisp_value(primitive);
        auto symbol = intern_symbol(symbol_name);
        auto binding = cons(symbol, prim_object);
        environment = cons(binding, environment);
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
#define BIND_PRIM(lisp_name, function) bind_primitive(environment, lisp_name, function)
        BIND_PRIM("%PRINT", lisp_prim_print);
        BIND_PRIM("%+", lisp_prim_plus);
        BIND_PRIM("%-", lisp_prim_minus);
        BIND_PRIM("%*", lisp_prim_multiply);
        BIND_PRIM("%/", lisp_prim_divide);
        BIND_PRIM("%<", lisp_prim_num_less);
        BIND_PRIM("%=", lisp_prim_num_equal);
        BIND_PRIM("%>", lisp_prim_num_greater);
        BIND_PRIM("%CAR", lisp_prim_car);
        BIND_PRIM("%CDR", lisp_prim_cdr);
        BIND_PRIM("%CONS", lisp_prim_cons);
        BIND_PRIM("%EQ", lisp_prim_eq);
        BIND_PRIM("%PUTCHAR", lisp_prim_putchar);
        BIND_PRIM("%TYPE-OF", lisp_prim_type_of);
        BIND_PRIM("%READ", lisp_prim_read);
        BIND_PRIM("%MACRO-EXPAND", lisp_prim_macro_expand);
        BIND_PRIM("%EVAL", lisp_prim_eval);
        BIND_PRIM("%APPLY", lisp_prim_apply);
        BIND_PRIM("%SET-CAR", lisp_prim_set_car);
        BIND_PRIM("%SET-CDR", lisp_prim_set_cdr);
        BIND_PRIM("%GET-ENV", lisp_prim_get_env);
        BIND_PRIM("%GENSYM", lisp_prim_gensym);
        BIND_PRIM("%MAKE-SYMBOL", lisp_prim_make_symbol);
        BIND_PRIM("%SYMBOL-NAME", lisp_prim_symbol_name);
        BIND_PRIM("%INTERN", lisp_prim_intern);
        BIND_PRIM("%EXIT", lisp_prim_exit);
        BIND_PRIM("%MAKE-ARRAY", lisp_prim_make_array);
        BIND_PRIM("%AREF", lisp_prim_aref);
        BIND_PRIM("%SET-AREF", lisp_prim_set_aref);
        BIND_PRIM("%ARRAY-LENGTH", lisp_prim_array_length);
        BIND_PRIM("%ARRAY-TYPE", lisp_prim_array_type);
        BIND_PRIM("%CHAR-CODE", lisp_prim_char_code);
        BIND_PRIM("%CODE-CHAR", lisp_prim_code_char);
        BIND_PRIM("%BITS-OF", lisp_prim_bits_of);
        BIND_PRIM("%OPEN", lisp_prim_open);
        BIND_PRIM("%CLOSE", lisp_prim_close);
        BIND_PRIM("%FILE-LENGTH", lisp_prim_file_length);
        BIND_PRIM("%FILE-MODE", lisp_prim_file_mode);
        BIND_PRIM("%FILE-EOF-P", lisp_prim_file_eof);
        BIND_PRIM("%FILE-OK", lisp_prim_file_ok);
        BIND_PRIM("%FILE-FLUSH", lisp_prim_file_flush);
        BIND_PRIM("%FILE-READ-BYTE", lisp_prim_file_read_byte);
        BIND_PRIM("%FILE-PEEK-BYTE", lisp_prim_file_peek_byte);
        BIND_PRIM("%FILE-READ-CHARACTER", lisp_prim_file_read_characater);
        
        BIND_PRIM("GET-WORKING-DIRECTORY", lisp_prim_get_working_directory);
        BIND_PRIM("CHANGE-DIRECTORY", lisp_prim_change_directory);
        BIND_PRIM("GET-EXECUTABLE-PATH", lisp_prim_get_executable_path);
        BIND_PRIM("GET-CLOCK-TICKS", lisp_prim_get_clock_ticks);
        BIND_PRIM("CLOCKS-PER-SECOND", lisp_prim_clocks_per_second);
        
        bind_value(environment, "*STANDARD-INPUT*", lisp_obj::standard_input_stream());
        bind_value(environment, "*STANDARD-OUTPUT*", lisp_obj::standard_output_stream());
        bind_value(environment, "*STANDARD-ERROR*", lisp_obj::standard_error_stream());
}
