#include <stdio.h>
#include "primitives.hpp"

using namespace lisp;

#define XSTR(x) #x
#define STR(x) XSTR(x)
#define NYI(fmt, ...) do {                                              \
                fprintf(stderr, "NYI: " __FILE__ ":" STR(__LINE__) "\n\tin %s\n", __PRETTY_FUNCTION__); \
                fprintf(stderr, fmt "\n", ##__VA_ARGS__);                     \
                abort();                                                \
        } while (0)


static
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
        return create_lisp_obj_fixnum(result);
}

static
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
        return create_lisp_obj_fixnum(result);
}


static
lisp_value lisp_prim_multiply(lisp_value env, lisp_value args)
{
        int result = 1;
        while (args != LISP_NIL) {
                // @TODO: Type validation
                auto tmp = car(args);
                result *= tmp.as_fixnum();
                args = cdr(args);
        }
        return create_lisp_obj_fixnum(result);
}

static
lisp_value lisp_prim_print(lisp_value env, lisp_value args) 
{
        printf("%s", repr(car(args)).c_str());
        return LISP_NIL;
}

static
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

static
lisp_value lisp_prim_car(lisp_value env, lisp_value args) 
{
        return caar(args);
}

static
lisp_value lisp_prim_cdr(lisp_value env, lisp_value args) 
{
        return cdar(args);
}

static
lisp_value lisp_prim_cons(lisp_value env, lisp_value args) 
{
        return cons(first(args), second(args));
}

static
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

static
lisp_value lisp_prim_putchar(lisp_value env, lisp_value args) 
{
        putchar(car(args).as_object()->character);
        return LISP_NIL;
}

static
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

static
lisp_value lisp_prim_read(lisp_value, lisp_value args)
{
        if (args.is_nil()) {
                lisp_value result;
                if (!read_stdin(">>> ", "... ", result))
                        return LISP_NIL;
                return result;
        }
        else {
                NYI("yer a boot.");
        }
        return LISP_NIL;
}

static
lisp_value lisp_prim_macro_expand(lisp_value, lisp_value args)
{
        return macro_expand(car(args));
}

static
lisp_value lisp_prim_eval(lisp_value env, lisp_value args)
{
        return lisp::evaluate(env, car(args));
}

static
lisp_value lisp_prim_apply(lisp_value env, lisp_value args)
{
        return lisp::apply(env, first(args), rest(args));
}

static inline
void bind_primitive(lisp_value &environment, const std::string &symbol_name, primitive_function primitive)
{
        auto prim_object = create_lisp_obj_primitive_function(primitive);
        auto symbol = intern_symbol(symbol_name);
        auto binding = cons(symbol, prim_object);
        environment = cons(binding, environment);
}

void primitives::bind_primitives(lisp_value &environment)
{
#define BIND_PRIM(lisp_name, function) bind_primitive(environment, lisp_name, function)
        BIND_PRIM("+", lisp_prim_plus);
        BIND_PRIM("PRINT", lisp_prim_print);
        BIND_PRIM("-", lisp_prim_minus);
        BIND_PRIM("<", lisp_prim_num_less);
        BIND_PRIM("*", lisp_prim_multiply);
        BIND_PRIM("CAR", lisp_prim_car);
        BIND_PRIM("CDR", lisp_prim_cdr);
        BIND_PRIM("CONS", lisp_prim_cons);
        BIND_PRIM("EQ", lisp_prim_eq);
        BIND_PRIM("PUTCHAR", lisp_prim_putchar);
        BIND_PRIM("TYPE-OF", lisp_prim_type_of);
        BIND_PRIM("READ", lisp_prim_read);
        BIND_PRIM("MACRO-EXPAND", lisp_prim_macro_expand);
        BIND_PRIM("EVAL", lisp_prim_eval);
        BIND_PRIM("APPLY", lisp_prim_apply);
}