#include "ffi.hpp"

#include <dlfcn.h>

using namespace lisp;

void *ffi::open(const char *path)
{
    return dlopen(path, RTLD_NOW);
}

int ffi::close(void *handle)
{
    return dlclose(handle);
}

void *ffi::getsym(void *handle, const char *symbol_name)
{
    return dlsym(handle, symbol_name);
}

void *ffi::alloc_mem(size_t size)
{
    return malloc(size);
}

void *ffi::calloc_mem(size_t size)
{
    return calloc(size, 1);
}

void ffi::free_mem(void *ptr)
{
    free(ptr);
}

void *ffi::marshal(lisp_value val) {
    if (val.is_type(SYSTEM_POINTER_TYPE)) {
        return val.as_object()->ptr();
    }
    if (val.is_fixnum()) {
        return reinterpret_cast<void*>(val.as_fixnum());
    }
    if (val.is_character()) {
        return reinterpret_cast<void*>(val.as_character());
    }
    if (val.is_object()) {
        if (val.is_type(SIMPLE_ARRAY_TYPE)) {
                auto array = val.as_object()->simple_array();
                if (array->type() == LISP_SYM_CHARACTER) {
                    auto str = lisp_string_to_native_string(val);
                    auto buffer = (char*)ffi::alloc_mem(str.size()+1); // @LEAK
                    memcpy(buffer, str.data(), str.size());
                    buffer[str.size()] = 0;
                    return buffer;
                }
        }
    }
    fprintf(stderr, "Cannot marshal this lisp_value to void*\n");
    abort();
}

template<typename ...Args>
inline static
void *do_call(void *f, Args... args)
{
    auto func = reinterpret_cast<void*(*)(Args...)>(f);
    return func(args...);
}

void *ffi::call(void *f, lisp_value args)
{
    void *a0, *a1, *a2, *a3, *a4, *a5, *a6, *a7, *a8, *a9, *a10, *a11, *a12, *a13, *a14, *a15;
    if (args.is_nil()) return do_call(f);

    a0 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0);
    
    a1 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1);
    
    a2 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2);
    
    a3 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3);
    
    a4 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4);
    
    a5 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5);
    
    a6 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6);
    
    a7 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7);
    
    a8 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7, a8);
    
    a9 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    
    a10 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
    
    a11 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
    
    a12 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
    
    a13 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
    
    a14 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);

    a15 = marshal(car(args));
    args = cdr(args);
    if (args.is_nil()) return do_call(f, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);

    fprintf(stderr, "ffi call with more than 16 arguments is no supported\n");
    return nullptr;
}
