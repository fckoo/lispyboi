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

void *ffi::call(void *f, lisp_value *args, uint32_t nargs)
{
#define M(n) marshal(args[n])
    switch (nargs) {
        case  0: return do_call(f);
        case  1: return do_call(f, M(0), M(1));
        case  2: return do_call(f, M(0), M(1), M(2));
        case  3: return do_call(f, M(0), M(1), M(2), M(3));
        case  4: return do_call(f, M(0), M(1), M(2), M(3), M(4));
        case  5: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5));
        case  6: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6));
        case  7: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7));
        case  8: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8));
        case  9: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9));
        case 10: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10));
        case 11: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11));
        case 12: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12));
        case 13: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12), M(13));
        case 14: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12), M(13), M(14));
        case 15: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12), M(13), M(14), M(15));
    }
    fprintf(stderr, "ffi call with more than 16 arguments is no supported\n");
    return nullptr;
}
