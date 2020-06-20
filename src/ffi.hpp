#ifndef _LISPYBOI_FFI_
#define _LISPYBOI_FFI_

#include "lisp.hpp"

namespace ffi {
void *open(const char *path);
int close(void *handle);

void *getsym(void *handle, const char *symbol_name);
void *call(void *function, lisp::lisp_value *args, uint32_t nargs);

void *marshal(lisp::lisp_value value);

void *alloc_mem(size_t size);
void *calloc_mem(size_t size);
void free_mem(void *ptr);
}

#endif