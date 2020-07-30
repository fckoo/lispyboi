#ifndef _LISPYBOI_FFI_
#define _LISPYBOI_FFI_

#include <stddef.h>
#include <stdint.h>

namespace ffi {
void *open(const char *path);
int close(void *handle);

void *getsym(void *handle, const char *symbol_name);
void *call(void *function, void **args, uint32_t nargs);
void *alloc_mem(size_t size);
void *calloc_mem(size_t size);
void free_mem(void *ptr);
}

#endif
