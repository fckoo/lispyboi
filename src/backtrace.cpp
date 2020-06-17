#include <stdio.h>
#include "backtrace.hpp"
#include <stdint.h>
#include <stdlib.h>

// glibc, exposes backtrace function.
#include <execinfo.h>
#include <cxxabi.h>
#include <string>

struct symbol_name
{
    std::string name;
    uintptr_t offset;
};

static inline
symbol_name get_name(const char *symbol)
{
    int lparen = -1;
    int rparen = -1;
    int i = 0;
    for (; symbol[i] != 0; ++i) {
        if (symbol[i] == '(') {
            lparen = i;
            break;
        }
    }
    if (lparen >= 0) {
        for (; symbol[i] != 0; ++i) {
            if (symbol[i] == ')') {
                rparen = i;
                break;
            }
        }
    }
    if (lparen >= 0 && rparen > lparen) {
        for (; i > 0 && symbol[i] != '+'; --i)
            ;
        if (i != 0) {
            auto offset_s = std::string(symbol + i + 1, rparen - i - 1);
            auto offset = stoull(offset_s, 0, 16);
            auto sym_name = std::string(symbol + lparen + 1, i - lparen - 1);
            return { sym_name, offset };
        }

    }
    return { "", 0 };
}

static inline
uintptr_t get_address(const char *symbol)
{
    int lbrack = -1;
    int rbrack = -1;
    int i = 0;
    for (; symbol[i] != 0; ++i) {
        if (symbol[i] == '[') {
            lbrack = i;
            break;
        }
    }
    if (lbrack >= 0) {
        for (; symbol[i] != 0; ++i) {
            if (symbol[i] == ']') {
                rbrack = i;
                break;
            }
        }
    }
    if (lbrack >= 0 && rbrack > lbrack) {
        auto addr = std::string(symbol + lbrack + 1, rbrack - lbrack - 1);
        return stoull(addr, 0, 16);
    }
    return 0;
}

void bt::trace(int max_depth)
{
    auto traces = new void*[max_depth];
    auto size = backtrace(traces, max_depth);
    auto symbols = backtrace_symbols(traces, size);
    int status;
    // start i at one to skip this function
    for (int i = 1; i < size; ++i) {
        auto it = symbols[i];
        if (it) {
            auto name = get_name(it);
            auto address = get_address(it);
            auto realname = abi::__cxa_demangle(name.name.c_str(), 0, 0, &status);
            if (realname) {
                fprintf(stderr, "\t[bt]: %s+0x%lx [0x%lx]\n", realname, name.offset, address);
                free(realname);
            }
            else if (name.name.size() != 0) {
                fprintf(stderr, "\t[bt]: %s+0x%lx [0x%lx]\n", name.name.c_str(), name.offset, address);
            }
            else {
                fprintf(stderr, "\t[bt]: %s\n", it);
            }
        }
    }
    free(symbols);
    delete[] traces;
}

void bt::trace_and_abort(int max_depth)
{
    trace(max_depth);
    abort();
}

