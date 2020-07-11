#ifndef _PRIMITIVES_HPP_
#define _PRIMITIVES_HPP_

#include "lisp.hpp"

namespace primitives {
void bind_primitives(lisp::lisp_value &environment, char **script_args);
std::string primitive_name(void *ptr);
}

#endif
