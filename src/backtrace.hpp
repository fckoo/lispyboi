#ifndef _BACKTRACE_HPP_
#define _BACKTRACE_HPP_

namespace bt {
void trace_and_abort(int max_depth = 5);
void trace(int max_depth = 5);
}

#endif
