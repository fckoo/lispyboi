#ifndef _LISP_HPP_
#define _LISP_HPP_

#include <string>
#include <cassert>

namespace lisp {

        enum LISP_OBJ_TYPE {
                SYM_TYPE = 0,
                CHAR_TYPE,
                CONS_TYPE,
                LAMBDA_TYPE,
                PRIMITIVE_FUNCTION_TYPE,
        };

        struct lisp_obj;

        struct lisp_value {
                /*
                  on 64-bit arch a pointer is 8 bytes so word-aligned addresses have the LSB(3) bits
                  set to 0. We can exploit this by giving meaning to those bits.

                  bits 210
                  001 -> integer
                  010 -> NIL
                  110 -> TBD
                  100 -> TBD
                  000 -> pointer
                */
                static constexpr uint64_t BITS_MASK = 0b111;

                static constexpr uint64_t NIL_TAG     = 0b010;
                static constexpr uint64_t TBD1_TAG    = 0b110;
                static constexpr uint64_t TBD2_TAG    = 0b100;
                static constexpr uint64_t POINTER_TAG = 0b000;

                inline lisp_value() { u.bits = NIL_TAG; }

                inline lisp_value(int64_t integer) 
                {
                        u.integer_layout.tag = 1;
                        u.integer_layout.number = integer;
                }

                inline lisp_value(lisp_obj *pointer) { u.obj = pointer; }

                inline bool is_fixnum() const { return u.integer_layout.tag != 0; }
        
                inline bool is_nil() const { return (u.bits & BITS_MASK) == NIL_TAG; }
        
                inline bool is_object() const { return (u.bits & BITS_MASK) == POINTER_TAG; }
        
                inline int64_t as_fixnum() const
                {
                        assert(is_fixnum());
                        return u.integer_layout.number;
                }
        
                inline lisp_value as_nil() const
                {
                        assert(is_nil());
                        return *this;
                }
        
                inline lisp_obj *as_object() const 
                {
                        assert(is_object());
                        return u.obj;
                }

                inline const lisp_obj *as_cobject() const 
                {
                        assert(is_object());
                        return u.obj;
                }
        
                inline bool operator==(lisp_value other) const 
                {
                        return other.u.bits == u.bits;
                }

                inline bool operator!=(lisp_value other) const 
                {
                        return other.u.bits != u.bits;
                }
                
                inline uint64_t bits() const { return u.bits(); }
        private:
        
                union 
                {
                        uint64_t bits;
                        struct {
                                int64_t tag : 1; // bit 0 == integer tag
                                int64_t number : 63;
                        } integer_layout;
                        lisp_obj *obj;
                } u;
        

        };

        typedef lisp_value (*primitive_function)(lisp_value env, lisp_value args);

        struct lisp_obj {
                lisp_obj() {}
                ~lisp_obj() {}
        
                // a symbol of integers or characters
                LISP_OBJ_TYPE type;
                union {
                        std::string *symbol;
                        char character;
                        int integer;
                        struct {
                                lisp_value car;
                                lisp_value cdr;
                        } cons;
                        struct {
                                lisp_value env;
                                lisp_value args;
                                lisp_value body;
                        } lambda;
                        primitive_function primitive;
                };
        };

        extern lisp_value LISP_T;
        extern lisp_value LISP_NIL;

        /* Commonly used symbols for easier access without having to call intern */
        extern lisp_value LISP_SYM_QUOTE;
        extern lisp_value LISP_SYM_IF;
        extern lisp_value LISP_SYM_LAMBDA;
        extern lisp_value LISP_SYM_SETQ;
        extern lisp_value LISP_SYM_DEFMACRO;
        extern lisp_value LISP_SYM_FIXNUM;
        extern lisp_value LISP_SYM_CONS;
        extern lisp_value LISP_SYM_CHARACTER;
        extern lisp_value LISP_SYM_FUNCTION;
        extern lisp_value LISP_SYM_SYMBOL;
        extern lisp_value LISP_SYM_NULL;
        extern lisp_value LISP_SYM_BOOLEAN;
        

        lisp_value intern_symbol(const std::string &symbol_name);
        std::string pretty_print(lisp_value obj);
        std::string repr(lisp_value obj);
        

        static inline lisp_value car(lisp_value obj) 
        {
                if (obj == LISP_NIL) return LISP_NIL; 
                return obj.as_object()->cons.car;
        }

        static inline lisp_value cdr(lisp_value obj) 
        {
                if (obj == LISP_NIL) return LISP_NIL; 
                return obj.as_object()->cons.cdr;
        }

        static inline lisp_value cddr(lisp_value obj)    { return cdr(cdr(obj)); }
        static inline lisp_value cdddr(lisp_value obj)   { return cdr(cdr(cdr(obj))); }
        static inline lisp_value cadr(lisp_value obj)    { return car(cdr(obj)); }
        static inline lisp_value caddr(lisp_value obj)   { return car(cdr(cdr(obj))); }
        static inline lisp_value cadddr(lisp_value obj)  { return car(cdr(cdr(cdr(obj)))); }
        static inline lisp_value caar(lisp_value obj)    { return car(car(obj)); }
        static inline lisp_value cdar(lisp_value obj)    { return cdr(car(obj)); }
        static inline lisp_value first(lisp_value obj)   { return car(obj); }
        static inline lisp_value rest(lisp_value obj)    { return cdr(obj); }
        static inline lisp_value second(lisp_value obj)  { return cadr(obj); }
        static inline lisp_value third(lisp_value obj)   { return caddr(obj); }
        static inline lisp_value fourth(lisp_value obj)  { return cadddr(obj); }
        
        static inline lisp_value cons(lisp_value car, lisp_value cdr)
        {
                lisp_obj *ret = new lisp_obj();
                ret->type = CONS_TYPE;
                ret->cons.car = car;
                ret->cons.cdr = cdr;
                return lisp_value(ret);
        }
        
        static inline lisp_value create_lisp_obj_character(char in)
        {
                lisp_obj *ret = new lisp_obj();
                ret->type = CHAR_TYPE;
                ret->character = in;
                return lisp_value(ret);
        }

        static inline lisp_value create_lisp_obj_integer(int64_t in)
        {
                return lisp_value(in);
        }

        static inline lisp_value create_lisp_obj_lambda(lisp_value env, lisp_value args, lisp_value body)
        {
                lisp_obj *ret = new lisp_obj();
                ret->type = LAMBDA_TYPE;
                ret->lambda.env = env;
                ret->lambda.args = args;
                ret->lambda.body = body;
                return lisp_value(ret);
        }

        static inline lisp_value create_lisp_obj_primitive_function(primitive_function primitive) 
        {
                lisp_obj *ret = new lisp_obj();
                ret->type = PRIMITIVE_FUNCTION_TYPE;
                ret->primitive = primitive;
                return lisp_value(ret);
        }
}

#endif
