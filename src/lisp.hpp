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
                  Do not be fooled into thinking this is another layer of indirection.
                  This struct, or rather _value_, is a mere 8-bytes and allows us to
                  represent several primitive data-types alongside a more general object 
                  structure. On 64-bit arch a pointer is 8 bytes so word-aligned 
                  addresses have the LSB(3) bits set to 0. We can exploit this and give 
                  meaning to those bits.

                  bits 2 1 0
                  ----------
                       0 0 1 -> FIXNUM
                       0 1 0 -> TBD
                       1 1 0 -> TBD
                       1 0 0 -> TBD
                       0 0 0 -> POINTER/OBJECT
                       
                  You'll notice we don't reuse LSB(0) and that is because we want to
                  maximize the range of fixnums. This means when there is a 1 at LSB(0)
                  then the other 63 bits must represent the fixnum.
                  
                  Another side-effect of this decision is that we may represent NIL as
                  all bits set to 0 which allows for more optimized NIL tests.
                */
                static constexpr uint64_t BITS_MASK = 0b111ULL;

                // "invalid" pointer, used by the reader to signify an unfinised parse.
                static constexpr uint64_t TAG_INVALID = 0b010ULL; 
                // TBD
                static constexpr uint64_t TAG_TBD1    = 0b110ULL; 
                // TBD
                static constexpr uint64_t TAG_TBD2    = 0b100ULL;
                // Pointer to general object
                static constexpr uint64_t TAG_POINTER = 0b000ULL; 

                // An uninitialized lisp_value will be defaulted to NIL
                inline lisp_value() { u.bits = 0; }

                inline lisp_value(int64_t fixnum) 
                {
                        u.fixnum_layout.tag = 1;
                        u.fixnum_layout.fixnum = fixnum;
                }

                inline lisp_value(lisp_obj *pointer) { u.obj = pointer; }

                inline bool is_fixnum() const { return u.fixnum_layout.tag != 0; }
        
                inline bool is_nil() const { return bits() == 0; }
        
                inline bool is_object() const { return tag_bits() == TAG_POINTER; }
        
                inline int64_t as_fixnum() const
                {
                        assert(is_fixnum());
                        return u.fixnum_layout.fixnum;
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
                
                inline const bool is_type(LISP_OBJ_TYPE type) const;
        
                inline bool operator==(lisp_value other) const 
                {
                        return other.u.bits == u.bits;
                }

                inline bool operator!=(lisp_value other) const 
                {
                        return other.u.bits != u.bits;
                }
                
                inline uint64_t bits() const { return u.bits; }
                inline uint64_t tag_bits() const
                {
                        return bits() & 0b111ULL;
                }
                
                static inline lisp_value invalid_object()
                {
                        lisp_value invalid;
                        invalid.u.bits = TAG_INVALID;
                        return invalid;
                }
                
                inline bool is_invalid() const
                {
                        return tag_bits() == TAG_INVALID;
                }
        private:
        
                union {
                        uint64_t bits;
                        struct {
                                int64_t tag : 1; // bit 0 == fixnum tag
                                int64_t fixnum : 63;
                        } fixnum_layout;
                        lisp_obj *obj;
                } u;
        

        };

        typedef lisp_value (*primitive_function)(lisp_value env, lisp_value args);

        struct lisp_obj {
                lisp_obj() {}
                ~lisp_obj() {}
        
                LISP_OBJ_TYPE type;
                union {
                        std::string *symbol;
                        char character;
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

        inline const bool lisp_value::is_type(LISP_OBJ_TYPE type) const
        {
                return is_object() && as_object()->type == type;
        }
        
        struct lisp_stream {
                static const int end_of_file = -1;
                virtual int getc() = 0;
                virtual int peekc(int n=0) const = 0;
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
        lisp_value parse(lisp_stream &stream);
        

        static inline void set_car(lisp_value cons, lisp_value val)
        {
                // @TODO: Throw error if not cons.
                cons.as_object()->cons.car = val;
        }

        static inline void set_cdr(lisp_value cons, lisp_value val)
        {
                // @TODO: Throw error if not cons.
                cons.as_object()->cons.cdr = val;
        }

        static inline lisp_value car(lisp_value obj) 
        {
                // @TODO: Throw error if not nil or cons.
                if (obj.is_nil()) return obj;
                return obj.as_object()->cons.car;
        }

        static inline lisp_value cdr(lisp_value obj) 
        {
                // @TODO: Throw error if not nil or cons.
                if (obj.is_nil()) return obj;
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

        static inline lisp_value create_lisp_obj_fixnum(int64_t fixnum)
        {
                return lisp_value(fixnum);
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
