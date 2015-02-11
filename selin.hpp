// Copyright (c) 2014, 2015, Nihat Engin Toklu < http://github.com/engintoklu >
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
//    1. The origin of this software must not be misrepresented; you must
//    not claim that you wrote the original software. If you use this
//    software in a product, an acknowledgment in the product documentation
//    would be appreciated but is not required.
//
//    2. Altered source versions must be plainly marked as such,
//    and must not be misrepresented as being the original software.
//
//    3. This notice may not be removed or altered from any source
//    distribution.

// TODO: implement LispVector::iterator
// TODO: documentation is missing about optional parameters and throw-catch.
// TODO: test if it works correctly with SELIN_STRING_USE_BOOST
// TODO: defun, defext, and lambda must create a deep copy of the actions list
// TODO: (shell-command "dir"), (getenv "PATH"), define _WIN32 on windows?
// TODO: provide I/O functions, at least for text files
// TODO: provide easy conversion functions (between LispNode and STL containers, also between LispObject and double and std::string, etc.)
// TODO: we need a table data type (LispHash, or LispTable, or ...)
// TODO: having a simple version of Common Lisp loop macro would be very useful
// DONE: When we have a big list (~100000 nodes), and when that list is destructed, it triggers the destruction of all the next nodes recursively, resulting in a stack overflow (at least in Windows). In the destructor of a node, we must employ an iterative node killer: it will traverse all the next nodes (to right) in advance, as long as those nodes have their referencecount=1. Then from the last node with referencecount=1, those nodes will be destructed in reverse order (to left).
// DONE: some functions refer to themselves by using Ref<> variables ( Ref<X>(this) ). Avoiding this would be nice.
// DONE: make sure that refnew and objrefnew are called on their own, not on the fly as argument to functions (for safety against unexpected leaks). Ok I did this. Only exception is at the end of seldefs.hpp, in the add_callable sections. Since the other arguments are just constant string literals in add_callable(...), which doesn't throw exception, the code is now hopefuly safe.
// DONE: refcount mechanism has been rewritten. now implicit casting is not allowed, hopefully giving us a safer code
// DONE: (apropos)
// DONE: implement unwind-protect
// DONE: finish writing docstring data
// DONE: instead of map<>, use unordered_map<> if possible (see seldict.hpp)
// DONE: fix: in environments where wchar_t is 2-byte, widestring utf8 is wrong. (temporary fix done: in Windows, widechar support is removed, see selstr.hpp)
// DONE: use a different scoping technique: all functions are bound to the global namespace by default (safer than dynamic scoping)

#ifndef SELIN_HPP
#define SELIN_HPP

// CUSTOMIZATION
// Deallocation of a very long list would normally trigger
// recursive calls of ~LispNode(), resulting in a stack overflow.
// When SELIN_USE_ITERATIVE_NODE_DEALLOCATION is defined,
// ~LispNode() iteratively travels all the next neighbour nodes
// with reference count 1, and adds them into a blacklist.
// Then, again iteratively, it traverses this blacklist
// in reverse order, and destroys all those nodes.
// This prevents stack overflow.
#define SELIN_USE_ITERATIVE_NODE_DEALLOCATION

namespace selin
{
    // CUSTOMIZATION
    // selin represents all numbers as double.
    // for bitwise operations, it temporarily converts the doubles into
    // an integer data type, which can be configured by the following line:
    typedef int bitwise_calc_int;



    #if defined(WIN32) || defined(_WIN32)
    typedef __int64 bigint;
    #else
    typedef long long bigint;
    #endif

}

#include "seldict.hpp"
#include "selfuncs.hpp"
#include "refcount.hpp"
#include <fstream>
#include <cstdlib>
#include <string>
#include <sstream>
#include <vector>
#include <map>
#include <cmath>
#include <iterator>

namespace selin
{
    using namespace refcount;

    // CUSTOMIZATION
    // In selin,
    // all the names of callable objects are mangled: they get a prefix.
    // This is to make selin behave like Common Lisp,
    // where functions and variables live in different namespaces.
    // The prefix can be configured below.
    // If you change the prefix into an empty string (""),
    // callables and variables will live in the same namespace (like in Scheme).
    const char callable_prefix[] = "callable:";

    // CUSTOMIZATION
    // in selin, there are two scoping approaches:
    // dynamic and global (in which all functions are bound to the global scope by default)
    // a variable within selin is defined to configure this.
    // the name of that variable is below:
    const char scoping_configuration_variable_name[] = "selin-scoping";

    class LispException;
    void raise_error(std::string errtype, std::string msg) throw(Ref<LispException>);
    void raise_args_out_of_range(std::string msg) throw(Ref<LispException>);
    void raise_wrong_type_argument(std::string msg) throw(Ref<LispException>);
    void raise_parse_error(std::string msg) throw(Ref<LispException>);
    void raise_unfinished_expression_error(std::string msg = "Missing ')'") throw(Ref<LispException>);


    class LispObject : public Collectable
    {
    public:
        LispObject()
        {
        }

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispObject::type_name;
        }

        virtual std::string to_string() const
        {
            std::stringstream ss;
            ss << (void*)this;

            std::string s;
            ss >> s;

            return "#<" + get_type() + " " + s +">";
        }

        virtual std::string to_repr_string() const
        {
            return to_string();
        }

        virtual bool equals(CRef<LispObject> other) const
            throw(Ref<LispException>);

        static bool equal(CRef<LispObject> a, CRef<LispObject> b)
            throw(Ref<LispException>);

    private:
        LispObject(const LispObject &other);
        LispObject &operator=(const LispObject &other);
    };
    const char *LispObject::type_name = "object";


    std::string type_of(CRef<LispObject> o)
    {
        if (o.is_null())
        {
            return "nil";
        }
        else
        {
            return o->get_type();
        }
    }

    std::string type_of(CRef<LispException> o);

    std::string as_string(CRef<LispObject> o)
    {
        if (o.is_null())
        {
            return "()";
        }
        else
        {
            return o->to_string();
        }
    }


    std::string as_repr_string(CRef<LispObject> o)
    {
        if (o.is_null())
        {
            return "()";
        }
        else
        {
            return o->to_repr_string();
        }
    }

    std::string as_short_string(CRef<LispObject> o,
        size_t max_elements=100,
        size_t max_depth=10);


    bool is_callable(CRef<LispObject> o);
    bool is_number(CRef<LispObject> o);


    class LispNode : public LispObject
    {
    private:
        Ref<LispNode> next_node;
        Ref<LispObject> storage;

    public:
        LispNode()
        {
            next_node.nullify();
            storage.nullify();
        }

        LispNode(Ref<LispObject> value)
        {
            next_node.nullify();
            storage = value;
        }

        Ref<LispObject> car() { return storage; }
        Ref<LispNode> cdr() { return next_node; }

        CRef<LispObject> car() const { return storage; }
        CRef<LispNode> cdr() const { return next_node; }

        void setcar(Ref<LispObject> o)
        {
            storage = o;
        }

        void setcdr(Ref<LispNode> node)
        {
            next_node = node;
        }

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNode::type_name;
        }

    private:
        std::string get_string_representation() const
        {
            CRef<LispNode> current_node;
            std::string result = "(";

            result += as_repr_string(storage);

            for (current_node = next_node;
                 current_node.is_not_null();
                 current_node = current_node->next_node)
            {
                result += " " + as_repr_string(current_node->storage);
            }

            result += ")";

            return result;
        }

    public:
        virtual std::string to_string() const
        {
            return get_string_representation();
        }

        virtual std::string to_repr_string() const
        {
            return get_string_representation();
        }

        virtual bool equals(CRef<LispObject> other) const throw(Ref<LispException>)
        {
            if (other.is_null())
            {
                return false;
            }

            if (other->get_type() != get_type())
            {
                return false;
            }
            else
            {
                CRef<LispNode> other_node(other.as<LispNode>());
                if (!(LispObject::equal(car(), other_node->car())))
                    return false;

                CRef<LispNode> a(next_node);
                CRef<LispNode> b(other_node->cdr());

                if (a.is_null())
                {
                    if (b.is_not_null())
                    {
                        return false;
                    }

                    return true;
                }

                if (b.is_null())
                {
                    if (a.is_not_null())
                    {
                        return false;
                    }

                    return true;
                }


                while (true)
                {
                    if (!(LispObject::equal(a->car(), b->car())))
                        return false;

                    a = a->cdr();
                    b = b->cdr();

                    if (a.is_null())
                    {
                        if (b.is_not_null())
                        {
                            return false;
                        }

                        break;
                    }

                    if (b.is_null())
                    {
                        if (a.is_not_null())
                        {
                            return false;
                        }

                        break;
                    }
                }

                return true;
            }
        }

        static Ref<LispObject> pop(Ref<LispNode> &node)
        {
            if (node.is_null())
            {
                return Ref<LispObject>(NULL);
            }

            Ref<LispObject> result(node->car());
            node = node->cdr();

            return result;
        }

        static Ref<LispObject> pop_or_complain(Ref<LispNode> &node, std::string msg)
            throw(Ref<LispException>);

        #ifdef SELIN_USE_ITERATIVE_NODE_DEALLOCATION
        ~LispNode()
        {
            std::list<LispNode *> blacklist;
            LispNode *current_node = next_node.get_pointer();

            while (current_node != NULL
                && current_node->get_reference_count() == 1)
            {
                blacklist.push_back(current_node);
                current_node = current_node->next_node.get_pointer();
            }

            if (blacklist.size() >= 2)
            {
                std::list<LispNode *>::reverse_iterator it;

                for (it = blacklist.rbegin(); it != blacklist.rend(); it++)
                {
                    (*it)->next_node = Ref<LispNode>(NULL);
                }

            }
        }
        #endif

    private:

        // iterator of LispNode
        // --------------------
        class node_holder
        {
        protected:
            typedef LispNode* nodeptr;
            typedef Ref<LispNode> noderef;
            typedef Ref<LispObject> objectref;
            Ref<LispNode> node;

        public:
            Ref<LispObject> &operator*()
            {
                return node->storage;
            }

            CRef<LispObject> operator*() const
            {
                CRef<LispNode> cnode(node);
                return cnode->car();
            }
        };

        class const_node_holder
        {
        protected:
            typedef const LispNode* nodeptr;
            typedef CRef<LispNode> noderef;
            typedef CRef<LispObject> objectref;
            CRef<LispNode> node;

        public:
            CRef<LispObject> operator*() const
            {
                return node->car();
            }
        };

        template <typename HolderT>
        class forward_iterator_tmpl :
            public HolderT,
            public std::iterator<Ref<LispObject>, std::forward_iterator_tag>
        {
            void visit_next_node()
                throw(Ref<LispException>)
            {
                if (HolderT::node.is_null())
                {
                    raise_args_out_of_range("the iterator can not proceed further");
                }

                HolderT::node = (HolderT::node)->cdr();
            }

        public:
            forward_iterator_tmpl() {}
            forward_iterator_tmpl(typename HolderT::nodeptr n) { HolderT::node = typename HolderT::noderef(n); }
            forward_iterator_tmpl(typename HolderT::noderef n) : HolderT::node(n) {}
            forward_iterator_tmpl(const forward_iterator_tmpl &other)
            {
                HolderT::node = other.node;
            }

            forward_iterator_tmpl(typename HolderT::objectref o)
                throw(Ref<LispException>)
            {
                if (o.is_not_null())
                {
                    if (type_of(o) == LispNode::type_name)
                    {
                        Ref<LispNode> no(o.template as<LispNode>());
                        HolderT::node = no;
                    }
                    else
                    {
                        raise_wrong_type_argument("tried to iterate over a non-iterable object");
                    }
                }
            }

            forward_iterator_tmpl &operator++()
                throw(Ref<LispException>)
            {
                visit_next_node();
                return *this;
            }

            forward_iterator_tmpl operator++(int)
                throw(Ref<LispException>)
            {
                forward_iterator_tmpl mycopy(*this);
                visit_next_node();
                return mycopy;
            }

            bool operator==(const forward_iterator_tmpl &other) const
            {
                return HolderT::node.get_pointer() == other.node.get_pointer();
            }

            bool operator!=(const forward_iterator_tmpl &other) const
            {
                return HolderT::node.get_pointer() != other.node.get_pointer();
            }
        };
    public:

        typedef forward_iterator_tmpl<node_holder> forward_iterator;
        typedef forward_iterator_tmpl<const_node_holder> const_forward_iterator;

        typedef forward_iterator iterator;
        typedef const_forward_iterator const_iterator;
        // ----------------

        forward_iterator begin()
        {
            forward_iterator it(this);
            return it;
        }

        forward_iterator end()
        {
            forward_iterator it;
            return it;
        }

        const_forward_iterator cbegin() const
        {
            const_forward_iterator it(this);
            return it;
        }

        const_forward_iterator cend() const
        {
            const_forward_iterator it;
            return it;
        }

        const_forward_iterator begin() const
        {
            return cbegin();
        }

        const_forward_iterator end() const
        {
            return cend();
        }

    };
    const char *LispNode::type_name = "cons";

    template<typename T>
    Ref<LispObject> objrefnew()
    {
        Ref<LispObject> o_result;
        Ref<T> result(new T());
        o_result = result.template as<LispObject>();
        return o_result;
    }

    template<typename T, typename T1>
    Ref<LispObject> objrefnew(T1 a1)
    {
        Ref<LispObject> o_result;
        Ref<T> result(new T(a1));
        o_result = result.template as<LispObject>();
        return o_result;
    }

    template<typename T, typename T1, typename T2>
    Ref<LispObject> objrefnew(T1 a1, T2 a2)
    {
        Ref<LispObject> o_result;
        Ref<T> result(new T(a1, a2));
        o_result = result.template as<LispObject>();
        return o_result;
    }

    template<typename T, typename T1, typename T2, typename T3>
    Ref<LispObject> objrefnew(T1 a1, T2 a2, T3 a3)
    {
        Ref<LispObject> o_result;
        Ref<T> result(new T(a1, a2, a3));
        o_result = result.template as<LispObject>();
        return o_result;
    }


    class NodesCreator
    {
    private:
        Ref<LispNode> first;
        Ref<LispNode> last;

    private:
        void clone_from(const NodesCreator &other)
        {
            first = other.first;
            last = other.last;
        }

    public:
        NodesCreator(const NodesCreator &other)
        {
            clone_from(other);
        }

        NodesCreator &operator=(const NodesCreator &other)
        {
            clone_from(other);
            return *this;
        }

        NodesCreator()
        {
        }


    public:
        void push_back(Ref<LispObject> o)
        {
            Ref<LispNode> node_o;
            node_o = refnew<LispNode>(o);

            if (first.is_null())
            {
                first = node_o;
                last = node_o;
            }
            else
            {
                last->setcdr(node_o);
                last = node_o;
            }
        }

        Ref<LispNode> get_first_node() const
        {
            return first;
        }
    };


    class LispNumber : public LispObject
    {
    private:
        double value;

    public:
        LispNumber()
        {
            value = 0;
        }

        LispNumber(double n)
        {
            value = n;
        }

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNumber::type_name;
        }

        virtual std::string to_string() const
        {
            std::stringstream ss;
            ss << value;
            char result[50];
            ss >> result;

            return result;
        }

        virtual bool equals(CRef<LispObject> other) const throw(Ref<LispException>)
        {
            if (other.is_null())
            {
                return false;
            }

            if (other->get_type() != get_type())
            {
                return false;
            }
            else
            {
                //return crefcast<LispObject, LispNumber>(other)->value == value;
                return other.as<LispNumber>()->value == value;
            }
        }

        double get_value() const
        {
            return value;
        }
    };
    const char *LispNumber::type_name = "number";


    class LispString : public LispObject
    {
    private:
        widestring value;

    public:
        LispString()
        {
            value = "";
        }

        LispString(widestring s)
        {
            value = s;
        }

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispString::type_name;
        }

        virtual std::string to_string() const
        {
            return value.to_string();
        }

        virtual std::string to_repr_string() const
        {
            std::string result = "\"";

            for (size_t i = 0; i < value.size(); i++)
            {
                if (value[i] == '\"')
                {
                    result += "\\\"";
                }
                else
                {
                    result += value.substr(i, 1).to_string();
                }
            }

            result += "\"";

            return result;
        }

        virtual bool equals(CRef<LispObject> other) const throw(Ref<LispException>)
        {
            if (other.is_null())
            {
                return false;
            }

            if (other->get_type() != get_type())
            {
                return false;
            }
            else
            {
                return other.as<LispString>()->value == value;
            }
        }

        widestring get_value() const
        {
            return value;
        }
    };
    const char *LispString::type_name = "string";


    class LispSymbol : public LispObject
    {
    private:
        std::string value;

    public:
        LispSymbol()
        {
        }

        LispSymbol(std::string s)
        {
            if (s.size() > 2 && s.substr(0, 2) == "#'")
            {
                value = callable_prefix + s.substr(2, s.size() - 2);
            }
            else
            {
                value = s;
            }
        }

        virtual std::string to_string() const
        {
            return value;
        }

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispSymbol::type_name;
        }

        virtual bool equals(CRef<LispObject> other) const throw(Ref<LispException>)
        {
            if (other.is_null())
            {
                return false;
            }

            if (other->get_type() != get_type())
            {
                return false;
            }
            else
            {
                return other.as<LispSymbol>()->value == value;
            }
        }

        static Ref<LispObject> truth()
        {
            Ref<LispSymbol> t(new LispSymbol("t"));
            return t.as<LispObject>();
        }

        std::string get_value() const
        {
            return value;
        }
    };
    const char *LispSymbol::type_name = "symbol";


    //class LispT : public LispObject
    //{
    //public:
    //    static const char *type_name;
    //    virtual std::string get_type()
    //    {
    //        return LispT::type_name;
    //    }
    //
    //    virtual std::string to_string()
    //    {
    //        return "t";
    //    }
    //
    //    virtual bool equals(CRef<LispObject> other) throw(Ref<LispException>)
    //    {
    //        if (other.is_null())
    //        {
    //            return false;
    //        }
    //
    //        return other->get_type() == get_type();
    //    }
    //};
    //const char *LispT::type_name = "t";


    class LispVector : public LispObject
    {
    private:
        std::vector< Ref<LispObject> > elements;

    public:
        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispVector::type_name;
        }

        virtual std::string to_string() const
        {
            std::string result = "[";

            std::vector< Ref<LispObject> >::const_iterator it;
            for (it = elements.begin(); it != elements.end(); it++)
            {
                if (it != elements.begin())
                {
                    result += " ";
                }

                result += as_repr_string(*it);
            }

            result += "]";

            return result;
        }

        virtual bool equals(CRef<LispObject> other) const throw(Ref<LispException>)
        {
            if (other.is_null())
            {
                return false;
            }

            if (get_type() != other->get_type())
            {
                return false;
            }

            CRef<LispVector> vec(other.as<LispVector>());

            if (vec->elements.size() != elements.size())
            {
                return false;
            }

            std::vector< Ref<LispObject> >::const_iterator it1;
            std::vector< Ref<LispObject> >::const_iterator it2;
            for (it1 = elements.begin(), it2 = vec->elements.begin();
                 it1 != elements.end(); it1++, it2++)
            {
                if (!(LispObject::equal(*it1, *it2)))
                {
                    return false;
                }
            }

            return true;
        }

        size_t size() const
        {
            return elements.size();
        }

        size_t capacity() const
        {
            return elements.capacity();
        }

        // TODO: throw exception if index is out of bounds
        Ref<LispObject> &at(size_t index)
        {
            return elements[index];
        }

        CRef<LispObject> at(size_t index) const
        {
            return elements[index];
        }

        void push_back(Ref<LispObject> o)
        {
            elements.push_back(o);
        }

        void resize(size_t newsize)
        {
            elements.resize(newsize);
        }

        void reserve(size_t newcapacity)
        {
            elements.reserve(newcapacity);
        }
    };
    const char *LispVector::type_name = "vector";

    //class LispBoolean : public LispObject
    //{
    //public:
    //    bool value;
    //
    //    virtual std::string get_type()
    //    {
    //        return "boolean";
    //    }
    //
    //    virtual std::string to_string()
    //    {
    //        if (value) return "#t";
    //        return "#f";
    //    }
    //};


    //bool is_true(Ref<LispObject> o)
    //{
    //    if (o.is_null()) return false;
    //    if (type_of(o) == "boolean")
    //    {
    //        Ref<LispBoolean> b = o.as<LispBoolean>();
    //        if (!(b->value)) return false;
    //    }
    //
    //    return true;
    //}


    bool is_true(CRef<LispObject> o)
    {
        if (o.is_null()) return false;
        return true;
    }


    class LispException : public LispObject
    {
    protected:
        Ref<LispObject> data;
        bool unfinished_expression; // meant to be used only by (read)

    public:

        LispException()
        {
            unfinished_expression = false;
        }

        LispException(std::string msg)
        {
            Ref<LispString> rmsg(new LispString(msg));
            data = rmsg.as<LispObject>();
            unfinished_expression = false;
        }

        LispException(Ref<LispObject> o)
        {
            data = o;
            unfinished_expression = false;
        }

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispException::type_name;
        }

        virtual std::string to_string() const
        {
            return as_string(data);
        }

        bool is_unfinished_expression_exception() const
        {
            return unfinished_expression;
        }

        void make_unfinished_expression_exception()
        {
            unfinished_expression = true;
        }
    };
    const char *LispException::type_name = "exception";

    std::string type_of(CRef<LispException> o)
    {
        if (o.is_null())
        {
            return "nil";
        }
        else
        {
            return o->get_type();
        }
    }


    class LispRequest : public LispException
    {
        int rtype;
        std::string rlabel;

    public:
        static const int req_quit = 0;
        static const int req_return = 1;
        static const int req_catch = 2;

        static std::string request_to_string(int req)
        {
            if (req == req_quit) return "quit";
            if (req == req_return) return "return";
            if (req == req_catch) return "catch";

            std::stringstream ss;
            ss << req;
            return ss.str();
        }

        LispRequest(int request_type)
        {
            rtype = request_type;
            data = Ref<LispObject>(NULL);
        }

        LispRequest(int request_type, Ref<LispObject> o)
        {
            rtype = request_type;
            data = o;
            rlabel = "";
        }

        LispRequest(int request_type, Ref<LispObject> o, std::string request_label)
        {
            rtype = request_type;
            data = o;
            rlabel = request_label;
        }

        int get_request_type() const
        {
            return rtype;
        }

        std::string get_request_label() const
        {
            return rlabel;
        }

        Ref<LispObject> get_data() const
        {
            return data;
        }

        virtual std::string to_string() const
        {
            std::stringstream result;

            result << "#<request ";
            result << request_to_string(rtype);
            result << " ";
            if (!(rlabel.empty()))
            {
                result << rlabel << " ";
            }
            result << as_short_string(data);
            result << ">";

            return result.str();
        }

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispRequest::type_name;
        }
    };
    const char *LispRequest::type_name = "request";


    class LispError : public LispException
    {
    public:
        static const char *s_wrong_type_argument;
        static const char *s_wrong_number_of_arguments;
        static const char *s_args_out_of_range;
        static const char *s_parse_error;
        static const char *s_void_variable;
        static const char *s_void_function;
        static const char *s_value_error;
        static const char *s_stdin_error;

    private:
        std::string errtype;

    public:
        LispError(std::string etype, std::string msg)
        {
            Ref<LispString> rmsg(new LispString(msg));
            data = rmsg.as<LispObject>();
            errtype = etype;
        }

        LispError(std::string etype, Ref<LispObject> o)
        {
            data = o;
            errtype = etype;
        }

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispError::type_name;
        }

        virtual std::string to_string() const
        {
            return "#<error " + errtype + " " + as_short_string(data) + ">";
        }

        std::string get_error_type() const
        {
            return errtype;
        }

        Ref<LispObject> get_data()
        {
            return data;
        }

        bool can_be_handled_by(std::string s) const
        {
            return (s == "error") || (s == errtype);
        }
    };
    const char *LispError::type_name = "error";
    const char *LispError::s_wrong_type_argument = "wrong-type-argument";
    const char *LispError::s_wrong_number_of_arguments = "wrong-number-of-arguments";
    const char *LispError::s_args_out_of_range = "args-out-of-range";
    const char *LispError::s_parse_error = "parse-error";
    const char *LispError::s_void_variable = "void-variable";
    const char *LispError::s_void_function = "void-function";
    const char *LispError::s_value_error = "value-error";
    const char *LispError::s_stdin_error = "stdin-error";

    void raise_error(std::string errtype, std::string msg)
        throw(Ref<LispException>)
    {
        Ref<LispError> err(new LispError(errtype, msg));
        throw err.as<LispException>();
    }

    void raise_args_out_of_range(std::string msg)
        throw(Ref<LispException>)
    {
        raise_error(LispError::s_args_out_of_range, msg);
    }

    void raise_wrong_type_argument(std::string msg)
        throw(Ref<LispException>)
    {
        raise_error(LispError::s_wrong_type_argument, msg);
    }

    void raise_parse_error(std::string msg)
        throw(Ref<LispException>)
    {
        raise_error(LispError::s_parse_error, msg);
    }

    void raise_unfinished_expression_error(std::string msg)
        throw(Ref<LispException>)
    {
        std::string errtype = LispError::s_parse_error;
        Ref<LispError> err(new LispError(errtype, msg));
        err->make_unfinished_expression_exception();
        throw err.as<LispException>();
    }

    Ref<LispObject> LispNode::pop_or_complain(Ref<LispNode> &node, std::string msg)
        throw(Ref<LispException>)
    {
        if (node.is_null())
        {
            raise_error(LispError::s_wrong_number_of_arguments, msg);
        }

        Ref<LispObject> result(node->car());
        node = node->cdr();

        return result;
    }



    bool LispObject::equals(CRef<LispObject> other) const
        throw(Ref<LispException>)
    {
        raise_error(LispError::s_wrong_type_argument, "Objects are not comparable");
        return false;  //<--- although we should not reach here
    }

    bool LispObject::equal(CRef<LispObject> a, CRef<LispObject> b)
        throw(Ref<LispException>)
    {
        if (a.is_null() && b.is_not_null())
            return false;

        if (b.is_null() && a.is_not_null())
            return false;

        if (a.is_null() && b.is_null())
            return true;

        return a->equals(b);
    }

    Ref<LispObject> quote_object(Ref<LispObject> o)
    {
        Ref<LispNode> first, second;

        Ref<LispSymbol> q(new LispSymbol("quote"));
        Ref<LispObject> oq(q.as<LispObject>());

        first = refnew<LispNode>(oq);
        second = refnew<LispNode>(o);

        first->setcdr(second);

        Ref<LispObject> result(first.as<LispObject>());

        return result;
    }


    Ref<LispObject> quote_object(Ref<LispObject> o, int quote_level)
    {
        int i;

        for (i = 0; i < quote_level; i++)
        {
            o = quote_object(o);
        }

        return o;
    }


    void tokenize(std::string s, std::list<std::string> &tokens)
    {
        char cs[2];
        char &c = cs[0];
        size_t i;
        std::string cell = "";
        bool quoted = false;
        bool escape_char = false;
        bool commented = false;

        tokens.clear();

        cs[0] = '\0';
        cs[1] = '\0';

        for (i = 0; i < s.size(); i++)
        {
            c = s[i];

            if (commented)
            {
                if (c == '\n' || c == '\r')
                {
                    // if new line character is encountered,
                    // unset the 'commented' state
                    commented = false;
                }
            }
            else if (escape_char)
            {
                // if escape-char state is on,
                // interpret the character preceding the backslash
                if (c == 't')
                {
                    cell += "\t";
                }
                else if (c == 'n')
                {
                    cell += "\n";
                }
                else if (c == 'r')
                {
                    cell += "\r";
                }
                else if (c == '\'')
                {
                    cell += "'";
                }
                else if (c == '\"')
                {
                    cell += "\"";
                }
                else if (c == '\\')
                {
                    cell += "\\";
                }
                else if (c == '0')
                {
                    cell += "\0";
                }
                else
                {
                    cell += cs;
                }

                escape_char = false;
            }
            else if (c == '\"')
            {
                // if double quotation mark is encountered
                // set or unset 'quoted' state
                if (!quoted)
                {
                    if (cell.size() > 0)
                    {
                        tokens.push_back(cell);
                    }
                    cell = "\"";
                    quoted = true;
                }
                else
                {
                    cell += "\"";
                    tokens.push_back(cell);
                    cell = "";
                    quoted = false;
                }

            }
            else if (quoted)
            {
                if (c == '\\')
                {
                    escape_char = true;
                }
                else
                {
                    cell += cs;
                }
            }
            else
            {
                // if not in quoted state

                if (c == '(' || c == ')')
                {
                    // these characters are separators: ( )

                    if (cell.size() > 0)
                    {
                        tokens.push_back(cell);
                    }
                    tokens.push_back(cs);
                    cell = "";
                }
                else if (c == '\'')
                {
                    bool single_quote_token = true;

                    if (cell.size() > 0)
                    {
                        if (cell[cell.size() - 1] == '#')
                        {
                            single_quote_token = false;
                        }
                        else
                        {
                            tokens.push_back(cell);
                        }
                    }

                    if (single_quote_token)
                    {
                        tokens.push_back(cs);
                        cell = "";
                    }
                    else
                    {
                        cell += "'";
                    }
                }
                else if (is_whitespace(c))
                {
                    // whitespace is also separator

                    if (cell.size() > 0)
                    {
                        tokens.push_back(cell);
                        cell = "";
                    }
                }
                else if (c == ';')
                {
                    // semicolon character starts the 'commented' state

                    if (cell.size() > 0)
                    {
                        tokens.push_back(cell);
                        cell = "";
                    }
                    commented = true;
                }
                else
                {
                    cell += cs;
                }
            }
        }

        if (cell.size() > 0)
        {
            tokens.push_back(cell);
        }
    }


    Ref<LispObject> create_data(std::string token, int quote_level = 0)
        throw(Ref<LispException>)
    {
        Ref<LispObject> result;

        if (token.size() == 0)
        {
            raise_error(LispError::s_parse_error, "Empty token encountered");
        }
        else if (token[0] == '\"')
        {
            std::string sub = token.substr(1, token.size() - 2);
            Ref<LispString> sresult(new LispString(sub));
            result = sresult.as<LispObject>();
        }
        else if (is_numeric_string(token))
        {
            double n = as_number(token);
            Ref<LispNumber> nresult(new LispNumber(n));
            result = nresult.as<LispObject>();
        }
        else
        {
            Ref<LispSymbol> syresult(new LispSymbol(token));
            result = syresult.as<LispObject>();
        }

        //Ref<LispObject> contained;
        //Ref<LispNode> result_node;
        //int i;
        //for (i = 0; i < quote_level; i++)
        //{
        //    contained = result;

        //    result = objrefnew<LispNode>();

        //    Ref<LispSymbol> syquote(new LispSymbol("quote"));
        //    Ref<LispObject> o_syquote(syquote.as<LispObject>());

        //    result_node = result.as<LispNode>();
        //    result_node->setcar(o_syquote.as<LispObject>());

        //    Ref<LispNode> next_node(new LispNode());

        //    result_node->setcdr(next_node);
        //    result_node->cdr()->setcar(contained);
        //}

        result = quote_object(result, quote_level);

        return result;
    }

    Ref<LispObject> create_data(std::list<std::string> &tokens, int quote_level = 0)
        throw(Ref<LispException>);

    Ref<LispObject> create_list_data(std::list<std::string> &tokens, int quote_level = 0)
        throw(Ref<LispException>)
    {
        std::string token;
        NodesCreator nc;
        Ref<LispObject> result;
        std::list<std::string>::iterator it;
        int quotes = 0;
        int depth = 0;
        std::list<std::string> sublist;

        for (it = tokens.begin(); it != tokens.end(); it++)
        {
            token = *it;

            if (token == "(")
            {
                depth++;
                sublist.push_back(token);
            }
            else if (token == ")")
            {
                depth--;

                if (depth < 0)
                {
                    raise_parse_error("')' was encountered more than expected");
                }

                sublist.push_back(token);
                if (depth == 0)
                {
                    nc.push_back(create_data(sublist, quotes));
                    sublist.clear();
                    quotes = 0;
                }
            }
            else if (depth >= 1)
            {
                sublist.push_back(token);
            }
            else if (token == "'")
            {
                quotes++;
            }
            else
            {
                nc.push_back(create_data(token, quotes));
                quotes = 0;
            }
        }

        if (depth > 0)
        {
            raise_unfinished_expression_error();
        }

        if (quotes > 0)
        {
            raise_parse_error("List expression ended right after the single quotation mark ('), before specifying what should be quoted");
        }

        Ref<LispNode> result_node(nc.get_first_node());
        result = result_node.as<LispObject>();
        result = quote_object(result, quote_level);

        return result;
    }

    Ref<LispObject> create_data(std::list<std::string> &tokens, int quote_level)
        throw(Ref<LispException>)
    {
        Ref<LispObject> result;
        bool result_provided = false;
        std::list<std::string>::iterator it;
        std::string token;
        int depth = 0;
        std::list<std::string> sublist;
        int quotes = 0;

        for (it = tokens.begin(); it != tokens.end(); it++)
        {
            token = *it;

            if (result_provided)
            {
                raise_parse_error("Unexpected tokens are received");
            }

            if (token == "(")
            {
                if (depth > 0)
                {
                    sublist.push_back(token);
                }
                depth++;
            }
            else if (token == ")")
            {
                if (depth == 0)
                {
                    raise_parse_error("Too much ')' tokens are received");
                }

                depth--;
                if (depth == 0)
                {
                    result = create_list_data(sublist, quotes);
                    result_provided = true;
                    sublist.clear();
                    quotes = 0;
                }
                else
                {
                    sublist.push_back(token);
                }
            }
            else if (depth > 0)
            {
                sublist.push_back(token);
            }
            else if (token == "'")
            {
                quotes++;
            }
            else
            {
                result = create_data(token, quotes);
                result_provided = true;
                quotes = 0;
            }
        }

        if (depth > 0)
        {
            raise_unfinished_expression_error();
        }

        if (quotes > 0)
        {
            //raise_parse_error("Quotation marks were provided, but which object should be quoted?");
            raise_unfinished_expression_error();
        }

        result = quote_object(result, quote_level);

        return result;
    }

    Ref<LispObject> read_from_stdin(std::string prompt1="", std::string prompt2="")
        throw(Ref<LispException>)
    {
        Ref<LispObject> result;
        std::string s;
        std::list<std::string> tokens;

        std::string line;
        std::string full_entry;

        while (true)
        {
            if (full_entry.empty())
            {
                std::cout << prompt1;
            }
            else
            {
                std::cout << prompt2;
            }

            if (std::getline(std::cin, line))
            {
                full_entry += line + "\n";
                tokenize(full_entry, tokens);

                try
                {
                    result = create_data(tokens);
                }
                catch (Ref<LispException> ex)
                {
                    if (ex.is_null())
                    {
                        throw ex;
                    }

                    if (ex->is_unfinished_expression_exception())
                    {
                        continue;
                    }
                    else
                    {
                        throw ex;
                    }
                }
            }
            else
            {
                Ref<LispError> err(new LispError(LispError::s_stdin_error, "stdin got closed"));
                Ref<LispException> raise_me(err.as<LispException>());
                throw raise_me;
            }

            break;
        }

        return result;
    }

    Ref<LispObject> read_from_string(std::string s)
        throw(Ref<LispException>)
    {
        std::list<std::string> tokens;
        tokenize(s, tokens);
        return create_data(tokens);
    }

    class Scope;


    class LispCallable : public LispObject
    {
    public:

        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispCallable::type_name;
        }

        virtual Ref<LispObject> execute(
            Scope *caller, Ref<LispNode> arglist) throw(Ref<LispException>) = 0;
    };
    const char *LispCallable::type_name = "callable";


    class Variable
    {
    private:
        std::string name;
        Ref<LispObject> value;

        void clone_from(const Variable &other)
        {
            name = other.name;
            value = other.value;
        }

    public:
        Variable(const Variable &other)
        {
            clone_from(other);
        }

        Variable &operator=(const Variable &other)
        {
            clone_from(other);
            return *this;
        }

        Variable()
        {
        }

        std::string get_type() const
        {
            return type_of(value);
        }

        bool is_callable() const
        {
            return ::selin::is_callable(value);
        }

        void set_name(std::string n)
        {
            name = n;
        }

        std::string get_name() const
        {
            return name;
        }

        void set_value(Ref<LispObject> v)
        {
            value = v;
        }

        Ref<LispObject> get_value()
        {
            return value;
        }

        CRef<LispObject> get_value() const
        {
            return value;
        }
    };


    class Scope
    {
    private:
        dictionary<std::string, Variable>::map variables;
        Scope *parent_scope;
        Scope *global_scope;
        Ref<LispNode> goodbye_actions;

        void clone_from(const Scope &other)
        {
            variables = other.variables;
            parent_scope = other.parent_scope;
            global_scope = other.global_scope;
            goodbye_actions = other.goodbye_actions;
        }

    public:
        Scope(const Scope &other)
        {
            clone_from(other);
        }

        Scope &operator=(const Scope &other)
        {
            clone_from(other);
            return *this;
        }

    private:
        void fill_with_builtins();

    public:
        Scope()
        {
            goodbye_actions.nullify();
            parent_scope = NULL;
            global_scope = this;
            fill_with_builtins();
        }

        Scope(Scope *parent)
        {
            goodbye_actions.nullify();
            if (parent == NULL)
            {
                parent_scope = NULL;
                global_scope = this;
            }
            else
            {
                parent_scope = parent;
                global_scope = parent->global_scope;
            }
        }

        Scope *get_parent_scope()
        {
            return parent_scope;
        }

        const Scope *get_parent_scope() const
        {
            return parent_scope;
        }

        Scope *get_global_scope()
        {
            return global_scope;
        }

        const Scope *get_global_scope() const
        {
            return global_scope;
        }

        Variable *get_local_variable(std::string varname)
        {
            dictionary<std::string, Variable>::map::iterator it;
            it = variables.find(varname);

            if (it == variables.end())
            {
                return NULL;
            }
            else
            {
                return &(it->second);
            }
        }

        const Variable *get_local_variable(std::string varname) const
        {
            dictionary<std::string, Variable>::map::const_iterator it;
            it = variables.find(varname);

            if (it == variables.end())
            {
                return NULL;
            }
            else
            {
                return &(it->second);
            }
        }



        Variable *get_variable(std::string varname)
        {
            Variable *result;

            result = get_local_variable(varname);

            if (result == NULL && parent_scope != NULL)
            {
                result = parent_scope->get_variable(varname);
            }

            return result;
        }


        void set_local_variable(std::string varname, Ref<LispObject> value)
        {
            Variable *v;
            v = get_local_variable(varname);

            if (v == NULL)
            {
                Variable vv;
                vv.set_name(varname);
                vv.set_value(value);
                variables[varname] = vv;
            }
            else
            {
                v->set_value(value);
            }
        }


        void set_variable(std::string varname, Ref<LispObject> value)
        {
            if (parent_scope == NULL)
            {
                set_local_variable(varname, value);
            }
            else
            {
                Variable *v;
                v = get_local_variable(varname);

                if (v == NULL)
                {
                    parent_scope->set_variable(varname, value);
                }
                else
                {
                    v->set_value(value);
                }
            }
        }


        Variable *get_callable(std::string callable_name)
        {
            return get_variable(callable_prefix + callable_name);
        }


        Variable *get_local_callable(std::string callable_name)
        {
            return get_local_variable(callable_prefix + callable_name);
        }


        void set_local_callable(std::string callable_name, Ref<LispObject> obj)
        {
            set_local_variable(callable_prefix + callable_name, obj);
        }


        void set_callable(std::string callable_name, Ref<LispObject> obj)
        {
            set_variable(callable_prefix + callable_name, obj);
        }

        template <typename TF>
        void set_native_callable(std::string callable_name, TF f);

        Ref<LispObject> evaluate(Ref<LispObject> obj)
            throw(Ref<LispException>)
        {
            Variable *v;
            std::string t;
            t = type_of(obj);

            if (t == "symbol")
            {
                std::string s_obj;
                s_obj = obj->to_string();

                if (s_obj.size() >= 2 && s_obj[0] == ':')
                {
                    // if the symbol starts with ':',
                    // then it should be treated as a keyword
                    // so, it just evaluates to itself

                    return obj;
                }
                else
                {
                    // symbols which are not keywords
                    // are assumed to be variable names
                    // and they evaluate to their values

                    v = get_variable(obj->to_string());

                    if (v == NULL)
                    {
                        raise_error(LispError::s_void_variable, "Unknown variable: " + obj->to_string());
                    }

                    return v->get_value();
                }
            }
            else if (t == "cons")
            {
                // cons objects (lists) are assumed to be statements

                Ref<LispNode> lst;
                lst = obj.as<LispNode>();

                Ref<LispObject> cmd;
                cmd = lst->car();

                if (type_of(cmd) == "symbol")
                {
                    Ref<LispSymbol> cmd_symbol;
                    cmd_symbol = cmd.as<LispSymbol>();

                    std::string to_call;
                    to_call = callable_prefix + cmd_symbol->get_value();

                    Ref<LispSymbol> sycmd(new LispSymbol(to_call));
                    cmd = sycmd.as<LispObject>();
                }

                cmd = evaluate(cmd);

                if (!is_callable(cmd))
                {
                    raise_error(LispError::s_wrong_type_argument, as_string(cmd) + " is not a callable");
                }

                Ref<LispCallable> fun;
                fun = cmd.as<LispCallable>();

                return fun->execute(this, lst->cdr());
            }
            else
            {
                // objects of other types evaluate to themselves
                return obj;
            }

            return Ref<LispObject>(NULL);
        }


        Ref<LispObject> evaluate(std::string s)
            throw(Ref<LispException>)
        {
            return evaluate(read_from_string(s));
        }


        Ref<LispNode> evaluate_nodes(Ref<LispNode> nodes)
            throw(Ref<LispException>)
        {
            Ref<LispNode> current_node;
            Ref<LispObject> obj;
            NodesCreator evaluated_list;

            for (current_node = nodes;
                 current_node.is_not_null();
                 current_node = current_node->cdr())
            {
                obj = current_node->car();
                evaluated_list.push_back(evaluate(obj));
            }

            return evaluated_list.get_first_node();
        }

        void apropos(std::string s, dictionary<std::string, Variable>::map &m)
        {
            for (dictionary<std::string, Variable>::map::iterator it = variables.begin();
                 it != variables.end();
                 it++)
            {
                std::string varname;
                varname = it->first;

                if (s.size() == 0 || varname.find(s) != std::string::npos)
                {
                    if (m.find(varname) == m.end())
                    {
                        m[varname] = it->second;
                    }
                }
            }

            if (parent_scope != NULL)
            {
                parent_scope->apropos(s, m);
            }
        }

        void set_goodbye_actions(Ref<LispNode> o)
        {
            goodbye_actions = o;
        }

        Ref<LispNode> get_goodbye_actions()
        {
            return goodbye_actions;
        }

        CRef<LispNode> get_goodbye_actions() const
        {
            return goodbye_actions;
        }

        ~Scope()
        {
            evaluate_nodes(goodbye_actions);
        }

        void repl(bool welcome=true, std::string promptmsg="selin % ", std::string promptmsg2="    ... ")
        {
            if (welcome)
            {
                std::cout << "Welcome to selin interpreter!" << std::endl;
                std::cout << "Type (quit) to quit." << std::endl;
                std::cout << "Type (apropos) to see all the commands/variables names." << std::endl;
                std::cout << "Type (apropos \"xx\") to list all the commands/variables which have 'xx' in their names." << std::endl;
                std::cout << std::endl;
            }

            Ref<LispObject> obj;
            std::string s;

            while (true)
            {
                try
                {
                    obj = read_from_stdin(promptmsg, promptmsg2);
                    obj = evaluate(obj);
                    //std::cout << "-> " << as_repr_string(obj) << std::endl;
                    std::cout << "-> " << as_short_string(obj) << std::endl;
                    obj.nullify();
                }
                catch (Ref<LispException> ex)
                {
                    if (ex.is_not_null())
                    {
                        if (ex->get_type() == LispError::type_name)
                        {
                            Ref<LispError> err(ex.as<LispError>());
                            if (err->get_error_type() == LispError::s_stdin_error)
                            {
                                break;
                            }
                        }
                        else if (ex->get_type() == LispRequest::type_name)
                        {
                            Ref<LispRequest> req(ex.as<LispRequest>());
                            if (req->get_request_type() == LispRequest::req_quit)
                            {
                                break;
                            }
                        }

                        std::cout << "Unhandled exception: " << ex->to_string() << std::endl;
                    }
                    else
                    {
                        std::cout << "Unhandled exception: nil was raised" << std::endl;
                    }
                }
            }

        }

        Ref<LispObject> run_script_file(std::string s)
            throw(Ref<LispException>)
        {
            std::ifstream f(s.c_str());
            std::string lisp_code = "";
            std::string line;

            while (std::getline(f, line))
            {
                lisp_code += line + "\n";
            }
            f.close();

            lisp_code = "(progn " + lisp_code + ")";

            return evaluate(lisp_code);
        }

    };


    class LispCustomCallable : public LispCallable
    {
    public:
        std::string name;
        std::list<std::string> arguments;
        Ref<LispNode> actions;
        bool evaluate_arguments;
        bool run_on_global_scope;

        LispCustomCallable()
        {
            evaluate_arguments = true;
            run_on_global_scope = true;
        }

        virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
            throw(Ref<LispException>)
        {
            Scope *parent;
            if (run_on_global_scope)
            {
                parent = caller->get_global_scope();
            }
            else
            {
                parent = caller;
            }

            Scope sc(parent);
            std::list<std::string>::iterator it_arguments;

            bool optional_args = false;
            bool rest_args = false;
            for (it_arguments = arguments.begin();
                 it_arguments != arguments.end();
                 it_arguments++)
            {
                if (*it_arguments == "&optional")
                {
                    optional_args = true;
                }
                else if (*it_arguments == "&rest")
                {
                    rest_args = true;
                }
                else if (rest_args)
                {
                    Ref<LispNode> lst;
                    lst = args;

                    if (evaluate_arguments)
                    {
                        lst = caller->evaluate_nodes(lst);
                    }

                    sc.set_local_variable(*it_arguments, lst.as<LispObject>());
                    break;
                }
                else
                {
                    if (args.is_null())
                    {
                        if (!optional_args)
                        {
                            raise_error(LispError::s_wrong_number_of_arguments, "Wrong number of arguments for " + name);
                        }
                        else
                        {
                            sc.set_local_variable(*it_arguments, Ref<LispObject>(NULL));
                        }
                    }
                    else
                    {
                        Ref<LispObject> v;
                        v = args->car();

                        if (evaluate_arguments)
                        {
                            v = caller->evaluate(v);
                        }

                        sc.set_local_variable(*it_arguments, v);
                        args = args->cdr();
                    }
                }
            }

            Ref<LispObject> result;

            for (Ref<LispNode> action = actions;
                 action.is_not_null();
                 action = action->cdr())
            {
                result = sc.evaluate(action->car());
            }

            return result;
        }

        virtual std::string to_repr_string() const
        {
            std::stringstream result;

            result << to_string();

            for (Ref<LispNode> action = actions;
                 action.is_not_null();
                 action = action->cdr())
            {
                if (type_of(action->car()) == "string")
                {
                    result << std::endl << as_string(action->car());
                }
                else
                {
                    break;
                }
            }

            return result.str();
        }
    };


    bool is_callable(CRef<LispObject> o)
    {
        std::string s;
        s = type_of(o);

        return (s == LispCallable::type_name);
    }


    bool is_number(CRef<LispObject> o)
    {
        std::string s;
        s = type_of(o);

        return (s == LispNumber::type_name);
    }

    double as_number(CRef<LispObject> o)
        throw(Ref<LispException>)
    {
        if (type_of(o) != LispNumber::type_name)
        {
            raise_error(LispError::s_wrong_type_argument, "Can not convert the object into a number");
        }

        CRef<LispNumber> n = o.as<LispNumber>();
        return n->get_value();
    }

    double as_number(CRef<LispNumber> n)
    {
        return n->get_value();
    }

    template <typename LispT>
    bool type_matches(CRef<LispObject> o)
    {
        std::string s;
        s = type_of(o);

        return (s == LispT::type_name);
    }

    bool is_string(CRef<LispObject> o)
    {
        return type_matches<LispString>(o);
    }

    bool is_symbol(CRef<LispObject> o)
    {
        return type_matches<LispSymbol>(o);
    }

    bool is_list(CRef<LispObject> o)
    {
        if (o.is_null()) return true;
        return type_matches<LispNode>(o);
    }

    bool is_vector(CRef<LispObject> o)
    {
        return type_matches<LispVector>(o);
    }

    bool is_error(CRef<LispObject> o)
    {
        return type_matches<LispError>(o);
    }

    bool is_request(CRef<LispObject> o)
    {
        return type_matches<LispRequest>(o);
    }

    Ref<LispObject> create_number_object(double x)
    {
        return objrefnew<LispNumber>(x);
    }

    Ref<LispObject> create_string_object(std::string s)
    {
        return objrefnew<LispString>(s);
    }

    Ref<LispObject> create_symbol_object(std::string s)
    {
        return objrefnew<LispSymbol>(s);
    }

    template <typename Iterable>
    Ref<LispObject> create_list_object(Iterable &sequence)
    {
        NodesCreator nc;
        for (typename Iterable::iterator it = sequence.begin();
             it != sequence.end();
             ++it)
        {
            nc.push_back(*it);
        }

        Ref<LispNode> node(nc.get_first_node());
        Ref<LispObject> result(node.as<LispObject>());
        return result;
    }

    template <typename Iterable>
    Ref<LispObject> create_vector_object(Iterable &sequence)
    {
        Ref<LispVector> vec(new LispVector());

        vec->reserve(sequence.size());

        for (typename Iterable::iterator it = sequence.begin();
             it != sequence.end();
             ++it)
        {
            vec->push_back(*it);
        }

        Ref<LispObject> result(vec.as<LispObject>());
        return result;
    }

    class NonConstUniversalTraverserTypes
    {
    public:
        typedef Ref<LispObject> RefObject;
        typedef Ref<LispNode> RefNode;
        typedef Ref<LispVector> RefVector;
    };

    class ConstUniversalTraverserTypes
    {
    public:
        typedef CRef<LispObject> RefObject;
        typedef CRef<LispNode> RefNode;
        typedef CRef<LispVector> RefVector;
    };

    template <typename Types>
    class UniversalTraverserT
    {
        UniversalTraverserT &operator=(const UniversalTraverserT &other);
        UniversalTraverserT (const UniversalTraverserT &other);

        typename Types::RefObject sequence;
        bool finished;

        typename Types::RefNode current_node;

        typename Types::RefVector current_vector;
        size_t next_i;
    public:
        static bool can_iterate_over(typename Types::RefObject o)
        {
            std::string typo = type_of(o);

            return (typo == LispNode::type_name)
                || (typo == LispVector::type_name);
        }

    private:
        void initialize(typename Types::RefObject o)
            throw(Ref<LispException>)
        {
            if (o.is_null())
            {
                finished = true;
            }
            else if (o->get_type() == LispNode::type_name)
            {
                current_node = o.template as<LispNode>();
                finished = false;
            }
            else if (o->get_type() == LispVector::type_name)
            {
                current_vector = o.template as<LispVector>();
                next_i = 0;
                if (current_vector->size() == 0)
                {
                    finished = true;
                }
                else
                {
                    finished = false;
                }
            }
            else
            {
                raise_error(LispError::s_wrong_type_argument, "Can only iterate over a list or a vector");
            }
        }

    public:
        UniversalTraverserT(typename Types::RefObject o)
            throw(Ref<LispException>)
        {
            initialize(o);
        }

        bool has_more() const
        {
            return !finished;
        }

        typename Types::RefObject get_next()
        {
            typename Types::RefObject result;

            if (current_vector.is_not_null())
            {
                result = current_vector->at(next_i);
                next_i++;

                if (next_i >= current_vector->size())
                {
                    finished = true;
                }
            }
            else
            {
                result = current_node->car();
                current_node = current_node->cdr();
                if (current_node.is_null())
                {
                    finished = true;
                }
            }

            return result;
        }
    };
    typedef UniversalTraverserT<NonConstUniversalTraverserTypes> UniversalTraverser;
    typedef UniversalTraverserT<ConstUniversalTraverserTypes> CUniversalTraverser;


    std::string as_short_string(CRef<LispObject> o,
        size_t max_elements,
        size_t max_depth)
    {
        if (max_depth == 0)
        {
            return "...";
        }

        if (CUniversalTraverser::can_iterate_over(o))
        {
            std::string typo(type_of(o));
            std::string beginning, ending;
            std::string result;

            if (typo == LispNode::type_name)
            {
                beginning = "(";
                ending = ")";
            }
            else if (typo == LispVector::type_name)
            {
                beginning = "[";
                ending = "]";
            }
            else
            {
                beginning = "(" + typo;
                ending = ")";
            }

            result = beginning;

            CUniversalTraverser it(o);

            for (size_t i = 0;
                 i <= max_elements && it.has_more();
                 i++)
            {
                if (i >= 1)
                {
                    result += " ";
                }

                if (i == max_elements)
                {
                    result += "...";
                }
                else
                {
                    CRef<LispObject> element;
                    element = it.get_next();

                    if (CUniversalTraverser::can_iterate_over(element))
                    {
                        result += as_short_string(element, max_elements, max_depth - 1);
                    }
                    else
                    {
                        result += as_repr_string(element);
                    }
                }
            }

            result += ending;

            return result;
        }
        else
        {
            return as_repr_string(o);
        }

    }



    class LispCallbackTypes
    {
    public:
        typedef Ref<LispObject> (*nativefunc0)(void);
        typedef Ref<LispObject> (*nativefunc1)(Ref<LispNode> args);
        typedef Ref<LispObject> (*nativefunc2)(Scope *caller, Ref<LispNode> args);

        typedef void (*vnativefunc0)(void);
        typedef void (*vnativefunc1)(Ref<LispNode> args);
        typedef void (*vnativefunc2)(Scope *caller, Ref<LispNode> args);
    };


    template <typename TF>
    class LispNativeCallable : public LispCallable
    {

    private:
        TF f;

    public:
        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNativeCallable::type_name;
        }

        LispNativeCallable(TF func)
        {
            f = func;
        }

        virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
            throw(Ref<LispException>)
        {
            args = caller->evaluate_nodes(args);
            return (*f)(caller, args);
        }

    };
    template<typename TF>
    const char *LispNativeCallable<TF>::type_name = "callable";


    // =========================================================
    template <>
    class LispNativeCallable<LispCallbackTypes::nativefunc0> : public LispCallable
    {

    private:
        LispCallbackTypes::nativefunc0 f;

    public:
        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNativeCallable::type_name;
        }

        LispNativeCallable(LispCallbackTypes::nativefunc0 func)
        {
            f = func;
        }

        virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
            throw(Ref<LispException>)
        {
            args = caller->evaluate_nodes(args);
            return f();
        }

    };
    const char *LispNativeCallable<LispCallbackTypes::nativefunc0>::type_name = "callable";

    template <>
    class LispNativeCallable<LispCallbackTypes::nativefunc1> : public LispCallable
    {

    private:
        LispCallbackTypes::nativefunc1 f;

    public:
        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNativeCallable::type_name;
        }

        LispNativeCallable(LispCallbackTypes::nativefunc1 func)
        {
            f = func;
        }

        virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
            throw(Ref<LispException>)
        {
            args = caller->evaluate_nodes(args);
            return f(args);
        }

    };
    const char *LispNativeCallable<LispCallbackTypes::nativefunc1>::type_name = "callable";

    template <>
    class LispNativeCallable<LispCallbackTypes::nativefunc2> : public LispCallable
    {

    private:
        LispCallbackTypes::nativefunc2 f;

    public:
        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNativeCallable::type_name;
        }

        LispNativeCallable(LispCallbackTypes::nativefunc2 func)
        {
            f = func;
        }

        virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
            throw(Ref<LispException>)
        {
            args = caller->evaluate_nodes(args);
            return f(caller, args);
        }

    };
    const char *LispNativeCallable<LispCallbackTypes::nativefunc2>::type_name = "callable";

    template <>
    class LispNativeCallable<LispCallbackTypes::vnativefunc0> : public LispCallable
    {

    private:
        LispCallbackTypes::vnativefunc0 f;

    public:
        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNativeCallable::type_name;
        }

        LispNativeCallable(LispCallbackTypes::vnativefunc0 func)
        {
            f = func;
        }

        virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
            throw(Ref<LispException>)
        {
            args = caller->evaluate_nodes(args);
            f();
            return Ref<LispObject>(NULL);
        }

    };
    const char *LispNativeCallable<LispCallbackTypes::vnativefunc0>::type_name = "callable";

    template <>
    class LispNativeCallable<LispCallbackTypes::vnativefunc1> : public LispCallable
    {

    private:
        LispCallbackTypes::vnativefunc1 f;

    public:
        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNativeCallable::type_name;
        }

        LispNativeCallable(LispCallbackTypes::vnativefunc1 func)
        {
            f = func;
        }

        virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
            throw(Ref<LispException>)
        {
            args = caller->evaluate_nodes(args);
            f(args);
            return Ref<LispObject>(NULL);
        }

    };
    const char *LispNativeCallable<LispCallbackTypes::vnativefunc1>::type_name = "callable";

    template <>
    class LispNativeCallable<LispCallbackTypes::vnativefunc2> : public LispCallable
    {

    private:
        LispCallbackTypes::vnativefunc2 f;

    public:
        static const char *type_name;
        virtual std::string get_type() const
        {
            return LispNativeCallable::type_name;
        }

        LispNativeCallable(LispCallbackTypes::vnativefunc2 func)
        {
            f = func;
        }

        virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
            throw(Ref<LispException>)
        {
            args = caller->evaluate_nodes(args);
            f(caller, args);
            return Ref<LispObject>(NULL);
        }

    };
    const char *LispNativeCallable<LispCallbackTypes::vnativefunc2>::type_name = "callable";
    // =========================================================



    template <typename TF>
    void Scope::set_native_callable(std::string callable_name, TF f)
    {
        LispNativeCallable<TF> *lnc = new LispNativeCallable<TF>(f);
        Ref< LispNativeCallable<TF> > rlnc(lnc);
        Ref<LispObject> o(rlnc.template as<LispObject>());
        set_variable(callable_prefix + callable_name, o);
    }


}

#include "seldefs.hpp"

namespace selin
{
    void Scope::fill_with_builtins()
    {
        builtin::fill_scope(*this);
    }
}

#endif
