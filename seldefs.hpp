// Copyright (c) 2014-2016, Nihat Engin Toklu < http://github.com/engintoklu >
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

#ifndef SELDEFS_HPP
#define SELDEFS_HPP

//#include "selin.hpp"

namespace selin
{
    void unpack1(std::string opname, Ref<LispNode> nodes, Ref<LispObject> &a)
        throw(Ref<LispException>)
    {
        if (nodes.is_null()) goto unpack1_error;
        a = nodes->car();

        nodes = nodes->cdr();
        if (nodes.is_not_null()) goto unpack1_error;

        return;

    unpack1_error:
        raise_error(LispError::s_wrong_number_of_arguments, "'" + opname + "' requires exactly 1 argument");
    }


    void unpack2(std::string opname, Ref<LispNode> nodes,
        Ref<LispObject> &a, Ref<LispObject> &b)
        throw(Ref<LispException>)
    {
        if (nodes.is_null()) goto unpack2_error;
        a = nodes->car();

        nodes = nodes->cdr();
        if (nodes.is_null()) goto unpack2_error;
        b = nodes->car();

        nodes = nodes->cdr();
        if (nodes.is_not_null()) goto unpack2_error;

        return;

    unpack2_error:
        raise_error(LispError::s_wrong_number_of_arguments, "'" + opname + "' requires exactly 2 arguments");
    }


    void unpack3(std::string opname, Ref<LispNode> nodes,
        Ref<LispObject> &a, Ref<LispObject> &b, Ref<LispObject> &c)
        throw(Ref<LispException>)
    {
        if (nodes.is_null()) goto unpack3_error;
        a = nodes->car();

        nodes = nodes->cdr();
        if (nodes.is_null()) goto unpack3_error;
        b = nodes->car();

        nodes = nodes->cdr();
        if (nodes.is_null()) goto unpack3_error;
        c = nodes->car();

        nodes = nodes->cdr();
        if (nodes.is_not_null()) goto unpack3_error;

        return;

    unpack3_error:
        raise_error(LispError::s_wrong_number_of_arguments, "'" + opname + "' requires exactly 3 arguments");
    }

    template<typename T>
    Ref<T> cast_or_complain(Ref<LispObject> o, std::string msg)
        throw(Ref<LispException>)
    {
        if (o.is_null())
        {
            raise_error(LispError::s_wrong_type_argument, msg);
        }

        if (o->get_type() != T::type_name)
        {
            raise_error(LispError::s_wrong_type_argument, msg);
        }

        return o.as<T>();
    }


    double get_number_or_complain(Ref<LispObject> o, std::string msg)
        throw(Ref<LispException>)
    {
        Ref<LispNumber> num;
        num = cast_or_complain<LispNumber>(o, msg);
        return num->get_value();
    }


    size_t get_index_or_complain(Ref<LispObject> o, std::string msg)
        throw(Ref<LispException>)
    {
        return size_t(round_double(get_number_or_complain(o, msg)));
    }


    widestring get_string_or_complain(Ref<LispObject> o, std::string msg)
        throw(Ref<LispException>)
    {
        Ref<LispString> s;
        s = cast_or_complain<LispString>(o, msg);
        return s->get_value();
    }

    std::string get_symbol_or_complain(Ref<LispObject> o, std::string msg)
        throw(Ref<LispException>)
    {
        Ref<LispSymbol> s;
        s = cast_or_complain<LispSymbol>(o, msg);
        return s->get_value();
    }


    namespace builtin
    {
        class Quote : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                if (args->cdr().is_not_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "Too many arguments for 'quote'");
                }

                return args->car();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(quote x)" << std::endl;
                result << "  returns the argument x without evaluating it";

                return result.str();
            }
        };


        class Quit : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                if (args.is_not_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "'quit' expects no arguments");
                }

                //exit(0);
                Ref<LispRequest> req(new LispRequest(LispRequest::req_quit));
                Ref<LispException> ex(req.as<LispException>());
                throw ex;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(quit)" << std::endl;
                result << "  quits the interpreter";

                return result.str();
            }
        };


        const int list_operation_car = 0;
        const int list_operation_cdr = 1;
        const int list_operation_length = 2;

        class ListOperation : public LispCallable
        {
        public:
            int opcode;

            std::string get_operation_name() const
            {
                if (opcode == list_operation_car) return "car";
                if (opcode == list_operation_cdr) return "cdr";
                if (opcode == list_operation_length) return "length";
                return "";
            }

            ListOperation(int _opcode)
            {
                opcode = _opcode;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                if (args.is_null() || args->cdr().is_not_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "'" + get_operation_name() + "' expects exactly 1 argument");
                }

                args = caller->evaluate_nodes(args);

                std::string typ;
                typ = type_of(args->car());

                if (typ != LispNode::type_name && typ != "nil")
                {
                    raise_error(LispError::s_wrong_type_argument, "'" + get_operation_name() + "' expects a list as argument");
                }

                Ref<LispNode> node;
                node = args->car().as<LispNode>();

                if (opcode == list_operation_length)
                {
                    int count = 0;
                    Ref<LispNode> current_node;
                    for (current_node = node;
                        current_node.is_not_null();
                        current_node = current_node->cdr())
                    {
                        count++;
                    }

                    return objrefnew<LispNumber>(count);
                }

                if (args->car().is_null())
                {
                    return Ref<LispObject>(NULL);
                }

                if (opcode == list_operation_car) return node->car();
                if (opcode == list_operation_cdr) return node->cdr().as<LispObject>();

                raise_error("error", "Internal error: Misconfigured list operation (unknown opcode)");
                return Ref<LispObject>(NULL);
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(car x) returns the first element of the list x" << std::endl;
                result << "(cdr x) ignores the first element of the list x and returns the rest" << std::endl;
                //result << "(length x) returns the length of the list x" << std::endl;
                result << std::endl;
                result << "  For example, let us assume that we have this list named x, by executing the following:" << std::endl;
                result << "    (setq x (list 1 4 7))   ;create a list (1 4 7) and assign it to x" << std::endl;
                result << "  which is represented in the memory as:" << std::endl;
                result << std::endl;
                result << "   variable x" << std::endl;
                result << "        |" << std::endl;
                result << "    ____v____" << std::endl;
                result << "    [ 1 | *-]--> [ 4 | *-]--> [ 7 | nil ]" << std::endl;
                result << "      ^   ^" << std::endl;
                result << "      |   |" << std::endl;
                result << "      |   pointer to the next node" << std::endl;
                result << "      value stored by the node" << std::endl;
                result << std::endl;
                result << "  in this example," << std::endl;
                result << "    (car x) would return 1," << std::endl;
                result << "    (cdr x) would return the list beginning from the next node of x: (4 7).";
                //result << "    (length x) would return 3.";
                return result.str();
            }

        };

        class NumericOperation : public LispCallable
        {
            char opr;

        public:
            NumericOperation(char opr_code): opr(opr_code) {}

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                args = caller->evaluate_nodes(args);

                Ref<LispObject> o_a, o_b;
                double a, b;
                double result;

                unpack2("<BasicNumericOperation>", args, o_a, o_b);

                a = as_number(o_a);
                b = as_number(o_b);

                if (opr == '+')
                {
                    result = a + b;
                }
                else if (opr == '-')
                {
                    result = a - b;
                }
                else if (opr == '*')
                {
                    result = a * b;
                }
                else if (opr == '/')
                {
                    result = a / b;
                }
                else if (opr == '&')
                {
                    result = nearest_bitwise_calc_int(a) & nearest_bitwise_calc_int(b);
                }
                else if (opr == '|')
                {
                    result = nearest_bitwise_calc_int(a) | nearest_bitwise_calc_int(b);
                }
                else
                {
                    raise_error("error", "Internal error: Misconfigured numeric operation (unknown opcode)");
                    result = 0.0;
                }
                
                return objrefnew<LispNumber>(result);
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                std::string oprname;
                std::string oprdesc;

                if (opr == '|')
                {
                    oprname = "logior";
                    oprdesc = "bitwise OR";
                }
                else if (opr == '&')
                {
                    oprname = "logand";
                    oprdesc = "bitwise AND";
                }
                else
                {
                    char oprc[2];
                    oprc[1] = '\0';
                    oprc[0] = opr;
                    oprname = oprc;
                    oprdesc = oprc;
                }

                result << to_string() << std::endl;
                result << "(" << opr << " a b)" << std::endl;
                result << "  applies the " << oprdesc << " operation on the 2 numeric arguments, then returns the result";

                return result.str();
            }

        };

       class NotOperator : public LispCallable
       {
       public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a;
                unpack1("not", args, a);

                a = caller->evaluate(a);

                if (is_true(a)) return Ref<LispObject>(NULL);
                return LispSymbol::truth();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << std::endl << "(not x)";
                result << "  returns non-nil (i.e. true) if x is nil (i.e. if x is not true)";
                return result.str();
            }
       };


        template <typename InnerT, typename OuterT>
        class ValueComparison : public LispCallable
        {
        private:
            std::string operation;
            std::string expected_type;

        public:
            ValueComparison(std::string op, std::string typ)
            {
                operation = op;
                // op must be:
                //   =  /=  >  <  >=  <=

                expected_type = typ;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a, b;

                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "'" + operation + "' must have at least one argument");
                }

                args = caller->evaluate_nodes(args);

                a = args->car();
                //a = caller->evaluate(args->car());
                args = args->cdr();

                for (; args.is_not_null(); args = args->cdr())
                {
                    b = args->car();
                    //b = caller->evaluate(args->car());

                    Ref<OuterT> na, nb;

                    if (!((type_of(a) == expected_type)
                        && (type_of(b) == expected_type)))
                    {
                        raise_error(LispError::s_wrong_type_argument, "Type mismatch in " + operation);
                    }

                    na = a.as<OuterT>();
                    nb = b.as<OuterT>();

                    InnerT va, vb;
                    va = na->get_value();
                    vb = nb->get_value();

                    if (operation == "=")
                    {
                        if (va != vb) return Ref<LispObject>(NULL);
                    }
                    else if (operation == "/=")
                    {
                        if (va == vb)
                        {
                            return Ref<LispObject>(NULL);
                        }
                        else
                        {
                            if (args->cdr().is_not_null())
                            {
                                raise_error(LispError::s_wrong_number_of_arguments, "/= expects 2 arguments");
                            }
                            else
                            {
                                return LispSymbol::truth();
                            }
                        }
                    }
                    else if (operation == ">")
                    {
                        if (va <= vb) return Ref<LispObject>(NULL);
                    }
                    else if (operation == "<")
                    {
                        if (va >= vb) return Ref<LispObject>(NULL);
                    }
                    else if (operation == ">=")
                    {
                        if (va < vb) return Ref<LispObject>(NULL);
                    }
                    else if (operation == "<=")
                    {
                        if (va > vb) return Ref<LispObject>(NULL);
                    }

                    a = b;
                }

                return LispSymbol::truth();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(";
                if (expected_type != "number")
                {
                    result << expected_type;
                }
                result << operation;
                result << " arg1 arg2 arg3 ...)" << std::endl;
                result << "  returns non-nil (i.e. true) if arg1 " << operation << " arg2, and arg2 " << operation << " arg3, and so on.";
                return result.str();
            }
        };


        class EqCheck : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a, b;
                std::string ta, tb;
                bool valuecheck = false;

                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "eq: at least 2 arguments are expected");
                }

                a = caller->evaluate(args->car());
                ta = type_of(a);
                args = args->cdr();

                if (ta == LispNumber::type_name
                    || ta == LispSymbol::type_name)
                {
                    valuecheck = true;
                }

                for (; args.is_not_null(); args = args->cdr())
                {
                    b = caller->evaluate(args->car());
                    tb = type_of(b);

                    if (ta != tb)
                    {
                        return Ref<LispObject>(NULL);
                    }

                    if (valuecheck)
                    {
                        if (!(LispObject::equal(a, b)))
                        {
                            return Ref<LispObject>(NULL);
                        }
                    }
                    else
                    {
                        // identity check
                        if (a.get_pointer() != b.get_pointer())
                        {
                            return Ref<LispObject>(NULL);
                        }
                    }
                }

                return LispSymbol::truth();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(eq a b c ...)" << std::endl;
                result << "  returns non-nil (true) if the arguments are the same." << std::endl;
                result << "  For numbers and symbols, being the same means that they have the same data/value." << std::endl;
                result << "  For other objects, being the same means that they are the same object"
                       << std::endl << "  (i.e. same address in memory).";
                return result.str();
            }
        };


        class EqualityCheck : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a, b;

                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "equal: at least 2 arguments are expected");
                }

                args = caller->evaluate_nodes(args);

                //a = caller->evaluate(args->car());
                a = args->car();
                args = args->cdr();

                for (; args.is_not_null(); args = args->cdr())
                {
                    //b = caller->evaluate(args->car());
                    b = args->car();

                    if (!(LispObject::equal(a, b)))
                    {
                        return Ref<LispObject>(NULL);
                    }
                }

                return LispSymbol::truth();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(equal a b c ...)" << std::endl;
                result << "  returns non-nil (true) if the arguments have the same value";
                return result.str();
            }

        };


        class ConsOperation : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> arg1, arg2;
                Ref<LispNode> result;

                args = caller->evaluate_nodes(args);

                unpack2("cons", args, arg1, arg2);

                std::string typ;
                typ = type_of(arg2);

                if (typ == "nil" || typ == LispNode::type_name)
                {
                    result = refnew<LispNode>();
                    result->setcar(arg1);
                    result->setcdr(arg2.as<LispNode>());
                }
                else
                {
                    raise_error(LispError::s_wrong_type_argument, "'cons' requires a list or nil as 2nd argument");
                }

                return result.as<LispObject>();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(cons value the-list)" << std::endl;
                result << "  returns the-list with value inserted into its beginning" << std::endl;
                return result.str();
            }

        };


        class CondStatement : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispNode> current_node;
                Ref<LispObject> result(NULL);

                for (current_node = args;
                    current_node.is_not_null();
                    current_node = current_node->cdr())
                {
                    Ref<LispObject> condition_action;
                    condition_action = current_node->car();

                    if (type_of(condition_action) != LispNode::type_name)
                    {
                        raise_error(LispError::s_wrong_type_argument, "'cond' requires list arguments, like (condition action1 action2 ...)");
                    }

                    Ref<LispNode> sublist;
                    sublist = condition_action.as<LispNode>();

                    if (is_true(caller->evaluate(sublist->car())))
                    {
                        Ref<LispNode> action;
                        action = sublist->cdr();

                        for (;action.is_not_null(); action = action->cdr())
                        {
                            result = caller->evaluate(action->car());
                        }

                        break;
                    }
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(cond (condition1 action1a action1b ...)" << std::endl;
                result << "      (condition2 action2a action2b ...)" << std::endl;
                result << "        ... )" << std::endl;
                result << "  executes action1a, action1b, ... if condition1 holds," << std::endl;
                result << "  otherwise, executes action2a, action2b, ... if condition2 holds," << std::endl;
                result << "  and so on." << std::endl;
                result << "    EXAMPLE:" << std::endl;
                result << "      (cond ((= a b) (print \"a and b are equal\"))" << std::endl;
                result << "            ((> a b) (print \"a is greater than b\"))" << std::endl;
                result << "            ((< a b) (print \"a is less than b\")))";
                return result.str();
            }

        };


        class WhileStatement : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "'while' requires at least 1 argument");
                }

                Ref<LispObject> condition, result;

                result = Ref<LispObject>(NULL);

                while (true)
                {
                    condition = caller->evaluate(args->car());

                    if (!is_true(condition)) break;

                    Ref<LispNode> action;

                    for (action = args->cdr();
                        action.is_not_null();
                        action = action->cdr())
                    {
                        result = caller->evaluate(action->car());
                    }
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(while condition action1 action2 ...)" << std::endl;
                result << "  executes actions repeatedly as long as condition holds" << std::endl;
                result << "    EXAMPLE:" << std::endl;
                result << "      (let ((x 0))            ; initialize x as 0" << std::endl;
                result << "        (while (< x 10)       ; as long as x<10" << std::endl;
                result << "          (print x)           ; print x into the screen" << std::endl;
                result << "          (setq x (+ x 1))))  ; increase x by 1";
                return result.str();
            }

        };


        class Setter : public LispCallable
        {
            bool evaluate_sym;

        public:
            Setter(bool eval_sym)
            {
                evaluate_sym = eval_sym;
            }

            Setter()
            {
                evaluate_sym = true;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispNode> current_arg;

                Ref<LispSymbol> sym;
                Ref<LispObject> val;

                current_arg = args;
                while (current_arg.is_not_null())
                {
                    Ref<LispObject> o_sym;
                    o_sym = current_arg->car();

                    if (evaluate_sym)
                    {
                        o_sym = caller->evaluate(o_sym);
                    }

                    if (type_of(o_sym) != LispSymbol::type_name)
                    {
                        raise_error(LispError::s_wrong_type_argument, "set: expected a symbol but got a "
                            + type_of(o_sym));
                    }

                    sym = o_sym.as<LispSymbol>();

                    current_arg = current_arg->cdr();
                    if (current_arg.is_null())
                    {
                        raise_error(LispError::s_value_error, "Unexpected end of list in 'setq'");
                    }

                    val = caller->evaluate(current_arg->car());

                    caller->set_variable(sym->get_value(), val);

                    current_arg = current_arg->cdr();
                }

                return val;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(set 'x value)" << std::endl;
                result << "  or" << std::endl;
                result << "(setq x value)" << std::endl;
                result << "  assigns the value to the variable x." << std::endl;
                result << "  if there is a local variable called x in the current scope," << std::endl;
                result << "  the value is assigned to that variable." << std::endl;
                result << "  otherwise, a variable x is searched in the parent scopes." << std::endl;
                result << "  if a variable named x is not found, a global variable is created," << std::endl;
                result << "  and initialized with the value.";
                return result.str();
            }

        };


        class SetCar : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_cell, o_newcar;

                unpack2("setcar", args, o_cell, o_newcar);

                o_cell = caller->evaluate(o_cell);
                o_newcar = caller->evaluate(o_newcar);

                if (type_of(o_cell) != LispNode::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "'setcar' expects a 'cons' object as its first argument");
                }

                Ref<LispNode> cell;
                cell = o_cell.as<LispNode>();

                cell->setcar(o_newcar);

                return o_newcar;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(setcar listnode value)" << std::endl;
                result << "  or" << std::endl;
                result << "(set-car listnode value)" << std::endl;
                result << "  assigns value to the listnode." << std::endl;
                result << std::endl;
                result << "  For example, let us assume that we have this list named x, by executing the following:" << std::endl;
                result << "    (setq x (list 1 4 7))   ;create a list (1 4 7) and assign it to x" << std::endl;
                result << "  which is represented in the memory as:" << std::endl;
                result << std::endl;
                result << "   variable x" << std::endl;
                result << "        |" << std::endl;
                result << "    ____v____" << std::endl;
                result << "    [ 1 | *-]--> [ 4 | *-]--> [ 7 | nil ]" << std::endl;
                result << "      ^   ^" << std::endl;
                result << "      |   |" << std::endl;
                result << "      |   pointer to the next node" << std::endl;
                result << "      value stored by the node" << std::endl;
                result << std::endl;
                result << "  if we execute (setcar x 8), then the first node [ 1 | * ] is modified as [ 8 | * ]" << std::endl;
                result << "  therefore, we now have (8 4 7)";
                return result.str();
            }
        };


        class SetCdr : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_cell, o_newcdr;

                unpack2("setcar", args, o_cell, o_newcdr);

                o_cell = caller->evaluate(o_cell);
                o_newcdr = caller->evaluate(o_newcdr);

                if (type_of(o_cell) != LispNode::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "'setcdr' expects a 'cons' object as its first argument");
                }

                if ((o_newcdr.is_not_null()) && (type_of(o_newcdr) != LispNode::type_name))
                {
                    raise_error(LispError::s_wrong_type_argument, "'setcdr' expects a 'cons' object or nil as its second argument");
                }

                Ref<LispNode> cell, newcdr;
                cell = o_cell.as<LispNode>();
                newcdr = o_newcdr.as<LispNode>();

                cell->setcdr(newcdr);

                return o_newcdr;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(setcdr the-list the-value)" << std::endl;
                result << "  or" << std::endl;
                result << "(set-cdr the-list the-value)" << std::endl;
                result << "  sets the first element's value of the-list as the-value" << std::endl;
                result << std::endl;
                result << "  For example, let us assume that we have this list named x, by executing the following:" << std::endl;
                result << "    (setq x (list 1 4 7))   ;create a list (1 4 7) and assign it to x" << std::endl;
                result << "  which is represented in the memory as:" << std::endl;
                result << std::endl;
                result << "   variable x" << std::endl;
                result << "        |" << std::endl;
                result << "    ____v____" << std::endl;
                result << "    [ 1 | *-]--> [ 4 | *-]--> [ 7 | nil ]" << std::endl;
                result << "      ^   ^" << std::endl;
                result << "      |   |" << std::endl;
                result << "      |   pointer to the next node" << std::endl;
                result << "      value stored by the node" << std::endl;
                result << std::endl;
                result << "  Let us also assume that we have this list named y:" << std::endl;
                result << "    (setq y (list 5 6))   ;create a list (5 6) and assign it to y" << std::endl;
                result << "  which is represented in the memory as:" << std::endl;
                result << std::endl;
                result << "   variable y" << std::endl;
                result << "        |" << std::endl;
                result << "    ____v____" << std::endl;
                result << "    [ 5 | *-]--> [ 6 | nil ]" << std::endl;
                result << std::endl;
                result << "  If we execute (setcdr x y), then the node referenced by the variable x is modified as:" << std::endl;
                result << std::endl;
                result << "   variable x" << std::endl;
                result << "        |" << std::endl;
                result << "    ____v____" << std::endl;
                result << "    [ 1 | * ]" << std::endl;
                result << "          |" << std::endl;
                result << "          |" << std::endl;
                result << "          |      variable y" << std::endl;
                result << "          |           |" << std::endl;
                result << "          |       ____v____" << std::endl;
                result << "          ------> [ 5 | *-]--> [ 6 | nil ]" << std::endl;
                result << std::endl;
                result << "  Therefore, x is now (1 5 6)";
                return result.str();
            }
        };


        class LetStatement : public LispCallable
        {
        public:
            bool starred;

            LetStatement()
            {
                starred = false;
            }

            LetStatement(bool starred_let)
            {
                starred = starred_let;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispNode> locals;
                Ref<LispNode> actions;
                Scope sc(caller);

                if (args.is_null() || type_of(args->car()) != LispNode::type_name)
                {
                    raise_error("error", "'let' expects a list of local variables");
                }

                locals = args->car().as<LispNode>();
                actions = args->cdr();

                for (; locals.is_not_null(); locals = locals->cdr())
                {
                    Ref<LispObject> o;
                    o = locals->car();

                    if (type_of(o) != LispNode::type_name)
                    {
                        raise_error("error", "Each member of the first argument of 'let' must be a list with 2 elements");
                    }

                    Ref<LispNode> pair;
                    pair = o.as<LispNode>();

                    Ref<LispObject> varname, varvalue;

                    unpack2("(variable value) sublist of let statement", pair, varname, varvalue);

                    std::string s_varname;

                    if (type_of(varname) != LispSymbol::type_name)
                    {
                        raise_error("error", "A symbol is expected as the first element of the (variable value) sublist in 'let'");
                    }
                    s_varname = varname.as<LispSymbol>()->get_value();

                    if (starred)
                    {
                        varvalue = sc.evaluate(varvalue);
                    }
                    else
                    {
                        varvalue = caller->evaluate(varvalue);
                    }

                    sc.set_local_variable(s_varname, varvalue);
                }

                Ref<LispObject> result;
                for (; actions.is_not_null(); actions = actions->cdr())
                {
                    result = sc.evaluate(actions->car());
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(let ((var1 value1) (var2 value2) ... ) action1 action2 ...)" << std::endl;
                result << "  creates a scope in which var1=value1 and var2=value2, and so on, and then executes the specified actions within this scope. The result of the let operation is the result of the last action.";
                result << std::endl;
                result << "EXAMPLE:" << std::endl;
                result << "  (let ((a 3) (b 4))" << std::endl;
                result << "    (print a)    ; prints 3" << std::endl;
                result << "    (print b)    ; prints 4" << std::endl;
                result << "    (+ a b))     ; returns 7";
                return result.str();
            }
        };


        class FunctionAccessor : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                if (args.is_null() || args->cdr().is_not_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "'function' expects exactly one argument");
                }


                Ref<LispObject> funcname;
                Ref<LispSymbol> funcsym;

                funcname = args->car();

                if (type_of(funcname) != LispSymbol::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "'function' expects a symbol as parameter");
                }

                funcsym = funcname.as<LispSymbol>();

                Variable *v;
                v = caller->get_callable(funcsym->get_value());

                if (v == NULL)
                {
                    raise_error(LispError::s_void_function, "Undefined function: " + funcsym->get_value());
                }

                return v->get_value();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(function myfunc)" << std::endl;
                result << "  returns the function object named myfunc";
                return result.str();
            }
        };


        class FunctionCaller : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "funcall: expected at least one argument");
                }

                Ref<LispObject> funcobj;
                Ref<LispCallable> func;

                funcobj = caller->evaluate(args->car());

                if (type_of(funcobj) == LispCallable::type_name)
                {
                    func = funcobj.as<LispCallable>();
                }
                else if (type_of(funcobj) == LispSymbol::type_name)
                {
                    Variable *v;
                    Ref<LispSymbol> sym;

                    sym = funcobj.as<LispSymbol>();
                    v = caller->get_callable(sym->get_value());
                    if (!v)
                    {
                        raise_error(LispError::s_void_function, "funcall: could not find a callable object with the name " + sym->get_value());
                    }

                    if (!(v->is_callable()))
                    {
                        raise_error(LispError::s_wrong_type_argument, "funcall: the symbol does not point to a callable object");
                    }

                    func = v->get_value().as<LispCallable>();
                }
                else
                {
                    raise_error(LispError::s_wrong_type_argument, "funcall: expected a callable or a symbol as argument");
                }

                return func->execute(caller, args->cdr());
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(funcall f arg1 arg2 ...)" << std::endl;
                result << "  calls the function object referenced by the variable f, with the specified arguments";
                return result.str();
            }
        };


        class FunctionApplier : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "apply: expected at least one argument");
                }

                Ref<LispObject> funcobj;
                Ref<LispCallable> func;

                funcobj = caller->evaluate(args->car());

                if (type_of(funcobj) == LispCallable::type_name)
                {
                    func = funcobj.as<LispCallable>();
                }
                else if (type_of(funcobj) == LispSymbol::type_name)
                {
                    Variable *v;
                    Ref<LispSymbol> sym;

                    sym = funcobj.as<LispSymbol>();
                    v = caller->get_callable(sym->get_value());
                    if (!v)
                    {
                        raise_error(LispError::s_void_function, "apply: could not find a callable object with the name " + sym->get_value());
                    }

                    if (!(v->is_callable()))
                    {
                        raise_error(LispError::s_wrong_type_argument, "apply: the symbol does not point to a callable object");
                    }

                    func = v->get_value().as<LispCallable>();
                }
                else
                {
                    raise_error(LispError::s_wrong_type_argument, "apply: expected a callable or a symbol as argument");
                }

                args = args->cdr();
                Ref<LispObject> o_arglist;
                Ref<LispNode> arglist;

                if (args.is_not_null())
                {
                    o_arglist = args->car();
                    o_arglist = caller->evaluate(o_arglist);
                    arglist = cast_or_complain<LispNode>(o_arglist, "apply: the second argument was expected to be a list, but something else was received");
                }

                // make a new list: quoted_arglist,
                // in which each argument in arglist exists quoted.
                NodesCreator quoted_arglist;
                for (Ref<LispNode> current_arg = arglist;
                     current_arg.is_not_null();
                     current_arg = current_arg->cdr())
                {
                    Ref<LispObject> o;
                    o = current_arg->car();
                    o = quote_object(o);
                    quoted_arglist.push_back(o);
                }

                // arglist was already evaluated here.
                // sending quoted_arglist's first node here
                // prevents those arguments from being double-evaluated.
                return func->execute(caller, quoted_arglist.get_first_node());
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(apply f arglist)" << std::endl;
                result << "  calls the function object referenced by the variable f, with the arguments stored within the list arglist.";
                return result.str();
            }
        };



        class AndOperator : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> v;

                for (; args.is_not_null(); args = args->cdr())
                {
                    v = caller->evaluate(args->car());

                    if (v.is_null())
                    {
                        return Ref<LispObject>(NULL);
                    }
                }

                return v;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(and a b c ...)" << std::endl;
                result << "  returns non-nil (i.e. true) if all the arguments are non-nil";
                return result.str();
            }
        };


        class OrOperator : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> v;

                for (; args.is_not_null(); args = args->cdr())
                {
                    v = caller->evaluate(args->car());

                    if (is_true(v))
                    {
                        return v;
                    }
                }

                return Ref<LispObject>(NULL);
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(or a b c ...)" << std::endl;
                result << "  returns non-nil (i.e. true) if at least one of the arguments is non-nil";
                return result.str();
            }
        };


        class Concatenator : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> result;
                Ref<LispObject> arg;
                std::string concattype = "";

                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "'concatenate' expects at least one argument.");
                }

                arg = caller->evaluate(args->car());
                if ((arg.is_null()) || (arg->get_type() != "symbol"))
                {
                    raise_error(LispError::s_wrong_type_argument, "'concatenate' expects a symbol as its first argument (One of these: 'list 'vector 'string).");
                }

                concattype = arg.as<LispSymbol>()->to_string();

                if (concattype == "list" || concattype == "vector")
                {
                    std::list< Ref<LispObject> > all_objects;

                    args = args->cdr();
                    for (; args.is_not_null(); args = args->cdr())
                    {
                        arg = caller->evaluate(args->car());

                        if (arg.is_null())
                        {
                            // ignore
                        }
                        else if (arg->get_type() == "cons" || arg->get_type() == "vector")
                        {
                            UniversalTraverser it(arg);
                            while (it.has_more())
                            {
                                all_objects.push_back(it.get_next());
                            }
                        }
                        else
                        {
                            raise_error(LispError::s_wrong_type_argument, "'concatenate' received an argument of unexpected type");
                        }

                        if (concattype == "list")
                        {
                            NodesCreator creator;
                            for (std::list< Ref<LispObject> >::iterator it = all_objects.begin();
                                 it != all_objects.end();
                                 it++)
                            {
                                creator.push_back(*it);
                            }

                            result = creator.get_first_node().as<LispObject>();
                        }
                        else if (concattype == "vector")
                        {
                            Ref< LispVector > vresult;
                            vresult = refnew<LispVector>();

                            vresult->reserve(all_objects.size());
                            for (std::list< Ref<LispObject> >::iterator it = all_objects.begin();
                                 it != all_objects.end();
                                 it++)
                            {
                                vresult->push_back(*it);
                            }

                            result = vresult.as<LispObject>();
                        }
                    }
                }
                else if (concattype == "string")
                {
                    widestring::character k[2];
                    k[1] = '\0';
                    StringBuilder sb;

                    args = args->cdr();
                    for (; args.is_not_null(); args = args->cdr())
                    {
                        arg = caller->evaluate(args->car());
                        if (type_of(arg) == LispNumber::type_name)
                        {
                            Ref<LispNumber> n;
                            n = arg.as<LispNumber>();
                            k[0] = widestring::character(round_double(n->get_value()));
                            sb.add(k);
                        }
                        else
                        {
                            sb.add(as_string(arg));
                        }
                    }

                    widestring ws(sb.str());

                    result = objrefnew<LispString>(ws);
                }
                else
                {
                    raise_error(LispError::s_wrong_type_argument, "'concatenate' expects a symbol as its first argument (One of these: 'list 'vector 'string).");
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(concatenate 'list a b c ...)" << std::endl;
                result << "(concatenate 'string a b c ...)" << std::endl;
                result << "(concatenate 'vector a b c ...)" << std::endl;
                result << "  returns the concatenations of the objects specified in the arguments a b c, ... The data type is specified in the first argument." << std::endl;
                result << "  This concatenation operation is done by copying, therefore, the original objects are not modified.";
                return result.str();
            }
        };


        class TypeOf : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o;
                unpack1("type-of", args, o);

                o = caller->evaluate(o);

                if (o.is_null())
                {
                    return Ref<LispObject>(NULL);
                }
                else
                {
                    return objrefnew<LispSymbol>(type_of(o));
                }
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(type-of a)" << std::endl;
                result << "  returns the data type name of a, as a symbol";
                return result.str();
            }
        };


        class CustomCallableDefinition : public LispCallable
        {
        public:
            bool evaluate_arguments;
            bool nameless;

            CustomCallableDefinition(bool eval_args)
            {
                evaluate_arguments = eval_args;
                nameless = false;
            }

            CustomCallableDefinition(bool eval_args, bool nameless_defn)
            {
                evaluate_arguments = eval_args;
                nameless = nameless_defn;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_name;
                Ref<LispObject> o_arguments;
                Ref<LispNode> body;

                Ref<LispCustomCallable> custom(new LispCustomCallable());
                custom->evaluate_arguments = evaluate_arguments;

                custom->run_on_global_scope = false;
                Variable *myvar = caller->get_variable(scoping_configuration_variable_name);
                if (myvar != NULL && myvar->get_value().is_not_null())
                {
                    if (myvar->get_value()->to_string() == "global")
                    {
                        custom->run_on_global_scope = evaluate_arguments;
                    }
                }

                std::string name;

                if (nameless)
                {
                    name = "<lambda>";
                }
                else
                {
                    if (args.is_null())
                    {
                        raise_error("error", "Missing the name, arguments and the body of the callable");
                    }

                    o_name = args->car();

                    if (type_of(o_name) != LispSymbol::type_name)
                    {
                        raise_error("error", "Callable name should be specified as a symbol");
                    }
                    custom->name = o_name.as<LispSymbol>()->get_value();

                    args = args->cdr();
                }

                if (args.is_null())
                {
                    raise_error("error", "Missing the arguments and the body of the callable");
                }

                o_arguments = args->car();

                if (o_arguments.is_not_null() && type_of(o_arguments) != LispNode::type_name)
                {
                    raise_error("error", "Arguments should be specified in a list");
                }

                Ref<LispNode> arguments;
                arguments = o_arguments.as<LispNode>();

                for (; arguments.is_not_null(); arguments = arguments->cdr())
                {
                    Ref<LispObject> a;
                    a = arguments->car();

                    if (type_of(a) != LispSymbol::type_name)
                    {
                        raise_error("error", "Encountered a non-symbol within the arguments list");
                    }

                    custom->arguments.push_back(a.as<LispSymbol>()->get_value());
                }

                args = args->cdr();
                custom->actions = args;

                Ref<LispObject> result;
                result = custom.as<LispObject>();

                if (!nameless)
                {
                    caller->set_callable(custom->name, result);
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(defun f (a b c) action1 action2 ...)" << std::endl;
                result << "  defines a function f, taking the arguments a b c, performing the specified actions." << std::endl;
                result << "(lambda (a b c) action1 action2 ...)" << std::endl;
                result << "  creates and returns a nameless function, taking the arguments a b c, performing the specified actions." << std::endl;
                result << "(defext e (a b c) action1 action2 ...)" << std::endl;
                result << "  defines a language-extension e, taking the arguments a b c, performing the specified actions. A language-extension is similar to function, but it does not evaluate its arguments. For example, assuming that we have a function f, if we execute (f (+ 1 2) 7), the arguments sent to f would be 3 and 7. If, assuming that we have a language-extension e, we execute (e (+ 1 2) 7), the arguments sent to e would be (+ 1 2) and 7. Evaluation of the arguments in a language-extension can be done explicitly by executing (eval x), assuming that the argument name is x." << std::endl;
                result << "  SCOPING RULES:" << std::endl;
                result << "    If the global setting variable " << scoping_configuration_variable_name << " is set as 'global , defun and lambda commands create functions with their parent scopes set as the global namespace. If " << scoping_configuration_variable_name << " is set as 'dynamic , the functions are created with their parent scopes set dynamically as the scope of the caller. The parent scope of a language-extension is always the caller's scope, regardless of the value of " << scoping_configuration_variable_name;
                return result.str();
            }

        };


        class Evaluator : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o;
                unpack1("eval", args, o);

                o = caller->evaluate(o);

                return caller->evaluate(o);
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(eval x)" << std::endl;
                result << "  evaluates the list x and returns its result";
                return result.str();
            }
        };


        class ListConstructor : public LispCallable
        {
        public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                return caller->evaluate_nodes(args).as<LispObject>();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(list a b c)" << std::endl;
                result << "  creates a list out of the arguments a b c ..." << std::endl;
                result << "  EXAMPLE:" << std::endl;
                result << "  Let us assume that we have this list named x, by executing the following:" << std::endl;
                result << "    (setq x (list 1 4 7))   ;create a list (1 4 7) and assign it to x" << std::endl;
                result << "  The data pointed by x would be represented in the memory as:" << std::endl;
                result << std::endl;
                result << "   variable x" << std::endl;
                result << "        |" << std::endl;
                result << "    ____v____" << std::endl;
                result << "    [ 1 | *-]--> [ 4 | *-]--> [ 7 | nil ]" << std::endl;
                result << "      ^   ^" << std::endl;
                result << "      |   |" << std::endl;
                result << "      |   pointer to the next node" << std::endl;
                result << "      value stored by the node" << std::endl;
                result << std::endl;
                result << "  Each node in this list (including the one pointed at by the variable x) is called a cons object";

                return result.str();
            }
        };

        class NumberToString : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_num;
                unpack1("number-to-string", args, o_num);
                o_num = caller->evaluate(o_num);

                if (type_of(o_num) != LispNumber::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "'number-to-string' expects a number as its argument");
                }

                Ref<LispNumber> num;
                num = o_num.as<LispNumber>();

                std::stringstream ss;
                ss << num->get_value();

                std::string s;
                ss >> s;

                Ref<LispString> result;
                result = refnew<LispString>(s);

                Ref<LispObject> o_result(result.as<LispObject>());

                return o_result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(number-to-string x)" << std::endl;
                result << "  takes a number x, converts it into string and returns the result";
                return result.str();
            }
        };


        class StringToNumber : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_str;
                unpack1("string-to-number", args, o_str);
                o_str = caller->evaluate(o_str);

                if (type_of(o_str) != LispString::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "'string-to-number' expects a string as its argument");
                }

                Ref<LispString> str;
                str = o_str.as<LispString>();

                std::string strnumber(str->get_value().to_string());
                double nn = as_number(strnumber);

                Ref<LispNumber> result(new LispNumber(nn));
                Ref<LispObject> o_result(result.as<LispObject>());

                return o_result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(string-to-number x)" << std::endl;
                result << "  takes a string x, converts it into number and returns the result";
                return result.str();
            }
        };


        class SymbolToString : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_sym;
                unpack1("symbol-to-string", args, o_sym);
                o_sym = caller->evaluate(o_sym);

                if (type_of(o_sym) != LispSymbol::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "'symbol-to-string' expects a symbol as its argument");
                }

                Ref<LispSymbol> sym;
                sym = o_sym.as<LispSymbol>();

                std::string symvalue(sym->get_value());

                Ref<LispString> result(new LispString(symvalue));
                Ref<LispObject> o_result(result.as<LispObject>());

                return o_result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(symbol-to-string x)" << std::endl;
                result << "  takes a symbol x, converts it into string and returns the result";
                return result.str();
            }
        };


        class StringToSymbol : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_str;
                unpack1("string-to-symbol", args, o_str);
                o_str = caller->evaluate(o_str);

                if (type_of(o_str) != LispString::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "'string-to-symbol' expects a string as its argument");
                }

                Ref<LispString> str;
                str = o_str.as<LispString>();

                std::string symstr(str->get_value().to_string());

                Ref<LispSymbol> result(new LispSymbol(symstr));
                Ref<LispObject> o_result(result.as<LispObject>());

                return o_result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(string-to-symbol x)" << std::endl;
                result << "  takes a symbol x, converts it into symbol and returns the result";
                return result.str();
            }
        };


        class VectorConstructor : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> v;
                std::list< Ref<LispObject> > objects;
                Ref<LispVector> result(new LispVector());

                for (; args.is_not_null(); args = args->cdr())
                {
                    v = caller->evaluate(args->car());

                    objects.push_back(v);
                }

                result->resize(objects.size());

                size_t i = 0;
                for (std::list< Ref<LispObject> >::iterator it = objects.begin();
                     it != objects.end();
                     it++)
                {
                    result->at(i) = *it;
                    i++;
                }

                return result.as<LispObject>();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(vector arg1 arg2 arg3 ...)" << std::endl;
                result << "  creates and returns a vector object containing the specified arguments";
                return result.str();
            }
        };


        class LengthCalculator : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> sequence;
                unpack1("length", args, sequence);
                sequence = caller->evaluate(sequence);

                if (sequence.is_null())
                {
                    return objrefnew<LispNumber>(0);
                }

                Ref<LispObject> result;

                std::string t;
                t = type_of(sequence);

                if (t == LispNode::type_name)
                {
                    Ref<LispNode> c = sequence.as<LispNode>();

                    size_t n = 0;
                    for (; c.is_not_null(); c = c->cdr())
                    {
                        n++;
                    }
                    result = objrefnew<LispNumber>(n);
                }
                else if (t == LispVector::type_name)
                {
                    Ref<LispVector> v = sequence.as<LispVector>();
                    result = objrefnew<LispNumber>(v->size());
                }
                else if (t == LispString::type_name)
                {
                    Ref<LispString> s = sequence.as<LispString>();
                    result = objrefnew<LispNumber>(s->get_value().size());
                }
                else
                {
                    raise_error(LispError::s_wrong_type_argument, "length: as the first argument, nil or an object of one of the types cons, string or vector was expected; but something else was received.");
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(length x)" << std::endl;
                result << "  returns the length of x." << std::endl;
                result << "  x can be a list, a vector, or a string." << std::endl;
                result << "  x can also be nil, in which case the result is 0.";
                return result.str();
            }
        };



        class Elt : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> result;
                Ref<LispObject> o, n;
                unpack2("elt", args, o, n);

                o = caller->evaluate(o);
                n = caller->evaluate(n);

                size_t ind;
                ind = get_index_or_complain(n, "'elt' expects a number as its second argument.");

                std::string t;
                t = type_of(o);
                if (t == LispVector::type_name)
                {
                    Ref<LispVector> v = o.as<LispVector>();

                    if (v->size() <= ind)
                    {
                        raise_error(LispError::s_args_out_of_range, "elt: The vector does not have the element with the specified index.");
                    }

                    result = v->at(ind);
                }
                else if (t == LispNode::type_name)
                {
                    Ref<LispNode> c = o.as<LispNode>();

                    bool found = false;

                    size_t nn = 0;
                    for (; c.is_not_null(); c = c->cdr())
                    {
                        if (nn == ind)
                        {
                            result = c->car();
                            found = true;
                            break;
                        }
                        nn++;
                    }

                    if (!found)
                    {
                        raise_error(LispError::s_args_out_of_range, "elt: The list does not have the element with the specified index.");
                    }

                }
                else if (t == LispString::type_name)
                {
                    Ref<LispString> s = o.as<LispString>();

                    if (ind >= s->get_value().size())
                    {
                        raise_error(LispError::s_args_out_of_range, "elt: The index is out of the boundaries of the string.");
                    }

                    widestring::character q;
                    q = s->get_value()[ind];

                    double qq = double(q);

                    result = objrefnew<LispNumber>(qq);
                }
                else
                {
                    raise_error(LispError::s_wrong_type_argument, "'elt' expects a cons, a vector or a string as its first argument.");
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(elt x n)" << std::endl;
                result << "  returns the n-th element of x." << std::endl;
                result << "  the indexing starts from 0." << std::endl;
                result << "  x can be a list, a vector, or a string.";
                return result.str();
            }
        };


        class SetElt : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> result;
                Ref<LispObject> o, n, xx;
                unpack3("set-elt", args, o, n, xx);

                o = caller->evaluate(o);
                n = caller->evaluate(n);
                xx = caller->evaluate(xx);

                size_t ind;
                ind = get_index_or_complain(n, "'set-elt' expects a number as its second argument.");

                std::string t;
                t = type_of(o);
                if (t == "vector")
                {
                    Ref<LispVector> v = o.as<LispVector>();

                    if (v->size() <= ind)
                    {
                        raise_error(LispError::s_args_out_of_range, "set-elt: The vector does not have the element with the specified index.");
                    }

                    v->at(ind) = xx;
                }
                else if (t == "cons")
                {
                    Ref<LispNode> c = o.as<LispNode>();

                    bool found = false;

                    size_t nn = 0;
                    for (; c.is_not_null(); c = c->cdr())
                    {
                        if (nn == ind)
                        {
                            c->setcar(xx);
                            found = true;
                            break;
                        }
                        nn++;
                    }

                    if (!found)
                    {
                        raise_error(LispError::s_args_out_of_range, "set-elt: The list does not have the element with the specified index.");
                    }

                }
                else
                {
                    raise_error(LispError::s_wrong_type_argument, "'set-elt' expects a cons or a vector as its first argument.");
                }

                return xx;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(set-elt x n val)" << std::endl;
                result << "  changes the value of n-th element of x to val." << std::endl;
                result << "  the indexing starts from 0." << std::endl;
                result << "  x can be a list or a vector.";
                return result.str();
            }
        };


        class UpcaseString : public LispCallable
        {
        public:

             virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                 throw(Ref<LispException>)
             {
                 Ref<LispString> s;
                 Ref<LispObject> a;
                 unpack1("upcase", args, a);

                 a = caller->evaluate(a);
                 if (type_of(a) != LispString::type_name)
                 {
                     raise_error(LispError::s_wrong_type_argument, "upcase: the argument must be a string.");
                 }

                 s = a.as<LispString>();

                 return objrefnew<LispString>(uppercase(s->get_value()));
             }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(upcase s)" << std::endl;
                result << "  returns an uppercased copy of the string s";
                return result.str();
            }
        };

        class DowncaseString : public LispCallable
        {
        public:

             virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                 throw(Ref<LispException>)
             {
                 Ref<LispString> s;
                 Ref<LispObject> a;
                 unpack1("downcase", args, a);

                 a = caller->evaluate(a);
                 if (type_of(a) != LispString::type_name)
                 {
                     raise_error(LispError::s_wrong_type_argument, "downcase: the argument must be a string.");
                 }

                 s = a.as<LispString>();

                 return objrefnew<LispString>(lowercase(s->get_value()));
             }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(downcase s)" << std::endl;
                result << "  returns a lowercased copy of the string s";
                return result.str();
            }
        };


        class Substring : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> s, i1, i2;
                args = caller->evaluate_nodes(args);

                unpack3("substring", args, s, i1, i2);

                widestring ss;
                ss = get_string_or_complain(s, "substring: the first argument must be a string");

                size_t ind1, ind2;
                ind1 = get_index_or_complain(i1, "substring: the second argument must be an integer");
                ind2 = get_index_or_complain(i2, "substring: the third argument must be an integer");

                if ((ind1 >= ss.size()) || (ind2 > ss.size()))
                {
                    raise_error(LispError::s_args_out_of_range, "substring: indices are out of range.");
                }

                return objrefnew<LispString>(ss.substr(ind1, ind2 - ind1));
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(substring s ind1 ind2)" << std::endl;
                result << "  returns the substring of the string s beginning from the index ind1 and ending at (excluding) the index ind2." << std::endl;
                result << "  the indexing starts from 0." << std::endl;
                return result.str();
            }
        };


        class ErrorRaiser : public LispCallable
        {
            public:

            ErrorRaiser()
            {
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o;
                unpack1("error", args, o);

                o = caller->evaluate(o);

                if (type_of(o) == LispError::type_name)
                {
                    throw o.as<LispException>();
                }
                else
                {
                    raise_error("error", as_string(o));
                }

                return o;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(error x)" << std::endl;
                result << "  raises an error. If x is an error object, then that error object is raised. Otherwise, x is converted to string and used as the message of the newly created error object to be raised.";

                return result.str();
            }

        };


        class OutputTool : public LispCallable
        {
            public:
            bool print_as_repr;
            bool add_new_line;

            OutputTool()
            {
                print_as_repr = false;
                add_new_line = true;
            }

            OutputTool(bool print_as_repr, bool add_new_line)
            {
                this->print_as_repr = print_as_repr;
                this->add_new_line = add_new_line;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o;
                unpack1("princ", args, o);

                o = caller->evaluate(o);

                if (print_as_repr)
                {
                    std::cout << as_repr_string(o);
                }
                else
                {
                    std::cout << as_string(o);
                }

                if (add_new_line)
                {
                    std::cout << "\n";
                }

                return o;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(print x)" << std::endl;
                result << "  prints the selin representation string of x to the screen and returns it" << std::endl;
                result << "(message x)" << std::endl;
                result << "  prints x to the screen and returns it" << std::endl;
                result << "(princ x)" << std::endl;
                result << "  prints the to the screen without appending a newline character at the end, and returns it";

                return result.str();
            }
        };


        class StringInputTool : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o;
                unpack1("read-string", args, o);
                o = caller->evaluate(o);

                std::cout << as_string(o);

                char line[256];
                std::cin.getline(line, 255);

                return objrefnew<LispString>(line);
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(read-string s)" << std::endl;
                result << "  writes the string message s to the string, expects an entry from standard input, and returns that entry as string";

                return result.str();
            }
        };



        class Reader : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {

                if (args.is_null())
                {
                    return read_from_stdin();
                }
                else
                {
                    std::string line;
                    Ref<LispObject> o;

                    if (args->cdr().is_not_null())
                    {
                        raise_error(LispError::s_wrong_number_of_arguments, "'read' expects one optional argument.");
                    }

                    o = caller->evaluate(args->car());

                    if (type_of(o) != LispString::type_name)
                    {
                        raise_error(LispError::s_wrong_type_argument, "'read' expects a string as argument.");
                    }

                    line = o.as<LispString>()->get_value().to_string();

                    return read_from_string(line);
                }

            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(read)" << std::endl;
                result << "  reads and returns a selin object from the standard input" << std::endl;
                result << "(read s)" << std::endl;
                result << "  reads and returns a selin object from the string s";

                return result.str();
            }
        };


        class Sorter : public LispCallable
        {
            class Comparer
            {
                Scope *s;
                Ref<LispCallable> fn;

            public:
                Comparer(Scope *scope, Ref<LispObject> comparing_func) throw(Ref<LispException>)
                {
                    if (!is_callable(comparing_func))
                    {
                        raise_error(LispError::s_wrong_type_argument, "sort: The specified comparer object is not a function");
                    }
                    fn = comparing_func.as<LispCallable>();
                    s = scope;
                }

                bool operator()(Ref<LispObject> a, Ref<LispObject> b)
                {
                    NodesCreator nc;
                    nc.push_back(a);
                    nc.push_back(b);
                    return (fn->execute(s, nc.get_first_node()).is_not_null());
                }
            };

        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                args = caller->evaluate_nodes(args);

                Ref<LispObject> seq;
                Ref<LispObject> comparer;

                unpack2("sort", args, seq, comparer);


                Ref<LispObject> fn_comparer;

                if (type_of(comparer) == LispSymbol::type_name)
                {
                    Ref<LispSymbol> sym_comparer;
                    sym_comparer = comparer.as<LispSymbol>();

                    Variable *vv;
                    vv = caller->get_callable(sym_comparer->get_value());

                    if (vv == NULL)
                    {
                        raise_error(LispError::s_void_variable, "sort: the symbol given as the second argument is not defined in this scope");
                    }

                    fn_comparer = vv->get_value();
                }
                else if (type_of(comparer) == LispCallable::type_name)
                {
                    fn_comparer = comparer;
                }
                else
                {
                    raise_error(LispError::s_wrong_type_argument, "sort: the second argument must be a symbol or a callable");
                }

                if (seq.is_null())
                {
                    return Ref<LispObject>(NULL);
                }


                std::string typ = type_of(seq);

                if (typ != LispVector::type_name && typ != LispNode::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "sort: the first argument is expected to be a cons or a vector");
                }


                UniversalTraverser it(seq);
                std::list< Ref<LispObject> > objs;

                while (it.has_more())
                {
                    objs.push_back(it.get_next());
                }

                objs.sort(Comparer(caller, fn_comparer));

                Ref<LispObject> result;

                if (typ == "cons")
                {
                    NodesCreator result_creator;
                    for (std::list< Ref<LispObject> >::iterator itt = objs.begin();
                         itt != objs.end();
                         itt++)
                    {
                        result_creator.push_back(*itt);
                    }

                    result = result_creator.get_first_node().as<LispObject>();
                }
                else if (typ == "vector")
                {
                    Ref<LispVector> vresult;
                    vresult = refnew<LispVector>();
                    vresult->reserve(objs.size());
                    for (std::list< Ref<LispObject> >::iterator itt = objs.begin();
                         itt != objs.end();
                         itt++)
                    {
                        vresult->push_back(*itt);
                    }

                    result = vresult.as<LispObject>();

                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(sort seq f)" << std::endl;
                result << "  returns the sorted copy of the sequence seq according to f" << std::endl;
                result << "  seq can be a list or a vector" << std::endl;
                result << "  f is expected to be a callable object, or a symbol specifying the name of the callable object";

                return result.str();
            }
        };


        class DoList : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_config;
                Ref<LispNode> actions;

                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "dolist: missing arguments");
                }

                o_config = args->car();
                if (type_of(o_config) != LispNode::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "dolist: the first argument must be a list");
                }

                Ref<LispNode> config;
                config = o_config.as<LispNode>();

                Ref<LispObject> o_symbol, o_lst;
                unpack2("first argument of dolist", config, o_symbol, o_lst);
                o_lst = caller->evaluate(o_lst);

                if (type_of(o_symbol) != LispSymbol::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "dolist: the first element in the first argument must be a symbol");
                }
                std::string s_symbol = o_symbol.as<LispSymbol>()->get_value();

                if (o_lst.is_null())
                {
                    return Ref<LispObject>(NULL);
                }

                if (!(UniversalTraverser::can_iterate_over(o_lst)))
                {
                    raise_error(LispError::s_wrong_type_argument, "dolist: expected an iterable object as the second element in the first argument, but received something else.");
                }
                //Ref<LispNode> lst;
                //lst = o_lst.as<LispNode>();

                actions = args->cdr();

                Ref<LispObject> result;
                try
                {
                    UniversalTraverser it(o_lst);
                    Scope scope(caller);
                    while (it.has_more())
                    {
                        scope.set_local_variable(s_symbol, it.get_next());
                        for (Ref<LispNode> act = actions;
                             act.is_not_null();
                             act = act->cdr())
                        {
                            //result = scope.evaluate(act->car());
                            scope.evaluate(act->car());
                        }
                    }

                }
                catch (Ref<LispException> ex)
                {
                    bool handled = false;

                    if (type_of(ex) == LispRequest::type_name)
                    {
                        Ref<LispRequest> req(ex.as<LispRequest>());
                        if (req->get_request_type() == LispRequest::req_return)
                        {
                            result = req->get_data();
                            handled = true;
                        }
                    }

                    if (!handled)
                    {
                        throw ex;
                    }
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(dolist (x seq) action1 action2 ...)" << std::endl;
                result << "  creates a scope with the temporary variable x, loops over each element in the list or vector called seq. At each iteration, x stores the value of the current element, and the specified actions are executed" << std::endl;
                result << "  EXAMPLE:" << std::endl;
                result << "    ; print each element i within a list:" << std::endl;
                result << "    (dolist (i (list 1 2 3))" << std::endl;
                result << "      (print i))";

                return result.str();
            }
        };


        class DoTimes : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> o_config;
                Ref<LispNode> actions;

                if (args.is_null())
                {
                    raise_error(LispError::s_wrong_number_of_arguments, "dotimes: missing arguments");
                }

                o_config = args->car();
                if (type_of(o_config) != LispNode::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "dotimes: the first argument must be a list");
                }

                Ref<LispNode> config;
                config = o_config.as<LispNode>();

                Ref<LispObject> o_symbol, o_upperbound;
                unpack2("first argument of dotimes", config, o_symbol, o_upperbound);
                o_upperbound = caller->evaluate(o_upperbound);

                if (type_of(o_symbol) != LispSymbol::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "dotimes: the first element in the first argument must be a symbol");
                }
                std::string s_symbol = o_symbol.as<LispSymbol>()->get_value();

                if (type_of(o_upperbound) != LispNumber::type_name)
                {
                    raise_error(LispError::s_wrong_type_argument, "dotimes: the second element in the first argument must be a number");
                }

                actions = args->cdr();

                Ref<LispObject> result;
                try
                {
                    Ref<LispNumber> upperbound(o_upperbound.as<LispNumber>());

                    bigint ubound;
                    ubound = bigint(upperbound->get_value());

                    Scope scope(caller);
                    for (bigint i = 0; i < ubound; i++)
                    {
                        Ref<LispNumber> mynum(new LispNumber(double(i)));
                        Ref<LispObject> o_mynum(mynum.as<LispObject>());
                        scope.set_local_variable(s_symbol, o_mynum);
                        for (Ref<LispNode> act = actions;
                             act.is_not_null();
                             act = act->cdr())
                        {
                            scope.evaluate(act->car());
                        }
                    }
                }
                catch (Ref<LispException> ex)
                {
                    bool handled = false;

                    if (type_of(ex) == LispRequest::type_name)
                    {
                        Ref<LispRequest> req(ex.as<LispRequest>());
                        if (req->get_request_type() == LispRequest::req_return)
                        {
                            result = req->get_data();
                            handled = true;
                        }
                    }

                    if (!handled)
                    {
                        throw ex;
                    }
                }

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(dotimes (i n) action1 action2 ...)" << std::endl;
                result << "  creates a scope with the temporary variable i, starts a loop beginning with i=0, as long as i<n. At each iteration, the specified actions are executed within the created scope." << std::endl;
                result << "  EXAMPLE:" << std::endl;
                result << "    ; print 01234" << std::endl;
                result << "    (dotimes (i 5)" << std::endl;
                result << "      (princ i))";

                return result.str();
            }
        };


       class Returner : public LispCallable
       {
       public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a;
                unpack1("return", args, a);

                a = caller->evaluate(a);

                Ref<LispRequest> req(new LispRequest(LispRequest::req_return, a));
                Ref<LispException> ex(req.as<LispException>());

                throw ex;

                return Ref<LispObject>(NULL); // but we should not reach here
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << std::endl << "(return x)";
                result << "  breaks the execution of the current dolist or dotimes construct, making it return x.";
                return result.str();
            }
       };



        class MakeVector : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                args = caller->evaluate_nodes(args);

                Ref<LispObject> o_howmany, o_initialvalue;
                unpack2("make-vector", args, o_howmany, o_initialvalue);

                size_t howmany = get_index_or_complain(o_howmany,
                    "make-vector: the first argument must be an integer");

                Ref<LispVector> result;
                result = refnew<LispVector>();
                result->resize(howmany);

                for (size_t i = 0; i < howmany; ++i)
                {
                    result->at(i) = o_initialvalue;
                }

                return result.as<LispObject>();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(make-vector n x)" << std::endl;
                result << "  creates and returns a vector with n elements, each element being x";

                return result.str();
            }
        };


        class MakeString : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                args = caller->evaluate_nodes(args);

                Ref<LispObject> o_howmany, o_initialvalue;
                unpack2("make-string", args, o_howmany, o_initialvalue);

                size_t howmany = get_index_or_complain(o_howmany,
                    "make-string: the first argument must be an integer");

                widestring::character k;
                k = widestring::character(round_double(get_number_or_complain(o_initialvalue, "make-string: the second argument must be a character code")));

                widestring::character *sresult;
                sresult = new widestring::character[howmany];

                for (size_t i = 0; i < howmany; ++i)
                {
                    sresult[i] = k;
                }

                Ref<LispString> result;
                result = refnew<LispString>(sresult);

                delete []sresult;

                return result.as<LispObject>();
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(make-string n c)" << std::endl;
                result << "  creates and returns a string consisting of the character c repeated n times";

                return result.str();
            }
        };

        class MathFunction : public LispCallable
        {
            typedef double (*mathfunc)(double);
            mathfunc mf;
            std::string funcname;

        public:
            MathFunction(std::string funcname, mathfunc mf)
            {
                this->funcname = funcname;
                this->mf = mf;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                args = caller->evaluate_nodes(args);

                Ref<LispObject> o;
                unpack1(funcname, args, o);

                double v = get_number_or_complain(o, funcname + ": the argument must be a number");

                return objrefnew<LispNumber>(mf(v));
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream ss;
                ss << to_string() << std::endl;
                ss << "(" << funcname << " x)" << std::endl;
                ss << "  returns " << funcname << "(x), x being a number";
                return ss.str();
            }
        };


        class MathFunction2 : public LispCallable
        {
            typedef double (*mathfunc)(double, double);
            mathfunc mf;
            std::string funcname;

        public:
            MathFunction2(std::string funcname, mathfunc mf)
            {
                this->funcname = funcname;
                this->mf = mf;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                args = caller->evaluate_nodes(args);

                Ref<LispObject> o1, o2;
                unpack2(funcname, args, o1, o2);

                double v1 = get_number_or_complain(o1, funcname + ": the first argument must be a number");
                double v2 = get_number_or_complain(o2, funcname + ": the second argument must be a number");

                return objrefnew<LispNumber>(mf(v1, v2));
            }

            std::string to_repr_string() const
            {
                std::stringstream ss;
                ss << "(" << funcname << " x y)" << std::endl;
                if (funcname == "%")
                {
                    ss << "  returns the remainder of the division x/y, x and y being integer numbers";
                }
                else if (funcname == "expt")
                {
                    ss << "  returns x to the power of y, x and y being numbers";
                }
                else if (funcname == "ash")
                {
                    ss << "  applies bitwise shifting operation on the integer x, and returns the result. The amount of shifting is specified by the integer y. If y > 0, the shifting is done towards left; otherwise, if y < 0, the shifting is done towards right.";
                }
                return ss.str();
            }
        };

        class SignalCommand : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                args = caller->evaluate_nodes(args);

                Ref<LispObject> a, b;
                unpack2("signal", args, a, b);

                std::string s_err;
                s_err = get_symbol_or_complain(a, "signal: the first argument must be a symbol, indicating the error type");

                Ref<LispException> err;
                err = refnew<LispError>(s_err, b).as<LispException>();

                throw err;
            }

            std::string to_repr_string() const
            {
                std::stringstream ss;
                ss << "(signal error-type details)" << std::endl;
                ss << "  raises an error, type of the error being specified by the symbol error-type, and the details of the error being specified by the string details.";
                return ss.str();
            }
        };

        class UnwindProtector : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                if (args.is_not_null())
                {
                    Scope myscope(caller);
                    myscope.set_goodbye_actions(args->cdr());
                    return myscope.evaluate(args->car());
                }
                else
                {
                    return Ref<LispObject>(NULL);
                }
            }

            std::string to_repr_string() const
            {
                std::stringstream ss;
                ss << "(unwind-protect action unwinding-action1 unwinding-action2 ...)" << std::endl;
                ss << "  executes the action and returns its result if the action is completed successfully." << std::endl;
                ss << "  Regardless of how the execution of the action went (whether the action finished successfully or not), the unwinding actions are executed at the end.";
                return ss.str();
            }
        };


        class ConditionCase : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> result;
                std::string s_err;
                Ref<LispObject> o_handlers;
                Ref<LispError> err;
                bool caught_error = false;
                bool handled_error = false;
                bool bind_err_var = true;

                for (size_t i = 0; args.is_not_null(); args = args->cdr(), i += 1)
                {
                    Ref<LispObject> arg;

                    arg = args->car();
                    if (i == 0)
                    {
                        if (arg.is_null())
                        {
                            bind_err_var = false;
                        }
                        else
                        {
                            s_err = get_symbol_or_complain(arg, "condition-case: a symbol is expected as the first argument");
                            if (s_err == "nil")
                            {
                                bind_err_var = false;
                            }
                        }
                    }
                    else if (i == 1)
                    {
                        try
                        {
                            result = caller->evaluate(arg);
                        }
                        catch (Ref<LispException> ex)
                        {
                            if (ex.is_null())
                            {
                                throw ex;
                            }

                            if (type_of(ex.as<LispObject>()) != LispError::type_name)
                            {
                                throw ex;
                            }
                            else
                            {
                                err = ex.as<LispError>();
                                caught_error = true;
                            }
                        }
                    }
                    else
                    {
                        if (!caught_error)
                        {
                            break;
                        }

                        Ref<LispNode> handler;
                        handler = cast_or_complain<LispNode>(arg, "condition-case: a handler must be expressed by a list");

                        std::string handler_name;
                        handler_name = get_symbol_or_complain(handler->car(), "condition-case: a handler must begin with a symbol");

                        if (err->can_be_handled_by(handler_name))
                        {
                            handled_error = true;

                            Ref<LispNode> handler_body;
                            handler_body = handler->cdr();

                            Scope handler_scope(caller);
                            if (bind_err_var)
                            {
                                handler_scope.set_local_variable(s_err, err.as<LispObject>());
                            }

                            for (; handler_body.is_not_null(); handler_body = handler_body->cdr())
                            {
                                result = handler_scope.evaluate(handler_body->car());
                            }

                            break;
                        }
                    }
                }

                if (caught_error && (!handled_error))
                {
                    throw err.as<LispException>();
                }

                return result;
            }

            std::string to_repr_string() const
            {
                std::stringstream ss;
                ss << to_string() << std::endl;
                ss << "(condition-case errorvar" << std::endl;
                ss << "    action" << std::endl;
                ss << std::endl;
                ss << "  (error-type-name1" << std::endl;
                ss << "    error-handling-action1a" << std::endl;
                ss << "    error-handling-action1b" << std::endl;
                ss << "    ...)" << std::endl;
                ss << std::endl;
                ss << "  (error-type-name2" << std::endl;
                ss << "    error-handling-action2a" << std::endl;
                ss << "    error-handling-action2b" << std::endl;
                ss << "    ...)" << std::endl;
                ss << std::endl;
                ss << "  ...)" << std::endl;
                ss << std::endl;
                ss << "executes the action. If action raises an error, the error handling regions below are traversed. If the error matches with the error type name written in the beginning of an error handling region, the error handling actions specified within that region are triggered. The details about the error can be accessed via the variable errorvar within the error handling regions.";
                return ss.str();
            }

            // TODO: in Emacs Lisp, errorvar is in the form: (error-type . error-details) . In selin, it is a LispError object. Try to make it more compatible with Emacs Lisp. Also, define the callable error-message-string

        };

        class ErrorTypeGetter : public LispCallable
        {
        public:

             virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                 throw(Ref<LispException>)
             {
                 Ref<LispError> err;
                 Ref<LispObject> a;
                 unpack1("error-type", args, a);

                 a = caller->evaluate(a);
                 if (type_of(a) != LispError::type_name)
                 {
                     raise_error(LispError::s_wrong_type_argument, "error-type: the argument must be an error object.");
                 }

                 err = a.as<LispError>();

                 std::string errtyp(err->get_error_type());

                 return objrefnew<LispSymbol>(errtyp);
             }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(error-type err)" << std::endl;
                result << "  returns the error type of the error object err";
                return result.str();
            }
        };

        class ErrorDataGetter : public LispCallable
        {
        public:

             virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                 throw(Ref<LispException>)
             {
                 Ref<LispError> err;
                 Ref<LispObject> a;
                 unpack1("error-data", args, a);

                 a = caller->evaluate(a);
                 if (type_of(a) != LispError::type_name)
                 {
                     raise_error(LispError::s_wrong_type_argument, "error-data: the argument must be an error object.");
                 }

                 err = a.as<LispError>();

                 return err->get_data();
             }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;

                result << to_string() << std::endl;
                result << "(error-data err)" << std::endl;
                result << "  returns the data of the error object err";
                return result.str();
            }
        };

        class Catcher : public LispCallable
        {
        public:
            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> result;

                Ref<LispObject> o_tag;
                Ref<LispSymbol> sym_tag;

                o_tag = LispNode::pop_or_complain(args, "catch: the first argument is missing");
                o_tag = caller->evaluate(o_tag);
                sym_tag = cast_or_complain<LispSymbol>(o_tag, "catch: expected a symbol as the first argument, but received something else");
                std::string tag;
                tag = sym_tag->get_value();

                try
                {
                    for (; args.is_not_null(); args = args->cdr())
                    {
                        Ref<LispObject> action(args->car());
                        result = caller->evaluate(action);
                    }
                }
                catch (Ref<LispException> ex)
                {
                    bool caught = false;

                    if (type_of(ex) == LispRequest::type_name)
                    {
                        Ref<LispRequest> req(ex.as<LispRequest>());
                        if (req->get_request_type() == LispRequest::req_catch
                            && req->get_request_label() == tag)
                        {
                            result = req->get_data();
                            caught = true;
                        }
                    }

                    if (!caught)
                    {
                        throw ex;
                    }
                }

                return result;

            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl
                       << "(catch tag action1 action2 ...)" << std::endl
                       << "  Executes the specified actions, and returns the result of the last action." << std::endl
                       << "  If during the execution of the actions, an object is thrown, and the tag of this thrown object matches, then the execution stops, and the caught object is returned. (see also throw)" << std::endl
                       << "  EXAMPLE:" << std::endl
                       << "    ;the following code will return 3, not 7:" << std::endl
                       << "    (catch 'mytag (throw 'mytag 3) 7)";
                return result.str();
            }
        };

       class Thrower : public LispCallable
       {
       public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a, b;
                unpack2("throw", args, a, b);

                a = caller->evaluate(a);
                b = caller->evaluate(b);

                Ref<LispSymbol> sym;
                sym = cast_or_complain<LispSymbol>(a, "throw: the first argument must be a symbol");

                std::string s;
                s = sym->get_value();

                Ref<LispRequest> req(new LispRequest(LispRequest::req_catch, b, s));

                Ref<LispException> exreq(req.as<LispException>());

                throw exreq;

                return Ref<LispObject>(NULL); // but we should not reach here
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl
                       << "(throw tag x)" << std::endl
                       << "  throws the object x with the specified tag. This thrown object is to be caught by the catcher using the same tag (see catch).";
                return result.str();
            }
       };


        class AproposCommand : public LispCallable
        {
            bool write_to_screen;

            std::string myname() const
            {
                if (write_to_screen)
                {
                    return "apropos";
                }
                else
                {
                    return "names";
                }
            }

        public:
            AproposCommand(bool write_to_screen)
            {
                this->write_to_screen = write_to_screen;
            }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a;

                std::string searched_name;

                if (args.is_null())
                {
                    searched_name = "";
                }
                else
                {
                    if (args->cdr().is_not_null())
                    {
                        raise_error(LispError::s_wrong_number_of_arguments, myname() + ": at most one argument was expected, but more than one arguments were received.");
                    }

                    a = args->car();
                    a = caller->evaluate(a);

                    if (type_of(a) != LispString::type_name
                        && type_of(a) != LispSymbol::type_name)
                    {
                        raise_error(LispError::s_wrong_type_argument, myname() + ": as the first argument, a symbol or a string was expected, but not found");
                    }
                    searched_name = as_string(a);
                }


                dictionary<std::string, Variable>::map names;
                caller->apropos(searched_name, names);

                Ref<LispObject> result(NULL);

                NodesCreator nc;

                for (dictionary<std::string, Variable>::map::iterator it = names.begin();
                     it != names.end();
                     it++)
                {
                    std::string varname;
                    varname = it->first;

                    if (write_to_screen)
                    {
                        std::cout << varname << std::endl;
                    }
                    else
                    {
                        nc.push_back(refnew<LispSymbol>(varname).as<LispObject>());
                    }
                }

                result = nc.get_first_node().as<LispObject>();

                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << "(" << myname() << ")" << std::endl;
                result << "  or" << std::endl;
                result << "(" << myname() << " x)" << std::endl;
                if (write_to_screen)
                {
                    result << "  writes to the screen all the variable and callable names accessible from the current scope.";
                }
                else
                {
                    result << "  returns a list of all the variable and callable names accessible from the current scope.";
                }
                result << "  If x (which can be a string or a symbol) is provided, the results contain only the variables and callables which have x in their names." << std::endl;
                return result.str();
            }
        };


       class BoundP : public LispCallable
       {
           bool fbound;

           std::string myname() const
           {
                std::string result;
                if (fbound)
                {
                    result = "fbound";
                }
                else
                {
                    result = "bound";
                }

                return result;
           }

       public:
           BoundP(bool in_callable_namespace)
           {
               fbound = in_callable_namespace;
           }

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a;
                unpack1(myname(), args, a);

                a = caller->evaluate(a);

                Ref<LispSymbol> sym(cast_or_complain<LispSymbol>(a, myname() + ": expected a symbol as the argument, but received something different."));

                Variable *v;
                if (fbound)
                {
                    v = caller->get_callable(sym->get_value());
                }
                else
                {
                    v = caller->get_variable(sym->get_value());
                }

                if (v == NULL)
                {
                    return Ref<LispObject>(NULL);
                }
                else
                {
                    return LispSymbol::truth();
                }
            }

            virtual std::string to_repr_string() const
            {
                std::string looking_for;
                if (fbound)
                {
                    looking_for = "variable";
                }
                else
                {
                    looking_for = "callable";
                }

                std::stringstream result;
                result << to_string() << std::endl;
                result << std::endl << "(" << myname() << " x)" << std::endl;
                result << "  returns non-nil (i.e. true) if there is a " << looking_for << " defined with the name x." << std::endl;
                result << "  x is expected to be a symbol.";
                return result.str();
            }
       };


       class EvalFile : public LispCallable
       {
       public:

            virtual Ref<LispObject> execute(Scope *caller, Ref<LispNode> args)
                throw(Ref<LispException>)
            {
                Ref<LispObject> a;
                unpack1("eval-file", args, a);

                a = caller->evaluate(a);

                Ref<LispString> sa(cast_or_complain<LispString>(a, "eval-file expected a string as its argument, but received something else."));

                std::string s = sa->to_string();

                Ref<LispObject> result(caller->run_script_file(s));
                return result;
            }

            virtual std::string to_repr_string() const
            {
                std::stringstream result;
                result << to_string() << std::endl;
                result << std::endl << "(eval-file filename)" << std::endl;
                result << "  evaluates the contents of the specified file in the current scope";
                return result.str();
            }
       };


        void fill_scope(Scope &main_scope)
        {
            // CUSTOMIZATION
            // This is where the builtin callables and global variables are defined.
            // You can make your additional definitions, or
            // change the existing builtin definitions here if necessary


            // TODO: implement: capitalize, split-string

            // TODO: implement: random

            // TODO: implement: make-hash-table, gethash, puthash, remhash, hash-table-count

            main_scope.set_callable("defun", objrefnew<CustomCallableDefinition>(true));
            main_scope.set_callable("defext", objrefnew<CustomCallableDefinition>(false));
            main_scope.set_callable("lambda", objrefnew<CustomCallableDefinition>(true, true));

            main_scope.set_callable("condition-case", objrefnew<ConditionCase>());
            main_scope.set_callable("error-type", objrefnew<ErrorTypeGetter>());
            main_scope.set_callable("error-data", objrefnew<ErrorDataGetter>());
            main_scope.set_callable("signal", objrefnew<SignalCommand>());
            main_scope.set_callable("unwind-protect", objrefnew<UnwindProtector>());
            main_scope.set_callable("catch", objrefnew<Catcher>());
            main_scope.set_callable("throw", objrefnew<Thrower>());

            main_scope.set_callable("quote", objrefnew<Quote>());
            main_scope.set_callable("cons", objrefnew<ConsOperation>());

            main_scope.set_callable("__+", objrefnew<NumericOperation>('+'));
            main_scope.set_callable("__-", objrefnew<NumericOperation>('-'));
            main_scope.set_callable("__*", objrefnew<NumericOperation>('*'));
            main_scope.set_callable("__/", objrefnew<NumericOperation>('/'));
            main_scope.set_callable("__logand", objrefnew<NumericOperation>('&'));
            main_scope.set_callable("__logior", objrefnew<NumericOperation>('|'));

            main_scope.set_callable("=",  objrefnew< ValueComparison<double, LispNumber> >("=", "number"));
            main_scope.set_callable("/=", objrefnew< ValueComparison<double, LispNumber> >("/=", "number"));
            main_scope.set_callable(">",  objrefnew< ValueComparison<double, LispNumber> >(">", "number"));
            main_scope.set_callable("<",  objrefnew< ValueComparison<double, LispNumber> >("<", "number"));
            main_scope.set_callable(">=", objrefnew< ValueComparison<double, LispNumber> >(">=", "number"));
            main_scope.set_callable("<=", objrefnew< ValueComparison<double, LispNumber> >("<=", "number"));

            main_scope.set_callable("string=",  objrefnew< ValueComparison<widestring, LispString> >("=", "string"));
            main_scope.set_callable("string/=", objrefnew< ValueComparison<widestring, LispString> >("/=", "string"));
            main_scope.set_callable("string>",  objrefnew< ValueComparison<widestring, LispString> >(">", "string"));
            main_scope.set_callable("string<",  objrefnew< ValueComparison<widestring, LispString> >("<", "string"));
            main_scope.set_callable("string>=", objrefnew< ValueComparison<widestring, LispString> >(">=", "string"));
            main_scope.set_callable("string<=", objrefnew< ValueComparison<widestring, LispString> >("<=", "string"));

            main_scope.set_callable("make-string", objrefnew<MakeString>());
            main_scope.set_callable("downcase", objrefnew<DowncaseString>());
            main_scope.set_callable("upcase", objrefnew<UpcaseString>());

            main_scope.set_callable("eq", objrefnew<EqCheck>());
            main_scope.set_callable("equal", objrefnew<EqualityCheck>());

            Ref<LispObject> op_car, op_cdr;
            op_car = objrefnew<ListOperation>(list_operation_car);
            op_cdr = objrefnew<ListOperation>(list_operation_cdr);
            main_scope.set_callable("car", op_car);
            main_scope.set_callable("cdr", op_cdr);
            main_scope.set_callable("first", op_car);
            main_scope.set_callable("rest", op_cdr);
            main_scope.set_callable("length", objrefnew<ListOperation>(list_operation_length));

            main_scope.set_variable("t", LispSymbol::truth());
            main_scope.set_variable("nil", Ref<LispObject>(NULL));

            main_scope.set_callable("cond", objrefnew<CondStatement>());
            main_scope.set_callable("while", objrefnew<WhileStatement>());
            main_scope.set_callable("dolist", objrefnew<DoList>());
            main_scope.set_callable("dotimes", objrefnew<DoTimes>());
            main_scope.set_callable("return", objrefnew<Returner>());

            main_scope.set_callable("function", objrefnew<FunctionAccessor>());
            main_scope.set_callable("funcall", objrefnew<FunctionCaller>());
            main_scope.set_callable("apply", objrefnew<FunctionApplier>());

            main_scope.set_callable("set", objrefnew<Setter>());
            main_scope.set_callable("setq", objrefnew<Setter>(false));
            main_scope.set_callable("let", objrefnew<LetStatement>(false));
            main_scope.set_callable("let*", objrefnew<LetStatement>(true));
            { //setcar, setcdr
                Ref<LispObject> setcar = objrefnew<SetCar>();
                Ref<LispObject> setcdr = objrefnew<SetCdr>();
                main_scope.set_callable("setcar", setcar);
                main_scope.set_callable("setcdr", setcdr);
                main_scope.set_callable("set-car", setcar);
                main_scope.set_callable("set-cdr", setcdr);
                main_scope.set_callable("set-first", setcar);
                main_scope.set_callable("set-rest", setcdr);
            }

            main_scope.set_callable("boundp", objrefnew<BoundP>(false));
            main_scope.set_callable("fboundp", objrefnew<BoundP>(true));

            // CUSTOMIZATION
            // by default, selin has global scoping (i.e. all functions work on global namespace)
            main_scope.evaluate(std::string("(setq ") + scoping_configuration_variable_name + std::string(" 'global)"));
            //main_scope.evaluate(std::string("(setq ") + scoping_configuration_variable_name + std::string(" 'dynamic)"));

            {
                // setf
                std::stringstream ss;
                ss << "(defext setf ($a $b)"
                   << "  \"(setf x y)\""
                   << "  \"  sets the value y to the variable x, same as setq (see setq).\""
                   << "  \"(setf (fieldname x arg1 arg2) y)\""
                   << "  \"is equivalent to calling (set-fieldname x arg1 arg2 y)\""
                   << "  \"  For example:\""
                   << "  \"    (setf (car x) y) is equivalent to (set-car x y)\""
                   << "  \"    (setf (elt x 0) y) is equivalent to (set-elt x 0 y)\""
                   << "  (cond ((symbolp $a) (eval (list 'setq $a $b)))"
                   << "        ((listp $a) (if (< (length $a) 2)"
                   << "                              (signal 'wrong-number-of-arguments \"'setf' expects a list with at least two elements as its first argument\"))"
                   << "                         (let (($c (string-to-symbol (concat \"set-\" (symbol-to-string (car $a))))) ($d (cdr $a)))"
                   << "                              (eval (concatenate 'list (list $c) $d (list $b)))))"
                   << "        (t (signal 'wrong-type-argument \"'setf' expects a list or a symbol as its first argument\"))))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$setf-"));
            }

            { // push
                //main_scope.evaluate(
                //    strreplace<std::string>(
                //        "(defext push ($x $list) (eval (list 'setq $list (list 'cons $x $list))))",
                //        "$", "$push-"));
                std::stringstream ss;
                ss << "(defext push ($x $list)"
                   << "  \"(push x lst)\""
                   << "  \"  inserts the value x into the beginning of the list lst\""
                   << "  (eval (list 'setq $list (list 'cons $x $list))))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$push-"));
            }

            { // pop
                std::stringstream ss;
                ss << "(defext pop ($list-sym)"
                   << "  \"(pop lst)\""
                   << "  \"  pops the first value from the list lst, and returns that value\""
                   << "  (let* (($list (eval $list-sym)) ($result (car $list)))"
                   << "    (eval (list 'setq $list-sym (cons 'list (cdr $list))))"
                   << "    $result))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$pop-"));
            }

            { // last
                std::stringstream ss;

                ss << "(defun last ($cell)"
                   << "  \"(last lst)\""
                   << "  \"  returns the last element of the list lst\""
                   << "  (if (not (listp $cell))"
                   << "      (signal 'wrong-type-argument \"last: the argument must be a list.\"))"
                   << "  (while (cdr $cell) (setq $cell (cdr $cell)))"
                   << "  $cell)";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$last-"));
            }

            { // nconc
                std::stringstream ss;

                ss << "(defun nconc (&rest $args)"
                   << "  \"(nconc lst arglist1 arglist2 ...)\""
                   << "  \"  appends all the elements of the lists specified in the arguments arglistN to the end of list lst\""
                   << "  (let (($result $args))"
                   << "    (while $args"
                   << "      (let (($a (car $args)) ($b (car (cdr $args))))"
                   << "        (setf (cdr (last $a)) $b))"
                   << "      (setq $args (cdr $args)))"
                   << "    (car $result)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$nconc-"));
            }

            { // append
                std::stringstream ss;

                ss << "(defext append (&rest $args)"
                   << "  \"(append list1 list2 ...)\""
                   << "  \"  returns the concatenation of all the lists\""
                   << "  (eval (concatenate 'list '(concatenate 'list) $args)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$append-"));
            }

            { // reverse
                std::stringstream ss;

                ss << "(defun reverse ($arg)"
                   << "  \"(reverse sequence)\""
                   << "  \"  returns a copy of sequence with the order of the elements reversed. The sequence can be a list or a vector.\""
                   << "  (let (($result nil))"
                   << "    (dolist ($x $arg)"
                   << "      (push $x $result))"
                   << "    (if (vectorp $arg)"
                   << "        (apply 'vector $result)"
                   << "      $result)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$reverse-"));
            }

            { // subseq
                std::stringstream ss;
                ss << "(defun subseq ($a $i &optional $j)"
                   << "  \"(subseq sequence i)\""
                   << "  \"  returns the subsequence of the sequence beginning from its i-th element.\""
                   << "  \"(subseq sequence i j)\""
                   << "  \"  returns the subsequence of the sequence beginning from its i-th element and ending at its j-th element.\""
                   << "  \"The sequence can be a string, a list, or a vector.\""
                   << "  (cond"
                   << "    ((stringp $a)"
                   << "      (if (not $j) (setq $j (length $a)))"
                   << "      (substring $a $i $j))"
                   << "    ((listp $a)"
                   << "      (let (($n 0) ($result nil))"
                   << "        (while (and $a (or (not $j) (< $n $j)))"
                   << "          (if (>= $n $i)"
                   << "              (push (car $a) $result))"
                   << "          (setq $a (cdr $a))"
                   << "          (setq $n (+ $n 1)))"
                   << "        (reverse $result)))"
                   << "    ((vectorp $a)"
                   << "      (if (not $j) (setq $j (length $a)))"
                   << "      (let (($n $i) ($result nil))"
                   << "        (while (< $n $j)"
                   << "          (push (elt $a $n) $result)"
                   << "          (setq $n (+ $n 1)))"
                   << "        (apply 'vector (reverse $result))))"
                   << "    (t (signal 'wrong-type-argument \"subseq expects string, list or vector as its first argument\"))))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$subseq-"));
            }

            { // coerce
                std::stringstream ss;

                ss << "(defun coerce ($seq $target)"
                   << "  \"(coerce sequence typename)\""
                   << "  \"  converts the sequence into the type specified by typename.\""
                   << "  \"For example, to convert a list to a vector: (coerce (list 1 2 3) 'vector)\""
                   << "  \"Similarly, to convert a vector to a list: (coerce '(vector 1 2 3) 'list)\""
                   << "  (concatenate $target $seq))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$coerce-"));
            }

            { // get-callable-object
                std::stringstream ss;
                ss << "(defun get-callable-object ($x)"
                   << "  (cond"
                   << "    ((symbolp $x)"
                   << "      (eval (list 'function $x)))"
                   << "    ((functionp $x)"
                   << "      $x)"
                   << "    (t (signal 'wrong-type-argument \"get-callable-object: expected a callable object or a symbol\"))))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$get-callable-object-"));
            }

            { // reduce
                std::stringstream ss;
                ss << "(defun reduce ($f $list)"
                   << "  \"(reduce f mylist)\""
                   << "  \"  keeps applying the operation f on the head of the list mylist, until that entire list is reduced to a single value, which is returned.\""
                   << "  \"EXAMPLE:\""
                   << "  \"  (reduce '+ (list 1 2 3 4 5)) would trigger:\""
                   << "  \"    (+ 1 2) 3 4 5 -> 3 3 4 5\""
                   << "  \"    (+ 3 3) 4 5   -> 6 4 5\""
                   << "  \"    (+ 6 4) 5     -> 10 5\""
                   << "  \"    (+ 10 5)      -> 15\""
                   << "  \"  so, the result would be 15.\""
                   << "  (cond"
                   << "   ((not $list)"
                   << "    (funcall $f))"
                   << "   ((not (cdr $list))"
                   << "    (car $list))"
                   << "   (t"
                   << "    (setq $f (get-callable-object $f))"
                   << "    (let (($result (car $list)))"
                   << "      (setq $list (cdr $list))"
                   << "      (while $list"
                   << "        (setq $result (funcall $f $result (pop $list))))"
                   << "      $result))))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$reduce-"));
            }

            { // +
                std::stringstream ss;
                ss << "(defun + (&rest $args)"
                   << "  \"(+ numericarg1 numericarg2 ...)\""
                   << "  \"  returns the addition of the given numeric arguments\""
                   << "  (reduce '__+ (cons 0 $args)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$+_"));
            }

            { // *
                std::stringstream ss;
                ss << "(defun * (&rest $args)"
                   << "  \"(* numericarg1 numericarg2 ...)\""
                   << "  \"  returns the multiplication of the given numeric arguments\""
                   << "  (reduce '__* (cons 1 $args)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$*_"));
            }

            { // logand
                std::stringstream ss;
                ss << "(defun logand (&rest $args)"
                   << "  \"(logand numericarg1 numericarg2 ...)\""
                   << "  \"  returns the result of applying the bitwise-and operation on the given numeric arguments\""
                   << "  (reduce '__logand (cons -1 $args)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$logand-"));
            }

            { // logior
                std::stringstream ss;
                ss << "(defun logior (&rest $args)"
                   << "  \"(logior numericarg1 numericarg2 ...)\""
                   << "  \"  returns the result of applying the bitwise-or operation on the given numeric arguments\""
                   << "  (reduce '__logior (cons 0 $args)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$logior-"));
            }

            { // -
                std::stringstream ss;
                ss << "(defun - (&rest $args)"
                   << "  \"applies the subtraction operation on the given numeric arguments and returns the result.\""
                   << "  \"  (- x) returns -x\""
                   << "  \"  (- n1 n2 n3 ...) returns n1 - n2 - n3 - ...\""
                   << "  (let (($result nil)"
                   << "        ($firstarg (pop $args)))"
                   << "    (if $args"
                   << "        (progn"
                   << "          (setf $result $firstarg)"
                   << "          (dolist ($arg $args)"
                   << "            (setf $result (__- $result $arg))))"
                   << "      (setf $result (__- 0 $firstarg)))"
                   << "    $result))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$-_"));
            }

            { // /
                std::stringstream ss;
                ss << "(defun / (&rest $args)"
                   << "  \"applies the division operation on the given numeric arguments and returns the result.\""
                   << "  \"  (/ x) returns 1/x\""
                   << "  \"  (/ n1 n2 n3 ...) returns n1 / n2 / n3 / ...\""
                   << "  (let (($result nil)"
                   << "        ($firstarg (pop $args)))"
                   << "    (if $args"
                   << "        (progn"
                   << "          (setf $result $firstarg)"
                   << "          (dolist ($arg $args)"
                   << "            (setf $result (__/ $result $arg))))"
                   << "      (setf $result (__/ 1 $firstarg)))"
                   << "    $result))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$/_"));
            }

            { // max
                std::stringstream ss;
                ss << "(defun max (&rest $args)"
                   << "  \"(max numericarg1 numericarg2 ...)\""
                   << "  \"  returns the maximum among the given numeric arguments\""
                   << "  (if (not $args)"
                   << "    (signal 'wrong-number-of-arguments \"'max' expected at least one argument but received none\"))"
                   << "  (let (($largest (pop $args)))"
                   << "    (dolist ($arg $args)"
                   << "      (if (> $arg $largest)"
                   << "          (setf $largest $arg)))"
                   << "    $largest))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$max-"));
            }

            { // min
                std::stringstream ss;
                ss << "(defun min (&rest $args)"
                   << "  \"(min numericarg1 numericarg2 ...)\""
                   << "  \"  returns the minimum among the given numeric arguments\""
                   << "  (if (not $args)"
                   << "    (signal 'wrong-number-of-arguments \"'min' expected at least one argument but received none\"))"
                   << "  (let (($smallest (pop $args)))"
                   << "    (dolist ($arg $args)"
                   << "      (if (< $arg $smallest)"
                   << "          (setf $smallest $arg)))"
                   << "    $smallest))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$min-"));
            }

            { // mapcar
                std::stringstream ss;
                ss << "(defun mapcar ($func $seq)"
                   << "  \"(mapcar f sequence)\""
                   << "  \"  applies the callable object f on each element of sequence. A new sequence is returned, containing the results of each f operation. The sequence can be a list or a vector.\""
                   << "  (let (($result nil))"
                   << "    (setq $func (get-callable-object $func))"
                   << "    (dolist ($x $seq)"
                   << "      (push (funcall $func $x) $result))"
                   << "    (setq $result (reverse $result))"
                   << "    (if (vectorp $seq)"
                   << "        (coerce $result 'vector)"
                   << "      $result)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$mapcar-"));
            }

            { // test-value-against
                std::stringstream ss;
                ss << "(defun test-value-against"
                   << "    ($f $x $opt)"
                   << "    (cond"
                   << "        ((eq $opt 'eq)"
                   << "            (eq $f $x))"
                   << "        ((eq $opt 'noteq)"
                   << "            (not (eq $f $x)))"
                   << "        ((eq $opt 'not)"
                   << "            (not (funcall $f $x)))"
                   << "        ((eq $opt 'funcall)"
                   << "            (funcall $f $x))))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$test-value-against-"));
            }

            { // filter-testing-against
                std::stringstream ss;
                ss << "(defun filter-testing-against ($f $list $opt)"
                   << "  (let (($result nil))"
                   << "    (dolist ($x $list)"
                   << "      (if (test-value-against $f $x $opt)"
                   << "          (push $x $result)))"
                   << "    (setq $result (reverse $result))"
                   << "    (if (vectorp $list)"
                   << "        (coerce $result 'vector)"
                   << "      $result)))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$filter-testing-against-"));
            }

            { // remove
                std::stringstream ss;
                ss << "(defun remove ($f $list)"
                   << "  \"(remove item sequence)\""
                   << "  \"  returns a copy of sequence, with all the occurences of item removed.\""
                   << "  \"  sequence can be a list or a vector.\""
                   << "  (filter-testing-against $f $list 'noteq))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$remove-"));
            }

            { // remove-if
                std::stringstream ss;
                ss << "(defun remove-if ($f $list)"
                   << "  \"(remove-if f sequence)\""
                   << "  \"  returns a copy of sequence, with each element removed if non-nil (i.e. true) is received when that element is given to the callable object f as an argument.\""
                   << "  \"  sequence can be a list or a vector.\""
                   << "  (filter-testing-against $f $list 'not))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$remove-if-"));
            }

            { // remove-if-not
                std::stringstream ss;
                ss << "(defun remove-if-not ($f $list)"
                   << "  \"(remove-if-not f sequence)\""
                   << "  \"  returns a copy of sequence, with each element removed if nil (i.e. false) is received when that element is given to the callable object f as an argument.\""
                   << "  \"  sequence can be a list or a vector.\""
                   << "  (filter-testing-against $f $list 'funcall))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$remove-if-not-"));
            }


            { // nthcdr
                std::stringstream ss;
                ss << "(defun nthcdr ($n $list)"
                   << "  \"(nthcdr n mylist)\""
                   << "  \"  returns the rest of mylist after skipping its first n elements\""
                   << "  (let (($i 0))"
                   << "    (while (< $i $n)"
                   << "      (setq $list (cdr $list))"
                   << "      (setq $i (+ 1 $i))))"
                   << "  $list)";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$nthcdr-"));
            }

            { // set-nthcdr
                std::stringstream ss;
                ss << "(defun set-nthcdr ($n $list $x)"
                   << "  \"(set-nthcdr n mylist list2)\""
                   << "  \"  sets the rest of the list mylist after skipping its first n elements, to list2\""
                   << "  \"EXAMPLE:\""
                   << "  \"(let ((a (list 1 2 3 4))\""
                   << "  \"      (b (list 'x 'y 'z)))\""
                   << "  \"  (print a)  ; prints (1 2 3 4)\""
                   << "  \"  (set-nthcdr 2 a b)\""
                   << "  \"  (print a)) ; prints (1 2 x y z)\""
                   << "  (let (($i 1))"
                   << "    (while (< $i $n)"
                   << "      (setq $list (cdr $list))"
                   << "      (setq $i (+ 1 $i))))"
                   << "  (setcdr $list $x))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$set-nthcdr-"));
            }

            { // position-testing-against
                std::stringstream ss;
                ss << "(defun position-testing-against ($f $items $opt)"
                   << "    (let (($i 0) ($result nil))"
                   << "        (if (listp $items)"
                   << "            (while $items"
                   << "                (let (($x (car $items)))"
                   << "                    (if (test-value-against $f $x $opt)"
                   << "                        (progn"
                   << "                            (setq $result $i)"
                   << "                            (setq $items nil))"
                   << "                      (progn"
                   << "                          (setq $items (cdr $items))"
                   << "                          (setq $i (+ $i 1))))))"
                   << "            (let (($n (length $items)))"
                   << "                (while (< $i $n)"
                   << "                    (if (test-value-against $f (elt $items $i) $opt)"
                   << "                        (progn"
                   << "                            (setq $result $i)"
                   << "                            (setq $i $n))"
                   << "                      (setq $i (+ $i 1))))))"
                   << "        $result))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$position-testing-against-"));
            }

            { // position

                std::stringstream ss;
                ss << "(defun position ($f $items)"
                   << "  \"(position item sequence)\""
                   << "  \"  returns the position index of the first occurence of item within sequence\""
                   << "  (position-testing-against $f $items 'eq))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$position-"));
            }

            { // position-if

                std::stringstream ss;
                ss << "(defun position-if ($f $items)"
                   << "  \"(position-if f sequence)\""
                   << "  \"  returns the position index of the first occurence of an element within sequence, if non-nil (i.e. true) is received after calling the callable object f with that element as argument\""
                   << "  (position-testing-against $f $items 'funcall))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$position-if-"));
            }

            { // position-if-not

                std::stringstream ss;
                ss << "(defun position-if-not ($f $items)"
                   << "  \"(position-if-not f sequence)\""
                   << "  \"  returns the position index of the first occurence of an element within sequence, if nil (i.e. false) is received after calling the callable object f with that element as argument\""
                   << "  (position-testing-against $f $items 'not))";

                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$position-if-not-"));
            }

            { // make-list
                std::stringstream ss;
                ss << "(defun make-list ($n $x)"
                   << "  \"(make-list n x)\""
                   << "  \"  creates and returns a list containing n nodes, each node storing x\""
                   << "  (let (($i 0) ($result nil))"
                   << "    (while (< $i $n)"
                   << "      (push $x $result)"
                   << "      (setf $i (+ $i 1)))"
                   << "    $result))";
                main_scope.evaluate(strreplace<std::string>(ss.str(), "$", "$make-list-"));
            }

            { // not
                Ref<LispObject> not_op = objrefnew<NotOperator>();
                main_scope.set_callable("not", not_op);
                main_scope.set_callable("null", not_op);
            }
            main_scope.set_callable("and", objrefnew<AndOperator>());
            main_scope.set_callable("or", objrefnew<OrOperator>());

            main_scope.set_callable("type-of", objrefnew<TypeOf>());

            main_scope.set_callable("number-to-string", objrefnew<NumberToString>());
            main_scope.set_callable("string-to-number", objrefnew<StringToNumber>());

            main_scope.set_callable("symbol-to-string", objrefnew<SymbolToString>());
            main_scope.set_callable("string-to-symbol", objrefnew<StringToSymbol>());

            main_scope.set_callable("eval", objrefnew<Evaluator>());
            main_scope.set_callable("eval-file", objrefnew<EvalFile>());
            main_scope.set_callable("list", objrefnew<ListConstructor>());
            main_scope.set_callable("vector", objrefnew<VectorConstructor>());
            main_scope.set_callable("make-vector", objrefnew<MakeVector>());
            main_scope.set_callable("sort", objrefnew<Sorter>());

            main_scope.set_callable("length", objrefnew<LengthCalculator>());
            { // elt, aref
                Ref<LispObject> elt = objrefnew<Elt>();
                Ref<LispObject> setelt = objrefnew<SetElt>();
                main_scope.set_callable("elt", elt);
                main_scope.set_callable("set-elt", setelt);
                main_scope.set_callable("aref", elt);
                main_scope.set_callable("set-aref", setelt);
            }

            main_scope.set_callable("substring", objrefnew<Substring>());

            main_scope.set_callable("concatenate", objrefnew<Concatenator>());

            main_scope.set_callable("error", objrefnew<ErrorRaiser>());

            main_scope.set_callable("message", objrefnew<OutputTool>(false, true));
            main_scope.set_callable("princ", objrefnew<OutputTool>(false, false));
            main_scope.set_callable("print", objrefnew<OutputTool>(true, true));

            main_scope.set_callable("read-string", objrefnew<StringInputTool>());
            main_scope.set_callable("read", objrefnew<Reader>());
            main_scope.evaluate("(defun read-number ($read-number-msg) \"(read-number msg)\" \"  writes msg to the screen and returns the number read from the standard input\" (string-to-number (read-string $read-number-msg)))");

            main_scope.evaluate("(defext progn (&rest $progn-actions) \"(progn action1 action2 ...)\" \"  executes all the specified action and returns the evaluation result of the last executed action\" (eval (list 'cond (cons t $progn-actions))))");

            main_scope.evaluate("(defext if ($if-condition $if-then &rest $if-otherwise) \"(if condition action)\" \"  if condition is non-nil (i.e. true), evaluates action and returns the result; otherwise returns nil.\" \"(if condition action otherwise-action1 otherwise-action2...)\" \"  if condition is non-nil (i.e. true), evaluates action and returns the result; otherwise, evaluates otherwise-actions and returns the result.\" (cond ((eval $if-condition) (eval $if-then)) (t (eval (cons 'progn $if-otherwise)))) )");

            main_scope.evaluate("(defext when ($when-condition &rest $when-actions) \"(when condition action1 action2 ...)\" \"  if condition is non-nil (i.e. true), executes all the actions and returns the result of the last action; otherwise, returns nil\" (if (eval $when-condition) (eval (cons 'progn $when-actions))))");

            main_scope.evaluate("(defun nth ($nth-n $nth-seq) \"(nth n seq) -> nth element of the sequence seq.\" \"seq can be a list, a vector, or a string.\" (elt $nth-seq $nth-n))");
            main_scope.evaluate("(defun set-nth ($set-nth-n $set-nth-seq $set-nth-val) \"(set-nth n seq x)\" \"  sets the n-th element of the sequence(list or a vector) seq to x\" (set-elt $set-nth-seq $set-nth-n $set-nth-val))");
            main_scope.evaluate("(defext concat (&rest $concat-args) \"(concat a b c ...)\" \"  returns a string which is the concatenation of all the arguments\" (eval (concatenate 'list '(concatenate 'string) $concat-args)))");

            main_scope.evaluate("(defun stringp   ($stringp-x)   \"(stringp x) -> non-nil (true) if x is a string; otherwise nil\" (equal (type-of $stringp-x) 'string))");
            main_scope.evaluate("(defun numberp   ($numberp-x)   \"(numberp x) -> non-nil (true) if x is a number; otherwise nil\" (equal (type-of $numberp-x) 'number))");
            main_scope.evaluate("(defun symbolp   ($symbolp-x)   \"(symbolp x) -> non-nil (true) if x is a symbol; otherwise nil\" (equal (type-of $symbolp-x) 'symbol))");
            main_scope.evaluate("(defun consp     ($consp-x)     \"(consp x) -> non-nil (true) if x is a cons object; otherwise nil\" (equal (type-of $consp-x) 'cons))");
            main_scope.evaluate("(defun atom      ($atom-x)      \"(atom x) -> non-nil (true) if x is an atom (not a cons object); otherwise nil\" (not (consp $atom-x)))");
            main_scope.evaluate("(defun listp     ($listp-x)     \"(listp x) -> non-nil (true) if x is a list (i.e. a cons object or nil); otherwise nil (i.e. false)\" (or (not $listp-x) (consp $listp-x)))");
            main_scope.evaluate("(defun vectorp   ($vectorp-x)   \"(vectorp x) -> non-nil (true) if x is a vector; otherwise nil\" (equal (type-of $vectorp-x) 'vector))");
            main_scope.evaluate("(defun nlistp    ($nlistp-x)    \"(nlistp x) -> non-nil (true) if x is not a list; otherwise nil\" (not (listp $nlistp-x)))");
            main_scope.evaluate("(defun functionp ($functionp-x) \"(functionp x) -> non-nil (true) if x is a callable object; otherwise nil\" (eq (type-of $functionp-x) 'callable))");

            main_scope.set_callable("ceiling", Ref<MathFunction>(new MathFunction("ceiling", ceil_as_double )).as<LispObject>());
            main_scope.set_callable("floor",   Ref<MathFunction>(new MathFunction("floor",   floor_as_double)).as<LispObject>());
            main_scope.set_callable("round",   Ref<MathFunction>(new MathFunction("round",   round_double   )).as<LispObject>());
            main_scope.set_callable("abs",     Ref<MathFunction>(new MathFunction("abs",   std::abs   )).as<LispObject>());
            main_scope.set_callable("sin",     Ref<MathFunction>(new MathFunction("sin",   std::sin   )).as<LispObject>());
            main_scope.set_callable("cos",     Ref<MathFunction>(new MathFunction("cos",   std::cos   )).as<LispObject>());
            main_scope.set_callable("tan",     Ref<MathFunction>(new MathFunction("tan",   std::tan   )).as<LispObject>());
            main_scope.set_callable("asin",    Ref<MathFunction>(new MathFunction("asin",  std::asin  )).as<LispObject>());
            main_scope.set_callable("acos",    Ref<MathFunction>(new MathFunction("acos",  std::acos  )).as<LispObject>());
            main_scope.set_callable("atan",    Ref<MathFunction>(new MathFunction("atan",  std::atan  )).as<LispObject>());
            main_scope.set_callable("log",     Ref<MathFunction>(new MathFunction("log",   std::log   )).as<LispObject>());
            main_scope.set_callable("log10",   Ref<MathFunction>(new MathFunction("log10", std::log10 )).as<LispObject>());
            main_scope.set_callable("sqrt",    Ref<MathFunction>(new MathFunction("sqrt",  std::sqrt  )).as<LispObject>());
            main_scope.set_callable("exp",     Ref<MathFunction>(new MathFunction("exp",   std::exp   )).as<LispObject>());
            main_scope.set_callable("lognot",  Ref<MathFunction>(new MathFunction("lognot", bitwise_not_as_double)).as<LispObject>());
            main_scope.set_callable("expt",    Ref<MathFunction2>(new MathFunction2("expt", std::pow)).as<LispObject>());
            main_scope.set_callable("%",       Ref<MathFunction2>(new MathFunction2("%", remainder_as_double)).as<LispObject>());
            main_scope.set_callable("ash",     Ref<MathFunction2>(new MathFunction2("ash", bitwise_shift_as_double)).as<LispObject>());
            main_scope.evaluate("(setq float-e (exp 1))");
            main_scope.evaluate("(setq float-pi (* 2 (asin 1)))");
            main_scope.evaluate("(setq e float-e)");
            main_scope.evaluate("(setq pi float-pi)");

            main_scope.set_callable("apropos", objrefnew<AproposCommand>(true));
            main_scope.set_callable("names", objrefnew<AproposCommand>(false));

            Ref<LispObject> quitter = objrefnew<Quit>();
            main_scope.set_callable("quit", quitter);
            main_scope.set_callable("exit", quitter);
        }

    }
}

#endif
