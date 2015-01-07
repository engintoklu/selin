#include <iostream>
#include <list>
#include <string>
#include <algorithm>
#include "selin.hpp"

// selin.hpp provides and uses a reference-counting
// pointer class template called Ref<>.
using selin::Ref;

// below is a function which writes the data type
// of a lisp object, and then its value as string
void explain_object(Ref<selin::LispObject> obj)
{
    // selin::type_of(obj) -> data type name of an object as std::string
    // selin::as_string(obj) -> value of an object as std::string
    std::cout << "type:" << selin::type_of(obj)
              << ", as string:" << selin::as_string(obj)
              << std::endl;
}

// below is a function which calls explain_object
// for each node of the list it receives.
void explain_args(Ref<selin::LispNode> args)
{
    int count = 0;
    double result = 0;

    // we iterate over all the argument nodes:
    for (selin::LispNode::iterator it = args->begin();
         it != args->end();
         ++it)
    {
        Ref<selin::LispObject> obj(*it);

        explain_object(obj);
    }
}


// explain_args2 does the same thing
// with explain_args, but by using std::for_each
void explain_args2(Ref<selin::LispNode> args)
{
    int count = 0;
    double result = 0;

    // we iterate over all the argument nodes:
    std::for_each(args->begin(), args->end(), explain_object);
}


Ref<selin::LispObject> give_me_a_list(Ref<selin::LispNode> args)
{
    if (args.is_not_null()) // equivalent to: if (!(args.is_null()))
    {
        std::cout << "I was not expecting arguments" << std::endl;
        return Ref<selin::LispObject>(NULL);
    }

    std::list< Ref<selin::LispObject> > lst_objs;
    lst_objs.push_back(selin::create_number_object(1));
    lst_objs.push_back(selin::create_string_object("two"));
    lst_objs.push_back(selin::create_symbol_object("three"));

    return selin::create_list_object(lst_objs);
}

Ref<selin::LispObject> give_me_a_vector(Ref<selin::LispNode> args)
{
    if (args.is_not_null()) // equivalent to: if (!(args.is_null()))
    {
        std::cout << "I was not expecting arguments" << std::endl;
        return Ref<selin::LispObject>(NULL);
    }

    std::list< Ref<selin::LispObject> > lst_objs;
    lst_objs.push_back(selin::create_number_object(1));
    lst_objs.push_back(selin::create_string_object("two"));
    lst_objs.push_back(selin::create_symbol_object("three"));

    return selin::create_vector_object(lst_objs);
}

int main()
{
    selin::Scope main_scope;
    main_scope.set_native_callable("explain-args", explain_args);
    main_scope.set_native_callable("explain-args2", explain_args2);
    main_scope.set_native_callable("give-me-a-list", give_me_a_list);
    main_scope.set_native_callable("give-me-a-vector", give_me_a_vector);
    main_scope.repl();

    return 0;
}
