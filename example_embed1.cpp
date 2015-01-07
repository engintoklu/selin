#include <iostream>
#include "selin.hpp"

// selin.hpp provides and uses a reference-counting
// pointer class template called Ref<>.
using selin::Ref;

Ref<selin::LispObject> sum(Ref<selin::LispNode> args)
{
    int count = 0;
    double result = 0;

    // we iterate over all the argument nodes:
    for (selin::LispNode::iterator it = args->begin();
         it != args->end();
         ++it)
    {
        Ref<selin::LispObject> obj(*it);

        // Below, we are testing if the object is number
        // by using selin::is_number(obj)
        // also, we could use:
        //  selin::is_string(obj) if we wanted to check if it is a string
        //  selin::is_list(obj) if we wanted to check if it a list
        //  selin::is_symbol(obj) if we wanted to check if it is a symbol
        //  selin::is_vector(obj) if we wanted to check if it is a vector
        if (selin::is_number(obj))
        {
            result += selin::as_number(obj);
        }
    }

    return selin::create_number_object(result);
    // we could use selin::create_string_object("...")
    // if we wanted to create a string object,
    // and, similarly, selin::create_symbol_object("...")
    // if we wanted to create a symbol object
}

int main()
{
    selin::Scope main_scope;
    main_scope.set_native_callable("sum", sum);
    main_scope.repl();

    return 0;
}
