#include <iostream>
#include "selin.hpp"

// this is a longer version of example_embed1.cpp
// this longer version can also be a useful example
// because it contains data type casting
// within its code

// selin.hpp provides and uses a reference-counting
// pointer class template called Ref<>.
using selin::Ref;

Ref<selin::LispObject> sum(Ref<selin::LispNode> args)
{
    double result = 0;

    // we iterate over all the argument nodes:
    for (Ref<selin::LispNode> arg = args;
         arg.is_not_null();
         arg = arg->cdr())  // cdr() gets next node
    {
        Ref<selin::LispObject> obj;
        obj = arg->car(); // car() gets the object stored by the node

        if (selin::type_of(obj) == "number")
        {
            result += selin::as_number(obj);
        }
    }

    // we have the result
    // now we create a lisp number object holding that result:
    Ref<selin::LispNumber> n(new selin::LispNumber(result));

    // now we cast it into an object:
    Ref<selin::LispObject> o(n.as<selin::LispObject>());

    // finally, return the result:
    return o;
}

int main()
{
    selin::Scope main_scope;
    main_scope.set_native_callable("sum", sum);
    main_scope.repl();

    return 0;
}
