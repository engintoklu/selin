#include <iostream>
#include "selin.hpp"

void say_hello()
{
    std::cout << "Hello!" << std::endl;
}

int main()
{
    selin::Scope main_scope;
    main_scope.set_native_callable("say_hello", say_hello);
    main_scope.repl();

    return 0;
}
