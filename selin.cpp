#include <clocale>
#include <fstream>
#include <iostream>
#include <string>
#include <list>

//#define SELIN_USE_TR1_UNORDERED_MAP
#include "selin.hpp"

using namespace std;
using namespace selin;


void repl()
{
    Scope sc;
    sc.repl();
}


void run_script_file(std::string s)
{
    Scope sc;

    try
    {
        sc.run_script_file(s);
    }
    catch (Ref<LispException> ex)
    {
        cout << "ERROR: " << ex->to_string() << endl;
    }
}


int main(int argc, char *argv[])
{
    setlocale(LC_ALL,"");

    if (argc >= 2)
    {
        run_script_file(argv[1]);
    }
    else
    {
        repl();

        if (alive_objects_count > 0)
        {
            cout << alive_objects_count << " objects are still alive!" << endl;
        }
    }

    return 0;
}
