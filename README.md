
# selin - a minimalist Lisp dialect for scripting

Nihat Engin Toklu < http://github.com/engintoklu >, 2014, 2015

## What is selin

selin is a minimalist scripting language developed in C++.
The selin interpreter is written as a C++ header library,
therefore embedding the interpreter is as easy as copying
the header files into the directory of your C++ project
and then including those header files as follows:

    #include "selin.hpp"

The selin header library is written in a portable way,
and it does not require anything other than the
standard libraries of C++.

The language selin is inspired by Emacs Lisp (see (EmacsLisp))
and designed to have basic compatibility with it.
Therefore, hopefully, you will be able to directly use simple
Emacs Lisp code snippets in selin.

Unlike many well-known Lisp dialects, including Emacs Lisp,
automatic memory management of selin relies on reference counting.
Reference counting has the disadvantage of not being able to reclaim
the memory taken by the objects which refer to each other
circularly (e.g. object `a` points to object `b`,
and object `b` points to object `a`),
even if those objects are not needed anymore.
Therefore, selin is not suitable for complicated algorithms
where cyclic references are found.
On the other hand, considering simple algorithms
without such cyclic references,
reference counting has the advantage
of removing an object as soon as its lifetime ends.

The portable, embeddable nature of selin makes it ideal
for basic scripting tasks.

## Programming in selin

Because selin is a Lisp dialect, you might want to check
out some online articles like (McCarthy, Wikipedia, Graham),
if you would like a general introduction to Lisp.

### Basics

As a Lisp dialect, at the heart of selin is an evaluator.
When this evaluator receives a list, it evaluates that list
and returns the result. For example, consider the following
list:

    (+ 1 2 3)

When the evaluator receives this list, it takes the symbol `+`
as the command, and the rest of this list as arguments.
The result of this evaluation is 6.
The evaluator runs recursively, therefore lists within
lists are evaluated, allowing us to give sub-expressions:

    (+ 1 2 (* 2 3))

and the result will be 9.

Now, let us imagine that we give the evaluator the following:

    x

In this case, the evaluator takes `x` as a symbol,
and looks for the variable named x, and returns its value
(or raises an error if the variable x is not found).

In addition to lists and symbols, selin has strings:

    "this is a string"

and numbers:

    3
    3.3
    3,3
    -7
    2_000_000

Notice that, in selin, both dot (.) and comma (,)
can be used as the decimal separator,
and the underscore (_) can be used as the whitespace
in number literals.

When the evaluator receives a string object
or a number object, it returns that object itself
(this means `3` returns `3`, and `"abc"` returns `"abc"`).

If you want to stop the evaluator from evaluating
lists and symbols, you can use the quote command:

    (quote x)

returns the symbol `x`, instead of the value of variable x.
Also:

    (quote (+ 1 2 (* 3 2)))

returns the list `(+ 1 2 (* 3 2))`, instead of 9.
There is also a shortcut syntax for quote:

    'x
    '(+ 1 2 (* 3 2))

The quotation syntax can be used to produce lists.
However, quotation will stop the evaluation of the list
entirely.
For example, the following expression

     '(a b c)

will return the list as it is: `(a b c)`.
But maybe, the programmer meant to create list
containing the values of variables a, b, and c, respectively.
In that case, the list command can be used:

    (list a b c)

And a list containing the values of a, b, and c will be created.

For writing comments, semicolon (;) can be used:

    ; this is a comment
    (+ 1 2)   ; this is another comment

### Boolean Logic

An empty list `()` is equivalent to nothingness (`nil`),
and represents falsehood in terms of boolean logic.
Anything which is not `nil` is an object which exists in memory,
and represents truth in terms of boolean logic.

See the following examples:

    ; equality checks:
    (= 1 1)   ;returns the symbol t to represent truth
    (= 1 0)   ;returns () to represent falsehood

    (and (= 1 1) (= 2 2))   ;returns t
    (and (= 1 1) (= 2 1))   ;returns ()
    (or  (= 1 1) (= 1 2))   ;returns t
    (or  (= 2 1) (= 3 1))   ;returns ()

Also, note that the commands `and` and `or` do short-circuiting.
This means, in the following example, the third argument
is not evaluated at all, because the outcome of the operation
is apparent from the first two arguments:

    (and (= 1 1) (= 3 2) (= 2 2))   ; returns ()

Finally the boolean `not` is as follows:

    (not ())    ; returns t
    (not 't)    ; returns ()

    (not "xx")  ; anything which is not () represents truth.
                ; the negation of truth is (),
                ; which is the result in this example.


### Data types, and basic operations on them

#### Numbers
Because selin is minimalistic, only one numeric data type is
supported, under the name `number`.
The selin interpreter represents a `number` by using
`double` type of C++.
This minimalistic approach is inpired by Lua programming language
(Lua, LuaNumbers).

* **Emacs Lisp compatibility note:**
In Emacs Lisp, there is a data type (`integer`) for integers
and another data type (`float`) for real numbers.
In selin, there is only one numeric data type (`number`).

Some operations on numbers are as follows:

    (+ 1 2)    ;adds 1 and 2
               ; also supported are - * and /

    (> 2 1)    ;checks if 2>1, and returns the result
               ;(t in the case of this example)
               ; also supported are >= < <= =
               ; and /= meaning not-equal

    (> 3 2 1)  ;checks if 3>2 and 2>1,
               ;and returns the result
               ;(t in the case of this example)

    (sin 0)    ;returns sin of 0.
        ;also supported are cos,asin,acos,abs,round...

    (expt 2 3)  ;returns 2 to the power of 3
    (exp 2)     ;returns e to the power of 2

#### Strings
In selin, string data type is supported,
and a string literal is expressed as `"abc"`.
Like in Emacs Lisp, backslash syntax is supported
within string literals:
`"this is a new-line character: \n"`,
`"this is a return character: \r"`,
`"this is a tab character: \t"`,
`"this is a single-quote character: \'"`,
`"this is a double-quote character: \""`.

Some operations on strings are:

    (length "abc")   ;length of the string
                     ;(3 in this example)

    (concat "aa" "bb")  ;concatenation of strings
                        ;result: "aabb"

    (substring "01234" 2 4)  ;substring of the string
                ;beginning from 2-nd character,
                ;up to (excluding) 4-th character.
                ;character indexing starts from 0.
                ;So, result: "23"

    (string= "a" "a")  ;equality check on two strings.
        ;returns t in this example.
        ;also supported are:
        ;string/=  string>  string<  string>=  string<=

#### Symbols
A symbol is an object which is used to represent
variable names or keywords.
A symbol object can be created by using:

    'x

(and not just `x`, because that would trigger the evaluator
to look for the value of a variable called `x`).
Equality check on symbols is done as follows:

    (eq 'a 'a)  ;returns t in this example

#### Lists
A list can be generated via: `(list 1 2 3)`.

In the memory, a list is represented by list nodes,
each node storing the element value
and also pointing to the next node, as follows:

    [ 1 | *-]--> [ 2 | *-]--> [ 3 | nil ]
      ^   ^
      |   |
      |   \--- pointer to the next node
      |
      \---- stored value

Some operations on lists are:

    (first (list 1 2 3))  ;returns the first value: 1

    (rest (list 1 2 3))
        ;skips the first node in the list
        ;and returns the rest of it.
        ;So, it returns: (2 3)

    ;aliases to first and rest are car and cdr:
    (car (list 1 2 3))   ;returns 1
    (cdr (list 1 2 3))   ;returns (2 3)

    (rest (list 1))
    ;the list (1) does not have further nodes,
    ;so returns: ()

    (length (list 1 2 3))  ;length of the list
                           ;result: 3

    (concatenate 'list (list 1 2) (list 3 4))
         ;concatenation of the lists.
         ;result: (1 2 3 4)

    (elt (list 'a 'b 'c 'd) 2)
         ;the element of the list with index 2.
         ;considering that indices start from 0,
         ;this returns c.

    (equal (list 1 2 3) (list 1 2 3))
        ;recursive, element-wise equality check
        ;returns t in this example.

    (eval (list '+ 1 2 3))
        ;evaluates the code stored by the list
        ;the list is (+ 1 2 3), so the result: 6

#### Vectors
Vectors are random-access containers.
A vector can be created via `(vector 1 2 3)`.

Some operations on vectors are:

    (length (vector 1 2 3))  ;length of the vector
                             ;result: 3

    (concatenate 'vector (vector 1 2) (vector 3 4))
         ;concatenation of the vectors.
         ;result: [1 2 3 4]

    (elt (vector 'a 'b 'c 'd) 2)
         ;the element of the vector with index 2.
         ;considering that indices start from 0,
         ;this returns c.

    (equal (vector 1 2 3) (vector 1 2 3))
        ;recursive, element-wise equality check
        ;returns t in this example.



#### Querying the data type of an object

The data type of an object can be learned by using `type-of`.
The command `type-of` returns the data type
of the object given to it as argument, as a symbol.
See the following examples:

    (type-of 3)     ;returns number
    (type-of 'aa)   ;returns symbol
    (type-of "aa")  ;returns string
    (type-of (list 1 2 3))  ;returns cons
    (type-of (vector 1 2 3))  ;returns vector

    ;Querying the data type of the function + :
    (type-of (function +))  ;returns callable

Other than type-of, you can use the following
predicate functions which return `t` (truth)
or `()` (falsehood):

    (numberp 3)               ;returns t
    (numberp "a")             ;returns ()
    (listp (list 1 2 3))      ;returns t
    (listp "a")               ;returns ()
    (listp ())                ;returns t
    (stringp "a")             ;returns t
    (symbolp 'a)              ;returns t
    (functionp (function +))  ;returns t

#### Conversion between data types

    (number-to-string 3)   ;returns "3"
    (string-to-number "3") ;returns 3
    (string-to-symbol "a") ;returns a
    (symbol-to-string 'a)  ;returns "a"
    (coerce (list 1 2 3) 'vector) ;returns [1 2 3]
    (coerce (vector 1 2 3) 'list) ;returns (1 2 3)

### Common programming tasks

Printing something to the screen:

    (print "Hello!")

Reading a string and a number from the standard input:

    (read-string "Enter a string:") ;returns the entered string
    (read-number "Enter a number:") ;returns the entered number

Assigning a value to a variable `x` is done by using
`setf` ("set field") command:

    (setf x 3)
    (print x)   ;outputs 3

Defining local variables:

    (let ((a 1) (b 2))
        (print a)   ;outputs 1
        (print (+ a b))  ;outputs 3
    )

    (print a)  ;ERROR: variable a does not exist here

#### Controlling the program flow

You can use the `if` statement to execute operations
based on some conditions:

    (setf x 4)

    (if (> x 3)
        (print "yes! x is bigger than 3")
    )

    (if (> x 7)
        (print "x is bigger than 7")
      (print "this is the else part.")
    )
    ;outputs: "this is the else part."

`progn` is a command which evaluates all its arguments
and returns the latest evaluation result.

    (progn
        (print "hey")
        (print "hello")
        (+ 1 2)
    )
    ; prints "hey" and then "hello", and returns 3

You can use `progn` to do more than one thing
in the "then" part or the "else" part of `if`:

    (setf x (read-number "Enter a number:"))

    (if (> x 0)
        (progn
            (print "this is a positive number")
            (print "sign: +")
        )
      (progn
          (print "this is the else part")
          (print "the number is not positive")
      )
    )

For handling multiple conditions, you can use `cond`
instead of `if`. For example:

    (setf x (read-number "Enter a number:"))

    (cond
        ((> x 0)
            (print "this is a positive number")
            (print "sign: +")
        )
        ((< x 0)
            (print "this is a negative number")
            (print "sign: -")
        )
        ((= x 0)
            (print "this is zero")
        )
    )

#### Loops

A while-loop is done as follows:

    (setf x 0)
    ; write the value of x until it reaches 10

    (while (< x 10)
        (print x)
        (setf x (+ x 1))
    )

A shorter version of the loop above is:

    (dotimes (x 10)
        (print x)
    )

Iterating over the elements of a list:

    (setf fruits (list 'apple 'banana 'orange))

    (dolist (fruit fruits)
        (print fruit)
    )
    ; prints each fruit

#### Functions and macros

You can define your own function by using `defun`:

    (defun say-hi (person-name)
        (print "Hello!")
        (print (concat "Nice to meet you, " person-name))
    )

    (say-hi "user")
    ; prints:
    ; "Hello!"
    ; "Nice to meet you, user"


    (defun add-two-numbers (a b)
        (+ a b)
    )

    (add-two-numbers 3 5)   ;returns 8

You can access to function objects by using `function`:

    (defun f ()
        (print "Hello!")
    )

    (f)   ;prints "Hello!"

    (type-of (function f))   ;returns callable

    (setf myfunc (function f))
    ;the variable myfunc now points to function f

    ;let us execute the function pointed to by myfunc:
    (funcall myfunc)  ;prints "Hello!"

In selin, when you define a function, the code
within that function accepts the global namespace
as its parent namespace.

    (setf x 3)  ;we have a global variable x, with value 3

    (defun increment-x ()
        (setf x (+ x 1))  ;direct access to global x
    )

    (increment-x)
    (print x) ;prints 4

Note that, in selin, the parent namespace of a function by default
is always global namespace, even if that function
is created within a non-global scope:

    (setf x 3)  ;we have a global variable x, with value 3

    (let ((x 7))
        ;this is another scope
        ;another x is defined here, with value 7

        (defun increment-x ()
            (setf x (+ x 1))  ;direct access to global x
        )

        (print x)  ;prints 7, this non-global x is not changed.
    )

    (print x) ;prints 4. the increment-x modified global x.

* **Emacs Lisp compatibility note:**
In Emacs Lisp, the scoping rule is different.
By default, Emacs Lisp uses dynamic scoping.
This means, when a function looks for a variable `x`,
it looks for the closest `x` within the callstack to its
own scope.
With dynamic scoping, the example above would
print 8 and 3, instead of 7 and 4.
If you want to have this behavior in selin,
you can write `(setf selin-scoping 'dynamic)`
before defining your functions.
With this configuration, the functions you define
will work with dynamic scoping rules.
By default, the value of `selin-scoping` is `global`.
Since GNU Emacs version 24.1, Emacs Lisp optionally
supports lexical scoping. In lexical scoping, functions
always remember in which scope they were created,
and accept that scope as their parent scope.
In selin, lexical scoping is not supported.

To define a macro, you can use `defext`,
which stands for define language extension.
A language extension is very much like a function,
except that it receives its arguments un-evaluated.
For example, if we have a function f,
executing `(f (+ 1 2))` would trigger the evaluation
of `(+ 1 2)`, and then would pass the result 3
as argument for `f`.
However, assuming that we have a language extension `ex`,
executing `(ex (+ 1 2))` would just pass `(+ 1 2)`
to `ex` as argument. It is then up to `ex` to evaluate `(+ 1 2)`.
Also, a language extension behaves according to the
rules of dynamic scoping, which means it runs
on the closest scope in the callstack.

Let us assume that, in addition to the `if` statement,
the programmer would like to have an `unless` statement.
We can define that `unless` statement as follows:

    (defext unless ($condition $action)
        (if (not (eval $condition))
            (eval $action)
        )
    )

that is, if the evaluation of `$condition` does not hold,
then `$action` is evaluated.
The dollar sign ($) in the variable names is not a special syntax,
it is just a way to avoid variable name conflict.
Now, an example usage of our `unless` statement:

    (setf x (read-number "Please enter a non-negative number:"))

    (unless (< x 0)
        (print "Thank you")
    )

* **Emacs Lisp compatibility note:**
In Emacs Lisp, also in Common Lisp, instead of defext,
there is a statement called `defmacro` to define macros.
In selin, `defmacro` is not supported.

#### Error handling

To raise an error:

    (signal 'my-error "Further information about the error")

The command above raises an error of type `my-error`,
with the additional data `"Further information about the error"`.
The additional data do not have to be of string type.
The following are all valid executions of `signal`:

    (signal 'my-error 23)
    (signal 'my-error 'details)
    (signal 'my-error (list 'aa 33 "details"))

By using the statement `condition-case`,
you can execute some code and specify how
you want to handle what kind of error.
You can consider `condition-case` as the Emacs Lisp-
(and selin-) equivalent of `try...catch` statement of C++.

Let us now look at the following code:

    (condition-case err
        (progn
            (print "Hello!")
            (signal 'my-error "details")
            (print "Bye.")
        )
        (my-error
            (print "I caught my-error")
            (print (error-type err))  ;prints my-error
            (print (error-data err))  ;prints "details"
        )
    )

In the code above, the interpreter will try to execute:

    (progn
        (print "Hello!")
        (signal 'my-error "details")
        (print "Bye.")
    )

When the error of type `my-error` is raised
(and this will happen in this particular example,
because we execute the `signal` command without depending
on any condition, so, an error will be raised
and we will not reach `(print "Bye.")`)
the control of execution is transferred to:

    (print "I caught my-error")
    (print (error-type err))  ;prints my-error
    (print (error-data err))  ;prints "details"

in which the variable `err` stores information about the error.
Note that you can write code to handle more than one
error type.
Also, you can write a handler for any type of error
by just writing `error` as the error type for the handler.
See the example below:

    (condition-case err
        (do-something)
        (my-error1
            (print "I caught my-error1")
        )
        (my-error2
            (print "I caught my-error2")
        )
        (error
            (print "I catch any type of error which is not handled above")
        )
    )

* **Emacs Lisp compatibility note:**
In Emacs Lisp, considering that the error information is
stored by the variable `err`,
we would use `(car err)` and `(cdr err)`
to get the error type and additional error data respectively,
instead of `(error-type err)` and `(error-data err)`.

Also, sometimes we would like to make sure that some
code are executed in a scope, even if an error is raised
in that scope and we have to leave that scope unexpectedly.
In this case, we use `unwind-protect`, which is
Emacs Lisp- and selin-equivalent of Java and Python's `finally`
clause.

An `unwind-protect` statement is structured as follows:

    (unwind-protect
        (this-might-or-might-not-finish-successfully)
        (this-will-be-executed-anyway)
        (this-will-be-executed-anyway-too)
        ...
    )

To elaborate more:

    (unwind-protect
        (progn
            (print "Maybe an error will be raised")
            (do-something)
            ;if (do-something) raises an error,
            ;the line below will not work:
            (print "(do-something) worked successfully")
        )

        (print "With or without error, this will be printed")
        (print "This too")
    )

### Getting further help about selin

In the selin interpreter environment, you can use the `apropos` command
to see the names of all variables and callables
accessible from the current scope:

    (apropos)

Also, for example, if you would like to see all the string-related
functions, you can write:

    (apropos "string")

and all the functions (and variables if defined) with `"string"`
in their names will be shown.
The listed function objects are shown with their prefix `callable:`.
For example, the function `substring` will be listed as
`callable:substring`. You can reach this `substring` function object
by writing `callable:substring`, or `(function substring)`.
If you type one of these options into the interpreter's input,
the interpreter will show you the documentation about
the `substring` command.

## Embedding selin in a C++ program

Embedding the selin interpreter in a C++ program
is very easy, because selin is written as a header library.
The following example triggers selin's REPL (read-eval-print-loop):

    #include "selin.hpp"

    int main()
    {
        selin::Scope main_scope;
        main_scope.repl();

        return 0;
    }

To add our own functions into selin's main scope,
we can do the following:

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

In the example above, we defined a function `say_hello`
in selin, which is a binding to the native C++ function.

Let us now define a function `sum` which takes the
sum of all the numeric arguments it receives:

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


You can also use `set_native_callable` with pointers
to your C++ functor objects:

    #include <iostream>
    #include "selin.hpp"

    // selin.hpp provides and uses a reference-counting
    // pointer class template called Ref<>.
    using selin::Ref;

    class MyFunctor
    {
        Ref<selin::LispObject> operator()(Ref<selin::LispNode> args)
        {
            // ....
        }
    };

    int main()
    {
        MyFunctor mf;

        selin::Scope main_scope;
        main_scope.set_native_callable("mf", &mf);
        main_scope.repl();

        return 0;
    }





## License

The license terms of selin are as follows:

    Copyright (c) 2014, 2015, Nihat Engin Toklu < http://github.com/engintoklu >

    This software is provided 'as-is', without any express or implied
    warranty. In no event will the authors be held liable for any damages
    arising from the use of this software.

    Permission is granted to anyone to use this software for any purpose,
    including commercial applications, and to alter it and redistribute it
    freely, subject to the following restrictions:

       1. The origin of this software must not be misrepresented; you must
       not claim that you wrote the original software. If you use this
       software in a product, an acknowledgment in the product documentation
       would be appreciated but is not required.

       2. Altered source versions must be plainly marked as such,
       and must not be misrepresented as being the original software.

       3. This notice may not be removed or altered from any source
       distribution.


## References

* (EmacsLisp) https://www.gnu.org/software/emacs/manual/html_node/elisp/
* (McCarthy) http://www-formal.stanford.edu/jmc/recursive.html
* (Wikipedia) https://en.wikipedia.org/wiki/Lisp_%28programming_language%29
* (Graham) http://www.paulgraham.com/rootsoflisp.html
* (Lua) http://www.lua.org
* (LuaNumbers) http://lua-users.org/wiki/NumbersTutorial