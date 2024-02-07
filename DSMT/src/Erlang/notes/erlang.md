Erlang: A Brief Overview
========================

Introduction
------------

`Erlang` is a programming language which is based on the **functional programming paradigm**, contrasting with the **imperative paradigm**. While imperative languages execute programs as sequences of instructions, resulting in state changes, functional programming languages approach execution by evaluating expressions or functions. In functional programming, the outcome of a program is not a state but rather a *value*.

Some popular concepts in functional programming are not present or are unusual in imperative languages. We'll briefly discuss them in the following section.

### Concepts in Functional Programming

**Referential Transparency** - A functional program can be seen as a sequence of expressions or functions evaluation. 

- An expression is said to be **referentially transparent** is it can be replaced with its own value with no change in the program behavior.

- A function is said pure if its calls are *referentially transparent*, i.e., different functions calls in time (with the same input parameters) results in the same outcome.


So, the "no side effect" property is very useful (and quite common in functional programs): thanks to this property, we can overcome many problems caused by race conditions in multi-threaded applications because we can apply **memo-ization**: whenever a function is *pure*, its result can be cached without the need to recompute it every time.


**Lack of State** - Functional programming does not provide any tools to implement the concept of state. Thus, in functional programs, ***mutable*** *variables* are not present: once a variable has been bound to a value, it will never change over the course of the program. 


**Eager vs Lazy Evaluation** - In computations, evaluation of expressions (mainly for function arguments) can be carried out according to different  strategies:

- Eager evaluation: an expression is evaluated as soon as it is encountered;  usually adopted in call-by-value and call-by-reference semantics of function argument passing.

- Lazy evaluation: expression evaluation is postponed to the time its value will be really needed; for actual parameters in functions, this leads to the call-by-need argument passing semantics.


**Use of Recursion** - Without the help of state variables, recursion represents the only way to support the repetition of specific computations.

> **Tail Call Optimization** - TCO is a technique used in some programming languages and compilers to optimize recursive function calls, particularly tail-recursive ones.
>
> In many programming languages, when a function calls itself recursively, a new stack frame is created for each call. This can lead to stack overflow errors if the recursion depth is too large.
> 
> However, with *tail call optimization*, the compiler recognizes when the result of a recursive call is the final result of the function, and it replaces the current stack frame with the one for the next recursive call, effectively reusing the current stack frame rather than creating a new one. This allows for efficient memory usage and prevents stack overflow errors in tail-recursive functions.


### What about concurrency?

Erlang's concurrency model provides several key features that make it well-suited for building concurrent, distributed, fault-tolerant systems:

- **Lightweight Processes**: Erlang utilizes lightweight processes, also known as actors, managed by the Erlang runtime system. These processes are exceptionally lightweight in terms of memory overhead and creation time, distinct from operating system threads. Instead, they are user-space constructs managed by the Erlang VM.

- **Isolation**: Each Erlang process operates independently with its own heap. This isolation guarantees that processes do not interfere with each other's data and execution.

- **Message Passing**: Concurrency in Erlang relies heavily on message passing between processes. Processes communicate asynchronously by sending and receiving messages. This approach, devoid of shared memory reliance, simplifies concurrent programming and enhances the ability to reason about it.


An `Erlang` Tutorial
--------------------

As for any functional program, an `Erlang` program is a sequence of expressions which lead to a value when they are evaluated. An expression can be:

1. Just an expression (e.g. a mathematical expression);
2. Composed of any number of sub-expressions;
3. A function call.

To deal with expressions, `Erlang` provides the following data types:

- **Constant data types**. These are data types which cannot be split into more primitive subtypes:

    1. *Numbers*, for example: 123, -789, 3.14159, 7.8e12, -1.2e-45. Numbers are further subdivided into integers and floats.

    2. *Atoms*, for example: `abc`, '`An atom with spaces`', `monday`, `green`,`hello_world`, `true`, `false`. Atoms are constants with names; thus, for example, the atoms `monday`, `tuesday`, ... could be used to represent days of the week in some program which performs calendar calculations.

- **Compound data types**. These are used to group together other data types. There are two compound data types:

    1. *Tuples*, for example: `{a, 12, b}`, `{}`, `{1, 2, 3}`, `{a, b, c, d, e}`. Tuples are used for storing a fixed number of items and are written as sequences of items enclosed in curly brackets. Tuples are similar to records or structures in conventional programming languages.
    
    2. *Lists*, for example: `[]`, `[a, b, 12]`, `[22]`, `[a, 'hello friend']`. Lists are used for storing a variable number of items and are written as sequences of items enclosed in square brackets. When processing lists it is often convenient to be able to refer to the first element of the list and the remainder of the list when the first element has been removed. By convention, we refer to the first element of the list as the **head** of the list and the remainder of the list as the **tail**. Thus, `[H|T]` is a list whose `H` is the head of such a list (and it can be any aforementioned data type) and `T` is the tail (`T` is a list itself).

The values of `Erlang` data types can be stored in variables. Variables always start with an upper-case letter. `Erlang`'s variables are **immutable**: once a variable has been bound to a value, it won't change its value anymore.

How to deal with values and variables? **Pattern matching**.

> Pattern matching provides the basic mechanism by which values become assigned to variables. A variable whose value has been assigned is said to be *bound*, otherwise it is said to be *unbound*. Once a variable has been bound its value can never be changed. Such variables are called **bind once** or **single assignment**. This contrasts with conventional imperative languages which have destructive assignment.
>
> Pattern matching is used to match patterns with terms. If a pattern and term have the same shape then the match will succeed and any variables occurring in the pattern will be bound to the data structures which occur in the corresponding positions in the term.

Pattern matching occurs:

1. When evaluating an expression of the form `Left = Right`.
2. When calling a function.
3. When matching a pattern in a `case` or a `receive` primitive.

Let's see how pattern matching works in practice.

```erl
$ erl -v
Erlang/OTP 24 [erts-12.2.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V12.2.1  (abort with ^G)
1> A = 2.
2
2> B = 3.
3
3> {A, B} = {2, 3}.
{2,3}
4> {A, B} = {2, 2}.
** exception error: no match of right hand side value {2,2}
5> {A, B} = {2, _}.
* 1:14: variable '_' is unbound
```

Expression `A = 2` is a *binding* operation: the variable `A` has not been previously bound, so the evaluation of the expression `A = 2.` results in binding the term `2` to the variable `A`.

Expression `{A, B} = {2, 3}` is an example of pattern matching: the tuple `{A, B}` matches the structure of `{2, 3}`, and both `A` and `B` has been previously bound to values `2` and `3`, respectively. If `{A, B}` were substituted with `{2, 3}`, it would not produce any *side effect*.

Expression `{A, B}` unfortunately cannot be matched to `{2, 2}` because variable `B` has been previously bound to the term `3`, thus, even if the structure of the right term is identical to the structure of the left term, the values are not compatible. 

Please, take a look at `{A, B} = {2, _}`: pattern matching fails because variable `B` is bounded. Once a variable has been bound, no other values than its own can match the variable!

### Exercises (1)

[**1**] Write an Erlang module named "lab_01" with a function <em>hello/0</em> that prints on the screen the sentence "Hello World!" using the function <em>io:format/1</em>. Compile the module from the shell and call the developed function.

```erl
%% lab_01.erl

-module(lab_01).
-export(hello/0).

hello() -> io:format("Hello World!~n").
```

To compile an `Erlang` module we can proceed as follows:

1. From the erlang interpreter we can compile a module with `compile:file("module_name.erl").`. 

2. We have another option: `c("module_name.erl").`.

```erl
$ erl -v                            
1> ls().
lab_01.erl      
ok
2> compile:file("lab_01.erl").
{ok,lab_01}
3> lab_01:hello().
Hello World!
ok
4> c("lab_01.erl").
{ok,lab_01}
5> lab_01:hello().
Hello World!
ok
```

[**2**] In the developed module, insert another function <em>hello/1</em> that takes a string as argument and prints on the screen "Hello", followed by the passed string. Use the function for formatted print <em>io:format/2</em>; the placeholder for strings in the template string is `~s`. Test it from the shell.

```erl
%% lab_01.erl

-module(lab_01).
-export([hello/0, hello/1]).

hello()
    -> io:format("Hello World!~n").

hello(S) ->
    -> io:format("Hello ~s~n", [S]).
```

Output:

```erl
$ erl -v                            
1> c("lab_01.erl").
{ok,lab_01}
2> lab_01:hello("Federico").
Hello Federico!
ok
```

[**3**] Add another function <em>hello/2</em> that behaves like the previous one, but prints the sentence on the screen as many times as indicated by the last argument.

```erl
%% lab_01.erl

-module(lab_01).
-export([hello/2]).

hello(S) ->
    io:format("Hello ~s~n", [S]).

hello(_, 0) ->
    ok;

hello(S, N) -->
    hello(S),
    hello(S, N-1).
```

Output:

```erl
$ erl -v                            
1> c("lab_01.erl").
{ok,lab_01}
2> lab_01:hello("Federico", 2).
Hello Federico!
Hello Federico!
ok
```

[**4**] Write the function <em>rsum/1</em> that, given a list of numbers, returns their sum, computed recursively. For the sake of performance, it must be implemented using tail recursion.

```erl
%% lab_01.erl

-module(lab_01).
-export([hello/2, rsum/1]).

hello(S) ->
    io:format("Hello ~s~n", [S]).

hello(_, 0) -> ok;

hello(S, N) ->
    hello(S),
    hello(S, N-1).

rsum(L) ->          
    rsum(L, 0).     %% When rsum/1 is called, it evaluates to a call to rsum/2.
                    %% The second argument of rsum/2 is a mechanism to store the 
                    %% result of the sum. 

rsum([], S) ->      %% If the list is empty, the total sum is S.
    S;

rsum([H|T], S) ->   %% If the list is not empty, we need to add the value of
    rsum(T, S+H).   %% the head to the current sum and continue iterating...
```

[**5**] Write a function `reverse/1` which reverses the order of the elements of a list.

```erl
%% lab_01.erl

-module(lab_01).
-export([reverse/1])

reverse(L) ->
    reverse(L, []).

reverse([], R) -> R;    %% If the list to be reversed is empty or it has been
                        %% iterated through the end, return the reversed list R.

reverse([H|T], R) ->
    reverse(T, [H|R]).
```

Output:

```erl
$ erl -v                            
1> c("lab_01.erl").
{ok,lab_01}
2> L = [1,2,3].          
[1,2,3]
3> R = lab_01:reverse(L).
[3,2,1]
```

[**6**] Write a function <em>take/2</em> that, given an integer and a list, returns the list element at the position indicated by the first parameter.

```erl
%% lab_01.erl

-module(lab_01).
-export([take/2])

take(L, P) ->
    take(L, P, 0).

take(_, P, _) when P < 0 ->     %% guard: P < 0
    io:format("Index is not a positive integer.~n"),
    done;

take([], _, C) when C == 0 ->   %% guard: C == 0
    io:format("Input list is empty.~n"),
    done;

take([H|_], P, C) when P == C -> %% guard: P == C
    H;

take([_|T], P, C) when C < P ->  %% guard: C < P
    take(T, P, C+1);

take(_,_,_) ->
    io:format("Index out of bound.~n"),
    done.
```

Output:

```erl
$ erl -v                            
1> c("lab_01.erl").
{ok,lab_01}
2> lab_01:take([1,2,3],0).
1
3> lab_01:take([1,2,3],-1).
Index is not a positive integer.
done
4> lab_01:take([1,2,3],2). 
3
5> lab_01:take([1,2,3],3).
Index out of bound.
done
6> lab_01:take([1,2,3],4).
Index out of bound.
done
```

In this example we exploits a new `Erlang` concept: **guards**. Guards are conditions which have to be fulfilled before a function clause is chosen. A guard can be a simple test or a sequence of simple tests separated by commas. A simple test is an arithmetic comparison, a term comparison, or a call to a system predefined test function. Guards can be viewed as an extension of pattern matching. User-defined functions cannot be used in guards. To evaluate a guard all the tests are evaluated. If all are true then the guard succeeds, otherwise it fails. 

[**7**] Implement a function that returns the maximum element of a list of numbers passed as argument.

```erl
%% lab_01.erl

-module(lab_01).
-export([mymax/1]).

mymax([]) ->
    io:format("Empty list.~n");

mymax([H|T]) ->
    mymax([H|T], H).

mymax([], M) -> M;

mymax([H|T], M) when M >= H ->
    mymax(T, M);

mymax([H|T], M) when M < H ->
    mymax(T, H).
```
Output:

```erl
$ erl -v  
1> c("lab_01.erl").
{ok,lab_01}
2> lab_01:mymax([]).
Empty list.
ok
3> lab_01:mymax([1]).
1
4> lab_01:mymax([1,-1]).
1
5> lab_01:mymax([0,-1]).
0
6> lab_01:mymax([1,2,-2,-1,3]).
3
7> lab_01:mymax([$A,$B,$C]).   
67
8> $C == 67.
true 
```

[**8**] Implement the function <em>splitter/1</em> that takes a list of integers and separates its elements in two lists, one with its even elements, and the other with the odd elements. It must return a tuple with both the calculated lists. Optionally, write a generalized version <em>splitter/2</em> that takes also a predicate as parameter, and splits the elements according to the predicate outcome; it corresponds to the standard library function <em>lists:partition/2</em>.

```erl
%% lab_01.erl

-module(lab_01).
-export([splitter/1]).


%% Plain recursion version 

splitter([]) ->
    io:format("Empty list.~n");
splitter(L) -> 
    splitter(L, [], []).

splitter(L1, O, E) when L1 == [] -> %% END: we iterated through the whole list
    {reverse(O), reverse(E)};
splitter([H|T], O, E) when H rem 2 == 0 -> %% H is even
    splitter(T, O, [H|E]);
splitter([H|T], O, E) when H rem 2 == 1 -> %% H is odd
    splitter(T, [H|O], E).
```