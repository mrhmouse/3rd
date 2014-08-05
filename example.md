Literal values / Basic types
----------------------------

- integers
    1
- floats
    1.0
- strings
    "hello \n world! \t \"escaped quotes\""
- lists
    (1 2 3 5.0 "foo")
- blocks
    [1 - "blocks are inline functions" .]
- booleans
    true false
 
Bultin functions
----------------

Stack manipulation
==================

- dup : duplicate top item on the stack
- drop : drop the top item from the stack
- swap : swap the top two items on the stack
- rot : rotate the top three items on the stack
    a b c --> b c a
- dip : bring the third item in the stack forward
    a b c --> c b a

Math
====

- + : add the top two numbers on the stack
    5 6 + --> 11
- - : subtract the top two numbers on the stack
    5 1 - --> 4
- * : multiply the top two numbers on the stack
    4 5 * --> 20
- / : divide the top two numbers on the stack
    4 2 * --> 2

Logic
=====

- if : the conditional statement
    [ "yep" ] [ "nope" ] [ 1 1 = ] if --> "yep"
- while : the loop
    50 [1 - dup .] [dup 0 >] while --> Print from 49 down to 0 # TODO; is a library function, needs to be a builtin
- =  : compare the top two items for equality
    1 1 = --> true
- >  : compare the top two items
    1 0 > --> true
- <  : compare the top two items
    1 0 < --> false
- !  : take the inverse of the top boolean
    true ! --> false

IO and function calling
=======================

- . : print the top item, consuming it
    1 . --> prints 1, leaves an empty stack
- call : call the block on the top of the stack
    ["Hello, world!" .] call --> prints Hello, world!, leaves an empty stack

List, string, and block manipulation
====================================

- pop     : pop the top item out of the list on the top of the stack
    (1 2 3) pop --> (2 3) 1
- push    : pushes the top item into the list behind it
    (2 3) 1 push --> (1 2 3)
- cat     : concatenate the top two items
    (1 2 3) (4 5) cat --> (1 2 3 4 5)
    "hello " "world!" cat --> "hello world!"
- empty   : return true if the top list is empty
    "" empty --> true
    (1 2 3) empty --> false
- length  : return the length of the top list in the stack
    (1 2 3) length --> 3
- pluck   : return the nth item in a list, pulling it out
    (1 2 3 4 5) 3 pluck --> (1 2 3 5) 4    # TODO
- insert  : insert an item into the nth position in a list
    (1 2 3 5) 4 3 insert --> (1 2 3 4 5)  # TODO

Conversion functions
====================

- string  : convert an item to a string
    4.5 string --> "4.5"                                     # TODO
