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

- pull an item to the front

        a b c 1 yank --> a c b
- pull a copy of an item to the front

        a b c 1 copy --> a b c b
- remove an item from the stack

        a b c 1 delete --> a c

Math
====

- add the top two numbers on the stack

        5 6 + --> 11
- subtract the top two numbers on the stack

        5 1 - --> 4
- multiply the top two numbers on the stack

        4 5 * --> 20
- divide the top two numbers on the stack

        4 2 * --> 2

Logic
=====

- the conditional statement

        [ "yep" ] [ "nope" ] [ 1 1 = ] if --> "yep"
- the loop

        50 [1 - 0 copy .] [0 copy 0 >] while --> Print from 49 down to 0
- compare the top two items for equality

        1 1 = --> true
- compare the top two items (greater than)

        1 0 > --> true
- compare the top two items (less than)

        1 0 < --> false
- take the inverse of the top boolean

        true ! --> false

IO and function calling
=======================

- print the top item, consuming it

        1 . --> prints 1, leaves an empty stack
- call the block on the top of the stack

        ["Hello, world!" .] call --> prints Hello, world!, leaves an empty stack

List, string, and block manipulation
====================================

- pop the top item out of the list on the top of the stack

        (1 2 3) pop --> (2 3) 1
- push the top item into the list behind it

        (2 3) 1 push --> (1 2 3)
- concatenate the top two items

        (1 2 3) (4 5) cat --> (1 2 3 4 5)
        "hello " "world!" cat --> "hello world!"
- return true if the top list is empty

        "" empty --> true
        (1 2 3) empty --> false
- return the length of the top list in the stack

        (1 2 3) length --> 3
- return the nth item in a list, pulling it out

        (1 2 3 4 5) 3 pluck --> (1 2 3 5) 4
- insert an item into the nth position in a list

        (1 2 3 5) 4 3 insert --> (1 2 3 4 5)

Conversion functions
====================

- convert an item to a string

        4.5 string --> "4.5" # TODO
