# Functions with contracts for Parenscript

###### \[in package CONTRACTS.PAREN\]
## contracts.paren ASDF System Details

- Version: 0.0.2
- Description: Contracts for parenscript (javascript), inspired by Racket contracts.
- Licence: The MIT License (MIT)
- Author: Crackbot <thecrackbot@gmail.com>
- Maintainer: Crackbot <thecrackbot@gmail.com>

This library is inspired by contracts found in Racket programming
langugage.

What are contracts? Here is a quote from Racket Guide.

Like a contract between two business partners, a software contract
is an agreement between two parties. The agreement specifies
obligations and guarantees for each “product” (or value) that is
handed from one party to the other.

A contract thus establishes a boundary between the two
parties. Whenever a value crosses this boundary, the contract
monitoring system performs contract checks, making sure the partners
abide by the established contract.

In ParenScript (unlike in Racket) contracts can be defined only at
function level. Those contracts impose constraints and provide
guarantees on the values being returned from the function.

## Main API

- [psmacro] LAMBDA/CONTRACT 

- [psmacro] DEFUN/CONTRACT 

When contract is violated it will call the *VIOLATION-FUNCTION*
which you need to define inside your code base

- [variable] *VIOLATION-FUNCTION* BLAME

    Function that is called when contract violation is detected, it's
    arguments are based on combinator type

- [variable] *IGNORE-CONTRACTS* NIL

    This is useful if you want to ignore contracts when compiling your
    definitions for production use and don't want to extra performance
    costs because of contracts. Setting this to t will compile
    defun/contract and lambda/contract as the usual defun and lambda.

## Contracts runtime library

Some basic contracts are included with this library.

Using those and combinators can get you far, but when your
application grows you may require custom contracts to be
build. Defining your own contract is easy, you need to create a
function that takes one argument and returns a boolean value

## Contracts combinators

Combinators are parenscript macros used to combine more than one
contract easily, combinator takes contracts as input and return a new
contract.

## Available contract types

        flat contracts


        - contract for functions with optional arguments or arbitrary many arguments


        i - named dependant contracts

