<a name='x-28SERVE-2EPAREN-3A-40MAIN-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Functions with contracts for Parenscript

## Table of Contents

- [1 contracts.paren ASDF System Details][1fde]
- [2 Main API][3e8b]
- [3 Contract types][4c4a]
    - [3.1 Flat contracts][ca08]
    - [3.2 Full contracts][c50a]
    - [3.3 Named contracts - not implemented][c8ae]
- [4 Contracts runtime library][7e60]
- [5 Contracts combinators][0d79]

###### \[in package CONTRACTS.PAREN\]
<a name='x-28-22contracts-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 contracts.paren ASDF System Details

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

<a name='x-28CONTRACTS-2EPAREN-3A-40API-MANUAL-20MGL-PAX-3ASECTION-29'></a>

## 2 Main API

<a name='x-28CONTRACTS-2EPAREN-3ALAMBDA-2FCONTRACT-20-23-3APSMACRO-29'></a>

- [psmacro] ***LAMBDA/CONTRACT*** *LAMBDA-LIST &BODY BODY* 

<a name='x-28CONTRACTS-2EPAREN-3ADEFUN-2FCONTRACT-20-23-3APSMACRO-29'></a>

- [psmacro] ***DEFUN/CONTRACT*** *NAME LAMBDA-LIST &BODY BODY* 

When contract is violated it will call the [`*VIOLATION-FUNCTION*`][78df]
which you need to define inside your code base

<a name='x-28CONTRACTS-2EPAREN-3A-2AVIOLATION-FUNCTION-2A-20VARIABLE-29'></a>

- [variable] **\*VIOLATION-FUNCTION\*** *BLAME*

    Function that is called when contract violation is detected, it's
    arguments are based on combinator type

<a name='x-28CONTRACTS-2EPAREN-3A-2AIGNORE-CONTRACTS-2A-20VARIABLE-29'></a>

- [variable] **\*IGNORE-CONTRACTS\*** *NIL*

    This is useful if you want to ignore contracts when compiling your
    definitions for production use and don't want to extra performance
    costs because of contracts. Setting this to t will compile
    defun/contract and lambda/contract as the usual defun and lambda.

<a name='x-28CONTRACTS-2EPAREN-3A-40CONTRACT-TYPES-20MGL-PAX-3ASECTION-29'></a>

## 3 Contract types

There are three types of contracts:
- >> flat contracts

- - full contracts, contract for functions with optional arguments
  or/and arbitrary many arguments or/and :pre :post clauses

- i named dependent contracts, where names you give to contracts
    can be used in subcontracts or/and :pre :post clauses


<a name='x-28CONTRACTS-2EPAREN-3A-40FLAT-CONTRACTS-20MGL-PAX-3ASECTION-29'></a>

### 3.1 Flat contracts

Flat contract is the basic building block, most of the time you
  will be using it. It suports positional and keyword arguments.

```lisp
  (defun hello (x y)
    (>> intp intp intp)
    (+ x y))
```

```lisp
  (defun hello (x y &key (add 2))
    (>> intp intp :add intp intp)
    (+ (+ x y) add))
```

  Flat contract does support all of the combinators, but not optional
  or rest arguments.

<a name='x-28CONTRACTS-2EPAREN-3A-40FULL-CONTRACTS-20MGL-PAX-3ASECTION-29'></a>

### 3.2 Full contracts

You can use this combinator type to check more advanced function
  signatures, which includes one or more of optional, keyword or
  arbitrary many arguments.

Combinator signature is:

```lisp
  (>>* (domain contracts)
       (optional / keywords / rest)
       :pre () :post ()
       range contract)
```

Domain and range contracts are required. If you provide optional or
  keyword contract it should be a plist of variable name and
  contract, you can also skip providing those contracts for variable
  that you don't want to check, unlike domain contracts where you need
  to provide a contract for every variable. If you don't want to
  provide any contracts for those, just pass the nil

```lisp
  (>>* (intp) (:keyword floatp) intp)
```

```lisp
  (>>* (intp) nil intp)
``` 

  If :rest is present it is expected to check the &rest arguments

  :pre and :post expect a lambda which should return either a boolean
  value or a string, in case it returns **f** or a string it's treated
  as a failure and string designates an error message. :pre will be
  dispatched before any contract and :post after checking all provided
  ones. This allows you to check the function environment without any
  connection to function domain or range.

  An example, checking that function saves the return value:

```lisp
  (defun/contract sum (x y)
    (>>* (intp intp) 
         :post (lambda ()
                 (intp (chain this result)))
         intp)
    (let ((res (+ x y)))
      (setf (chain this result) res)
      res))
```


<a name='x-28CONTRACTS-2EPAREN-3A-40NAMED-CONTRACTS-20MGL-PAX-3ASECTION-29'></a>

### 3.3 Named contracts - not implemented

Named contracts is similar to optional combinator with the
  difference that the ->i contract combinator differs from the ->\*
  combinator in that each argument and result is named and these names
  can be used in the subcontracts and in the pre-/post-condition
  clauses. In other words, ->i expresses dependencies among arguments
  and results.

Combinator signature is:

`lisp
  (>>i (domain contracts)
       (optional or keywords)
       :rest () ()
       :pre () ()
       :post () ()
       (range contract))
`

  `lisp
  (->i ((x number?)
        (y (x) (>=/c x)))
       (:a (a number?)
        :b (b (a) (>=/c a)))
       :pre () (set! c0 count)
       :post (id nn) (string=? (name id) nn)
     (result (x y) (and/c number? (>=/c (+ x y)))))
`lisp

<a name='x-28CONTRACTS-2EPAREN-3A-40CONTRACTS-RUNTIME-20MGL-PAX-3ASECTION-29'></a>

## 4 Contracts runtime library

Some basic contracts are included with this library:

Using those and combinators can get you far, but when your
application grows you may require custom contracts to be
build. Defining your own contract is easy, you need to create a
function that takes one argument and returns a boolean value.

For example to create a contract that checks if argument is a `DOM` node
you can do the following:

```lisp
defun nodep (obj)
  (if (eq (typeof *node) "object")
      (instanceof obj *node)
      (and obj
           (eq (typeof o) "object")
           (eq (chain o node-type) "number")
           (eq (chain o node-name) "string"))))
```


<a name='x-28CONTRACTS-2EPAREN-3A-40CONTRACTS-COMBINATORS-20MGL-PAX-3ASECTION-29'></a>

## 5 Contracts combinators

Combinators are parenscript macros used to combine more than one
contract easily, combinator takes contract(s) as input and return a new
contract.

<a name='x-28CONTRACTS-2EPAREN-3AINSTANCEOF-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***INSTANCEOF/C*** *CLS* 

<a name='x-28CONTRACTS-2EPAREN-3AOR-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***OR/C*** *&REST PREDS* 

<a name='x-28CONTRACTS-2EPAREN-3AAND-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***AND/C*** *&REST PREDS* 

<a name='x-28CONTRACTS-2EPAREN-3ANOT-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***NOT/C*** *CONTRACT* 

<a name='x-28CONTRACTS-2EPAREN-3ALISTOF-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***LISTOF/C*** *PRED* 

<a name='x-28CONTRACTS-2EPAREN-3AONEOF-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***ONEOF/C*** *&REST PREDS* 

<a name='x-28CONTRACTS-2EPAREN-3ALIST-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***LIST/C*** *&REST PREDS* 

<a name='x-28CONTRACTS-2EPAREN-3AOBJECT-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***OBJECT/C*** *&REST PREDS* 

<a name='x-28CONTRACTS-2EPAREN-3AMAYBE-2FC-20MGL-PAX-3APSMACRO-29'></a>

- [psmacro] ***MAYBE/C*** *PRED* 

  [0d79]: #x-28CONTRACTS-2EPAREN-3A-40CONTRACTS-COMBINATORS-20MGL-PAX-3ASECTION-29 "(CONTRACTS.PAREN:@CONTRACTS-COMBINATORS MGL-PAX:SECTION)"
  [1fde]: #x-28-22contracts-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"contracts.paren\" ASDF/SYSTEM:SYSTEM)"
  [3e8b]: #x-28CONTRACTS-2EPAREN-3A-40API-MANUAL-20MGL-PAX-3ASECTION-29 "(CONTRACTS.PAREN:@API-MANUAL MGL-PAX:SECTION)"
  [4c4a]: #x-28CONTRACTS-2EPAREN-3A-40CONTRACT-TYPES-20MGL-PAX-3ASECTION-29 "(CONTRACTS.PAREN:@CONTRACT-TYPES MGL-PAX:SECTION)"
  [78df]: #x-28CONTRACTS-2EPAREN-3A-2AVIOLATION-FUNCTION-2A-20VARIABLE-29 "(CONTRACTS.PAREN:*VIOLATION-FUNCTION* VARIABLE)"
  [7e60]: #x-28CONTRACTS-2EPAREN-3A-40CONTRACTS-RUNTIME-20MGL-PAX-3ASECTION-29 "(CONTRACTS.PAREN:@CONTRACTS-RUNTIME MGL-PAX:SECTION)"
  [c50a]: #x-28CONTRACTS-2EPAREN-3A-40FULL-CONTRACTS-20MGL-PAX-3ASECTION-29 "(CONTRACTS.PAREN:@FULL-CONTRACTS MGL-PAX:SECTION)"
  [c8ae]: #x-28CONTRACTS-2EPAREN-3A-40NAMED-CONTRACTS-20MGL-PAX-3ASECTION-29 "(CONTRACTS.PAREN:@NAMED-CONTRACTS MGL-PAX:SECTION)"
  [ca08]: #x-28CONTRACTS-2EPAREN-3A-40FLAT-CONTRACTS-20MGL-PAX-3ASECTION-29 "(CONTRACTS.PAREN:@FLAT-CONTRACTS MGL-PAX:SECTION)"
