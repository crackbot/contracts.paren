<a id='x-28SERVE-2EPAREN-3A-40MAIN-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Functions with contracts for Parenscript

## Table of Contents

- [1 contracts.paren ASDF System Details][1fde]
- [2 Main API][3e8b]
- [3 Contract types][4c4a]
    - [3.1 Flat contracts][ca08]
    - [3.2 Full contracts][c50a]
    - [3.3 Named contracts - not fully implemented][c8ae]
- [4 Contracts runtime library][7e60]
- [5 Contracts combinators][0d79]

###### \[in package CONTRACTS.PAREN\]
<a id='x-28-22contracts-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 contracts.paren ASDF System Details

- Version: 0.0.3
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
lambda/function level. Those contracts impose constraints and
provide guarantees on the values being returned from the
function. There are some differences in both implementation and
usage between ParenScript and Racket, so make sure to read the docs
even if you're familiar with Racket contracts.

ParenScript f*unction* definition supports positional arguments, &key,
&optional, &rest. &optional and &keywoard does not work well
together, also when you include &rest and &key expect &rest variable
to capture input passed into &key, you need to remember this things
when writing contracts.

Simple function definition with two positional arguments looks like
so:

```lisp
(defun sum (x y)
  (+ x y))
```

To make this a function with contract you can use [`DEFUN/CONTRACT`][2d58] form:

```lisp
(defun/contract sum (x y)
  (>> intp intp intp)
  (+ x y))
```

In the code above each **intp** is called a contract and **>>** sign
is called a contract combinator. Right now there are three types of
contract combinators, and numerous contracts that can be used.

<a id='x-28CONTRACTS-2EPAREN-3A-40API-MANUAL-20MGL-PAX-3ASECTION-29'></a>

## 2 Main API

API consists of two parenscript macros **defun/contract** and
**lambda/contract** both are based on core **defun** and **lambda**
taking exactly the same lambda list and body. Except first form of the
body should be a contract.

<a id='x-28CONTRACTS-2EPAREN-3ALAMBDA-2FCONTRACT-20-28CONTRACTS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **LAMBDA/CONTRACT** *LAMBDA-LIST &BODY BODY* 

<a id='x-28CONTRACTS-2EPAREN-3ADEFUN-2FCONTRACT-20-28CONTRACTS-2EPAREN-3A-3APSMACRO-29-29'></a>

- [psmacro] **DEFUN/CONTRACT** *NAME LAMBDA-LIST &BODY BODY* 

There are a number of variables you can configure. Most important
one is [`*VIOLATION-FUNCTION*`][37ab], it gets called when contract is
violated. You can use something basic as a starting point, like

```javascript
function blame (obj) {
    console.log("contract violated");
    console.log(obj.given);
    console.log(obj.expected);
}
```


<a id='x-28CONTRACTS-2EPAREN-3A-2AVIOLATION-FUNCTION-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*VIOLATION-FUNCTION\*** *BLAME*

    Function that is called when contract violation is detected, it's
    called with one argument that is an object. Exact object keys and
    values are based on combinator type.

<a id='x-28CONTRACTS-2EPAREN-3A-2AIGNORE-CONTRACTS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*IGNORE-CONTRACTS\*** *NIL*

    This is useful if you want to ignore contracts when compiling your
    definitions for production use and don't want to pay extra
    performance costs because of contracts. Setting this to **t** will
    compile [`DEFUN/CONTRACT`][2d58] and [`LAMBDA/CONTRACT`][6c4a] as the usual defun and
    lambda forms.

<a id='x-28CONTRACTS-2EPAREN-3A-40CONTRACT-TYPES-20MGL-PAX-3ASECTION-29'></a>

## 3 Contract types

There are three types of contracts:
- >> flat contracts

- - full contracts, contract for functions with optional arguments
  or/and arbitrary many arguments or/and :pre :post clauses

- i named dependent contracts, where names you give to contracts
    can be used in subcontracts or/and :pre :post clauses


<a id='x-28CONTRACTS-2EPAREN-3AREMOVE-TL-CONTRACT-20FUNCTION-29'></a>

- [function] **REMOVE-TL-CONTRACT** *FORMS*

    Given forms it will remove any top-level contracts found in it

<a id='x-28CONTRACTS-2EPAREN-3A-40FLAT-CONTRACTS-20MGL-PAX-3ASECTION-29'></a>

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

<a id='x-28CONTRACTS-2EPAREN-3A-40FULL-CONTRACTS-20MGL-PAX-3ASECTION-29'></a>

### 3.2 Full contracts

You can use this combinator type to check more advanced function
  signatures, which includes one or more of optional, keyword or
  arbitrary many arguments.

Combinator signature is:

```lisp
  (>>* (domain contracts)
       (optional / keywords / rest)
       :pre ()
       :post ()
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


<a id='x-28CONTRACTS-2EPAREN-3A-40NAMED-CONTRACTS-20MGL-PAX-3ASECTION-29'></a>

### 3.3 Named contracts - not fully implemented

Named contracts is similar to optional combinator with the
  difference that each argument and result is named and these names
  can be used in subcontracts and in pre-/post-condition clauses. In
  other words ->i combinator expresses dependencies among arguments,
  results and environment.

Combinator signature is:

```lisp
  (>>i (domain contracts)
       (optional / keywords / rest)
       :pre () ()
       :post () ()
       (result contract))
```

Few examples:

Check that input X is a number and output `RES` is a number that is at
  least a double of X.

```
  (>>i ((x numberp))
       (res (x) (and/c numberp (>=/c (* x 2)))))
```

Check that X is a number, Y is number greater or equal than X.
  If :A keyword is given it should be a number. If :B is also given it
  should be greater or equal than :A. `RESULT` is a number that is
  greater or equal than sum of X and Y.

```lisp
  (>>i ((x numberp)
        (y (x) (>=/c x)))
       (:a (a numberp)
        :b (b (a) (>=/c a)))
       (result (x y) (and/c numberp (>=/c (+ x y)))))
```

```lisp
  (>>i ((x intp)
        (y (x) (eql/c (+ x x))))
       ()
       (result intp))
```

  Note that contract's names are mandatory for both inputs and
  outputs.

<a id='x-28CONTRACTS-2EPAREN-3A-40CONTRACTS-RUNTIME-20MGL-PAX-3ASECTION-29'></a>

## 4 Contracts runtime library

Some basic contracts are included with this library:

<a id='x-28CONTRACTS-2EPAREN-3AANYP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] ANYP  

    Returns t for any argument

<a id='x-28CONTRACTS-2EPAREN-3AHASP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] HASP *KEY OBJ* 

    Checks if object has property key

<a id='x-28ALEXANDRIA-2E0-2EDEV-3AEMPTYP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] EMPTYP *OBJ* 

    True if var is empty, obj might be one of: string array
    arguments object

<a id='x-28CONTRACTS-2EPAREN-3AELEMENTP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] ELEMENTP *OBJ* 

    Returns true if object is a DOM element.

<a id='x-28CONTRACTS-2EPAREN-3ATRUEP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] TRUEP *OBJ* 

    Return true if `OBJ` equals true.

<a id='x-28PARENSCRIPT-3ABOOLEANP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] BOOLEANP *OBJ* 

    Return true if `OBJ` is boolean.

<a id='x-28NUMBERP-20FUNCTION-29'></a>

- [function] **NUMBERP** *OBJECT*

    Return true if OBJECT is a `NUMBER`, and `NIL` otherwise.

<a id='x-28CONTRACTS-2EPAREN-3ANANP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] NANP *OBJ* 

    Return true if `OBJ` is NaN value.

<a id='x-28CONTRACTS-2EPAREN-3AEVENTP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] EVENTP *OBJ* 

    Return true if `OBJ` is an instance of Event class.

<a id='x-28CONTRACTS-2EPAREN-3ACLASSP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] CLASSP *OBJ* 

    Return true if `OBJ` is a class. There is no special way to define classes in javascript, and what's called a javascript class is represented as a function. Internally this function checks if `OBJ` is a function.

<a id='x-28PARENSCRIPT-3AOBJECTP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] OBJECTP *OBJ* 

    Return true if `OBJ` is of type object.

<a id='x-28CONTRACTS-2EPAREN-3ANULLP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] NULLP *OBJ* 

    Return true if `OBJ` is null.

<a id='x-28CONTRACTS-2EPAREN-3AUNDEFP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] UNDEFP *OBJ* 

    Return true if `OBJ` is udefined.

<a id='x-28ZEROP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] ZEROP *NUM* 

    Return true if `NUM` equals to zero.

<a id='x-28CONTRACTS-2EPAREN-3APOSITIVEP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] POSITIVEP *NUM* 

    Return true if `NUM` is positive.

<a id='x-28CONTRACTS-2EPAREN-3ANEGATIVEP-20-28CONTRACTS-2EPAREN-3A-3ASTATIC-PS-FUNCTION-20CONTRACTS-2EPAREN-3A-2ACONTRACTS-LIBRARY-2A-29-29'></a>

- [contract] NEGATIVEP *NUM* 

    Return true if `NUM` is negative.

Using those and combinators can get you far, but when your
application grows you may require custom contracts to be
created. Defining your own contract is easy, you need to create a
function that takes one argument and returns a boolean value.

For example to create a contract that checks if argument is a DOM node
you can do the following:

```lisp
(defun nodep (obj)
  (if (eq (typeof *node) "object")
      (instanceof obj *node)
      (and obj
           (eq (typeof obj) "object")
           (eq (chain obj node-type) "number")
           (eq (chain obj node-name) "string"))))
```


<a id='x-28CONTRACTS-2EPAREN-3A-40CONTRACTS-COMBINATORS-20MGL-PAX-3ASECTION-29'></a>

## 5 Contracts combinators

Combinators are parenscript macros used to combine more than one
contract easily, combinator takes contract(s) as input and return a new
contract.

<a id='x-28CONTRACTS-2EPAREN-3AINSTANCEOF-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **INSTANCEOF/C** *CLS* 

    Returns contract that checks if `ARG` is an instance of `CLS`

<a id='x-28CONTRACTS-2EPAREN-3AOR-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **OR/C** *&REST PREDS* 

    (>> (or/c intp floatp)) value should pass at least one predicate
    check

<a id='x-28CONTRACTS-2EPAREN-3AAND-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **AND/C** *&REST PREDS* 

    (>> (and/c intp bigger-than-five-p)) value should pass all
    predicates

<a id='x-28CONTRACTS-2EPAREN-3ANOT-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **NOT/C** *CONTRACT* 

<a id='x-28CONTRACTS-2EPAREN-3ALISTOF-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **LISTOF/C** *PRED* 

    (>> (listof/c intp)) value should be a list (array) and each list
    value should pass a predicate

<a id='x-28CONTRACTS-2EPAREN-3AONEOF-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **ONEOF/C** *&REST PREDS* 

    (>> (oneof/c intp floatp)) value should be a list and pass at least
    one predicate

<a id='x-28CONTRACTS-2EPAREN-3ALIST-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **LIST/C** *&REST PREDS* 

    (>> (list/c intp intp intp) intp) Produces a contract for a
    list (array). The number of elements in the list must match the number
    of arguments supplied to list/c, and each element of the list must
    match the corresponding contract.

<a id='x-28CONTRACTS-2EPAREN-3AOBJECT-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **OBJECT/C** *&REST PREDS* 

    (>> (object/c :value intp :another floatp) any)

<a id='x-28CONTRACTS-2EPAREN-3AMAYBE-2FC-20-28CONTRACTS-2EPAREN-3A-3ACOMBINATOR-29-29'></a>

- [combinator] **MAYBE/C** *PRED* 

    (maybe/c intp) is equal to (or/c undefp intp)

  [0d79]: #x-28CONTRACTS-2EPAREN-3A-40CONTRACTS-COMBINATORS-20MGL-PAX-3ASECTION-29 "Contracts combinators"
  [1fde]: #x-28-22contracts-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"contracts.paren\" ASDF/SYSTEM:SYSTEM)"
  [2d58]: #x-28CONTRACTS-2EPAREN-3ADEFUN-2FCONTRACT-20-28CONTRACTS-2EPAREN-3A-3APSMACRO-29-29 "(CONTRACTS.PAREN:DEFUN/CONTRACT (CONTRACTS.PAREN::PSMACRO))"
  [37ab]: #x-28CONTRACTS-2EPAREN-3A-2AVIOLATION-FUNCTION-2A-20-28VARIABLE-29-29 "(CONTRACTS.PAREN:*VIOLATION-FUNCTION* (VARIABLE))"
  [3e8b]: #x-28CONTRACTS-2EPAREN-3A-40API-MANUAL-20MGL-PAX-3ASECTION-29 "Main API"
  [4c4a]: #x-28CONTRACTS-2EPAREN-3A-40CONTRACT-TYPES-20MGL-PAX-3ASECTION-29 "Contract types"
  [6c4a]: #x-28CONTRACTS-2EPAREN-3ALAMBDA-2FCONTRACT-20-28CONTRACTS-2EPAREN-3A-3APSMACRO-29-29 "(CONTRACTS.PAREN:LAMBDA/CONTRACT (CONTRACTS.PAREN::PSMACRO))"
  [7e60]: #x-28CONTRACTS-2EPAREN-3A-40CONTRACTS-RUNTIME-20MGL-PAX-3ASECTION-29 "Contracts runtime library"
  [c50a]: #x-28CONTRACTS-2EPAREN-3A-40FULL-CONTRACTS-20MGL-PAX-3ASECTION-29 "Full contracts"
  [c8ae]: #x-28CONTRACTS-2EPAREN-3A-40NAMED-CONTRACTS-20MGL-PAX-3ASECTION-29 "Named contracts - not fully implemented"
  [ca08]: #x-28CONTRACTS-2EPAREN-3A-40FLAT-CONTRACTS-20MGL-PAX-3ASECTION-29 "Flat contracts"
