# CLPCL
Yet Another Parsec like parser combinator library for Common Lisp

[![common lisp][shield-commonLisp]](#)
[![parser combinator][shield-parser]](#)
[![haskell][shield-haskell]](#)

## Description

A parser combinator library for Common Lisp
inspired by Parsec library for Haskell
(http://hackage.haskell.org/package/parsec)

I show a simple calculator program using clpcl bellow.
This is a straightforward port of 
an example of the use of pChainl1 
in the Text.Parsec hackage page

```lisp
  (let* (
	 (multop (clpcl-or
		  (clpcl-let ((nil (token-regexp "\\*"))) #'*)
		  (clpcl-let ((nil (token-regexp "/"))) #'/)))
	 
	 (addop  (clpcl-or
		  (clpcl-let ((nil (token-regexp "\\+"))) #'+)
		  (clpcl-let ((nil (token-regexp "-"))) #'-)))
	 
	 (digit (clpcl-bind (token-regexp "\\d+")
			    #'parse-integer))
	 (factor (clpcl-or
		  (clpcl-paren (token-regexp "\\(")
			       (clpcl-lazy expr)
			       (token-regexp "\\)"))
		  digit))

	 (term (clpcl-chainl-1 factor multop))
	 (expr (clpcl-chainl-1 term addop))
	 )
    (clpcl-parse expr text)
)
```
## Usage

Basically, the following combinators correspond to
Parsec's combinators, respectively.

* clpcl-lookahead
* clpcl-many
* clpcl-many-1	   
* clpcl-many-till
* clpcl-not-followed
* clpcl-eof
* clpcl-seq
* clpcl-string
* clpcl-or
* clpcl-token
* clpcl-try
* clpcl-option
* clpcl-parse
* clpcl-paren
* clpcl-sep-by
* clpcl-sep-by-1
* clpcl-end-by
* clpcl-end-by-1
* clpcl-sep-end-by
* clpcl-sep-end-by-1
* clpcl-return
* clpcl-chainr-1
* clpcl-chainl-1
  
I will explain some of my own.

* clpcl-let

This is a macro, intended to do the same thing as do syntax.
When p1, p2 and p3 are parsers, they are used as follows.

```lisp

   (clpcl-let ((x p1)
               p2
               (y p3))
     (list x y))
     
```

This expression defines a parser that parses the p1, p2 and p3
syntax sequences and returns a list of p1 and p3 results
as a semantic value.

This can be compared to the following haskell representation:

```haskell

   do {
     x <- p1
     p2
     y <- p3
     return [x y]
   }

```
     
In this example, p2 must be a symbol to which a parser is bound.
If you want to put an expression to be evaluated as a parser
instead of p2, use:

```lisp

   (clpcl-let ((x p1)
               (nil p2)
               (y p3))
     (list x y))
     
```
In this example, the p2 position can have any expression.

* clpcl-let*

This more closely mimics the monad's semantics.
That is, you can use the result of the previous parser
in the right-hand side of the let expression,
as the following:

```lisp

   (clpcl-let ((x p1)
               (y (if x p2 p3)))
      (list x y))
```

* clpcl-lazy

This is another macro.
It is used to delay the evaluation of the symbols bound to
the parser until runtime,
in order to define a (mutually) recursive parser(s).
See the example bellow.

Since p2 is not bound at the time of definition of p1,
it is necessary to delay evaluation by clpcl-lazy.
This defines a valid parser.

```lisp

  (let* ((p1 (clpcl-or
               digit
               (clpcl-seq
                 (clpcl-regexp "\\(")
	         (clpcl-lazy p2)
                 (clpcl-regexp "\\)")
	        )))
	 (p2 (clpcl-seq
	       p1 (clpcl-regexp "\\+") p1 )))
     p1
   )

```

Also see the example above for parsing arithmetic expressions.


* clpcl-def-parsers

This macro is for automatically using the
combination of let * and clpcl-lazy.
The equivalent of the above example can be written as:


```lisp

  (clpcl-def-parsers
　　　　　((p1 (clpcl-or
               digit
               (clpcl-seq
                 (clpcl-regexp "\\(")
	          p2
                 (clpcl-regexp "\\)")
	        )))
	 (p2 (clpcl-seq
	       p1 (clpcl-regexp "\\+") p1 )))
     p1
   )

```

clpcl-def-parsers finds an undefined symbol
from its definition rvalue and wraps it in clpcl-lazy.


## Examples

Simple Calculator
https://github.com/HiroShinke/clpcl/blob/master/clpcl-calc.asd
https://github.com/HiroShinke/clpcl/blob/master/clpcl-calc.lisp

## Contribution

## Licence

## Author

   Hiofumi SHINKE <hiro.shinke@gmail.com>


[shield-commonLisp]: https://img.shields.io/badge/lang-commonLisp-brightgreen.svg
[shield-parser]: https://img.shields.io/badge/tag-parser_combinator-green.svg
[shield-haskell]: https://img.shields.io/badge/tag-haskell-green.svg
