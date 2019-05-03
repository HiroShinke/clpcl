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
