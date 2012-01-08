Copyright 2012, Vincent Toups
This program is distributed under the terms of the GNU Lesser 
General Public License (see license.txt).

Parseltongue
============
A Parser Combinator Library for CL
----------------------------------

This is a parser combinator library for Common Lisp inspired by [SMUG][],
by Drew Crampsie and augmented with improvements inspired by my own
usage of this library and other monadic computations in various Lisps.

This library probably has the same functionality as SMUG but migth be
more familiar to those used to Haskell's do notation.

Fear not, you needn't understand how monads work in order to make good
use of this library.  For those uninterested in how the library works,
there are only two things you need to know to use it: what parsers
are, how to use combinators on them, and how to use the special syntax
in the `parser` and `defparser` forms.

Parsers
-------

Parsers, in this library, are functions which accept a single
parameter, the `input`, and return either nil, if they cannot parse
anything from that input, or a list of `parser-pair` structs (defined
by the library), each of which represents a possible parsing of the
input, along with the remainder of the input which was not parsed.
For instance, a parser that parses "a" from a string looks like:

    (defun =parse-a (input)
     (if (empty? input)
         nil
         (let ((first-char (next input)))
           (if (string= first-char "a")
               (list (parser-pair first-char
                       (rest-of input)))
               nil))))

The function `next` is a method which fetches the next item from an
input.  `next` is defined for lists and strings by default, but can be
extended by the user with `defmethod`.  `rest-of` is its partner, it
returns the _rest_ of the input.  `parser-pair` constructs a
`parser-pair` struct instance whose first value is the parsed result,
and whose second value is the rest of the input that was not parsed.
As indicated above, returning `nil` means nothing was parsed at all.

Any function which conforms to this type can be treated as a parser by
the library.  If you want to write you own parsers without ever
touching the special syntax in the library, you do so just as we did
above.  By convention, parsers in the library start with the `=`
character, to distinguish them visually from other functions.

We could have written the above parser with a combinator from the
library, `=>string`, like so:

    (defun =parse-a (input) (funcall (=>string "a") input))

`=>string` is a function which takes a string and _returns_ a parser,
which we use to parse the input.  Any function which produces a parser
begins with `=>` in this library.  The simplest such function is the
function referred to as the parser return function, `=>`.

    (=> 'some-value)

`=>` returns a parser which does nothing to the input, and returns a
single parser-pair (in a list), whose return value is `some-value`.

The whole idea of this library is to construct parsers from simpler
parsers.

Special Syntax for Parser Construction and Definition
-----------------------------------------------------

The library provides syntax to make writing parsers easier.  We could
have defined `=parse-a` above like so, for instance:

    (defparser =parse-a 
      (x <- =item)
      (if (string= x "a") (=> x) =nil))

Since we often treat parsers as both functions _and_ regular
variables, `defparser` establishes both a variable `=parse-a` and a
function by the same name.  This function, as defined above, is
equivalent to the previous definitions, but how do we read it?

Within the body of a `defparser` (or `parser` form, which is the
anonymous version), each expression must be either a _parser_ itself
or a "binding expression" of the form

    `(variable <- parser-expression)` 

When an expression is just a parser, that parser must succeed or the
entire parser being defined will return nil.  Subsequent parsers
executed in the body will then see only the input not parsed by
previous parsers.  

When a _binding expression_ is encountered, the parser on the right
hand side is to the current input, which may have been parsed down by
previous expressions in the body, and, if the parse succeeds, then in
the rest of the body, the `variable` in the left-hand-side is bound to
the value in the value-slot of the `parser-pairs` returned by the
parser.  Hence, in the above expression, the first form:

    (x <- =item)

Sees `=item` applied to the input of the parser.  `=item` always pulls
one item off of the input and only fails when the input is empty.
If the input is empty, then the parse fails, and no further forms are
executed.  If `=item` succeeds, then we look to the next line.  The
next line is not a binding form.  It is an if statement, which is
fine, subject to the constraint that each branch must return a
parser.  If the item we've parsed from the input is "a", then we use
`=>` to create a parser which inserts "a" as its parser-pair's value.
If not, we return the `=nil` parser, which always fails.  

If we wanted to parse "a" and then "b", we'd write:

    (defparser =parse-ab
     (=>string "a")
     (=>string "b"))

Important Combinators
---------------------

The `=>or` combinator produces a parser if any of its input parsers
succeed, returning the value of the first success from left to
right. For instance, to parser "a" _or_ "b":

    (defparser =a-or-b 
     (=>or (=>string "a")
           (=>string "b")))

Then:

    (=a-or-b "abc") -> (list (parser-pair "a" "bc"))
    (=a-or-b "bbc") -> (list (parser-pair "b" "bc"))

The combinator `=>and` succeeds only when all of its input parsers
succeed _in turn_, finally returning the result of the last parser:

    (funcall (=>and (=>string "a") (=>string "b")) "ab") ->
     (list (parser-pair "b" ""))

The combinator `=>items` parses `n` or fewer items from the input,
regardless of what they are:

    (funcall (=>items 3) "abcd") -> 
     (list (parser-pair (list "a" "b" "c") "d"))

The combinator `=>zero-plus-more` parsers as many of its input parser
as possible and returns them in a list, possibly an empty one:

    (funcall (=>zero-plus-more
              (=>string "a"))
             "aaaab") ->
    (list (parser-pair (list "a" "a" "a" "a")
                       "b"))

The combinator `=>one-plus-more` does the same except it fails if
there is not at least one parsable object.

Non-determinism
---------------

Parseltongue, like SMUG, is actually a non-deterministic library -
parsers can parse in multiple ways _simultaneously_.  I'll write some
documentation about that later, but if you are using it for regular
deterministic parser, you are usually interested in only the first
parser-result.  Hence, the function `parse/first-result` is a handy
thing:

    (parser/first-result (=>string "a") "abc") -> "a"

It returns the first parse result and leaves off the leftover input.  

Thanks
------

I'd like to thank Drew for writing up SMUG, which was critical in
developing an understanding of monads in Lisp and obviously in
inspiring this library.  He also provided some correspondence when I
didn't understand aspects of his code.

Other Notes:
------------

If you like this library, it is almost a line for line port of an
Elisp parser combinator library I also wrote, available in my
[emacs-utils][] repository here on github.

* * *

[SMUG]:http://common-lisp.net/~dcrampsie/smug.html   
[emacs-utils]:https://github.com/VincentToups/emacs-utils


