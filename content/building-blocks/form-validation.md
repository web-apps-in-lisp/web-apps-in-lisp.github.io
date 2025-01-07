+++
title = "Form validation"
weight = 140
+++

We can recommend the [clavier](https://github.com/mmontone/clavier/)
library for input validation.

See also the [cl-forms](https://github.com/mmontone/cl-forms) library
that offers many features:

- automatic forms
- form validation with in-line error messages
- CSRF protection
- client-side validation
- subforms
- Djula and Spinneret renderers
- default themes
- an online demo for you to try
- etc

Get them:

```lisp
(ql:quickload '(:clavier :cl-forms))
```

## Form validation with the Clavier library

Clavier defines validators as class instances. They come in many
types, for example the `'less-than`,
`greater-than` or `len` validators. They may
take an initialization argument.

We create them with either `make-instance` + the class name, either with their shortcut:

```lisp
(make-instance 'clavier:less-than-validator :number 10)
;; OR
(clavier:less-than 10)

;; both return =>
#<CLAVIER:LESS-THAN-VALIDATOR {10066FF47B}>
```

Then, validate them by calling `clavier:validate` with the value to validate:

```lisp
(clavier:validate * 9)
;; =>
T
NIL
```

It return two values: the status, an optional error message.

```lisp
(clavier:validate ** 11)
;; =>
NIL
"11 is not lower than 10"
```

It's also possible to just `funcall` the validator objects (they are
funcallable classes).

### Composing validators

You can compose them with boolean logic, using clavier's `||`, `&&` operators:

```lisp
(defparameter *validator* (clavier:||
                                   (clavier:blank)
                                   (clavier:&& (clavier:is-a-string)
                                               (clavier:len :min 10)))
  "Allow a blank value. When non blank, validate.")
```

This validator allows an input to be an empty string, but if it isn't, it validates it.

```lisp
(funcall *validator* "")
;; =>
T
NIL

(funcall *validator* "foo")
;; =>
NIL
"Length of \"asdf\" is less than 10"
```

## List of validators

This is the list of available validator classes and their shortcut function.

Some take an initialization argument. Look at your editor's tooltip for the function signature.

* equal-to-validator `(==)`
* not-equal-to-validator `(~=)`
* blank-validator `(blank)`
* not-blank-validator `(not-blank)`
* true-validator `(is-true)`
* false-validator `(is-false)`
* type-validator `(is-a type)`
* string-validator `(is-a-string)`
* boolean-validator `(is-a-boolean)`
* integer-validator `(is-an-integer)`
* symbol-validator `(is-a-symbol)`
* keyword-validator `(is-a-keyword)`
* list-validator `(is-a-list)`
* function-validator `(fn function message)`
* email-validator `(valid-email)`
* regex-validator `(matches-regex)`
* url-validator `(valid-url)`
* datetime-validator `(valid-datetime)`
* pathname-validator `(valid-pathname)`
* not-validator `(~ validator)`
* and-validator `(&& validator1 validator2)`
* or-validator `(|| validator1 validator2)`
* one-of-validator `(one-of options)`
* less-than-validator `(less-than number)`
* greater-than-validator `(greater-than number)`
* length-validator `(len)`
* `:allow-blank` (not merged, only in my fork)

### Validation utils

We share some functions we used to complement Clavier or make it more convenient.

For example we wanted a shorter construct for this common need seen
above of allowing empty strings.

We also want a function to validate a list of validators and collect
the error messages.

We define a `validate-all` function that takes a list of
validators and validates them in turn on `object`.

It recognizes a `:allow-blank` keyword.

```lisp
(defun validate-all (validators object)
  "Run all validators in turn. Return two values: the status (boolean), and a list of messages.

  Allow a keyword validator: :allow-blank. Accepts a blank value. If not blank, validate."
  ;; I wanted this to be part of clavier, but well.
  ;; https://github.com/mmontone/clavier/pull/10
  (let ((messages nil)
        (valid t))
    (loop for validator in validators
          if (and (eql :allow-blank validator)
                  (str:blankp object))
            return t
          else
            do (unless (symbolp validator)
                 (multiple-value-bind (status message)
                     (clavier:validate validator object :error-p nil)
                   (unless status
                     (setf valid nil))
                   (when message
                     (push message messages)))))
    (values valid
            (reverse (uiop:ensure-list messages)))))
```

Usage:

```lisp
(validate-all (list :allow-blank (clavier:len :min 5)) "")
T
NIL


(validate-all (list :allow-blank (clavier:len :min 5)) "foo")
NIL
("Length of \"foo\" is less than 5")
```

Note that Clavier has a "validator-collection" thing, but not shown in
the README, and is in our opininion too verbose in comparison to a
simple list.
