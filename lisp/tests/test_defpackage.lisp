(in-package :lispyboi)

(require "asserts")

(defpackage :foo
  (:use :lispyboi)
  (:export foo
           bar)
  (:export +ok+
           +yep+))

(defun foo () 'foo)
(defun bar () 'bar)
(defun not-exported () 'not-exported)

(setq +ok+ 99999)
(setq +yep+ 11111)

(in-package :foo)

(defun foo ()
  'foo)

(defun bar ()
  'bar)

(defun not-exported ()
  'not-exported)

(setq +ok+ 123456)
(setq +yep+ 314159)

(in-package :lispyboi)

(assert-eq 'foo (foo))
(assert-eq 'bar (bar))
(assert-eq 'not-exported (not-exported))

(assert-eq 'foo::foo (foo:foo))
(assert-eq 'foo::bar (foo:bar))
(assert-eq 'foo::foo (foo::foo))
(assert-eq 'foo::bar (foo::bar))
(assert-eq 'foo::not-exported (foo::not-exported))

(assert-= 99999 +ok+)
(assert-= 11111 +yep+)

(assert-= 123456 foo:+ok+)
(assert-= 314159 foo:+yep+)
