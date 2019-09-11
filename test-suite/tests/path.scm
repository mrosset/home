;; path.scm
;; Copyright (C) 2017-2019 Michael Rosset <mike.rosset@gmail.com>

;; This file is part of Home

;; Home is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Home is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (tests path)
  #:use-module (oop goops)
  #:use-module (home path)
  #:use-module (unit-test))

(define-class <test-path> (<test-case>))

(define-method (test-~-macro (self <test-path>))
  (with-fluids ((fluid~ "/tmp/home"))
    (assert-equal "/tmp/home" (fluid-ref fluid~))
    (assert-equal "/tmp/home" (~))
    (assert-equal "/tmp/home/file" (~ "file"))
    (assert-equal "/tmp/home/foobar" (~ (string-append "foo" "bar")))
    (assert-equal "/tmp/home/dir/file" (~ "dir" "file"))
    ))

(define-method (test-home-path (self <test-path>))
  (assert-equal <path> (class-of home))
  (with-fluids ((fluid~ "/tmp/home"))
    (assert-equal "/tmp/home" (path home)))
  (assert (ensure? home))
  (assert-equal #o700 (mode home))
  )

(exit-with-summary (run-all-defined-test-cases))
