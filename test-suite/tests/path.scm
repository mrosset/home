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
  #:use-module (unit-test)
  #:use-module (home path)
  #:duplicates (merge-generics))

(define-class <test-path> (<test-case>))

(define-method (test-path-fluid (self <test-path>))
  (with-fluids ((fluid~ "/tmp/home"))
    (assert-equal "/tmp/home" (path-name (make <path> #:thunk ~)))))

(define-method (test-path-~ (self <test-path>))
  (with-fluids ((fluid~ "/tmp/home"))
    (assert-equal "/tmp/home" (~))))

(define-method (test-path-~/ (self <test-path>))
  (with-fluids ((fluid~ "/tmp/home"))
    (assert-equal "/tmp/home/foo" (~/ "foo"))))

(define-method (test-path-defaults (self <test-path>))
  (let ((path (make <path>)))
    (assert-equal #f (path-name path))
    (assert-equal #o644 (mode path))
    (assert-equal #f (type path))))

(define-method (test-file-defaults (self <test-path>))
  (let ((path (make <file>)))
    (assert-equal #o644 (mode path))
    (assert-equal 'file (type path))))

(define-method (test-dir-defaults (self <test-path>))
  (let ((path (make <dir>)))
    (assert-equal #o755 (mode path))
    (assert-equal 'dirctory (type path))))

(define-method (test-path-equality (self <test-path>))
  (assert-equal (make <path> #:thunk (lambda _ "/tmp") #:type 'directory #:mode 422)
                      (make <path> #:thunk (lambda _ "/tmp") #:type 'directory #:mode 422))
  (assert-false (equal? (make <path> #:thunk (lambda _ "/foo") #:type 'directory  #:mode 422)
                 (make <path> #:thunk (lambda _ "/bar") #:type 'directory #:mode 422)))
  (assert-false (equal? (make <path> #:thunk (lambda _ "/tmp") #:type 'file #:mode 422)
                        (make <path> #:thunk (lambda _ "/tmp") #:type 'directory #:mode 422)))
  (assert-false (equal? (make <path> #:thunk (lambda _ "/tmp") #:type #f #:mode 420)
                        (make <path> #:thunk (lambda _ "/tmp") #:type #f #:mode 422))))

(define-method (test-constructors (self <test-path>))
  (assert-true (path= "/tmp/home" (string->path "/tmp/home")))
  (let* ((tmp (tmpnam))
         (data (make <path> #:thunk (lambda _ tmp) #:type 'directory #:mode 420)))
    (dynamic-wind
      (lambda _
        (mkdir tmp #o700))
      (lambda _
        (assert-equal data (disk->path tmp)))
      (lambda _
        (rmdir tmp)))))

(exit-with-summary (run-all-defined-test-cases))
