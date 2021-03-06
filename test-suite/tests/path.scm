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
  #:use-module (oop goops describe)
  #:use-module (unit-test)
  #:use-module (home modules))

(define-class <test-path> (<test-case>))

(define-method (test-path-fluid (self <test-path>))
  (with-fluids ((fluid~ "/tmp/home"))
    (assert-equal "/tmp/home" (path-name (make <path> #:path ~)))))

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

(define-method (test-path-equality (self <test-path>))
  (assert-equal (make <path> #:path "/tmp" #:type 'directory #:mode 422)
                (make <path> #:path "/tmp" #:type 'directory #:mode 422))
  (assert-false (equal? (make <path> #:path "/foo" #:type 'directory  #:mode 422)
                (make <path> #:path "/bar" #:type 'directory #:mode 422)))
  (assert-false (equal? (make <path> #:path "/tmp" #:type 'file #:mode 422)
                        (make <path> #:path "/tmp" #:type 'directory #:mode 422)))
  (assert-false (equal? (make <path> #:path "/tmp" #:type #f #:mode 420)
                        (make <path> #:path "/tmp" #:type #f #:mode 422)))
  (assert-true  (path= "/tmp" (make <path> #:path "/tmp" #:type 'directory #:mode 422))))

(define-method (test-path-name (self <test-path>))
  (assert-equal "/tmp/home" (path-name (make <path> #:path "/tmp/home")))
  (assert-equal "/tmp/home" (path-name (make <path> #:path (lambda _ "/tmp/home"))))
  (with-parent "/tmp"
               (lambda (parent)
                 (assert-equal "/tmp/home" (path-name (make <child> #:root parent #:path (lambda _ "home"))))
                 (assert-equal "/tmp/home" (path-name (make <child> #:root parent #:path "home"))))))

(define-method (test-child (self <test-path>))
  (with-fluid* fluid~ "/tmp/home"
               (lambda ()
               (with-parent ~
                (lambda (parent)
                  (let ((child (make <child> #:root parent #:path "foo")))
                   (assert-equal "/tmp/home/foo" (path-name child)))))
                 )))

(define-method (test-constructors (self <test-path>))
  (assert-true (path= "/tmp/home" (string->path "/tmp/home")))
  (assert-equal (make <path> #:path "/tmp/foo") (make <path> #:path "/tmp/foo")))
