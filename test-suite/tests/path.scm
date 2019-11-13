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

(define (println var)
  (format #t "~a~%" var))

(define-class <test-path> (<test-case>))

(define-method (test-equal? (self <test-path>))
  (assert-true (equal? (make <path> #:path (lambda _ "/tmp") #:dir? #f #:mode 422)
                       (make <path> #:path (lambda _ "/tmp") #:dir? #f #:mode 422)))
  (assert-false (equal? (make <path> #:path (lambda _ "/foo") #:dir? #f #:mode 422)
                        (make <path> #:path (lambda _ "/bar") #:dir? #f #:mode 422)))
  (assert-false (equal? (make <path> #:path (lambda _ "/tmp") #:dir? #t #:mode 422)
                        (make <path> #:path (lambda _ "/tmp") #:dir? #f #:mode 422)))
  (assert-false (equal? (make <path> #:path (lambda _ "/tmp") #:dir? #f #:mode 420)
                        (make <path> #:path (lambda _ "/tmp") #:dir? #f #:mode 422))))

(define-method (test-constructors (self <test-path>))
  (assert-true (path= "/tmp/home" (make-path "/tmp/home")))
  (let* ((tmp (tmpnam))
         (data (make <path> #:path (lambda _ tmp) #:dir? #t #:mode 420)))
    (dynamic-wind
      (lambda _
        (mkdir tmp #o700))
      (lambda _
        (assert-true (equal? data (disk->path tmp))))
      (lambda _
        (rmdir tmp)))))

(define-method (test-path-to-string (self <test-path>))
  (assert-equal "/tmp/home" (path->string (make-path "/tmp/home"))))

(define-method (test-path-equal (self <test-path>))
  (let ((test-path (make-path "/tmp/home")))
    (assert-true (path= "/tmp/home" test-path))
    (assert-true (path= test-path "/tmp/home"))
    (assert-true (path= test-path test-path))))

(define-method (test-join-path (self <test-path>))
  (assert-equal "/tmp/home" (join "/tmp" "home")))

(define-method (test-home-path (self <test-path>))
  (assert-equal <path> (class-of ~))
  (with-fluids ((fluid~ "/tmp/home"))
    (assert-true (path= "/tmp/home" ~))
    (assert-equal "/tmp/home/file" (join ~ "file"))
    (assert-equal "/tmp/home/dir/file" (join ~ '("dir" "file")))
    (assert-equal '() (paths ~))
    (assert-true (begin (add-child ~ "child")
                        (path= "/tmp/home/child"(car (paths ~)))))
    (with-fluids ((fluid~ "/tmp/home2"))
      (assert-equal "/tmp/home2/child"  (path->string (car (paths ~))))))
  (assert-equal #o700 (mode ~))
  (assert-true (dir? ~)))

(exit-with-summary (run-all-defined-test-cases))
