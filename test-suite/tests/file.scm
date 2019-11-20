;; file.scm
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

(define-module (tests file)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (unit-test)
  #:use-module (home modules))

(define-class <test-file> (<test-case>))

(define-method (test-file-defaults (self <test-file>))
  (let ((file (make <file>)))
    (assert-equal #o644 (mode file))
    (assert-equal 'file (type file))))

(define-method (test-file-equality (self <test-file>))
  (let* ((file-a (make <file> #:path "/tmp/foo" #:hash "foo"))
        (file-b (shallow-clone file-a))
        (file-c (make <file> #:path "/tmp/fo" #:hash "foo")))
    (assert-equal file-a file-b)
    (assert-false (equal? file-a file-c))))

(define-method (test-file-ensure (self <test-file>))
  (let* ((tmp (tmpnam))
         (file (make <file>
                 #:hash "82781e26505c5484af6435ae1aab1b44a5f4f49ffec39a4bdee63f9d347862b0"
                 #:path tmp
                 #:mode #o644
                 #:input (make <doc-here> #:path tmp #:content "GNU"))))
    (dynamic-wind
      (lambda _
        (assert-true (ensure file #f))
        (assert-true (exists? file))
        (assert-false (ensure file #f)))
      (lambda _
        (assert-true (check-sum? file)))
      (lambda _
        (delete-file tmp)
        (assert-false (exists? file))))))
