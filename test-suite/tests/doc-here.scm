;; doc-here.scm
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

(define-module (tests doc-here)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (unit-test)
  #:use-module (home modules))

(define-class <test-doc-here> (<test-case>))

(define-method (test-doc-here-methods (self <test-doc-here>))
  (let* ((doc (make <doc-here> #:content "GNU"))
	 (fail-doc (shallow-clone doc)))
    (set! (file-hash fail-doc) "fail")
    (assert-true (exists? doc))
    (assert-true (check-sum? doc))
    (assert-false (sum= doc fail-doc))))

(define-method (test-doc-here-copy (self <test-doc-here>))
  (let* ((tmp (tmpnam))
	 (doc (make <doc-here> #:content "GNU"))
	 (file (make <file> #:path tmp)))
    (dynamic-wind
      (lambda _
	(copy doc file))
      (lambda _
	(assert-equal (sum doc) (sum file)))
      (lambda _
	(delete-file tmp)))
    (copy doc file)))

(exit-with-summary (run-all-defined-test-cases))
