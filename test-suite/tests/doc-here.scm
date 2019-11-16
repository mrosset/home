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
  #:use-module (home doc-here)
  #:use-module (home file)
  #:duplicates (merge-generics))

(define-class <test-doc-here> (<test-case>))

 (define-method (test-validation (self <test-doc-here>))
  (let* ((tmp (tmpnam))
	 (data "GNU")
	 (doc (make <doc-here>
		#:content "GNU"))
	 (file (make <file>
		 #:path tmp
		 #:hash "82781e26505c5484af6435ae1aab1b44a5f4f49ffec39a4bdee63f9d347862b0"))
	 (fail-doc (shallow-clone doc))
	 (fail-file (shallow-clone doc)))
    (set! (file-hash fail-doc) "fail")
    (set! (file-hash fail-file) "fail")
    (dynamic-wind
      (lambda _
	(call-with-output-file tmp
	  (lambda (port)
	    (display data port)
	    (close-output-port port))))
      (lambda _
	(assert-true (sum= doc file))
	(assert-true (check-sum? doc))
	(assert-true (check-sum? file))
	(assert-false (check-sum? fail-doc))
	(assert-false (check-sum? fail-file)))
      (lambda _
	(delete-file tmp)
	))))

(exit-with-summary (run-all-defined-test-cases))
