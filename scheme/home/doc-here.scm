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

(define-module (home doc-here)
  #:use-module (oop goops)
  #:use-module (home file)
  #:use-module ((gcrypt hash) #:prefix gcrypt:)
  #:use-module (gcrypt base16)
  #:export (<doc-here>
	    sum
	    check-sum?))

(define-class <doc-here> (<file>)
  (content #:accessor content #:init-keyword #:content #:init-value #f)
  (type #:accessor type #:init-keyword #:type #:init-value 'document))

(define-method (initialize (self <doc-here>) args)
  (next-method)
  (set! (file-hash self) (sum self)))

(define-method (sum (self <doc-here>))
  (call-with-input-string (content self)
    (lambda (port)
      (bytevector->base16-string (gcrypt:port-sha256 port)))))

(define-method (check-sum? (self <doc-here>))
  (string= (file-hash self) (sum self)))
