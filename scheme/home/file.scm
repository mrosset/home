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

(define-module (home file)
  #:use-module (home path)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module ((gcrypt hash) #:prefix gcrypt:)
  #:use-module (gcrypt base16)
  #:export (
	    <file>
	    sum
	    sum=
	    check-sum?
	    file-hash))

(define-class <file> (<path>)
  (mode #:accessor mode #:init-keyword #:mode #:init-value #o644)
  (hash #:accessor file-hash #:init-keyword #:hash #:init-value #f)
  (type #:accessor type #:init-keyword #:type #:init-value 'file))

(define-method (sum= (a <file>) (b <file>))
  (string= (file-hash a) (file-hash b)))

(define-method (sum (self <file>))
  "Returns the sha256 sum of file"
  (bytevector->base16-string (gcrypt:file-sha256 (path-name self))))

(define-method (check-sum? (self <file>))
   (string= (file-hash self) (sum self)))
