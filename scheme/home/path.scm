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

(define-module (home path)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module ((gcrypt hash) #:prefix gcrypt:)
  #:use-module (gcrypt base16)
  #:use-module (home records)
  #:export (
            ~
            ~/
            //
            home
            <path>
            <file>
            <dir>
            <doc-here>
            string->path
            disk->path
            path
            path=
            file=
            file-hash
            sum
            sum=
            mode
            type
            check-sum?
            path-name
            fluid~))

(define fluid~ (make-fluid (getenv "HOME")))

(define // file-name-separator-string)

(define ~ (lambda _ (fluid-ref fluid~)))

(define ~/
  (cut string-append (~) // <>))

(define-class <path> (<string>)
  (input #:accessor input #:init-keyword #:input #:init-value #f)
  (path #:accessor path #:init-keyword #:path #:init-value (lambda _ #f))
  (paths #:accessor paths #:init-keyword #:paths #:init-value #f)
  (mode #:accessor mode #:init-keyword #:mode #:init-value #o644)
  (type #:accessor type #:init-keyword #:type #:init-value #f))

(define-class <file> (<path>)
  (mode #:accessor mode #:init-keyword #:mode #:init-value #o644)
  (hash #:accessor file-hash #:init-keyword #:hash #:init-value #f)
  (type #:accessor type #:init-keyword #:type #:init-value 'file))

(define-method (sum= (a <file>) (b <file>))
  (string= (file-hash a) (file-hash b)))

(define-method (file= (a <file>) (b <file>))
  (if (and (string= (file-hash a) (file-hash b))
           (equal? a b))
      #t
      (begin (describe a)
             (describe b)
             #f)))

(define-method (sum (self <file>))
  (bytevector->base16-string (gcrypt:file-sha256 (path-name self))))

(define-method (check-sum? (self <file>))
   (string= (file-hash self) (sum self)))

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

(define-class <dir> (<path>)
  (mode #:accessor mode #:init-keyword #:mode #:init-value #o755)
  (type #:accessor type #:init-keyword #:type #:init-value 'directory))

;; Path methods
(define-method (path-name (self <path>))
  (if (string? (path self))
      (path self)
      ((path self))))

(define-method (custom-write (self <path>) port)
  (with-output-to-port port
    (lambda _
      (format #t "#<~a name: ~s input: ~a mode: ~a type: ~a>"
              (class-name (class-of self))
              (path-name self)
              (input self)
              (mode self)
              (type self))
      (close-port port))))

(define-method (equal? (a <path>) (b <path>))
  (if (and (string= (path-name a) (path-name b))
           (eq? (paths a) (paths b))
           (eq? (mode a) (mode b))
           (eq? (type a) (type b)))
      #t
      #f))

(define-method (string->path (self <string>))
  (make <path> #:path (lambda _ self)))

(define-method (disk->path (self <string>))
  (let ((fi (stat self)))
    (make <path>
      #:path (lambda _ self)
      #:type (stat:type fi))))

(define-method (path= (a <string>) (b <path>))
  (string= a (path-name b)))

(define-method (path= (a <path>) (b <path>))
  (string= (path-name a) (path-name b)))
