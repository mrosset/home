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
  #:export (
            ~
            ~/
            //
            home
            input
            <path>
            string->path
            disk->path
            path
            path=
            mode
            type
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
