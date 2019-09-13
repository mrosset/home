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
  #:use-module (oop goops)
  #:export (// fluid~ ~ home))

(define // file-name-separator-string)

(define fluid~ (make-fluid (getenv "HOME")))

(define (prefix-join prefix args)
  "join ARGS path under PREFIX"
  (string-append prefix
		 //
		 (string-join args //)))

(define-class <path> (<string>)
  (path #:init-keyword #:path #:init-value (lambda _ #f))
  (paths #:accessor paths #:init-keyword #:dirs #:init-value '())
  (mode #:accessor mode #:init-keyword #:mode #:init-value #o644)
  (dir? #:accessor dir? #:init-keyword #:dir? #:init-value #f)
  )

(define-method (make-path (self <string>))
  (make <path> #:path (lambda _ self)))

(define-method (path (self <path>))
  ((slot-ref self 'path)))

(define-method (join (self <string>) (p <string>))
  (string-append self // p))

(define-method (join (self <path>) (p <string>))
  (string-append (path self) // p))

(define-method (join (self <path>) (paths <list>))
  (prefix-join (path self) paths))

(define-method (home (self <path>))
  (path self))

(export <path> home join paths path make-path mode dir?)

(define-public ~
  (make <path>
    #:path (lambda _ (fluid-ref fluid~))
    #:dir? #t
    #:mode #o700
    ))

;; (define-method (home (p <path>))
  ;; (path p))
