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
  #:export (// fluid~ ~ home <path> path dirs ensure? mode))

(define // file-name-separator-string)

(define fluid~ (make-fluid (getenv "HOME")))

(define (prefix-join prefix . args)
  "join ARGS path under PREFIX"
  (string-append prefix
		 //
		 (string-join args //)))

(define-class <path> (<string>)
  (path #:init-keyword #:path #:init-value (lambda _ #f))
  (dirs #:accessor dirs #:init-keyword #:dirs #:init-value '())
  (ensure #:accessor ensure? #:init-keyword #:ensure #:init-value #t)
  (mode #:accessor mode #:init-keyword #:mode #:init-value #o644)
  )

(define-method (path (path <path>))
  ((slot-ref path 'path)))

(define-syntax ~
  (syntax-rules ()
    ((~)
     (fluid-ref fluid~))
    ((~ exp)
     (string-append (~) // exp))
    ((~ . args)
     (prefix-join (~) . args))))

(define home
  (make <path>
    #:path (lambda _ (fluid-ref fluid~))
    #:ensure #t
    #:mode #o700))
