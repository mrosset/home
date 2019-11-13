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
  #:export (fluid~ // ~ home <path> join paths path->string
                   make-path mode dir? add-child path path= disk->path))

(define fluid~ (make-fluid (getenv "HOME")))

;; (read-hash-extend #\~
;;                   (lambda (c port)
;;                     (~~)))

(define // file-name-separator-string)

(define (prefix-join prefix args)
  "join ARGS path under PREFIX"
  (string-append prefix
                 //
                 (string-join args //)))

(define-class <path> (<string>)
  (path #:init-keyword #:path #:init-value (lambda _ #f))
  (paths #:accessor paths #:init-keyword #:dirs #:init-value '())
  (mode #:accessor mode #:init-keyword #:mode #:init-value #o644)
  (dir? #:accessor dir? #:init-keyword #:dir? #:init-value #f))

(define-method (make-path (self <string>))
  (make <path> #:path (lambda _ self)))

(define-method (disk->path (self <string>))
  (let ((fi (stat self)))
    (make <path>
      #:path (lambda _ self)
      #:dir? (eq? (stat:type fi) 'directory))))

(define-generic equal?)

(define-method (equal? (a <path>) (b <path>))
  (if (and (string= (path->string a) (path->string b))
           (eq? (mode a) (mode b))
           (eq? (dir? a) (dir? b)))
      #t
      #f))

(define-method (write (self <path>) port)
  (write (string-concatenate (list "#<path '" (path->string self) "'>")) port))

(define-method (path->string (self <path>))
  ((slot-ref self 'path)))

(define-method (path= (s <string>) (self <path>))
  (string= s (path->string self)))

(define-method (path= (a <path>) (b <path>))
  (string= (path->string a) (path->string b)))

(define-method (path= (a <path>) (b <string>))
  (string= (path->string a) b))

(define-method (join (self <string>) (p <string>))
  (string-append self // p))

(define-method (join (self <path>) (p <string>))
  (string-append (path->string self) // p))

(define-method (join (self <path>) (paths <list>))
  (prefix-join (path->string self) paths))

(define-method (add-child (self <path>) (p <string>))
  (set! (paths self) (cons* (make <path> #:path (lambda _ (join self p))) (paths ~))))

(define-public ~
  (make <path>
    #:path (lambda _ (fluid-ref fluid~))
    #:dir? #t
    #:mode #o700))

;; (define-method (home (p <path>))
  ;; (path p))
