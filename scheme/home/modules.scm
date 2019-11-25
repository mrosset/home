;; modules.scm
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

(define-module (home modules)
  #:use-module (home path)
  #:use-module (home dir)
  #:use-module (home git)
  #:use-module (home file)
  #:use-module (home init)
  #:use-module (home doc-here)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (srfi srfi-26)
  #:use-module (home generics)
  #:duplicates (merge-generics))

 ;;; Taken from Emacsy
(define (re-export-modules . modules)
  "Re-export modules"
  (define (re-export-module module)
    (module-for-each
     (lambda (sym var)
       ;;(format #t "re-exporting ~a~%" sym)
       (module-re-export! (current-module) (list sym)))
     (resolve-interface module)))
  (for-each re-export-module modules))

(re-export-modules '(srfi srfi-26)
		   '(oop goops)
		   '(oop goops describe)
		   '(home path)
		   '(home dir)
		   '(home git)
		   '(home init)
		   '(home file)
		   '(home doc-here)
		   '(home generics))
