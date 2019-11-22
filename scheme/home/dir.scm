;; dir.scm
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
(define-module (home dir)
  #:use-module (home path)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export (<dir> path= remove entries type))

(define-class <dir> (<path>)
  (mode #:accessor mode #:init-keyword #:mode #:init-value #o755)
  (entries #:accessor entries #:init-keyword #:entries #:init-value #f)
  (type #:accessor type #:init-keyword #:type #:init-value 'directory))

(define-method (remove (self <dir>))
  (when (entries self)
    (for-each (lambda (entry)
                (remove entry))
              (entries self)))
  (rmdir (path-name self)))

(define-method (path= (a <dir>) (b <dir>))
  (if (and (string= (path-name a) (path-name b))
           ;; (eq? (entries a) (entries b))
           (equal? (mode a) (mode b)))
      #t
      #f))
