;; git.scm
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

(define-module (home git)
  #:use-module (home path)
  #:use-module (home dir)
  #:use-module (oop goops)
  #:use-module (git clone)
  #:use-module (git repository)
  #:use-module (git reference)
  #:use-module (git bindings)
  #:use-module (git oid)
  #:export (<git>))

(libgit2-init!)

(define-class <git> (<dir>)
  (hash #:accessor file-hash #:init-keyword #:hash #:init-value #f)
  (url #:accessor url #:init-keyword #:url)
  (branch #:accessor branch #:init-keyword #:branch #:init-value "master")
  (commit #:accessor commit #:init-keyword #:commit #:init-value #f))

(define-method (sum (self <git>))
  (let* ((repo (repository-open (path-name self)))
           (ref-name (reference-name (repository-head repo)))
           (oid (oid->string (reference-name->oid repo ref-name))))
      oid))

(define-method (sum= (self <git>))
  (string= (file-hash self) (sum self)))

(define-method (build (self <git>))
  (clone (url self) (path-name self))
  (unless (sum= self)
    (error (format #f "git hash does not match: expect ~a got ~a\n" (file-hash self) (sum self)))))
