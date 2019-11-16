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

(define-module (tests file)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (unit-test)
  #:use-module (home file))

(define-class <test-file> (<test-case>))

(define-method (test-file-defaults (self <test-file>))
  (let ((path (make <file>)))
    (assert-equal #o644 (mode path))
    (assert-equal 'file (type path))))
