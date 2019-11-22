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

(define-module (tests path)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (unit-test)
  #:use-module (home modules))

(define-class <test-dir> (<test-case>))

(define-method (test-dir-defaults (self <test-dir>))
  (let ((path (make <dir>)))
    (assert-equal #o755 (mode path))
    (assert-equal 'directory (type path))))

(define-method (test-dir-build (self <test-dir>))
  (let* ((tmp (tmpnam))
         (tmp-dir (make <dir>
                    #:path tmp
                    #:entries (list (make <dir>
                                      #:path (string-append tmp // "dir1"))))))
    (dynamic-wind
      (lambda _ #t)
      (lambda _
        (assert-true (ensure tmp-dir #f))
        (assert-true (path= (disk->file (path-name tmp-dir)) tmp-dir))
        (assert-true (exists? tmp-dir)))
      (lambda _
        #t
        (when #f (exists? tmp-dir)
            (remove tmp-dir)
         (assert-false (exists? tmp-dir)))))))
