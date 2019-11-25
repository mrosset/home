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

(define-module (tests git)
  #:use-module (unit-test)
  #:use-module (home modules))

(define-class <test-dir> (<test-case>))

(define-method (test-git-build (self <test-dir>))
  (let ((dir (make <git>
	       #:mode #o755
	       #:path "/tmp/git-test"
	       #:hash "7ff0d888dcdf061b46d227cacf6d50cb7f5442ea"
	       #:url "/home/mrosset/src/nomad")))
    (dynamic-wind
      (lambda _ #t)
      (lambda _
	(build dir)
	(assert-true (exists? dir)))
      (lambda _
	(remove dir)))
    ))
