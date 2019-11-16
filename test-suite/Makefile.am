## Makefile.am
## Copyright (C) 2017-2019 Michael Rosset <mike.rosset@gmail.com>

## This file is part of Home

## Home is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## Home is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
## See the GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this program.  If not, see <http://www.gnu.org/licenses/>.

TESTS =
TESTS += tests/path.scm
TESTS += tests/dir.scm
TESTS += tests/file.scm
TESTS += tests/doc-here.scm
TESTS += tests/init.scm

TEST_EXTENSIONS = .scm

TESTS_ENVIRONMENT = \
	$(abs_top_builddir)/pre-inst-env		\
	$(GUILE) -L $(abs_top_builddir)/test-suite	\
		 --no-auto-compile

EXTRA_DIST = \
	$(TESTS)
