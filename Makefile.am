SUBDIRS = scheme test-suite src

CLEANFILES = coverage lcov.info

lcov.info: all
	$(top_builddir)/pre-inst-env $(top_builddir)/tools/coverage

coverage: lcov.info
	$(GENHTML) -o $(top_builddir)/coverage lcov.info

test: check
	$(MAKE) -C src test
