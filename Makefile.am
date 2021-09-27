## Makefile.am
## Copyright (C) 2021 Michael Rosset <mike.rosset@gmail.com>

## This file is part of Prospect

## Prospect is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## Prospect is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
## See the GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this program.  If not, see <http://www.gnu.org/licenses/>.

ACLOCAL_AMFLAG = -I m4

.NOTPARALLEL:

SUBDIRS = . scripts scheme test-suite

do_subst = sed -e 's,[@]abs_top_builddir[@],$(abs_top_builddir),g' \
	-e 's,[@]SHELL[@],$(SHELL),g'

## Pre Environment Script
bin_SCRIPTS = pre-inst-env

CLEANFILES = $(bin_SCRIPTS)

pre-inst-env: pre-inst-env.in
	$(do_subst) < $< > $@
	chmod +x $@

## Utility Targets
repl: all
	./pre-inst-env guile

py-merkle:
	python ./scripts/merkle.py

debug: all
	./pre-inst-env gdb --args guile scripts/debug.scm

run: scripts
	./pre-inst-env guile scripts/debug.scm
