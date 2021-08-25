
####
#### Copyright (C) 2016 - 2018
#### Free Software Foundation, Inc.

#### This file is part of GNU G-Golf

#### GNU G-Golf is free software; you can redistribute it and/or modify
#### it under the terms of the GNU Lesser General Public License as
#### published by the Free Software Foundation; either version 3 of the
#### License, or (at your option) any later version.

#### GNU G-Golf is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### Lesser General Public License for more details.

#### You should have received a copy of the GNU Lesser General Public
#### License along with GNU G-Golf.  If not, see
#### <https://www.gnu.org/licenses/lgpl.html>.
####


moddir=@SITEDIR@
godir=@SITECCACHEDIR@


GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

AM_CFLAGS = -I. -I$(srcdir) $(WARN_CFLAGS) $(DEBUG_CFLAGS)

# For overriding from the command line (e.g. --debug)
# GUILE_FLAGS =

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env \
	guild compile $(GUILE_WARNINGS) -o "$@" "$<"
