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

include $(top_srcdir)/am/guile.mk

## Guile extention
lib_LTLIBRARIES = libguile-prospect.la
libguile_prospect_la_SOURCES = extention.c
libguile_prospect_la_CFLAGS  = $(GUILE_CFLAGS) $(BLKMK_CFLAGS) -fPIC  -g
libguile_prospect_la_LDFLAGS = $(BLKMK_LIBS) $(LIBGCRYPT_LIBS)

## Guile scheme
SOURCES  = prospect/util.scm
SOURCES += prospect/work.scm
SOURCES += prospect/rpc.scm
SOURCES += prospect/ffi.scm

CLEANFILES = $(GOBJECTS)
