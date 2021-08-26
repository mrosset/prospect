dnl configure.ac
dnl Copyright (C) 2021 Michael Rosset <mike.rosset@gmail.com>

dnl This file is part of Prospect

dnl Prospect is free software: you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.

dnl Prospect is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl See the GNU General Public License for more details.

dnl You should have received a copy of the GNU General Public License along
dnl with this program.  If not, see <http://www.gnu.org/licenses/>.

dnl autoconf
AC_INIT([prospect], [0.0.1-prospect], [mike.rosset@gmail.com])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])

dnl automake
AM_INIT_AUTOMAKE([gnu -Wall -Werror -Wno-portability -Wno-syntax  color-tests serial-tests])
AM_MAINTAINER_MODE([enable])
AM_SILENT_RULES([yes])

dnl libtool
LT_INIT([dlopen])

dnl Guile
GUILE_PKG([2.2])
GUILE_PROGS
GUILE_SITE_DIR
GUILE_FLAGS

SITEDIR="$datadir/guile/site/$GUILE_EFFECTIVE_VERSION";
SITECCACHEDIR="$libdir/guile/$GUILE_EFFECTIVE_VERSION/site-ccache";

AC_SUBST([SITEDIR])
AC_SUBST([SITECCACHEDIR])

AC_CONFIG_FILES([
Makefile
scheme/Makefile
test-suite/Makefile
])

AC_CONFIG_FILES([pre-inst-env],[chmod +x pre-inst-env])

dnl Sub project
AC_CONFIG_SUBDIRS([libblkmaker])

AC_OUTPUT
