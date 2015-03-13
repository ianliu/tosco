# optspline - Optimally fit a cubic spline to sampled points
# Copyright (C) 2001-2009 Ricardo Biloti <biloti@ime.unicamp.br>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([optspline],[0.1.0])
AC_CONFIG_SRCDIR([src/optspline.c])
AM_INIT_AUTOMAKE

AC_SUBST(VERSION)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_CC

dnl Checks for header files.
AC_HEADER_STDC

AC_HEADER_MAJOR
AC_FUNC_ALLOCA
AC_FUNC_CLOSEDIR_VOID

dnl Check for libraries
AC_CHECK_LIB(m,main,,[AC_MSG_ERROR(could not find m library)])

dnl Checking for GSL
AC_CHECK_LIB(gslcblas,main,,[AC_MSG_ERROR(Optspline requires cblas library)])
AC_CHECK_LIB(gsl,main,,     [AC_MSG_ERROR(Optspline requires gsl)])

# If ATLAS is not available
PKG_CHECK_MODULES(GLIB,glib-2.0)

LIBS="  $LIBS   $GLIB_LIBS"
CFLAGS="$CFLAGS $GLIB_CFLAGS"

CFLAGS="$CFLAGS -O2 -g \
        -funroll-loops \
	-Wall \
        -Wbad-function-cast \
        -Wformat \
        -Wparentheses \
	-Wsign-compare \
        -Wmissing-prototypes"

AC_OUTPUT( Makefile src/Makefile src/tests/Makefile gebr/Makefile )

