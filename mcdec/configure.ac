# mcdec - Direct multichannel predictive deconvolution
# Copyright (c) 2009 Milton J. Porsani <porsani@cpgg.ufba.br>
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

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/mcdec.f90)

dnl Every other copy of the package version number gets its value from here
AM_INIT_AUTOMAKE(mcdec,0.1.0)

AC_SUBST(VERSION)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_FC

dnl Libs needed to link fortran code on C code
LIBS="$LIBS `gfortran -print-file-name=libgfortran.so`"

dnl Checks for header files.
AC_HEADER_STDC

AC_HEADER_MAJOR
AC_FUNC_ALLOCA
AC_FUNC_CLOSEDIR_VOID

dnl Check for libraries
AC_CHECK_LIB(m,main,,[AC_MSG_ERROR(could not find m library)])

PKG_CHECK_MODULES(GLIB,glib-2.0)

LIBS="  $LIBS   $GLIB_LIBS"
CFLAGS="$CFLAGS $GLIB_CFLAGS"

AC_OUTPUT( Makefile src/Makefile gebr/Makefile )

