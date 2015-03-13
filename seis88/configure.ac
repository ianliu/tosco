# seis88 - Rays tracing tool
# Copyright (C) 2009 Ivan Psencik <ip@ig.cas.cz>
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

AC_INIT([seis88],[0.1.0])
AC_CONFIG_SRCDIR([src/seis.f])
AM_INIT_AUTOMAKE

AC_SUBST(VERSION)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

AC_CANONICAL_HOST

FFLAGS="-std=legacy -g -O0 -freal-4-real-8 -finteger-4-integer-8"

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_F77( ifort gfortran f77 )

AC_OUTPUT( Makefile src/Makefile )

