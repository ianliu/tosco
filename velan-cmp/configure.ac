# velan-cmp - Velocity analisys on CMP sorted data
# Copyright (C) 2010 Alam Souza CPGG/UFBA <aavbs@cpgg.ufba.br>
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

AC_INIT([velan-cmp],[0.1.0])
AC_CONFIG_SRCDIR([src/velan-cmp])
AM_INIT_AUTOMAKE

AC_SUBST(VERSION)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

#### AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL
#### AC_PROG_CC

dnl Checks for header files.
#### AC_HEADER_STDC

#### AC_HEADER_MAJOR
#### AC_FUNC_ALLOCA
#### AC_FUNC_CLOSEDIR_VOID

dnl Check for libraries
dnl-----------------------------------------------------------------------
#### AC_CHECK_LIB(m,main,,[AC_MSG_ERROR(could not find m library)])

dnl Check for SU
dnl-----------------------------------------------------------------------

echo -n "Checking for SU variable... "

if test "x${CWPROOT}" == "x"; then
  echo "failed"
  AC_MSG_ERROR([environment variable CWPROOT is not defined])
else
  echo "CWPROOT=${CWPROOT}"
fi

SU_LIBS="-L${CWPROOT}/lib -lsu -lxplot -lSM -lICE -lX11 -lpar -lcwp"

SU_CFLAGS="-I${CWPROOT}/include"
SU_CFLAGS="${SU_CFLAGS} -DCWP_LITTLE_ENDIAN"
SU_CFLAGS="${SU_CFLAGS} -D_FILE_OFFSET_BITS=64"
SU_CFLAGS="${SU_CFLAGS} -D_LARGEFILE_SOURCE"
SU_CFLAGS="${SU_CFLAGS} -D_LARGEFILE64_SOURCE"
SU_CFLAGS="${SU_CFLAGS} -DGNU_SOURCE"
SU_CFLAGS="${SU_CFLAGS} -D_POSIX_SOURCE"

LIBS="  $LIBS   $SU_LIBS"
CFLAGS="$CFLAGS $SU_CFLAGS"

dnl-----------------------------------------------------------------------

AC_OUTPUT( Makefile src/Makefile gebr/Makefile )

dnl-----------------------------------------------------------------------
