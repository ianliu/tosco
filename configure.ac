# ToSCo - Tools for Scientific Computing
# Copyright (C) 2009 Luis D'Afonseca <luis.dafonseca@gebrproject.com>
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

AC_INIT(s88modeling)

PACKAGE=tosco
VERSION=0.3.0
AC_SUBST(PACKAGE)
AC_SUBST(VERSION)
AC_SUBST(TODAY)

AM_INIT_AUTOMAKE($PACKAGE,$VERSION)
PACKAGE_NAME=$PACKAGE
PACKAGE_VERSION=$VERSION
TOSCO_MENU_DIR='$(datadir)/gebr/menus/tosco'
AC_SUBST([TOSCO_MENU_DIR])

AC_CONFIG_SUBDIRS([cropcoher mcdec optspline rays2 s88modeling seis88 sugeom suktmig2d-offset suxkill suxmute velan-cmp])

AC_OUTPUT( Makefile packages/Makefile )

