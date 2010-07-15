#!/bin/bash
# ToSCo SU Packaging
# Copyright (C) 2010 Luis D'Afonseca <luis.dafonseca@gebrproject.com>
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
#-----------------------------------------------------------------------------#
# dbuild.sh
# This script is used only for building the ToSCo deb pacage
#
# $Id$
#-----------------------------------------------------------------------------#

if [ $# -lt 2 ]; then
  echo $0 version description
  exit 1;
fi

LOGFILE="${PWD}/dbuild.log"

echo "Building: $1 - $2" > ${LOGFILE}
date >> ${LOGFILE}

if [ ! -e export ]; then

  mkdir export && sudo mount -t tmpfs tmpfs export

  svn export --force . export

  cd export

  dch -v $*

else

  echo Recicled export

fi

export CWPROOT="/tmp/tosco-su"

for ARCH in amd64 i386; do

  export SUARCH=${ARCH}

  for DIST in jaunty lenny karmic lucid; do

    echo "$ARCH - $DIST" >> ${LOGFILE}

    sudo rm -f /export_local/pbuilder/$DIST-$ARCH/result/*
    sudo mount -t tmpfs tmpfs /var/cache/pbuilder/build

    perl -i -pe "s/\) .*; urgency/) $DIST; urgency/" */debian/changelog

    ARCH=$ARCH pdebuild                                                   && \
    sudo dput -u local /export_local/pbuilder/$DIST-$ARCH/result/*changes && \
    sudo umount /var/cache/pbuilder/build                                 || exit 55

  done

done

echo "Cleaning" >> ${LOGFILE}

cd ..

sudo umount export
sudo rmdir  export

rm -f "tosco_*_amd64.build"
rm -f "tosco_*.dsc"
rm -f "tosco_*_source.changes"
rm -f "tosco_*.tar.gz"

echo "Done" >> ${LOGFILE}

