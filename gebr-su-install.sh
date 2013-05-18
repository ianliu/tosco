#!/bin/bash

#   Copyright 2010-2013 Ricardo Biloti <biloti@gebproject.com>

#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

function usage ()
{
CWPROOT="$STOW_PATH/su-$SU_VERSION"

echo "SU install made easy (3rd ed) - A cortesy of GeBR Project
Syntax: $0 [options]

Options:
    -h,-? : show this help
       -V : SU version ($SU_VERSION)
       -p : SU version prefix ($SU_VERSION_PREFIX)
       -a : SU archive ($SU_ARCHIVE)
       -d : CWP FTP URL ($CWP_SRC_URL)
       -i : Display the final CWPROOT directory ($CWPROOT)
       -o : Download path ($DOWNLOAD_PATH)
       -u : Uninstall SU ($UNINSTALL)
       -U : Keep previously installed SU ($KEEP_OLDER_SUS)
       -T : Target path to install SU ($TARGET_PATH)
       -F : Force reinstall ($REINSTALL)
       -t : Text mode (do not use graphical client to ask
            for passwords)

SU install path: $CWPROOT

This scripts is designed to Ubuntu/Debian Linux systems.
Debian users however should have administration privileges
through sudo utility. See the post
http://sl.gebrproject.com/easy-su-install

The newest version of this script is on
http://sl.gebrproject.com/gebr-su-install.sh

Any problem with this script, please report to
Ricardo Biloti <biloti@gebrproject.com>

2009-2013 (c) Ricardo Biloti - GeBR Project
http://www.gebrproject.com/
"
}

function check_pkg {

    dpkg -l $1 | grep ^ii > /dev/null 2>&1
    if [ $? -eq 1 ]; then
	echo "missing. Scheduling it for installing"
	PKGS_TO_INSTALL="$PKGS_TO_INSTALL $1"
    else
	echo "present"
    fi
}

# Default values
#------------------------------------------------------------------------------#

umask 022
SU_VERSION="43R3"
SU_VERSION_PREFIX="43"
SU_ARCHIVE="cwp_su_all_$SU_VERSION.tgz"
SU_SHA256SUM="1d043f0a6366fcebba3707eaa15283aae1186bb383c6e7cebf11afcea4f3ac81"
DOWNLOAD_PATH="$HOME/Downloads"
CWP_SRC_URL="ftp://ftp.cwp.mines.edu/pub/cwpcodes"
UNINSTALL="FALSE"
KEEP_OLDER_SUS="FALSE"
REINSTALL="FALSE"
TEXT_MODE="-A"
if [ $EUID == 0 ]; then
    TARGET_PATH="/usr/local"
else
    TARGET_PATH="$HOME/.local"
fi
STOW_PATH="$TARGET_PATH/stow"

# Parsing command line parameters
#------------------------------------------------------------------------------#
while getopts "T:V:p:a:o:d:uUFthis" OPT; do
  case $OPT in
      "h") usage; exit 0               ;;
      "?") usage; exit 0               ;;
      "V") SU_VERSION="$OPTARG";SU_ARCHIVE="cwp_su_all_$SU_VERSION.tgz"  ;;
      "p") SU_VERSION_PREFIX="$OPTARG" ;;
      "a") SU_ARCHIVE="$OPTARG"        ;;
      "o") DOWNLOAD_PATH="$OPTARG"     ;;
      "d") CWP_SRC_URL="$OPTARG"       ;;
      "u") UNINSTALL="TRUE"            ;;
      "U") KEEP_OLDER_SUS="TRUE"       ;;
      "F") REINSTALL="TRUE"            ;;
      "t") TEXT_MODE=""                ;;
      "i") INFO="1"                    ;;
      "s") SUPER="1"                   ;;
      "T") TARGET_PATH="$OPTARG";STOW_PATH="$TARGET_PATH/stow";;
  esac
done

# Try to become root before start
if [ $SUPER ] && [ $EUID != 0 ]; then
    SUDO_ASKPASS=${SUDO_ASKPASS:-"/usr/lib/openssh/gnome-ssh-askpass"} sudo $TEXT_MODE $0 $@ 2> /dev/null || echo "Please, try run $0 as root."
    exit
fi

CWPROOT="$STOW_PATH/su-$SU_VERSION"

if [ $INFO ]; then
  echo $CWPROOT
  exit
fi

cat <<EOF
SU install made easy - A cortesy of the GeBR Project

Try "$0 -h" to see the usage guide.
EOF

# Uninstall SU and exits
if [ "$UNINSTALL" == 'TRUE' ]; then
    cd $CWPROOT/..
    if [ -d su-$SU_VERSION ]; then
	echo "Uninstalling SU $SU_VERSION"
	stow -D su-$SU_VERSION
    else
	echo "Nothing to uninstall"
    fi
    exit 0;
fi

# Checks whether this intended version is already (partially) installed
if [ -d "$CWPROOT" -a "$REINSTALL" == "FALSE" ]; then 
    echo "
It seems that SU $SU_VERSION is already installed or, at least,
partially installed. This install attempt has been aborted now.

To reinstall SU $SU_VERSION, overwriting such previous install,
check the reinstall option of this script.

See the help with \"$0 -h\"."
    exit 0
else
    echo "
It seems that SU $SU_VERSION is already installed or, at least,
partially installed. Proceeding anyway, as requested."
fi
    
if [ ! -d "$CWPROOT" ]; then 
    mkdir -p "$CWPROOT"
fi

if [ ! -d "$DOWNLOAD_PATH" ]; then
    mkdir -p "$DOWNLOAD_PATH"
fi

cd "$DOWNLOAD_PATH"
if [ -e "$SU_ARCHIVE" ]; then 
    SHA256SUM=`sha256sum "$SU_ARCHIVE" | cut -f 1 -d\ `
    if [ "$SHA256SUM" = "$SU_SHA256SUM" ]; then
	echo "SU $SU_VERSION archive.............. Found"
    else
	echo "SU $SU_VERSION archive.............. Corrupted"
	rm "$SU_ARCHIVE"
    fi
fi

if [ ! -e "$SU_ARCHIVE" ]; then 
    echo "SU $SU_VERSION archive.............. Downloading it now to $DOWNLOAD_PATH"
    wget "$CWP_SRC_URL/$SU_ARCHIVE"
fi    

echo "Testing for required packages"

PKGS_TO_INSTALL=""

echo -n "gcc........................ "
check_pkg gcc

echo -n "gfortran................... "
check_pkg gfortran

echo -n "stow....................... "
check_pkg stow

echo -n "lesstif.................... "
check_pkg lesstif2-dev

echo -n "GLUT....................... "
check_pkg freeglut3-dev

echo -n "Xmu........................ "
check_pkg libxmu-dev

if [ "$PKGS_TO_INSTALL"'x' != 'x' ]; then
    echo "Installing missing packages"
    apt-get -y install $PKGS_TO_INSTALL
fi

cd "$CWPROOT"
echo "Extracting SU source files..."
tar zxf "$DOWNLOAD_PATH/$SU_ARCHIVE"

cat > /tmp/Makefile.config <<EOF
CWPROOT = $CWPROOT
include \$(CWPROOT)/src/Rules/gnumake.rules
include \$(CWPROOT)/src/Rules/abbrev.rules
include \$(CWPROOT)/src/Rules/cflags.rules
include \$(CWPROOT)/src/Rules/suffix.rules
include \$(CWPROOT)/src/Rules/misc.rules
include \$(CWPROOT)/src/Rules/opengl.rules
LINEHDRFLAG = 
ENDIANFLAG = -DCWP_LITTLE_ENDIAN
LARGE_FILE_FLAG = -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE
CWP_FLAGS = \$(LARGE_FILE_FLAG) \$(ENDIANFLAG) \$(XDRFLAG) \$(LINEHDRFLAG)
SHELL = /bin/sh
ROOT = \$(CWPROOT)
LN = ln
AR = ar
ARFLAGS = rv
RANLIB = ranlib
RANFLAGS = 
ICHMODLINE = chmod 644 \$@
MCHMODLINE = chmod 755 \$@
IX11 = /usr/X11/include
LX11 = /usr/X11/lib
IMOTIF = /usr/X11R6/include
LMOTIF = /usr/X11R6/lib
LD_LIBRARY_PATH += \$(CWPROOT)/lib:\${LX11}:\${LMOTIF}
CPP = cpp
CC = gcc
OPTC = -g  -std=c99 -Wall -pedantic -Wno-long-long 
CFLAGS = -I\$I \$(OPTC) \$(CWP_FLAGS) -D_BSD_SOURCE -D_POSIX_SOURCE
FC = gfortran
FOPTS = -g 
FFLAGS = \$(FOPTS) -ffixed-line-length-none
C++FLAGS = -I\$I \$(OPTC) \$(CWP_FLAGS)SHELL = /bin/sh
EOF

cd src
if [ ! -e Makefile.config-original ]; then
    mv Makefile.config Makefile.config-original
fi
mv /tmp/Makefile.config .

touch LICENSE_"$SU_VERSION_PREFIX"_ACCEPTED
touch LICENSE_"$SU_VERSION"_ACCEPTED
touch MAILHOME_"$SU_VERSION_PREFIX"
touch MAILHOME_"$SU_VERSION"

if [ ! -e chkroot.sh-original ]; then
   cp chkroot.sh chkroot.sh-original
fi
if [ ! -e license.sh-original ]; then
   cp license.sh license.sh-original
fi
cat chkroot.sh-original | sed 's/read RESP/RESP="y"/' > /tmp/chkroot.sh
cat license.sh-original | sed 's/read RESP/RESP="y"/;s/more/cat/' > /tmp/license.sh
chmod 755 /tmp/chkroot.sh /tmp/license.sh
mv /tmp/chkroot.sh /tmp/license.sh .

echo "Compiling SU package"

for target in install xtinstall finstall \
    mglinstall utils xminstall sfinstall; do
    CWPROOT="$CWPROOT" make $target
done

echo -e "\nCompilation done."

cd $CWPROOT/..
# Seach for installed older versions of SU
OLDER_SUS=`find . -name su-\* | grep -v "$SU_VERSION" | sed 's/^\..//' `
if [ `echo $OLDER_SUS | wc -l` -gt 1 ]; then
    echo it seems that this versions of SU are installed:
    echo -e $OLDER_SUS "\n"
    if [ "$KEEP_OLDER_SUS" == 'FALSE' ]; then
	echo "$OLDER_SUS" | while read oldsu; do
	    echo -n "Removing $oldsu from system path (it will "
	    echo not be purged from the system however\).
	    stow -D $oldsu
	done
    else
	echo Unless you remove older versions of SU from path,
	echo they may conflict with this newer version your are trying
	echo to install.
	echo -e "\nIt is safe to set -U\n"
	echo This will only remove such older versions from path,
	echo without purging their files.
	exit -1
    fi
fi

stow -D su-$SU_VERSION
stow -v su-$SU_VERSION

if [ $EUID != 0 ]; then
    PROFILE="$HOME/.profile"
    BASHRC="$HOME/.bashrc"
else 
    PROFILE="/etc/profile"
    BASHRC="/etc/bash.bashrc"
fi

if [ -w $PROFILE ]; then
    TMP=`mktemp`
    sed '/CWPROOT=/d' $PROFILE > $TMP
    echo "export CWPROOT=$CWPROOT" >> $TMP
    mv $TMP $PROFILE
fi

if [ -w $BASHRC ]; then
    TMP=`mktemp`
    echo "export CWPROOT=$CWPROOT" > $TMP
    sed '/CWPROOT=/d' $BASHRC >> $TMP
    mv $TMP $BASHRC
fi

echo "Installation done."
echo -e "SU will be available next time you log in.\n"
echo "Any problem with this script, please report to"
echo "Ricardo Biloti <biloti@gebrproject.com>."
