C  seis88 - Rays tracing tool
C  Copyright (C) 2009 Ivan Psencik <ip@ig.cas.cz>
C 
C  This program is free software: you can redistribute it and/or modify
C  it under the terms of the GNU General Public License as published by
C  the Free Software Foundation, either version 3 of the License, or
C  (at your option) any later version.
C 
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C  GNU General Public License for more details.
C 
C  You should have received a copy of the GNU General Public License
C  along with this program.  If not, see <http://www.gnu.org/licenses/>.


C     INCLUDE 'calcops.inc'
C     ------------------------------------------------------------------
      INTEGER LUCFG,MCOLOR,KOLOR
      PARAMETER (LUCFG=97,MCOLOR=255)
      REAL R(0:MCOLOR),G(0:MCOLOR),B(0:MCOLOR)
      REAL SCALE,STARTX,STARTY,XOLD,YOLD,HOLD,B1,B2,B3,B4
      PARAMETER (SCALE=72/2.54)
      COMMON/PLOTC/ KOLOR,R,G,B,STARTX,STARTY,XOLD,YOLD,HOLD,B1,B2,B3,B4
      SAVE /PLOTC/
C     ------------------------------------------------------------------
C     LUCFG...Logical unit number of the CalComp configuration file
C             calcomp.lst.
C     STARTX,STARTY... Origin of CalComp coordinates in world
C             coordinates.
C     XOLD,YOLD... Point referred during the previous invocation of a
C             CalComp subroutine, in CalComp coordinates.
C     B1,B2,B3,B4... Bounding box.
C
C     Common block /PLOTC/ is included in FORTRAN 77 source code file
C     'calcops.for'.
C
C Date: 1996, July 8
C Coded by Ludek Klimes
C
C=======================================================================
C
