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


      SUBROUTINE SOURCE(lin,lou,MPRINT,MWAVE,MTYPE,az,dcl,AMSOUR,phsour)
C
      DIMENSION IPAR(4),PAR(6),T(3,3),h1(3),h2(3),h3(3),sf(3)
      COMMON/SOUR/ROS,VPS,VSS
      common auxp,auxs
C
C     RADIATION PATTERNS OF THE SOURCE
C
C     FOR MWAVE.EQ.0, READ THE INPUT DATA FOR RADIATION PATTERNS
C     FOR MWAVE.NE.0, COMPUTE THE RADIATION PATTERNS
C
c     READ THE INPUT DATA
c
      PI=4.*ATAN(1.)
      IF(MWAVE.NE.0) GO TO 1
      IF(MTYPE.LE.0) RETURN
      READ(lin,100) IPAR,PAR
      WRITE(lou,100) IPAR,PAR
C
C     THE RADIATION PATTERN DOES NOT DEPEND ON THE RAY
C     PARAMETER (MTYPE=0)
C
    1 IF(MTYPE.LT.0) RETURN
      AMSOUR=1.
      PHSOUR=0.
      IF(MTYPE.EQ.0) RETURN
C
C     THE RADIATION PATTERN DEPENDS ON THE RAY PARAMETER
C     (MTYPE.GT.0)
C
C     COMPUTE THE RADIATION PATTERNS
C
      GO TO (11,21,31),MTYPE
C
C
C.....SINGLE FORCE
C
   11 CONTINUE
      IF(MWAVE.NE.0)GO TO 12
      AUX1=PAR(2)/(4.*PI*ROS)
      AUXP=AUX1/(VPS*VPS)
      AUXS=AUX1/(VSS*VSS)
      sfaz=PAR(1)
      sfdc=PAR(3)
      sf(1)=cos(sfaz)*cos(sfdc)
      sf(2)=sin(sfaz)*cos(sfdc)
      sf(3)=sin(sfdc)
      RETURN
C
   12 csa=cos(az)
      sna=sin(az)
      csdl=cos(dcl)
      sndl=sin(dcl)
      if(mwave.lt.3)go to 14
C
C     P WAVE
      h3(1)=csa*csdl
      h3(2)=sna*csdl
      h3(3)=sndl
      amsour=0.
      do 13 i=1,3
      amsour=amsour+h3(i)*sf(i)
   13 continue
      AMSOUR=AUXP*amsour
      GO TO 51
C
   14 if(mwave.eq.2)go to 16
c
C     S1 WAVE
      h1(1)=csa*sndl
      h1(2)=sna*sndl
      h1(3)=-csdl
      amsour=0.
      do 15 i=1,3
      amsour=amsour+h1(i)*sf(i)
   15 continue
      AMSOUR=AUXS*amsour
      GO TO 51
C
c     S2 WAVE
   16 h2(1)=-sna
      h2(2)=csa
      h2(3)=0.
      amsour=0.
      do 17 i=1,3
      amsour=amsour+h2(i)*sf(i)
   17 continue
      amsour=auxs*amsour
      go to 51
C
C.....DOUBLE COUPLE
C
   21 CONTINUE
      IF(MWAVE.NE.0)GO TO 22
      SM=PAR(2)
      FS=PAR(3)
      D=PAR(1)
      AL=PAR(4)
      SNF=SIN(FS)
      CSF=COS(FS)
      SND=SIN(D)
      CSD=COS(D)
      SNL=SIN(AL)
      CSL=COS(AL)
      A=CSL*CSF+CSD*SNL*SNF
      B=CSL*SNF-CSD*SNL*CSF
      C=SNF*SND
      D=SNL*SND
      T(1,1)=-2.*A*C
      T(1,2)=SND*(A*CSF-B*SNF)
      T(2,1)=T(1,2)
      T(1,3)=-(A*CSD-C*D)
      T(3,1)=T(1,3)
      T(2,2)=2.*SND*csf*b
      T(2,3)=-(B*CSD+D*SND*CSF)
      T(3,2)=T(2,3)
      T(3,3)=2.*D*CSD
      IF(MPRINT.GE.1)WRITE(lou,102)T
      AUX1=SM/(4.*PI*ROS)
      AUXP=AUX1/(VPS*VPS*VPS)
      AUXS=AUX1/(VSS*VSS*VSS)
      RETURN
C
   22 csa=cos(az)
      sna=sin(az)
      csdl=cos(dcl)
      sndl=sin(dcl)
      h3(1)=csa*csdl
      h3(2)=sna*csdl
      h3(3)=sndl
      IF(MWAVE.lt.3)GO TO 24
C
C     P WAVE
      AMSOUR=0.
      DO 23 I=1,3
      DO 23 J=1,3
      AMSOUR=AMSOUR+h3(I)*T(I,J)*h3(J)
   23 CONTINUE
      AMSOUR=AUXP*AMSOUR
      GO TO 51
C
C     S1 WAVE
   24 IF(MWAVE.EQ.2)GO TO 26
      h1(1)=csa*sndl
      h1(2)=sna*sndl
      h1(3)=-csdl
      AMSOUR=0.
      DO 25 I=1,3
      DO 25 J=1,3
      AMSOUR=AMSOUR+h1(I)*T(I,J)*h3(J)
   25 CONTINUE
      AMSOUR=AUXS*AMSOUR
      GO TO 51
C
C     S2 WAVE
   26 h2(1)=-sna
      h2(2)=csa
      h2(3)=0.
      amsour=0.
      do 27 i=1,3
      do 27 j=1,3
      amsour=amsour+h2(i)*t(i,j)*h3(j)
   27 continue
      amsour=amsour*auxs
      GO TO 51
C
C.....EXPLOSIVE (IMPLOSIVE) SOURCE
C
   31 CONTINUE
      IF(MWAVE.NE.0)GO TO 32
      AUX1=4.*PI*ROS*VPS*VPS*VPS
      AUXP=PAR(1)*FLOAT(IPAR(1))/AUX1
      RETURN
   32 IF(MWAVE.ne.3) GO TO 33
C
C     P WAVE
      AMSOUR=AUXP
      GO TO 51
C
C     S WAVE
   33 AMSOUR=0.
      PHSOUR=0.
      RETURN
C
C
   51 PHSOUR=0.
   50 IF(AMSOUR)52,53,53
   52 AMSOUR=-AMSOUR
      PHSOUR=PHSOUR-PI
C
  100 FORMAT(4I5,6F10.5)
  101 FORMAT(8F10.5)
  102 FORMAT(//2X,3(3E15.5/))
C
   53 RETURN
      END
