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


      SUBROUTINE BORDER(GLX,DX,GLY,DY,SC,TEXT,N,XMIN,XMAX,YMIN,YMAX,
     1NX,NY,NDECX,NDECY)
C
      character*80 TEXT
C
      IXX=0
      IYY=0
      GGG=.15*SC
      GGH=.225*SC
      DCX=.2*NDECX
      DCY=.4*NDECY
      DNX=ABS(DX)/NX
      DNY=ABS(DY)/NY
      XMER=GLX/(XMAX-XMIN)
      YMER=GLY/(YMAX-YMIN)
      A=DNX*XMER
      B=DNY*YMER
      XST=0.
      YST=0.
      IF(DX.GT.0.)GO TO 1
      DX=-DX
      IX=XMIN/DX
      XM=(IX+1)*DX
      IXX=(XM-XMIN)/DNX
      XST=(XM-XMIN)-IXX*DNX
      XST=XST*XMER
      GO TO 2
    1 XM=XMIN
    2 AVST=(XMAX-XM)/DNX+.0001
      IX=IXX+AVST+1.
      CALL PLOT(0.,0.,3)
      DO 100 I=1,IX
      I1=I-IXX-1
      C=(I-1)*A+XST
      CALL PLOT(C,0.,2)
      CALL PLOT(C,GGG,2)
      IF(I1/NX*NX.NE.I1)GO TO 100
      VAL=XM+I1*DNX
      R=-(.3+DCX)*SC
      IF(VAL.GE.10.)R=-(.5+DCX)*SC
      IF(VAL.GE.100.)R=-(.7+DCX)*SC
      IF(VAL.GE.1000.)R=-(.9+DCX)*SC
      CALL PLOT(C,GGH,2)
      CALL NUMBER(C+R,-.7*SC,.4*SC,VAL,0.,NDECX)
  100 CALL PLOT(C,0.,3)
      CALL PLOT(GLX,0.,2)
      IF(DY.GT.0.)GO TO 3
      DY=-DY
      IY=YMIN/DY
      YM=(IY+1)*DY
      IYY=(YM-YMIN)/DNY
      YST=(YM-YMIN)-IYY*DNY
      YST=YST*YMER
      GO TO 4
    3 YM=YMIN
    4 AVST=(YMAX-YM)/DNY+.0001
      IY=IYY+AVST+1.
      DO 200 I=1,IY
      IF(N.EQ.1)I1=IY-I-IYY
      IF(N.NE.1)I1=I-IYY-1
      D=(I-1)*B+YST
      CALL PLOT(GLX,D,2)
      CALL PLOT(GLX-GGG,D,2)
      IF(I1/NY*NY.NE.I1)GO TO 200
      CALL PLOT(GLX-GGH,D,2)
  200 CALL PLOT(GLX,D,3)
      CALL PLOT(GLX,GLY,2)
      DO 300 I=1,IX
      I1=IX-I-IXX
      E=(IX-I)*A+XST
      CALL PLOT(E,GLY,2)
      CALL PLOT(E,GLY-GGG,2)
      IF(I1/NX*NX.NE.I1)GO TO 300
      CALL PLOT(E,GLY-GGH,2)
  300 CALL PLOT(E,GLY,3)
      CALL PLOT(0.,GLY,2)
      DO 400 I=1,IY
      IF(N.EQ.1)I1=I-IYY-1
      IF(N.NE.1)I1=IY-IYY-I
      F=(IY-I)*B+YST
      CALL PLOT(0.,F,2)
      CALL PLOT(GGG,F,2)
      IF(I1/NY*NY.NE.I1)GO TO 400
      VAL=YM+I1*DNY
      IF(VAL.LT.10..AND.VAL.GE.0.)R=-(.8+DCY)*SC
      IF(VAL.LT.0..OR.VAL.GE.10.)R=-(1.2+DCY)*SC
      IF(VAL.LE.(-10.).OR.VAL.GE.100.)R=-(1.6+DCY)*SC
      CALL PLOT(GGH,F,2)
      CALL NUMBER(R,F-.2*SC,.4*SC,VAL,0.,NDECY)
  400 CALL PLOT(0.,F,3)
      CALL PLOT(0.,0.,2)
      CALL SYMBOL(0.,-3.*SC,.45*SC,TEXT,0.,80)
      RETURN
      END
