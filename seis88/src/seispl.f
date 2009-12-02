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


C
C     P R O G R A M   S E I S P L O T
C     *******************************
C
C     PROGRAM SEISPLOT IS DESIGNED FOR THE PLOTTING OF SYNTHETIC
C     SEISMOGRAMS STORED AT THE LOGICAL UNIT LU3
C
C     ************************************************************
C
      character*80 text
      DIMENSION SEIS(3001),IEP(100)
C
C     ***
C     ***
C
C*..........................
      mode=0
      call serv(mode,lin,lou,llu3,ldum1,ldum2)
      if(mode.eq.0)CALL PLOTS(ldum1,ldum2,7)
      if(mode.eq.0)lin=5
      if(mode.eq.0)lou=6
C*..........................
C
      IRUN=0
      CALL PLOT(5.,5.,-3)
    1 READ(lin,101)LU3,ISHIFT,IPRINT
      WRITE(lou,101)LU3,ISHIFT,IPRINT
      if(mode.eq.1)lu3=llu3 
C
C     ***
C     ***
C
      IF(ISHIFT.EQ.0)ISHIFT=8
      SHIFT=FLOAT(ISHIFT)
    2 READ(lin,101)MCONT,MEPIC,NTICX,NTICY,NDX,NDY
      WRITE(lou,101)MCONT,MEPIC,NTICX,NTICY,NDX,NDY
      IF(MCONT.EQ.0)GO TO 99
      IF(MCONT.EQ.(-1))REWIND LU3
      IF(MCONT.EQ.(-1))GO TO 1
      IF(MEPIC.EQ.0)GO TO 3
      READ(lin,101)NEPIC,(IEP(I),I=1,NEPIC)
      WRITE(lou,101)NEPIC,(IEP(I),I=1,NEPIC)
    3 CONTINUE
      READ(lin,102)XMIN,XMAX,XLEN,DTICX,YMIN,YMAX,YLEN,DTICY
      WRITE(lou,102)XMIN,XMAX,XLEN,DTICX,YMIN,YMAX,YLEN,DTICY
      READ(lin,102)AMP,B1,EPICS,EPS,SC
      WRITE(lou,102)AMP,B1,EPICS,EPS,SC
      IF(ABS(SC).LT..00001)SC=1.
      IF(LU3.NE.0)REWIND LU3
      READ(LU3,100)TEXT
      WRITE(lou,100)TEXT
      READ(LU3,105)MDIST,MRED,MCOMP,itpr,VRED,RSTEP,XSOUR,DT
      WRITE(lou,105)MDIST,MRED,MCOMP,itpr,VRED,RSTEP,XSOUR,DT
      READ(LU3,104)XMX,SMAXIM
      WRITE(lou,104)XMX,SMAXIM
      IF(EPICS.LT.0.00001)EPICS=10.
      IF(ABS(B1).LT.0.00001)B1=1.
C
C     PLOT OF FRAME
      XMER=XLEN/(XMAX-XMIN)
      YMER=YLEN/(YMAX-YMIN)
      DDX=RSTEP*XMER
      IF(IRUN.NE.0)CALL PLOT(XLEN+SHIFT,0.,-3)
      IRUN=1
C*..................
C      CALL COLOR(14)
C*..................
      CALL BORDER(XLEN,DTICX,YLEN,DTICY,SC,TEXT,0,XMIN,XMAX,
     1YMIN,YMAX,NTICX,NTICY,NDX,NDY)
      T=.5*(XLEN-6.3*SC)
      if(itpr.ne.22) 
     1CALL SYMBOL(T,-1.6*SC,.45*SC,'DISTANCE IN KM',0.,14)
      if(itpr.eq.22)
     1CALL SYMBOL(T,-1.6*SC,.45*SC,'DEPTH IN KM',0.,11)
      T=.5*(YLEN-8.1*SC)
      U=-(1.6+.4*NDX)*SC
      IF(MRED.EQ.0)
     1CALL SYMBOL(U,T,.45*SC,'TRAVEL TIME IN SEC',90.,18)
      IF(MRED.EQ.0)GO TO 4
      CALL SYMBOL(U,T,.45*SC,'T-D/ ',90.,5)
      T=T+1.8*SC
      CALL NUMBER(U,T,.45*SC,VRED,90.,2)
      T=T+2.7*SC
      CALL SYMBOL(U,T,.45*SC,'(IN SEC)',90.,8)
    4 CONTINUE
      IF(MCOMP.EQ.0)
     1CALL SYMBOL(.45*SC,YLEN+SC,.45*SC,'VERTICAL',0.,8)
      IF(MCOMP.EQ.1)
     1CALL SYMBOL(.45*SC,YLEN+SC,.45*SC,'RADIAL',0.,6)
      IF(MCOMP.EQ.2)
     1CALL SYMBOL(.45*SC,YLEN+SC,.45*SC,'TRANSVERSE',0.,10)
      T=XLEN-7.5*SC
      CALL NUMBER(T,YLEN+.5*SC,.3*SC,AMP,0.,0)
      T=T+1.5*SC
      CALL NUMBER(T,YLEN+.5*SC,.3*SC,B1,0.,2)
      T=T+1.5*SC
      CALL NUMBER(T,YLEN+.5*SC,.3*SC,EPS,0.,1)
      T=T+1.5*SC
      CALL NUMBER(T,YLEN+.5*SC,.3*SC,SMAXIM,0.,5)
      CALL PLOT(0.,0.,3)
C*.................
C      CALL COLOR(4)
C*.................
C
C     LOOP FOR THE RECEIVER POSITIONS
C
      DO 10 I=1,MDIST
      READ(LU3,110)XX,SMAXI,TMIN,NPTS
      READ(LU3,109)(SEIS(M),M=1,NPTS)
      IF(I.EQ.1)SMAX1=SMAXI
      SAUX=SMAXI/999.
      DO 22 M=1,NPTS
   22 SEIS(M)=SEIS(M)*SAUX
      IF(XX.LE.XMIN.OR.XX.GE.XMAX)GO TO 10
      IF(MEPIC.EQ.0)GO TO 5
      DO 6 J=1,NEPIC
      IF(I.EQ.IEP(J))GO TO 5
    6 CONTINUE
      GO TO 10
C
    5 IF(SMAXI.LT.0.000001)GO TO 7
      IF(ABS(AMP).LT.0.00001)FACTOR=B1*DDX/SMAXI
      IF(ABS(AMP).LT.0.00001)GO TO 21
      IF(ABS(EPS).GT.0.00001)GO TO 20
      IF(AMP.LT.(-0.00001))FACTOR=B1*DDX/SMAXIM
      IF(AMP.GT.0.00001.AND.AMP.LT.5.)FACTOR=B1
      IF(AMP.GT.5.)FACTOR=B1*DDX/SMAX1
      SF1=.003*SFMAX
      GO TO 21
   20 IF(AMP.LT..00001)FACTOR=B1*DDX*((ABS(XX-XSOUR)/EPICS)**EPS)
     1/SMAXIM
      IF(AMP.GT.0.00001)FACTOR=B1*(ABS(XX-XSOUR)/EPICS)**EPS
   21 CONTINUE
      GO TO 8
    7 FACTOR=0.
    8 CONTINUE
      SFMAX=FACTOR*SMAXI
      SF1=.003*SFMAX
      IF(IPRINT.EQ.1)WRITE(lou,103)XX,SMAXI,FACTOR,SFMAX
C
C
      X0=(XX-XMIN)*XMER
      XNEW=X0
      AMPL=0.
      YNEW=0.
      ISTART=1
      IF(TMIN.GE.YMIN)GO TO 12
      IAUX=(YMIN-TMIN)/DT+1
      TL=TMIN+DT*FLOAT(IAUX-1)
      AMPL=SEIS(IAUX)+(SEIS(IAUX+1)-SEIS(IAUX))*(YMIN-TL)/DT
      XNEW=X0-FACTOR*AMPL
      IF(XNEW.LT.0..OR.XNEW.GT.XLEN)GO TO 15
      CALL PLOT(XNEW,YNEW,3)
   15 ISTART=IAUX+1
   12 CONTINUE
      IF(ISTART.EQ.1)CALL PLOT(XNEW,YNEW,3)
      IF(ISTART.GT.NPTS)CALL PLOT(X0,YLEN,2)
      IF(ISTART.GT.NPTS)GO TO 10
      S2=FACTOR*SEIS(ISTART)
      S3=FACTOR*SEIS(ISTART+1)
      DO 11 J=ISTART,NPTS
      IF(J.EQ.ISTART)GO TO 14
      S1=S2
      S2=S3
      IF(J.EQ.NPTS)GO TO 14
      S3=FACTOR*SEIS(J+1)
      IF(ABS(S1).LT.SF1.AND.ABS(S2).LT.SF1.AND.ABS(S3).LT.SF1)
     1GO TO 11
   14 XNEW=X0-S2
      YNEW=(TMIN+DT*FLOAT(J-1)-YMIN)*YMER
      IF(YNEW.GT.YLEN)GO TO 13
      IF(XNEW.LT.0..OR.XNEW.GT.XLEN)GO TO 11
      CALL PLOT(XNEW,YNEW,2)
      GO TO 11
   13 AMPL=SEIS(J-1)+(SEIS(J)-SEIS(J-1))*(YMAX-TMIN-DT*FLOAT(J-1))/DT
      XNEW=X0-FACTOR*AMPL
      IF(XNEW.LT.0..OR.XNEW.GT.XLEN)GO TO 10
      CALL PLOT(XNEW,YLEN,2)
      GO TO 10
   11 CONTINUE
      CALL PLOT(X0,YLEN,2)
   10 CONTINUE
C
C     END OF THE LOOP FOR RECEIVER POSITIONS
C
      GO TO 2
C
C
  100 FORMAT(A)
  101 FORMAT(16I5)
  102 FORMAT(8F10.5)
  103 FORMAT(2X,4E15.5)
  104 FORMAT(22X,F10.5,9X,E15.9)
  105 FORMAT(4I5,4F10.5)
  109 FORMAT(20F4.0)
  110 FORMAT(F10.5,E15.8,F10.5,I5)
   99 CALL PLOT(0.,0.,999)
C
C     ***
C     ***
C
      STOP
      END
