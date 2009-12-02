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
C     *************************************************************
C
      SUBROUTINE RAY2(X0,Z0,T0,FI0,X,Z,T,FI,TMAX,DT,AC)
C
C     RAY TRACING BY THE RUNGE-KUTTA METHOD
C
      INTEGER CODE
      DIMENSION Y(4),DERY(4),DIN(4),PRM(5),AUX(8,4),VEL(6)
      COMMON/INTRF/A1(30,20),B1(29,20),C1(30,20),D1(29,20),X1(30,20),
     1BRD(2),III(30,20),NPNT(20),NINT
      COMMON/AUXI/INTR,INT1,IOUT,KRE,IREFR,LAY,ITYPE,NDER,IPRINT,MPRINT,
     1NTR,IMET
      COMMON/RAY/AY(12,400),DS(11,50),KINT(50),MREG,N,IREF,IND,IND1
      COMMON/COD/CODE(50),KREF,KC
      EXTERNAL FCT,OUT

C
C
      Y(1)=X0
      Y(2)=Z0
      IREFR=0
      KRE=KREF
      IF(KC.EQ.0)KRE=0
      N=0
      IREF=1
      IOUT=0
      isour=ind
C
C     DETERMINATION OF INITIAL CONDITIONS FOR THE RUNGE-KUTTA PROCEDURE
C
C
      LAY=ISOUR
      IF(ISOUR.NE.IABS(CODE(1)))IND=14
      IF(ISOUR.NE.IABS(CODE(1)))RETURN
      ITYPE=ISIGN(1,CODE(1))
      Y(3)=0.
      Y(4)=0.
      CALL VELOC(Y,VEL)
      V=VEL(1)
      Y(3)=COS(FI0)/V
      Y(4)=SIN(FI0)/V
      IND=0
      IND1=0
      PRM(1)=T0
      PRM(2)=TMAX
      PRM(3)=DT
      PRM(4)=AC
      T=PRM(1)
      DO 21 I=1,4
   21 DIN(I)=.25
    9 DO 10 I=1,4
   10 DERY(I)=DIN(I)
C
C     COMPUTATION OF THE RAY
C
      CALL RKGS(PRM,Y,DERY,4,IHLF,FCT,OUT,AUX)
      IF(IHLF.EQ.11)IND=5
      IF(IHLF.EQ.12)IND=6
      IF(IHLF.EQ.13)IND=7
      IF(IND.GE.5.AND.IND.LE.7)RETURN
      IF(IND.EQ.20)RETURN
      IF(IND.EQ.21)RETURN
      IF(ABS(PRM(5)).GT..0001) GO TO 11
C
C     INTERUPTION OF THE COMPUTATION OF THE RAY DUE TO (T.GE.PRM(2))
C     IF PRM(2).EQ.TMAX - TERMINATION OF THE COMPUTATION
C     IF PRM(2).NE.TMAX - THE COMPUTATION CONTINUES FURTHER ON WITH
C     THE REGULAR TIME STEP ALONG THE RAY
C
      T=AY(1,N)
      IF(T.GE.TMAX)GO TO 15
      GO TO 14
   11 IF(ABS(PRM(5)-1.1).GT..0001) GO TO 13
C
C     LOOP FOR THE ITERATIVE DETERMINATION OF THE POINT OF INCIDENCE
C     (NOT USED IN THIS VERSION)
      PRM(1)=AY(1,N)
      PRM(3)=PRM(3)/(2.**(IHLF+1))
      DO 12 I=1,4
   12 Y(I)=AY(I+1,N)
      T=AY(1,N)
      N=N-1
      GO TO 9
   13 X=Y(1)
      Z=Y(2)
      T=AY(1,N)
      IF(ABS(PRM(5)-2.).GT..0001.AND.IREFR.EQ.1)IND1=-IND1
      IF(ABS(PRM(5)-2.).GT..0001)GO TO 20
C
C     INTEGRATION FROM THE POINT OF REFLECTION/TRANSMISSION
C     TO THE CLOSEST POINT OF THE RAY CORRESPONDING TO THE REGULAR
C     TIME STEP ALONG THE RAY
C
      PRM(1)=AY(1,N)
      I=INT((PRM(1)-T0)/DT)
      PRM(2)=FLOAT(I+1)*DT+T0
      GO TO 9
   14 PRM(1)=PRM(2)
      PRM(2)=TMAX
      PRM(3)=DT
      N=N-1
      GO TO 9
   15 IND=12
      X=Y(1)
      Z=Y(2)
      IF(IREFR.EQ.1)IND1=-IND1
C
   20 V=AY(6,N)
      CS=Y(3)*V
      SN=Y(4)*V
      FI=ATAN2(SN,CS)
      RETURN
      END
C
C     *************************************************************
C
      SUBROUTINE OUT(X,Y,DERY,IHLF,NDIM,PRMT)
C
C     EXTERNAL ROUTINE IN THE RUNGE-KUTTA RAY TRACING.
C     PERFORMS VARIOUS COMPUTATIONS AT EACH POINT OF THE RAY
C
      INTEGER CODE
      DIMENSION Y(4),DERY(4),PRMT(5),YOLD(2),YINT(2),VEL(6),Y1(2)
      COMMON/COD/CODE(50),KREF,KC
      COMMON/INTRF/A1(30,20),B1(29,20),C1(30,20),D1(29,20),X1(30,20),
     1BRD(2),III(30,20),NPNT(20),NINT
      COMMON/RAY/AY(12,400),DS(11,50),KINT(50),MREG,N,IREF,IND,IND1
      COMMON/AUXI/INTR,INT1,IOUT,KRE,IREFR,LAY,ITYPE,NDER,IPRINT,MPRINT,
     1NTR,IMET
      COMMON/DEN/RHO1(19),RHO2(19),NRHO
      common/vsp/xvsp,icod,ivsp
C
C
      INCD=0
      INSPL=0
      N=N+1
      NRR=N
      NTR=1
      IF(N.GT.400)GO TO 50
C
C     NORMALIZATION OF SLOWNESS VECTOR
C
      YAN=Y(3)
      Y(3)=101.
      CALL VELOC(Y,VEL)
      Y(3)=YAN
      IF(N.EQ.1)GO TO 67
      VN=VEL(1)
      VRT=VN*VN*(Y(3)*Y(3)+Y(4)*Y(4))
      VRT=1./SQRT(VRT)
      Y(3)=Y(3)*VRT
      Y(4)=Y(4)*VRT
C
C     CHECK OF THE POSITION OF THE POINT OF RAY WITH RESPECT TO INTERFAC
C
      INTR=LAY
      NL=NPNT(INTR)-1
      ICH=1
      IF(Y(1).LT.BRD(1).OR.Y(1).GT.BRD(2))GO TO 7
   65 ICH=0
      IND=0
      DO 2 I=1,NL
      J=I
      IF(Y(1).LT.X1(I+1,INTR)) GOTO 3
    2 CONTINUE
    3 IF(IREF.EQ.1)GO TO 27
      IF(KINT(IREF-1).EQ.(N-1))GO TO 21
   27 A2=A1(J,INTR)
      B2=B1(J,INTR)
      C2=C1(J,INTR)
      D2=D1(J,INTR)
      X2=X1(J,INTR)
      AUX=Y(1)-X2
      ZINT=((D2*AUX+C2)*AUX+B2)*AUX+A2
      ZINT1=ZINT
      IF(Y(2).LT.ZINT) GO TO 9
      INTR=LAY+1
      NL=NPNT(INTR)-1
      DO 4 I=1,NL
      J=I
      IF(Y(1).LT.X1(I+1,INTR)) GO TO 5
    4 CONTINUE
    5 A2=A1(J,INTR)
      B2=B1(J,INTR)
      C2=C1(J,INTR)
      D2=D1(J,INTR)
      X2=X1(J,INTR)
      AUX=Y(1)-X2
      ZINT=((D2*AUX+C2)*AUX+B2)*AUX+A2
      IF(ZINT1.GT.ZINT) GO TO 54
      IF(Y(2).GT.ZINT) GO TO 9
   21 IF(Y(1).LE.BRD(1).OR.Y(1).GE.BRD(2))GO TO 7
c
c     CHECK WHETHER THE RAY DID NOT CROSS THE BOREHOLE
      if(icod.eq.0)go to 67
      if(icod.gt.iref)go to 67
      aux=(ay(2,n-1)-xvsp)*(y(1)-xvsp)
      if(aux.gt.0.)go to 67
c
c     THE RAY CROSSED THE BOREHOLE
c   
   69 ind=22
      avert=xvsp
      go to 33      
C
C     THE RAY DID NOT CROSS ANY INTERFACE
C
   67 AY(1,N)=X
      DO 6 I=1,4
    6 AY(I+1,N)=Y(I)
      RETURN
C
C     THE RAY CROSSED ONE OF THE VERTICAL BOUNDARIES
C
    7 IF(Y(1).LE.BRD(1)) IND=1
      IF(Y(1).GE.BRD(2)) IND=2
      avert=brd(ind)
   33 AUX=Y(1)-AY(2,N-1)
      IF(ABS(AUX).LT..00001)AUX=0.
      IF(ABS(AUX).GE..00001)AUX=(avert-AY(2,N-1))/AUX
      X=AY(1,N-1)+AUX*(X-AY(1,N-1))
      Y(1)=avert
      DO 8 I=2,4
    8 Y(I)=AY(I+1,N-1)+AUX*(Y(I)-AY(I+1,N-1))
      IF(ICH.EQ.1)GO TO 65
      AY(1,N)=X
      DO 64 I=1,4
   64 AY(I+1,N)=Y(I)
      PRMT(5)=3.
      DS(2,IREF)=Y(3)
      DS(3,IREF)=Y(4)
      KINT(IREF)=N
      GO TO 49
C
C     THE RAY CROSSED AN INTERFACE
C
C     THE DETERMINATION OF THE POINT OF INCIDENCE
C
    9 YOLD(1)=AY(2,N-1)
      YOLD(2)=AY(3,N-1)
      IRR=IREF
      CALL ROOT(YOLD,Y,YINT,ANYX,ANYZ,RC,J,IROOT)
c
c     CHECK WHETHER THE RAY DID NOT CROSS THE BOREHOLE
      if(icod.eq.0)go to 12
      if(icod.gt.iref)go to 12
      aux=(yold(1)-xvsp)*(yint(1)-xvsp)
      if(aux.gt.0.)go to 12
c
c     THE RAY CROSSED THE BOREHOLE
c   
      go to 69
   12 ind1=intr
      IF(IROOT.EQ.100)GO TO 30
C
C     THE LOOP FOR THE ITERATIVE DETERMINATION OF THE POINT OF INCIDENCE
C     (NOT USED IN THIS VERSION)
      IF(IOUT.NE.0)GO TO 11
   10 N=N-1
      Y1(1)=YINT(1)
      Y1(2)=YINT(2)
      PRMT(5)=1.1
      IOUT=1
      RETURN
   11 IAC=IAC+1
      IF(IAC.EQ.10)GO TO 30
      AUX1=Y1(1)-YINT(1)
      AUX2=Y1(2)-YINT(2)
      AUX=AUX1*AUX1+AUX2*AUX2
      IF(AUX.GT..00001) GO TO 10
C
C     DETERMINATION OF PARAMETERS OF INCIDENT WAVE
C     AT THE POINT OF INCIDENCE
C
   30 IAC=0
      IOUT=0
      AY(2,N)=YINT(1)
      AY(3,N)=YINT(2)
      XDIF=Y(1)-AY(2,N-1)
      YDIF=Y(2)-AY(3,N-1)
      IF(ABS(XDIF).LT.ABS(YDIF))GO TO 60
      IF(ABS(XDIF).LT..001)GO TO 61
      AUX=(AY(2,N)-AY(2,N-1))/XDIF
      GO TO 62
   60 IF(ABS(YDIF).LT..001)GO TO61
      AUX=(AY(3,N)-AY(3,N-1))/YDIF
      GO TO 62
   61 AUX=0.
   62 CONTINUE
      AY(1,N)=AY(1,N-1)+AUX*(X-AY(1,N-1))
      X=AY(1,N)
      AY5=(X-AY(1,N-1))/AY(6,N-1)
      AY4=AY(4,N-1)-AY(7,N-1)*AY5
      AY5=AY(5,N-1)-AY(8,N-1)*AY5
      Y(1)=AY(2,N)
      Y(2)=AY(3,N)
C
C     NORMALIZATION OF SLOWNESS VECTOR AT THE POINT OF INCIDENCE
C
      CALL VELOC(Y,VEL)
      VN=VEL(1)
      VRT=AY4*AY4+AY5*AY5
      VRT=VN*VN*VRT
      VRT=1./SQRT(VRT)
      Y(3)=AY4*VRT
      Y(4)=AY5*VRT
      AY(4,N)=Y(3)
      AY(5,N)=Y(4)
C
      IF(IREF.EQ.1)GO TO 49
      NNN=KINT(IREF-1)
      IF(NNN.LE.0)GO TO 49
      IF(ABS(AY(2,NNN)-YINT(1)).GT..0001.OR.INTR.NE.INT1)GO TO 49
      IRR=IRR-1
      NTR=2
      IF(AINDEX.GT.1)GO TO 28
      NTR=3
      GO TO 26
C
C
   49 ITYPE1=ITYPE
      ITYPE=1
      CALL VELOC(Y,VEL)
      DS(4,IREF)=VEL(1)
      DS(8,IREF)=RHO1(LAY)+RHO2(LAY)*VEL(1)
      ITYPE=-1
      CALL VELOC(Y,VEL)
      DS(5,IREF)=VEL(1)
      ITYPE=ITYPE1
      VVV=DS(4,IREF)
      IF(ITYPE.LT.0)VVV=DS(5,IREF)
      DS(1,IREF)=0.
      DS(6,IREF)=0.
      DS(7,IREF)=0.
      DS(9,IREF)=0.
      DS(10,IREF)=0.
      DS(11,IREF)=0.
      NTR=4
      IF(IND.EQ.1.OR.IND.EQ.2.or.ind.eq.22)GO TO 32
      LAY1=LAY
      PX=Y(3)
      PZ=Y(4)
      YAN=Y(3)
      AUX=ANYX*PX+ANYZ*PZ
      IF(AUX.LE.0)GO TO 13
      ANYX=-ANYX
      ANYZ=-ANYZ
      AUX=-AUX
      RC=-RC
   13 AUXX=(ANYX*PZ-ANYZ*PX)*VVV
      AUXZ=AUX*VVV
      DS(1,IREF)=RC
      DS(2,IREF)=AUXX
      DS(3,IREF)=AUXZ
      NTR=5
      IF(KRE.EQ.1)GO TO 24
      IF(KRE.LT.1)GO TO 31
C
C     MULTIPLY REFLECTED WAVE
C
      NTR=6
      IF((IREF+1).GT.KRE.AND.INTR.EQ.INT1)GO TO 26
      IF((IREF+1).GT.KRE) GO TO 24
      NCD=CODE(IREF+1)-CODE(IREF)
      NTR=7
      IF(KC.GT.0.AND.IREF.EQ.1.AND.INTR.EQ.LAY)GO TO 26
      NTR=8
      IF(KC.LT.0.AND.IREF.EQ.1.AND.INTR.NE.LAY)GO TO 26
      IF(IREF.EQ.1)GO TO 46
      NTR=9
      IF(INTR.EQ.INT1.AND.NCD.NE.0)GO TO 26
      IF(INTR.NE.INT1.OR.NCD.NE.0)GO TO 46
      IREFR=1
      KINT(IREF)=0
      IREF=IREF+1
      IRR=IREF
      DS(1,IREF)=DS(1,IREF-1)
      DS(2,IREF)=DS(2,IREF-1)
      DS(3,IREF)=DS(3,IREF-1)
      DS(4,IREF)=DS(4,IREF-1)
      DS(5,IREF)=DS(5,IREF-1)
      DS(6,IREF)=0.
      DS(7,IREF)=0.
      DS(8,IREF)=DS(8,IREF-1)
      DS(9,IREF)=0.
      DS(10,IREF)=0.
      DS(11,IREF)=0.
      DO 53 I=1,9
   53 DS(I,IREF-1)=0.
      NCD=CODE(IREF+1)-CODE(IREF)
      NTR=10
      IF((IREF+1).GT.KRE) GO TO 24
   46 INT1=INTR
      J11=J
      I11=INTR
      L11=LAY1
      IF(NCD.NE.0) GO TO 22
C
C     REFLECTION OF UNCONVERTED WAVE
C
      IIII=III(J,INTR)
      NTR=11
      IF(IIII.EQ.(-2))GO TO 48
      NTR=12
      IF(IIII.GT.0)GO TO 23
      Y(3)=101.
      CALL VELOC(Y,VEL)
      Y(4)=PZ-2.*AUX*ANYZ
      Y(3)=PX-2.*AUX*ANYX
      IREF=IREF+1
      V1=VEL(1)
      V2=V1
      AINDEX=1.
      GO TO 20
C
C     REFRACTED WAVE
C
   31 NTR=13
      IF(INTR.EQ.LAY.AND.LAY.EQ.1) GO TO 24
      NTR=14
      IF(INTR.GT.LAY.AND.INTR.EQ.NINT) GO TO 24
      NCD=1
      INT1=INTR
      GO TO 14
C
C     REFRACTION OR REFLECTION OF CONVERTED WAVE
C
   22 NCD=IABS(CODE(IREF+1))-IABS(CODE(IREF))
      IIII=III(J,INTR)
      NTR=15
      IF(NCD.EQ.0.AND.IIII.EQ.(-2)) GO TO 48
      NTR=16
      IF(NCD.EQ.0.AND.IIII.GT.0)GO TO 23
   14 Y(3)=101.
      CALL VELOC(Y,VEL)
      Y(3)=YAN
      V1=VEL(1)
      IF(KRE.GT.1)ITYPE=ISIGN(1,CODE(IREF))
   45 IREF=IREF+1
C
C     CHECK WHETHER A TRANSMISSION TAKES PLACE AT AN INTERFACE
C     WHICH COINCIDES WITH ANOTHER INTERFACE
C
      IF(NCD.EQ.0.AND.INSPL.EQ.0)GO TO 16
      IF(NCD.EQ.0.AND.INSPL.EQ.1)GO TO 68
      IF(INTR.EQ.LAY) GO TO 15
      NTR=17
      IF(NCD.LT.0)GO TO 26
      NTR=18
      IF(KRE.LE.1.AND.INTR.EQ.NINT)GO TO 24
      NTR=19
      IF(INTR.EQ.NINT) GO TO 26
      LAY=LAY+1
      GO TO 29
   68 INTR=INTRA
      J=JA
      INCD=1
      GO TO 37
   15 NTR=20
      IF(NCD.GT.0.AND.KRE.GT.1)GO TO 26
      NTR=21
      IF(KRE.LE.1.AND.LAY.EQ.1)GO TO 24
      NTR=22
      IF(LAY.EQ.1)GO TO 26
      LAY=LAY-1
   29 INTRA=INTR
      JA=J
      IF(INTR.EQ.LAY1)GO TO 36
      NC=NPNT(INTR+1)
      DO 34 I=1,NC
      JJ1=I
      II1=III(I,INTR+1)
      IF(J.EQ.II1)GO TO 35
   34 CONTINUE
      GO TO 16
   35 INTR=INTR+1
      J=JJ1
      GO TO 37
   36 II1=III(J,INTR)
      IF(II1.LE.0)GO TO 16
      INTR=INTR-1
      J=II1
C
C     TRANSMISSION AT AN INTERFACE WHICH COINCIDES
C     WITH ANOTHER INTERFACE
C
   37 N=N+1
      N1=N+1
      NTR=23
      IF(N1.Ge.400) GO TO 50
      INSPL=1
      NDR=8
      IF(NDER.EQ.1)NDR=11
      DO 38 I=N,N1
      DO 38 L=1,NDR
   38 AY(L,I)=AY(L,N-1)
      N=N1
      LAY1=LAY
      IND1=INTR
      KINT(IREF)=-1
      DO 47 I=1,11
   47 DS(I,IREF)=0.
      NTR=24
      IF(KRE.GT.1.AND.(CODE(IREF)*ITYPE).LT.0)GO TO 66
      NTR=25
      IF(KRE.GT.1.AND.(IREF+1).GT.KRE)GO TO 24
      INT1=INTR
      IF(KRE.GT.1)NCD=IABS(CODE(IREF+1))-IABS(CODE(IREF))
      NTR=26
      IF(NCD.EQ.0.AND.III(J,INTR).GT.0)GO TO 23
      GO TO 45
C
C     DETERMINATION OF THE ANGLE OF THE RAY OF GENERATED WAVE
C     AT THE POINT OF INCIDENCE
C
   16 LAY2=LAY
      IF(KRE.GT.1)ITYPE=ISIGN(1,CODE(IREF))
      CALL VELOC(Y,VEL)
      V2=VEL(1)
      IF(INCD.EQ.1)NCD=0
      INCD=0
      INSPL=0
      AINDEX=V2/V1
      AUX1=1./(V2*V2)-1./(V1*V1)+AUX*AUX
      IF(AUX1.GT.0.) GO TO 17
      NTR=27
      GO TO 28
   17 IF(NCD.EQ.0)AUXZ=-SQRT(AUX1)
      IF(NCD.NE.0)AUXZ=SQRT(AUX1)
      AUX1=AUXZ+AUX
      AUXZ=AUXZ*V2
      Y(3)=PX-ANYX*AUX1
      Y(4)=PZ-ANYZ*AUX1
C
C
C     DETERMINATION OF PARAMETERS OF GENERATED WAVE
C     AT THE POINT OF INCIDENCE
C
   20 PRMT(5)=2.
      KINT(IRR)=NRR
      DS(1,IRR)=RC
      DS(10,IRR)=AINDEX*AUXX
      DS(11,IRR)=-AUXZ
      IF(NCD.NE.0)GO TO 52
C
C     DETERMINATION OF VELOCITIES AND DENSITIES
C     ON THE OPPOSITE SIDE OF A REFLECTOR
C
      JA=J11
      INTRA=I11
      LAY2=L11
      IF(INTRA.EQ.L11)GO TO 43
   40 LAY2=LAY2+1
      NTR=28
      IF(INTRA.EQ.NINT.AND.NDER.NE.0)GO TO 51
      IF(INTRA.EQ.NINT)RETURN
      NC=NPNT(INTRA+1)
      DO 41 I=1,NC
      JJ1=I
      II1=III(I,INTRA+1)
      IF(JA.EQ.II1)GO TO 42
   41 CONTINUE
      GO TO 44
   42 INTRA=INTRA+1
      JA=JJ1
      GO TO 40
   43 LAY2=LAY2-1
      II1=III(JA,INTRA)
      IF(II1.LE.0)GO TO 44
      INTRA=INTRA-1
      JA=II1
      GO TO 43
   44 IF(LAY2.NE.0)GO TO 52
      DS(6,IRR)=0.
      DS(7,IRR)=0.
      DS(9,IRR)=0.
      RETURN
   52 ITYPE1=ITYPE
      ITYPE=1
      L11=LAY
      LAY=LAY2
      CALL VELOC(Y,VEL)
      DS(6,IRR)=VEL(1)
      DS(9,IRR)=RHO1(LAY2)+RHO2(LAY2)*VEL(1)
      ITYPE=-1
      CALL VELOC(Y,VEL)
      DS(7,IRR)=VEL(1)
      ITYPE=ITYPE1
      LAY=L11
      RETURN
C
C     CHECK WHETHER THE RAY DOES NOT TERMINATE AT A FICTITIOUS INTERFACE
C
   24 IND=INTR+1000
      IF(IREF.EQ.1.AND.KC.GT.0.AND.INTR.EQ.INT1)GO TO 26
      IF(IREF.EQ.1.AND.KC.LT.0.AND.INTR.NE.INT1)GO TO 26
      IF(INTR.EQ.1)IND=3
      IF(INTR.EQ.NINT)IND=4
      NTR=29
      IF(III(J,INTR).EQ.(-2))GO TO 48
      IF(III(J,INTR).LE.0)GO TO 39
      IEL=J
      IDS=INTR
   63 II1=III(IEL,IDS)
      NTR=30
      IF(III(II1,IDS-1).EQ.(-2)) GO TO 48
      IF(III(II1,IDS-1).LE.0)GO TO 39
      IEL=III(II1,IDS-1)
      IDS=IDS-1
      GO TO 63
C
C     TERMINATION OF COMPUTATION
C
   48 IND=16
      GO TO 25
   51 IND=15
      GO TO 39
   23 IND=17
      GO TO 39
   66 IND=18
      GO TO 39
   26 IND=8
      GO TO 39
   54 IND=20
      GO TO 39
   28 IND=9
   39 IREF=IRR
   25 PRMT(5)=3.
      KINT(IRR)=NRR
      GO TO 32
   50 IND=13
      PRMT(5)=3.
   32 IF(IND.EQ.9)RETURN
      YAN=Y(3)
      Y(3)=101.
      CALL VELOC(Y,VEL)
      Y(3)=YAN
      RETURN
      END
C
C     *****************************************************************
C
      SUBROUTINE FCT(X,Y,DERY)
C
C     COMPUTATION OF THE RIGHT-HAND SIDES
C     OF THE RAY TRACING SYSTEM EQUATIONS
C
      DIMENSION VEL(6),Y(4),DERY(4)
      COMMON/AUXI/INTR,INT1,IOUT,KRE,IREFR,LAY,ITYPE,NDER,IPRINT,MPRINT,
     1NTR,IMET
C
      CALL VELOC(Y,VEL)
      VV=VEL(1)
      V1=1./VV
      VV=VV*VV
      DERY(1)=VV*Y(3)
      DERY(2)=VV*Y(4)
      DERY(3)=-V1*VEL(2)
      DERY(4)=-V1*VEL(3)
      RETURN
      END
C
C     ****************************************************************
C
      SUBROUTINE ROOT(YOLD,YNEW,YINT,ANYX,ANYZ,RC,J,IROOT)
C
C     DETERMINATION OF THE POINT OF INCIDENCE AT AN INTERFACE
C     (INTERSECTION OF THE RAY WITH AN INTERFACE)
C
C
      DIMENSION YDIF(3),YOLD(2),YNEW(2),YINT(2)
      COMMON/INTRF/A1(30,20),B1(29,20),C1(30,20),D1(29,20),X1(30,20),
     1BRD(2),III(30,20),NPNT(20),NINT
      COMMON/AUXI/INTR,INT1,IOUT,KRE,IREFR,LAY,ITYPE,NDER,IPRINT,MPRINT,
     1NTR,IMET
C
      SH(X9)=(EXP(X9)-EXP(-X9))/2.
      CH(X9)=(EXP(X9)+EXP(-X9))/2.
      ARCCOS(X9)=ATAN(SQRT(1.-X9*X9)/X9)
      ARCCH(X9)=ALOG(X9+SQRT(X9*X9-1.))
      ARCSH(X9)=ALOG(X9+SQRT(X9*X9+1.))
C
C
      IROOT=100
C
C     DETERMINATION OF THE ELEMENT OF THE INTERFACE CONTAINING
C     THE POINT OF INCIDENCE
C
      LRT=0
   30 NC=NPNT(INTR)
      XA=YOLD(1)
      XB=YNEW(1)
      AUX=XB-XA
      AUXAB=AUX
      X2=X1(J,INTR)
      A2=A1(J,INTR)
      B2=B1(J,INTR)
      C2=C1(J,INTR)
      D2=D1(J,INTR)
      AUX1=XB-X2
      IF(ABS(AUX).GT..00001)GO TO 1
      YINT(1)=YOLD(1)
      GO TO 2
    1 PP=(YNEW(2)-YOLD(2))/AUX
      QQ=YOLD(2)-PP*XA
      IF(AUX.LT.0.)GO TO 21
      IF(XA.GE.X2)GO TO 2
      X3=XB
      A3=((D2*AUX1+C2)*AUX1+B2)*AUX1+A2
      Y2=PP*X2+QQ
      Y3=PP*X3+QQ
   20 AUX1=(Y3-A3)*(Y2-A2)
      IF(AUX1.LE.0.)GO TO 2
      J=J-1
      X3=X2
      X2=X1(J,INTR)
      A3=A2
      A2=A1(J,INTR)
      IF(XA.GE.X2)GO TO 2
      Y3=Y2
      Y2=PP*X2+QQ
      GO TO 20
   21 X3=X1(J+1,INTR)
      IF(XA.LE.X3)GO TO 2
      A3=A1(J+1,INTR)
      X2=XB
      A2=((D2*AUX1+C2)*AUX1+B2)*AUX1+A2
      Y2=PP*X2+QQ
      Y3=PP*X3+QQ
   22 AUX1=(Y3-A3)*(Y2-A2)
      IF(AUX1.LE.0.)GO TO 2
      J=J+1
      X2=X3
      X3=X1(J+1,INTR)
      A2=A3
      IF(XA.LE.X3)GO TO 2
      A3=A1(J+1,INTR)
      Y2=Y3
      Y3=PP*X3+QQ
      GO TO 22
C
    2 X2=X1(J,INTR)
      A2=A1(J,INTR)
      B2=B1(J,INTR)
      C2=C1(J,INTR)
      D2=D1(J,INTR)
      IF(ABS(AUX).LE..00001)GO TO 17
      XX=XA-X2
C
C     DETERMINATION OF COEFFICIENTS OF CUBIC EQUATION 
C     D*X**3+C*X**2+B*X+A=0
C     ROOTS ARE LOOKED FOR IN INTERVAL (0,1)
C
      D=D2*AUX*AUX*AUX
      C=(3*D2*XX+C2)*AUX*AUX
      B=((3.*D2*XX+2.*C2)*XX+B2-PP)*AUX
      A=((D2*XX+C2)*XX+B2)*XX+A2-PP*XA-QQ
C
      IF(ABS(D).LT..000001)GO TO 10
C
C     TRANSFORMATION OF CUBIC EQUATION INTO FORM Y**3+3*P*Y+Q=0
C     SUBSTITUTING Y=X+C/(3*D)
C
      AUX1=C/(3.*D)
      Q=AUX1*AUX1*AUX1-.5*(B*AUX1-A)/D
      P=B/(3.*D)-AUX1*AUX1
      DISKR=Q*Q+P*P*P
      IF(Q.EQ.0.) GO TO 8
      IF(P.EQ.0.) GO TO 7
      R=SIGN(1.,Q)*SQRT(ABS(P))
      AX=Q/(R*R*R)
      IF(P.GT.0.) GO TO 6
      IF(DISKR)3,3,5
C
C     P.LT.0.AND.DISKR.LE.0
C
    3 D=ARCCOS(AX)/3.
      XR=-2.*R*COS(D)-AUX1
      XR1=2.*R*COS(1.047198-D)-AUX1
      XR2=2.*R*COS(1.047198+D)-AUX1
   25 NRT=0
      IF(XR.LT.0..OR.XR.GT.1.)GO TO 18
      RR=XA+XR*AUX-YOLD(1)
      NRT=NRT+1
      YDIF(NRT)=RR
   18 IF(XR1.LT.0..OR.XR1.GT.1.)GO TO 19
      RR=XA+XR1*AUX-YOLD(1)
      NRT=NRT+1
      YDIF(NRT)=RR
   19 IF(XR2.LT.0..OR.XR2.GT.1.)GO TO 4
      RR=XA+XR2*AUX-YOLD(1)
      NRT=NRT+1
      YDIF(NRT)=RR
    4 IF(NRT.EQ.1)GO TO 26
      IF(NRT.NE.0)GO TO 40
      IF(ABS(XR).LE.ABS(XR1))GO TO 41
      XR=XR1
   41 IF(ABS(XR).LE.ABS(XR2))GO TO 42
      XR=XR2
   42 YDIF(1)=XA+XR*AUX-YOLD(1)
      GO TO 26
   40 Y1=YDIF(1)
      Y2=YDIF(2)
      IF(ABS(Y1).LT..00001.AND.INTR.EQ.INT1)Y1=1000000.
      IF(ABS(Y2).LT..00001.AND.INTR.EQ.INT1)Y2=1000000.
      IF(ABS(Y2).LT.ABS(Y1))YDIF(1)=Y2
      IF(ABS(Y2).LT.ABS(Y1))Y1=Y2
      IF(NRT.EQ.2)GO TO 26
      Y3=YDIF(3)
      IF(ABS(Y3).LT..00001.AND.INTR.EQ.INT1)Y3=1000000.
      IF(ABS(Y3).LT.ABS(Y1))YDIF(1)=Y3
   26 YINT(1)=YDIF(1)+YOLD(1)
      GO TO 17
C
C     P.LT.0..AND.DISKR.GT.0.
C
    5 XR=-2.*R*CH(ARCCH(AX)/3.)-AUX1
      GO TO 15
C
C     P.GT.0
C
    6 XR=-2.*R*SH(ARCSH(AX)/3.)-AUX1
      GO TO 15
C
C     P.EQ.0.
C
    7 XR=-SIGN(1.,Q)*EXP(ALOG(2.*ABS(Q))/3.)-AUX1
      GO TO 15
C
C     Q.EQ.0
C
    8 XR=-AUX1
      IF(P)9,15,15
C
C     Q.EQ.0..AND.P.LT.0.
C
    9 XR1=SQRT(-3.*P)-AUX1
      XR2=-XR1-2.*AUX1
      GO TO 25
C
C     REDUCTION OF CUBIC EQUATION TO QUADRATIC EQUATION
C
   10 IF(ABS(C).LT..000001) GO TO 14
      DISKR=B*B-4.*C*A
      P=-B/(2.*C)
      IF(DISKR)11,11,12
C
   11 XR=P
      GO TO 15
C
   12 Q=SQRT(DISKR)/(2.*C)
      XR=P+Q
      XR1=P-Q
   13 NRT=2
      IF(XR.GE.0..AND.XR.LE.1.)GO TO 23
      NRT=NRT-1
      XR=XR1
   23 IF(XR1.GE.0..AND.XR1.LE.1.)GO TO 24
      NRT=NRT-1
   24 IF(NRT.EQ.1)GO TO 15
      RR=XA+XR*AUX-YOLD(1)
      IF(ABS(RR).LT..00001.AND.INTR.EQ.INT1)RR=1000000.
      RR1=XA+XR1*AUX-YOLD(1)
      IF(ABS(RR1).LT..00001.AND.INTR.EQ.INT1)RR1=1000000.
      IF(ABS(RR1).LT.ABS(RR))XR=XR1
      GO TO 15
C
C     REDUCTION OF CUBIC EQUATION TO LINEAR EQUATION
C
   14 XR=-A/B
   15 YINT(1)=XA+XR*(XB-XA)
      GO TO 17
   27 J=J+1
      GO TO 30
   28 J=J-1
      GO TO 30
   17 IF(YINT(1).GE.YOLD(1).AND.YINT(1).LE.YNEW(1))GO TO 16
      IF(YINT(1).GE.YNEW(1).AND.YINT(1).LE.YOLD(1))GO TO 16
      YINT(1)=.5*(YOLD(1)+YNEW(1))
   16 AUX=YINT(1)-X2
      YINT(2)=((D2*AUX+C2)*AUX+B2)*AUX+A2
      AUX1=((3.*D2*AUX+2.*C2)*AUX+B2)
      AUX2=1./(AUX1*AUX1+1.)
      AUX2=SQRT(AUX2)
      ANYX=-AUX1*AUX2
      ANYZ=AUX2
      IF(LRT.NE.1)GO TO 31
C
C     CHECK WHETHER THE RAY DID NOT CROSS THE INTERFACE TWICE
C
      XAA=YINT(1)-YOLD(1)
      XBB=YINT(2)-YOLD(2)
      IF(ABS(XBB).LT..0001.OR.ABS(XAA).LT..0001)GO TO 35
      XAA=YNEW(1)-YOLD(1)
      XBB=YNEW(2)-YOLD(2)
      AUX=XAA*ANYX+XBB*ANYZ
      IF(INTR.EQ.LAY.AND.AUX.LT.0.)GO TO 31
      IF(INTR.NE.LAY.AND.AUX.GT.0.)GO TO 31
      IF(YINT(1).GT.YOLD(1))GO TO 28
      IF(YINT(1).LT.YOLD(1))GO TO 27
   31 IF(INTR.EQ.LAY)GO TO 32
      IF(III(J,INTR).LE.0)GO TO 35
      LRT=1
      J=III(J,INTR)
      INTR=INTR-1
      GO TO 30
   32 NC1=NPNT(INTR+1)-1
      DO 33 I=1,NC1
      I1=I
      IF(J.EQ.III(I,INTR+1))GO TO 34
   33 CONTINUE
      GO TO 35
   34 J=I1
      LRT=1
      INTR=INTR+1
      GO TO 30
   35 RC=(6.*D2*AUX+2.*C2)
      RC=RC*ANYZ*ANYZ*ANYZ
      RETURN
      END
C
C     *************************************************************
C
      SUBROUTINE AMPL(FJ,AX,AAY,AZ,PHX,PHY,PHZ)
C
C     COMPUTATION OF AMPLITUDES
C
      INTEGER CODE
      COMMON/COD/CODE(50),KREF,KC
      COMMON/RAY/AY(12,400),DS(11,50),KINT(50),MREG,N,IREF,IND,IND1
      COMMON/SOUR/ROS,VPS,VSS
      COMMON/SH/KSH,NSH
C
      PH=0.
      PHSH=0.
      Q=1.
      QSH=1.
      V=1.
      AL=1.
      AX=0.
      AAY=0.
      AZ=0.
      PHX=0.
      PHY=0.
      PHZ=0.
      IREF1=IREF-1
      IF(IREF1.EQ.0)IC2=CODE(1)
      IF(IREF1.EQ.0)GO TO 10
C
C     LOOP FOR INTERFACES
C
      DO 5 I=1,IREF1
      I1=KINT(I)
      IF(I1.LE.0)GO TO 5
      V1=AY(6,I1)
      PP=DS(2,I)/V1
      P=ABS(PP)
      VP1=DS(4,I)
      VS1=DS(5,I)
      RO1=DS(8,I)
      VP2=DS(6,I)
      VS2=DS(7,I)
      RO2=DS(9,I)
      IF(KREF.LE.1)IC1=CODE(1)
      IF(KREF.LE.1)IC2=CODE(1)
      IF(KREF.LE.1)GO TO 11
      IC1=CODE(I)
      II=I
   12 II=II+1
      IF(KINT(II).LE.0)GO TO 12
      IC2=CODE(II)
      IF((IABS(IC2)-IABS(IC1)).EQ.0)GO TO 2
   11 IF(IC1.LT.0)GO TO 1
      AV=RO1*VP1
      IF(IC2.GT.0)NC=3
      IF(IC2.GT.0)AV=(RO2*VP2)/AV
      IF(IC2.LT.0)NC=4
      IF(IC2.LT.0)AV=(RO2*VS2)/AV
      GO TO 4
    1 AV=RO1*VS1
      IF(IC2.GT.0)NC=7
      IF(IC2.GT.0)AV=(RO2*VP2)/AV
      IF(IC2.LT.0)NC=8
      IF(NSH.NE.0)NCY=10
      IF(IC2.LT.0)AV=(RO2*VS2)/AV
      GO TO 4
    2 IF(ABS(VP2).LT..00001)RO2=0.
      IF(IC1.LT.0)GO TO 3
      IF(IC2.GT.0)NC=1
      IF(IC2.GT.0)AV=1.
      IF(IC2.LT.0)NC=2
      IF(IC2.LT.0)AV=VS1/VP1
      GO TO 4
    3 IF(IC2.GT.0)NC=5
      IF(IC2.GT.0)AV=VP1/VS1
      IF(IC2.LT.0)NC=6
      IF(NSH.NE.0)NCY=9
      IF(IC2.LT.0)AV=1.
      IF(ABS(RO2).LT..00001)NC=NC-2
      IF(ABS(RO2).LT..00001.AND.NSH.NE.0)NCY=9
      GO TO 4
    4 V=V*AV
      ND=0
      IF(PP.LT.0.)ND=1
      IF(KSH.EQ.0)GO TO 15
      CALL COEF8(P,VP1,VS1,RO1,VP2,VS2,RO2,NCY,ND,RY,PHSY)
      QSH=QSH*RY
      PHSH=PHSH+PHSY
      IF(NSH.EQ.1)GO TO 16
   15 CALL COEF8(P,VP1,VS1,RO1,VP2,VS2,RO2,NC,ND,R,PHS)
      Q=Q*R
      PH=PH+PHS
   16 SN1=DS(3,I)
      SN2=DS(11,I)
      AL=AL*ABS(SN1/SN2)
    5 CONTINUE
C
C     END OF LOOP FOR INTERFACES
C
   10 V0=AY(6,1)
      V=V*ROS*V0
      RO1=DS(8,IREF)
      I=KINT(IREF)
      VF=AY(6,I)
      V=V/(RO1*VF)
      V=SQRT(V)
      AL=AL*FJ
      FJ=SQRT(AL)
      U=(V*Q)/FJ
C
C
      TX=AY(4,I)*VF
      TZ=-AY(5,I)*VF
      IF(IND.NE.3)GO TO 8
      IF(MREG.EQ.1)GO TO 8
      VP1=DS(4,IREF)
      VS1=DS(5,IREF)
      RO2=0.
      P=DS(2,IREF)/VF
      IF(NSH.EQ.0)GO TO 14
      CALL COEF8(P,VP1,VS1,RO1,VP2,VS2,RO2,10,ND,RY,PHSY)
      USH=(V*QSH)/FJ
      AAY=USH*RY
      PHY=PHSY+PHSH+3.14159265
      IF(KSH.EQ.1)RETURN
   14 TG=DS(2,IREF)
      TH=DS(3,IREF)
      ND=0
      IF(P.LT.0.)ND=1
      P=ABS(P)
      IF(IC2.LT.0)GO TO 6
      CALL COEF8(P,VP1,VS1,RO1,VP2,VS2,RO2,5,ND,RX,PHX)
      CALL COEF8(P,VP1,VS1,RO1,VP2,VS2,RO2,6,ND,RZ,PHZ)
      GO TO 7
    6 CONTINUE
      CALL COEF8(P,VP1,VS1,RO1,VP2,VS2,RO2,7,ND,RX,PHX)
      CALL COEF8(P,VP1,VS1,RO1,VP2,VS2,RO2,8,ND,RZ,PHZ)
    7 AX=U*RX
      AZ=U*RZ
      PHX=PHX+PH
      PHZ=PHZ+PH
      AUX1=TX*TH-TZ*TG
      IF(ABS(AUX1).GT..00001)GO TO 13
      PHX=PHX+3.14159265
      PHZ=PHZ+3.14159265
      GO TO 9
   13 AUX2=TG*TX+TH*TZ
      AUX3=AX*COS(PHX)
      AUX4=AZ*COS(PHZ)
      AUX5=AX*SIN(PHX)
      AUX6=AZ*SIN(PHZ)
      AR1=(AUX3*AUX2+AUX4*AUX1)
      AI1=(AUX5*AUX2+AUX6*AUX1)
      AR2=(AUX4*AUX2-AUX3*AUX1)
      AI2=(AUX6*AUX2-AUX5*AUX1)
      AX=SQRT(AR1*AR1+AI1*AI1)
      PHX=ATAN2(AI1,AR1)
      AZ=SQRT(AR2*AR2+AI2*AI2)
      PHZ=ATAN2(AI2,AR2)
      GO TO 9
    8 IF(IC2.LT.0)GO TO 17
      AX=U*TX
      AZ=-U*TZ
      GO TO 19
   17 IF(KSH.EQ.1)GO TO 18
      AX=-U*TZ
      AZ=-U*TX
      IF(NSH.EQ.0)GO TO 19
   18 AAY=(V*QSH)/FJ
      PHY=PHSH
      IF(AAY.LT.0.)PHY=PHSH-3.14159
      IF(AAY.LT.0.)AAY=-AAY
      IF(KSH.EQ.1)RETURN
   19 PHX=PH
      PHZ=PH
      IF(AX.LT.0.)PHX=PH-3.14159
      IF(AX.LT.0.)AX=-AX
      IF(AZ.LT.0.)PHZ=PH-3.14159
      IF(AZ.LT.0.)AZ=-AZ
    9 RETURN
      END
C
C     ***********************************************************
C
      SUBROUTINE JPAR(Y,AMULT,KMAH)
C
C     DYNAMIC RAY TRACING
C
      INTEGER CODE
      DIMENSION Y(15)
      COMMON/RAY/AY(12,400),DS(11,50),KINT(50),MREG,N,IREF,IND,IND1
      COMMON/COD/CODE(50),KREF,KC
      COMMON/AUXI/INTR,INT1,IOUT,KRE,IREFR,LAY,ITYPE,NDER,KPRINT,MPRINT,
     1NTR,IMET
      common/curv/LU6
C
      IRF=0
      AMULT=1.
      KMAH=0
      NDIM=6
      IF(IMET.EQ.1)NDIM=15
      IREF1=IREF
      X=AY(1,1)
      Y(1)=1.
      Y(2)=0.
      Y(3)=0.
      Y(4)=1.
      DO 10 I=5,NDIM
   10 Y(I)=0.
    2 I1=KINT(IREF1)
      IF(I1.LE.0)IREF1=IREF1-1
      IF(I1.LE.0)GO TO 2
    8 IRF=IRF+1
      I1=KINT(IRF)
      IF(I1.LE.0)GO TO 8
    1 I2=I1+1
      IRF1=IRF
    3 IRF1=IRF1+1
      IF(IRF1.GE.IREF1)GO TO 5
      I3=KINT(IRF1)
      IF(I3.LT.0)I2=I2+2
      IF(I3.LT.0)GO TO 3
    5 CONTINUE
      CALL RK(X,Y,IRF,NDIM,KMAH)
      IF(N.EQ.KINT(IREF1))RETURN
C
C     Tentativa de gravar a curvatura dos refletores
C     durante transmissao/refleccao
C     
      write(LU6,'(f13.8,f15.8)')x,ds(1,irf)
C
      D11=.5*DS(1,IRF)
      IF(KREF.LE.1)GO TO 6
    6 V1=AY(6,I1)
      VX=AY(7,I1)
      VY=AY(8,I1)
      CS=AY(5,I1)
      SN=AY(4,I1)
      AKAPA1=VX*CS-VY*SN
      VS1=(VX*SN+VY*CS)*V1
      V2=AY(6,I2)
      VX=AY(7,I2)
      VY=AY(8,I2)
      CS=AY(5,I2)
      SN=AY(4,I2)
      AKAPA2=VX*CS-VY*SN
      VS2=(VX*SN+VY*CS)*V2
      CS1=DS(2,IRF)
      SN1=-DS(3,IRF)
      SN2=-DS(11,IRF)
      S1=2.*(AKAPA1*SN1-AKAPA2*SN2)*CS1/V1
      S1=S1+2.*D11*(SN1/V1-SN2/V2)
      S1=S1+(VS1-VS2)*(CS1*CS1/(V1*V1))
      Y(2)=(Y(2)*SN1-Y(1)*S1/SN1)/SN2
      Y(1)=Y(1)*SN2/SN1
      AMULT=AMULT*SN1/SN2
      Y(4)=(Y(4)*SN1-Y(3)*S1/SN1)/SN2
      Y(3)=Y(3)*SN2/SN1
    7 IRF=IRF+1
      I1=KINT(IRF)
      IF(I1.LE.0)GO TO 7
      GO TO 1
C
      END
C
C     ***********************************************************
C
      SUBROUTINE RK(X,Y,IRF,NDIM,KMAH)
C
C     MODIFIED EULER'S METHOD TO SOLVE A SYSTEM OF LINEAR
C     ORDINARY DIFFERENTIAL EQUATIONS OF FIRST ORDER
C
      DIMENSION Y(15),DERY(15),Y1(15),Y2(15)
      COMMON/RAY/AY(12,400),DS(11,50),KINT(50),MREG,N,IREF,IND,IND1
      common/qp/LU5
C
      I1=KINT(IRF)
      N=N+1
    1 H=AY(1,N+1)-AY(1,N)

C     ***********************************************************
C     Modificacao sugerida por Ivan Psencik para gravar os elementos
C     das matrizes de propagacao
C      write(LU5,'(f13.8,5e15.5)')x,y(1),y(2),y(3),y(4),y(6)
C     ***********************************************************
      X=X+H
      CALL FCTA(Y,DERY,NDIM)
      DO 2 I=1,NDIM
      Y1(I)=DERY(I)
    2 Y2(I)=Y(I)+H*Y1(I)
      N=N+1
      CALL FCTA(Y2,DERY,NDIM)
      AUX=Y(3)
      DO 3 I=1,NDIM
    3 Y(I)=Y(I)+.5*H*(Y1(I)+DERY(I))
      IF(AUX*Y(3).LT.0.)KMAH=KMAH+1

C     ***********************************************************
      IF(N.EQ.I1) then
         write(LU5,'(f13.8,5e15.5)')x,y(1),y(2),y(3),y(4),y(6)
         GO TO 4
      end if
      GO TO 1
    4 RETURN
      END
C
C     ***********************************************************
C
      SUBROUTINE FCTA(Y,DERY,NDIM)
C
C     COMPUTATION OF THE RIGHT-HAND SIDES OF THE DYNAMIC
C     RAY TRACING SYSTEM EQUATIONS
C
      DIMENSION Y(NDIM),DERY(NDIM)
      COMMON/RAY/AY(12,400),DS(11,50),KINT(50),MREG,N,IREF,IND,IND1
      COMMON/AUXI/INTR,INT1,IOUT,KRE,IREFR,LAY,ITYPE,NDER,IPRINT,MPRINT,
     1NTR,IMET
C
      V=AY(6,N)
      VV=V*V
      CS=AY(5,N)*V
      SN=AY(4,N)*V
      VXX=AY(9,N)
      VXY=AY(10,N)
      VYY=AY(11,N)
      VN=(AY(7,N)*SN-AY(8,N)*CS)**2
      V22=VXX*CS*CS-2.*VXY*CS*SN+VYY*SN*SN
      DERY(1)=VV*Y(2)
      DERY(2)=-(V22*Y(1))/V
      DERY(3)=VV*Y(4)
      DERY(4)=-(V22*Y(3))/V
      DERY(5)=0.
      IF(ABS(AY(12,N)).GT..1)DERY(5)=1./AY(12,N)
      DERY(6)=VV
      IF(IMET.NE.1)RETURN
      DERY(7)=VV*Y(1)*Y(1)
      DERY(8)=VV*Y(1)*Y(3)
      DERY(9)=VV*Y(3)*Y(3)
      DERY(10)=VV*Y(2)*Y(2)
      DERY(11)=VV*Y(2)*Y(4)
      DERY(12)=VV*Y(4)*Y(4)
      DERY(13)=VN*Y(1)*Y(1)
      DERY(14)=VN*Y(1)*Y(3)
      DERY(15)=VN*Y(3)*Y(3)
      RETURN
      END
C
C     ***********************************************************
C
      SUBROUTINE COEF8(P,VP1,VS1,RO1,VP2,VS2,RO2,NCODE,ND,RMOD,RPH)
C
C     THE ROUTINE COEF8 IS DESIGNED FOR THE COMPUTATION OF REFLECTION
C     AND TRANSMISSION COEFFICIENTS AT A PLANE INTERFACE BETWEEN TWO
C     HOMOGENEOUS SOLID HALFSPACES OR AT A FREE SURFACE OF A HOMOGENEOUS
C     SOLID HALFSPACE.
C
      COMPLEX B(4),RR,C1,C2,C3,C4,H1,H2,H3,H4,H5,H6,H,HB,HC
      DIMENSION PRMT(4),D(4),DD(4)
      IF(NCODE.GE.9)GO TO 300
      IF(RO2.LT.0.000001)GO TO 150
      PRMT(1)=VP1
      PRMT(2)=VS1
      PRMT(3)=VP2
      PRMT(4)=VS2
      A1=VP1*VS1
      A2=VP2*VS2
      A3=VP1*RO1
      A4=VP2*RO2
      A5=VS1*RO1
      A6=VS2*RO2
      Q=2.*(A6*VS2-A5*VS1)
      PP=P*P
      QP=Q*PP
      X=RO2-QP
      Y=RO1+QP
      Z=RO2-RO1-QP
      G1=A1*A2*PP*Z*Z
      G2=A2*X*X
      G3=A1*Y*Y
      G4=A4*A5
      G5=A3*A6
      G6=Q*Q*PP
      DO 21 I=1,4
      DD(I)=P*PRMT(I)
   21 D(I)=SQRT(ABS(1.-DD(I)*DD(I)))
      IF(DD(1).LE.1..AND.DD(2).LE.1..AND.DD(3).LE.1..AND.DD(4).LE.1.)
     1GO TO 100
C
C     COMPLEX COEFFICIENTS
      DO 22 I=1,4
      IF(DD(I).GT.1.)GO TO 23
      B(I)=CMPLX(D(I),0.)
      GO TO 22
   23 B(I)= CMPLX(0.,D(I))
   22 CONTINUE
      C1=B(1)*B(2)
      C2=B(3)*B(4)
      C3=B(1)*B(4)
      C4=B(2)*B(3)
      H1=G1
      H2=G2*C1
      H3=G3*C2
      H4=G4*C3
      H5=G5*C4
      H6=G6*C1*C2
      H=1./(H1+H2+H3+H4+H5+H6)
      HB=2.*H
      HC=HB*P
      GO TO (1,2,3,4,5,6,7,8),NCODE
    1 RR=H*(H2+H4+H6-H1-H3-H5)
      GO TO 26
    2 RR=VP1*B(1)*HC*(Q*Y*C2+A2*X*Z)
      IF(ND.NE.0)RR=-RR
      GO TO 26
    3 RR=A3*B(1)*HB*(VS2*B(2)*X+VS1*B(4)*Y)
      GO TO 26
    4 RR=-A3*B(1)*HC*(Q*C4-VS1*VP2*Z)
      IF(ND.NE.0)RR=-RR
      GO TO 26
    5 RR=-VS1*B(2)*HC*(Q*Y*C2+A2*X*Z)
      IF(ND.NE.0)RR=-RR
      GO TO 26
    6 RR=H*(H2+H5+H6-H1-H3-H4)
      GO TO 26
    7 RR=A5*B(2)*HC*(Q*C3-VP1*VS2*Z)
      IF(ND.NE.0)RR=-RR
      GO TO 26
    8 RR=A5*B(2)*HB*(VP1*B(3)*Y+VP2*B(1)*X)
      GO TO 26
C     REAL COEFFICIENTS
  100 E1=D(1)*D(2)
      E2=D(3)*D(4)
      E3=D(1)*D(4)
      E4=D(2)*D(3)
      S1=G1
      S2=G2*E1
      S3=G3*E2
      S4=G4*E3
      S5=G5*E4
      S6=G6*E1*E2
      S=1./(S1+S2+S3+S4+S5+S6)
      SB=2.*S
      SC=SB*P
      GO TO (101,102,103,104,105,106,107,108),NCODE
  101 R=S*(S2+S4+S6-S1-S3-S5)
      GO TO 250
  102 R=VP1*D(1)*SC*(Q*Y*E2+A2*X*Z)
      IF(ND.NE.0)R=-R
      GO TO 250
  103 R=A3*D(1)*SB*(VS2*D(2)*X+VS1*D(4)*Y)
      GO TO 250
  104 R=-A3*D(1)*SC*(Q*E4-VS1*VP2*Z)
      IF(ND.NE.0)R=-R
      GO TO 250
  105 R=-VS1*D(2)*SC*(Q*Y*E2+A2*X*Z)
      IF(ND.NE.0)R=-R
      GO TO 250
  106 R=S*(S2+S5+S6-S1-S3-S4)
      GO TO 250
  107 R=A5*D(2)*SC*(Q*E3-VP1*VS2*Z)
      IF(ND.NE.0)R=-R
      GO TO 250
  108 R=A5*D(2)*SB*(VP1*D(3)*Y+VP2*D(1)*X)
      GO TO 250
C
C     EARTHS SURFACE,COMPLEX COEFFICIENTS AND COEFFICIENTS OF CONVERSION
  150 A1=VS1*P
      A2=A1*A1
      A3=2.*A2
      A4=2.*A1
      A5=A4+A4
      A6=1.-A3
      A7=2.*A6
      A8=2.*A3*VS1/VP1
      A9=A6*A6
      DD(1)=P*VP1
      DD(2)=P*VS1
      DO 151 I=1,2
  151 D(I)=SQRT(ABS(1.-DD(I)*DD(I)))
      IF(DD(1).LE.1..AND.DD(2).LE.1.)GO TO 200
      DO 154 I=1,2
      IF(DD(I).GT.1.)GO TO 155
      B(I)=CMPLX(D(I),0.)
      GO TO 154
  155 B(I)= CMPLX(0.,D(I))
  154 CONTINUE
      H1=B(1)*B(2)
      H2=H1*A8
      H=1./(A9+H2)
      GO TO (161,162,163,164,165,166,167,168),NCODE
  161 RR=(-A9+H2)*H
      GO TO 26
  162 RR=-A5*B(1)*H*A6
      IF(ND.NE.0)RR=-RR
      GO TO 26
  163 RR=A5*B(2)*H*A6*VS1/VP1
      IF(ND.NE.0)RR=-RR
      GO TO 26
  164 RR=-(A9-H2)*H
      GO TO 26
  165 RR=A5*H1*H
      IF(ND.NE.0)RR=-RR
      GO TO 26
  166 RR=-A7*B(1)*H
      GO TO 26
  167 RR=A7*B(2)*H
      GO TO 26
  168 RR=A5*VS1*H1*H/VP1
      IF(ND.NE.0)RR=-RR
   26 Z2=REAL(RR)
      Z3=AIMAG(RR)
      IF(Z2.EQ.0..AND.Z3.EQ.0.)GO TO 157
      RMOD=SQRT(Z2*Z2+Z3*Z3)
      RPH=ATAN2(Z3,Z2)
      RETURN
  157 RMOD=0.
      RPH=0.
      RETURN
C
C     EARTHS SURFACE,REAL COEFFICIENTS AND COEFFICIENTS OF CONVERSION
  200 S1=D(1)*D(2)
      S2=A8*S1
      S=1./(A9+S2)
      GO TO (201,202,203,204,205,206,207,208),NCODE
  201 R=(-A9+S2)*S
      GO TO 250
  202 R=-A5*D(1)*S*A6
      IF(ND.NE.0)R=-R
      GO TO 250
  203 R=A5*D(2)*S*A6*VS1/VP1
      IF(ND.NE.0)R=-R
      GO TO 250
  204 R=(S2-A9)*S
      GO TO 250
  205 R=A5*S1*S
      IF(ND.NE.0)R=-R
      GO TO 250
  206 R=-A7*D(1)*S
      GO TO 250
  207 R=A7*D(2)*S
      GO TO 250
  208 R=A5*VS1*S1*S/VP1
      IF(ND.NE.0)R=-R
  250 IF(R.LT.0.)GO TO 251
      RMOD=R
      RPH=0.
      RETURN
  251 RMOD=-R
      RPH=-3.14159
      RETURN
C
C     SH COEFFICIENTS OF REFLECTION, TRANSMISSION AND CONVERSION
C
  300 if(ro2.lt..000001)go to 302
      A1=P*VS1
      A2=P*VS2
      A3=RO1*VS1
      A4=RO2*VS2
      A5=SQRT(ABS(1.-A1*A1))
      A6=SQRT(ABS(1.-A2*A2))
      C1=A5
      IF(A2.LE.1.)C2=A6
      IF(A2.GT.1.)C2=CMPLX(0.,A6)
      C1=C1*A3
      C2=C2*A4
      H=1./(C1+C2)
      IF(NCODE.EQ.10)GO TO 301
      RR=H*(C1-C2)
      GO TO 26
  301 RR=2.*C1*H
      GO TO 26
  302 if(ncode.eq.10)go to 303
      RMOD=1.
      RPH=0.
      RETURN
  303 RMOD=2.
      RPH=0.
      RETURN
      END
C
C     *************************************************************
C
      SUBROUTINE RKGS(PRMT,Y,DERY,NDIM,IHLF,FCT,OUTP,AUX)
      DIMENSION Y(1),DERY(1),AUX(8,1),A(4),B(4),C(4),PRMT(5)
      DO 1 I=1,NDIM
    1 AUX(8,I)=.0666667*DERY(I)
      X=PRMT(1)
      XEND=PRMT(2)
      H=PRMT(3)
      PRMT(5)=0.
      CALL FCT(X,Y,DERY)
      IF(H*(XEND-X))38,37,2
   2  A(1)=.5
      A(2)=.2928932
      A(3)=1.707107
      A(4)=.1666667
      B(1)=2.
      B(2)=1.
      B(3)=1.
      B(4)=2.
      C(1)=.5
      C(2)=.2928932
      C(3)=1.707107
      C(4)=.5
      DO 3 I=1,NDIM
      AUX(1,I)=Y(I)
      AUX(2,I)=DERY(I)
      AUX(3,I)=0.
    3 AUX(6,I)=0.0
      IREC=0
      H=H+H
      IHLF=-1
      ISTEP=0
      IEND=0
    4 IF((X+H-XEND)*H)7,6,5
    5 H=XEND-X
    6 IEND=1
    7 CALL OUTP(X,Y,DERY,IREC,NDIM,PRMT)
      IF(PRMT(5))40,8,40
    8 ITEST=0
    9 ISTEP=ISTEP+1
      J=1
   10 AJ=A(J)
      BJ=B(J)
      CJ=C(J)
      DO 11 I=1,NDIM
      R1=H*DERY(I)
      R2=AJ*(R1-BJ*AUX(6,I))
      Y(I)=Y(I)+R2
      R2=R2+R2+R2
   11 AUX(6,I)=AUX(6,I)+R2-CJ*R1
      IF(J-4)12,15,15
   12 J=J+1
      IF(J-3)13,14,13
   13 X=X+.5*H
   14 CALL FCT(X,Y,DERY)
      GO TO 10
   15 IF(ITEST)16,16,20
   16 DO 17 I=1,NDIM
   17 AUX(4,I)=Y(I)
      ITEST=1
      ISTEP=ISTEP+ISTEP-2
   18 IHLF=IHLF+1
      X=X-H
      H=.5*H
      DO 19 I=1,NDIM
      Y(I)=AUX(1,I)
      DERY(I)=AUX(2,I)
   19 AUX(6,I)=AUX(3,I)
      GO TO 9
   20 IMOD=ISTEP/2
      IF(ISTEP-IMOD-IMOD)21,23,21
   21 CALL FCT(X,Y,DERY)
      DO 22 I=1,NDIM
      AUX(5,I)=Y(I)
   22 AUX(7,I)=DERY(I)
      GO TO 9
   23 DELT=0.
      DO 24 I=1,NDIM
   24 DELT=DELT+AUX(8,I)*ABS(AUX(4,I)-Y(I))
      IF(DELT-PRMT(4))28,28,25
   25 IF(IHLF-10)26,36,36
   26 DO 27 I=1,NDIM
   27 AUX(4,I)=AUX(5,I)
      ISTEP=ISTEP+ISTEP-4
      X=X-H
      IEND=0
      GO TO 18
   28 CALL FCT(X,Y,DERY)
      DO 29 I=1,NDIM
      AUX(1,I)=Y(I)
      AUX(2,I)=DERY(I)
      AUX(3,I)=AUX(6,I)
      Y(I)=AUX(5,I)
   29 DERY(I)=AUX(7,I)
      CALL OUTP(X-H,Y,DERY,IHLF,NDIM,PRMT)
      IF(PRMT(5))40,30,40
   30 DO 31 I=1,NDIM
      Y(I)=AUX(1,I)
   31 DERY(I)=AUX(2,I)
      IREC=IHLF
      IF(IEND)32,32,39
   32 IHLF=IHLF-1
      ISTEP=ISTEP/2
      H=H+H
      IF(IHLF)4,33,33
   33 IMOD=ISTEP/2
      IF(ISTEP-IMOD-IMOD)4,34,4
   34 IF(DELT-.02*PRMT(4))35,35,4
   35 IHLF=IHLF-1
      ISTEP=ISTEP/2
      H=H+H
      GO TO 4
   36 IHLF=11
      CALL FCT(X,Y,DERY)
      GO TO 39
   37 IHLF=12
      GO TO 39
   38 IHLF=13
   39 CALL OUTP(X,Y,DERY,IHLF,NDIM,PRMT)
   40 RETURN
      END
