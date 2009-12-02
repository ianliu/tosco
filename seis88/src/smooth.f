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


C     PROGRAM   S M O O T H
C     *********************
C
C     PROGRAM SMOOTH IS DESIGNED FOR A GENERATION OF SMOOTH VELO-
C     CITY DATA SPECIFIED IN A RECTANGULAR NETWORK, WHICH MAY BE
C     USED AS INPUT DATA FOR PROGRAM SEIS83 OR OTHER PROGRAMS WITH
C     THE SAME STRUCTURE OF INPUT DATA FOR VELOCITY DISTRIBUTION.
C
C     *************************************************************
C
      DIMENSION G(30,30),FIZ(10),HY(30),FX(30),NN(30),MTEXT(20)
      COMMON/BIS/F(30,30),ABN(30,30),XM(30,30),X(30),Y(30),M,N
C
C     ***
      mode=0
      call serv(mode,lin,lou,lu,lu2,llu4)
      if(mode.eq.0)lin=5
      if(mode.eq.0)lou=6      
C     ***
      IRUN=0
      READ(lin,5)MTEXT
      WRITE(lou,7)MTEXT
  100 READ(lin,1)M,N,IPRINT,DELTA
      WRITE(lou,1)M,N,IPRINT,DELTA
      KODE=1
      IF(M.EQ.0)GO TO 23
      IRUN=1
      IF(LU.NE.0)WRITE(LU,14)M,N
      IF(IPRINT.EQ.0)GO TO 54
      READ(lin,1)NIZ
      WRITE(lou,1)NIZ
      READ(lin,2)(FIZ(I),I=1,NIZ)
      WRITE(lou,4)(FIZ(I),I=1,NIZ)
   54 READ(lin,2)(X(I),I=1,M)
      WRITE(lou,4)(X(I),I=1,M)
      READ(lin,2)(Y(I),I=1,N)
      WRITE(lou,4)(Y(I),I=1,N)
      IF(LU.NE.0)WRITE(LU,12)(X(I),I=1,M)
      IF(LU.NE.0)WRITE(LU,12)(Y(I),I=1,N)
      READ(lin,9)(NN(I),I=1,M)
      WRITE(lou,9)(NN(I),I=1,M)
      DO 41 I=1,M
      N1=NN(I)
      IF(N1.EQ.0)N1=N
      READ(lin,2)(HY(J),J=1,N1)
      WRITE(lou,2)(HY(J),J=1,N1)
      DO 53 J=1,N
      f(i,j)=y(j)
   53 G(I,J)=HY(J)
      GO TO 41
   52 CONTINUE
      DO 42 J=1,N1
   42 FX(J)=F(I,J)
      K=1
      DO 43 J=1,N
   50 HH1=FX(K+1)-Y(J)
      IF(HH1.GE.0..OR.(K+1).EQ.N1)GO TO 44
      K=K+1
      GO TO 50
   44 CONTINUE
      G(I,J)=HY(K+1)-HH1/(FX(K+1)-FX(K))*(HY(K+1)-HY(K))
   43 CONTINUE
   41 CONTINUE
      DO 51 I=1,M
      IF(IPRINT.GT.1)WRITE(lou,11)(G(I,J),J=1,N)
      IF(DELTA.GE..000001.OR.LU.EQ.0)GO TO 51
      WRITE(LU,12)(G(I,J),J=1,N)
      KODE=0
      DO 55 J=1,N
      F(I,J)=G(I,J)
   55 CONTINUE
   51 CONTINUE
      CALL BISPL(KODE,G,F,ABN,XM,X,Y,DELTA,M,N,lou)
      IF(DELTA.LT..000001)GO TO 49
      WRITE(lou,13)
      DO 48 I=1,M
      IF(LU.NE.0)WRITE(LU,12)(F(I,J),J=1,N)
      IF(IPRINT.GT.1)WRITE(lou,12)(F(I,J),J=1,N)
   48 CONTINUE
   49 IF(IPRINT.GT.0)CALL CMAP1(FIZ,NIZ,lou)
      GO TO 100
   23 IF(LU.NE.0)REWIND LU
C
      STOP
    1 FORMAT(3I5,4F10.5)
    2 FORMAT(8F10.5)
    4 FORMAT(16F7.1)
    5 FORMAT(20A4)
    6 FORMAT(1H1,'VALUES OF THE FUNCTION AND ITS DERIVATIVES'//4X,1HX,
     /5X,1HY,8X,1HF,11X,2HFX,10X,2HFY,10X,3HFXY,9X,3HFXX,9X,3HFYY/)
    7 FORMAT(20A4//1X,'INPUT DATA'/)
    8 FORMAT(2F6.1,6F12.4)
    9 FORMAT(26I3)
   10 FORMAT(1H1,'RECTANG. NETWORK FROM VERT.LIN.INTERPOL.')
   11 FORMAT(12F10.5)
   12 FORMAT(8F10.5)
   13 FORMAT(1H1,'SMOOTHED VALUES')
   14 FORMAT(16I5)
      END
C
C     ***************************************************************
C
      SUBROUTINE CMAP1(FIZ,NIZ,lou)
C
C     PRINTER PLOT OF SMOOTHED DATA
C
      DIMENSION FIZ(10),GX(50),NVAH(115)
      COMMON/BIS/F(30,30),ABN(30,30),XM(30,30),U(30),V(30),M,N
      WRITE(lou,16)U(1),U(M)
   16 FORMAT(1H1,7X,F8.3,103X,F8.3/)
      H1=(U(M)-U(1))/114.
      H2=(V(N)-V(1))/59.
      X=0.
      L=2
      YL=V(2)
      YK=V(1)
      DO 10 J1=1,60
      IF(YK.GE.V(N))YK=V(N)
      IF(YK.LE.YL)GO TO 1
      L=L+1
      YL=V(L)
    1 HL=YL-YK
      HS=V(L)-V(L-1)
      HL1=HS-HL
      K=2
      X=U(1)
      XK=U(2)
      DO 11 I=1,115
      IF(X.LE.XK)GO TO 2
      K=K+1
      XK=U(K)
    2 HK=XK-X
      HR=U(K)-U(K-1)
      HK1=HR-HK
      YN1=0.
      YN=0.
      DO 6 J=1,N
      GX(J)=XM(K-1,J)*HK/6.*(HK*HK/HR-HR)+XM(K,J)*HK1/6.*(HK1*HK1/HR-HR)
     /      +(F(K-1,J)*HK+F(K,J)*HK1)/HR
      YN1=YN1+GX(J)*ABN(L-1,J)
    6 YN =YN +GX(J)*ABN(L,J)
      YN1=YN1*6.
      YN=YN*6.
      SF=YN1*HL/6.*(HL*HL/HS-HS)+YN*HL1/6.*(HL1*HL1/HS-HS)+(GX(L-1)*HL+
     /   GX(L)*HL1)/HS
      DO 20 IZ=1,NIZ
      IF(SF.GE.FIZ(IZ))GO TO 20
      NVAH(I)=IZ-1
      GO TO 21
   20 CONTINUE
      NVAH(I)=NIZ
      IF(NVAH(I).GE.10)NVAH(I)=9
   21 IF(NVAH(I).LT.0)NVAH(I)=0
      X=X+H1
   11 CONTINUE
      WRITE(lou,13)YK,(NVAH(I),I=1,115)
   13 FORMAT(1X,F8.3,3X,115I1)
      YK=YK+H2
   10 CONTINUE
      RETURN
      END
C
C     ******************************************************************
C
      SUBROUTINE BISPL(KODE,G,F,ABN,XM,U,V,DELTA,M,N,lou)
C
C     APPROXIMATION OF A FUNCTION GIVEN BY ITS VALUES IN A RECTANGULAR G
C
C     THE TYPE OF THE APPROXIMATION IS CONTROLED BY THE PARAMETER KODE
C     KODE=0-INTERPOLATION BY NATURAL BICUBIC SPLINES
C     KODE=1-SMOOTHING BY NATURAL BICUBIC SPLINES
C     KODE=2-INTERPOLATION BY PERIODICAL BICUBIC SPLINES
C     KODE=3-SMOOTHING BY PERIODICAL BICUBIC SPLINES
C
C     INPUT PARAMETERS
C
C     G-MATRIX OF GIVEN VALUES...M*N
C     U-HORIZONTAL GRID COORDINATES...M
C     V-VERTICAL GRID COORDINATES...N
C     DELTA-PROBABLE DEVIATION
C     M,N-DIMENSIONS OF THE GRID
C
C     OUTPUT PARAMETERS
C
C     F-MATRIX OF SMOOTHED VALUES...M*N
C     ABN-MATRIX A**(-1)*B-SEE THE ALGORITHM...N*N
C     XM-MATRIX M OF THE MOMENTS-SEE THE ALGORITHM...M*N
C
C     IF M.GT.30 OR N.GT.30, CHANGE THE DIMENSIONS ACCORDING TO THE
C     FOLLOWING PATTERN: DIMENSION G(MDIM,NDIM),F(MDIM,NDIM),DM(MDIM,MDI
C     DN(NDIM,NDIM),XM(MAXD,MAXD),ABN(MAXD,MAXD),WM(MDIM),WN(NDIM),
C     WHERE MDIM.GE.M, NDIM.GE.N,MAXD=MAX(MDIM,NDIM). FURTHER REPLACE TH
C     CARDS MDIM=30, NDIM=30 BY THOSE WITH ACTUAL VALUES OF DIMENSIONS.
C
C     ******************************************************************
      DIMENSION U(1),V(1)
C
      DIMENSION G(30,30),F(30,30),DM(30,30),DN(30,30),XM(30,30),ABN(30,
     /30),WM(30),WN(30)
      MDIM=30
      NDIM=30
      MAXD=MAX0(MDIM,NDIM)
C
      KOP=4
      M1=M
      T=1.
      IF(KODE.LT.2)GO TO 13
      M1=M-1
      T=-1.
   13 P=0.
      ITER=0
      L=0
      L3=-1
      S=FLOAT(M1*N)*DELTA*DELTA
      EPS=.0201*S
      L1=1
      DO 1 I=1,M1
      DO 1 J=1,N
    1 F(I,J)=G(I,J)
      SUM=0.
      IF(KODE.EQ.0.OR.KODE.EQ.2)GO TO 31
      DO 30 I=2,N
   30 WN(I-1)=V(I)-V(I-1)
      CALL MATW(XM,ABN,N,WN,1.,MAXD)
      CALL JACOBI(XM,N,999,1.E-07,DN,NDIM,MAXD)
      DO 33 I=1,N
   33 WN(I)=XM(I,I)
   31 DO 32 I=2,M
   32 WM(I-1)=U(I)-U(I-1)
      CALL MATW(XM,ABN,M,WM,T,MAXD)
      IF(KODE.EQ.0.OR.KODE.EQ.2)GO TO 20
      CALL JACOBI(XM,M1,999,1.E-7,DM,MDIM,MAXD)
      DO 36 I=1,M1
   36 WM(I)=XM(I,I)
    7 CALL RES(F,DM,DN,WM,WN,XM,P/36.,T,M1,N,MDIM,NDIM,MAXD)
      SUM=0.
      DO 2 I=1,M1
      DO 2 J=1,N
    2 SUM=SUM+(F(I,J)-G(I,J))**2
      IF(ITER.EQ.0.AND.SUM.LE.S)GO TO 20
      IF(ABS(SUM-S).LE.EPS)GO TO 20
      IF(ITER.GE.200)GO TO 20
      ITER=ITER+1
      IF(ITER.GT.1)GO TO 6
      P=1.
      GO TO 15
    6 LL=L
      L=1
      IF(SUM.LT.S)L=-1
      IF(LL*L.LT.0)L3=1
      GO TO (17,14),L1
   17 IF(L3.GT.0)KOP=KOP/2
      IF(KOP.NE.0)GO TO 16
      L1=2
      P=5.*P
      IF(L.LT.0)P=.1*P
      DP=.5*P
      GO TO 15
   16 P=P*10.**(L*KOP)
      GO TO 15
   14 P=P+DP*FLOAT(L)
      DP=DP*.5
      IF(DP.LT.1.E-6)GO TO 20
   15 DO 12 I=1,M1
      DO 12 J=1,N
   12 F(I,J)=G(I,J)
      GO TO 7
   20 CONTINUE
      DL=SQRT(SUM/FLOAT(M*N))
      WRITE(lou,11)ITER,P,DL
   11 FORMAT(1H0,10HITERATION ,I3,5X,2HP=,E10.3,5X,6HDELTA=,E12.5/)
      DO 8 I=1,M1
      DO 8 J=1,N
      XM(I,J)=0.
      DO 10 K=1,M1
   10 XM(I,J)=XM(I,J)+ABN(I,K)*F(K,J)
    8 XM(I,J)=6.*XM(I,J)
      DO 37 I=2,N
   37 WN(I-1)=V(I)-V(I-1)
      IF(MDIM.NE.MAXD)GO TO 4
      CALL MATW(DM,ABN,N,WN,0.,MAXD)
      GO TO 9
    4 CALL MATW(DN,ABN,N,WN,0.,MAXD)
    9 IF(M1.EQ.M)RETURN
      DO 5 J=1,N
      DO 3 I1=1,M1
      I=M-I1
      XM(I+1,J)=XM(I,J)
    3 F(I+1,J)=F(I,J)
      XM(1,J)=XM(M,J)
    5 F(1,J)=F(M,J)
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE JACOBI(T,N,ITMAX,EPS,R,NDIM,MAXD)
      DOUBLE PRECISION LAMBDA,MI,NY,SN,CS,TPJ,TQJ,RIP,RIQ,
     /SIGMA1,SIGMA2,CHYBA
      INTEGER P,Q
      DIMENSION R(NDIM,NDIM),T(MAXD,MAXD)
      SIGMA1=0.D0
      SIGMA2=0.D0
      EPS2=EPS*EPS
      DO 1 I=1,N
      DO 2 J=1,N
    2 R(I,J)=0.
      R(I,I)=1.
      TPJ=T(I,I)
    1 SIGMA2=SIGMA2+TPJ*TPJ
      IND=2
      DO 10 ITER=1,ITMAX
      DO 12 P=IND,N
      Q=P-IND+1
      IF(ABS(T(P,Q)).LT.EPS2)GO TO 12
      TPJ=T(P,Q)
      SIGMA2=SIGMA2+2.D0*TPJ*TPJ
      LAMBDA=-T(P,Q)
      MI=.5*(T(P,P)-T(Q,Q))
      NY=DSQRT(LAMBDA*LAMBDA+MI*MI)
      CS=DSQRT((NY+DABS(MI))/2.D0/NY)
      SN=CS*LAMBDA/DABS(LAMBDA)
      IF(MI.NE.0.D0)SN=MI/DABS(MI)*LAMBDA*.5D0/NY/CS
      DO 5 J=1,N
      TPJ=T(P,J)
      TQJ=T(Q,J)
      T(P,J)=TPJ*CS-TQJ*SN
    5 T(Q,J)=TPJ*SN+TQJ*CS
      TPJ=T(P,P)
      TQJ=-T(P,Q)
      T(P,P)=TPJ*CS+TQJ*SN
      TPJ=T(Q,Q)
      TQJ=T(Q,P)
      T(Q,Q)=TPJ*CS+TQJ*SN
      T(P,Q)=0.
      DO 6 I=1,N
      T(I,P)=T(P,I)
    6 T(I,Q)=T(Q,I)
      DO 7 I=1,N
      RIP=R(I,P)
      RIQ=R(I,Q)
      R(I,P)=RIP*CS-RIQ*SN
    7 R(I,Q)=RIP*SN+RIQ*CS
   12 CONTINUE
      TMAX=0.
      DO 3 I=2,N
      IM1=I-1
      DO 3 J=1,IM1
      TABS= ABS(T(I,J))
      IF(TABS.LE.TMAX)GOTO3
      TMAX=TABS
      P=I
      Q=J
    3 CONTINUE
      TPJ=T(P,Q)
      CHYBA=  1.D0-SIGMA2/(SIGMA2+2.D0*TPJ*TPJ)
      TPJ=EPS
      IF(CHYBA.LT.TPJ)GOTO11
      IND=P-Q+1
      ITM=ITER
   10 CONTINUE
   11 RETURN
      END
C
C     **************************************************************
C
      SUBROUTINE RES(F,Q,QV,W,WV,X,SIGMA,T,N,NV,MDIM,NDIM,MAXD)
      DIMENSION F(MDIM,NDIM),Q(MDIM,MDIM),QV(NDIM,NDIM),W(1),
     /WV(1),X (MAXD,MAXD)
      LB=1
      IF(SIGMA.EQ.0.)LB=2
      DO 10 I=1,N
      DO 10 J=1,NV
      X(I,J)=0.
      DO 10 L=1,NV
   10 X(I,J)=X(I,J)+F(I,L)*QV(L,J)
      DO 1 I=1,N
      DO 1 J=1,NV
      F(I,J)=0.
      POM=SIGMA/(W(I)*WV(J)+SIGMA)
      GO TO (5,6),LB
    6 POM=1.
      IF(J.EQ.1.OR.J.EQ.NV)GO TO 5
      IF(T.LT.0.)GO TO 1
      IF(I.NE.1.AND.I.NE.N)GO TO 1
    5 DO 2 L=1,N
    2 F(I,J)=F(I,J)+Q(L,I)*X(L,J)
      F(I,J)=F(I,J)*POM
    1 CONTINUE
      DO 3 I=1,N
      DO 3 J=1,NV
      X(I,J)=0.
      DO 3 L=1,N
    3 X(I,J)=X(I,J)+Q(I,L)*F(L,J)
      DO 4 I=1,N
      DO 4 J=1,NV
      F(I,J)=0.
      DO 4 L=1,NV
    4 F(I,J)=F(I,J)+X(I,L)*QV(J,L)
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE MATW(W,AB,MP1,H,T,MX)
      DIMENSION W(MX,MX),AB(MX,MX),H(1)
      M=MP1-1
      MM1=M-1
      IF(T.LT.0.)GO TO 3
      CALL AINV(H,W,MM1,MX)
      DO 1 J=1,MP1
      AB(1,J)=0.
    1 AB(MP1,J)=0.
      DO 2 I=2,M
      AB(I,1)=W(I-1,1)/H(1)
      AB(I,2)=(-W(I-1,1))*(1./H(1)+1./H(2))+W(I-1,2)/H(2)
      AB(I,M)=W(I-1,M-2)/H(M-1)-W(I-1,MM1)*(1./H(MM1)+1./H(M))
      AB(I,MP1)=W(I-1,MM1)/H(M)
      IF(MM1.LT.3)GO TO 7
      DO 2 L=3,MM1
    2 AB(I,L)=W(I-1,L-2)/H(L-1)-W(I-1,L-1)*(1./H(L-1)+1./H(L))+
     /W(I-1,L)/H(L)
    7 IF(T.EQ.0)RETURN
      DO 6 J=1,MP1
      W(1,J)=AB(2,J)/H(1)
      W(MP1,J)=AB(M,J)/H(M)
      DO 6 I=2,M
    6 W(I,J)=AB(I-1,J)/H(I-1)-AB(I,J)*(1./H(I-1)+1./H(I))+AB(I+1,J)/H(I)
      RETURN
    3 CALL AINVP(H,W,M,AB,MX)
      DO 4 I=1,M
      AB(I,1)=W(I,M)/H(1)-(1./H(1)+1./H(2))*W(I,1)+W(I,2)/H(2)
      AB(I,M)=W(I,MM1)/H(M)-(1./H(M)+1./H(1))*W(I,M)+W(I,1)/H(1)
      DO 4 J=2,MM1
    4 AB(I,J)=W(I,J-1)/H(J)-(1./H(J)+1./H(J+1))*W(I,J)+W(I,J+1)/H(J+1)
      DO 5 J=1,M
      W(1,J)=AB(M,J)/H(1)-(1./H(1)+1./H(2))*AB(1,J)+AB(2,J)/H(2)
      W(M,J)=AB(MM1,J)/H(M)-(1./H(M)+1./H(1))*AB(M,J)+AB(1,J)/H(1)
      DO 5 I=2,MM1
    5 W(I,J)=AB(I-1,J)/H(I)-(1./H(I)+1./H(I+1))*AB(I,J)+AB(I+1,J)/H(I+1)
      RETURN
      END
C
C     *************************************************************
C
      SUBROUTINE AINV(H,X,M,MX)
      DIMENSION X(MX,MX),H(1),Q(30),P(30),U(30)
      P(1)=2.*(H(1)+H(2))
      DO 1 K=2,M
      Q(K-1)=(-H(K)/P(K-1))
    1 P(K)=H(K)*Q(K-1)+2.*(H(K)+H(K+1))
      Q(M)=(-H(M)/P(M))
      U(1)=1./P(1)
      DO 2 L=1,M
      DO 3 K=2,M
      D=0.
      IF(K.EQ.L)D=1.
    3 U(K)=(D-H(K)*U(K-1))/P(K)
      ML=M-L
      X(M,L)=U(M)
      X(L,M)=U(M)
      IF(M.EQ.L)GO TO 5
      DO 4 K=1,ML
      J=M-K
      X(J,L)=Q(J)*X(J+1,L)+U(J)
    4 X(L,J)=X(J,L)
    2 U(1)=0.
    5 RETURN
      END
C
C     ******************************************************
C
      SUBROUTINE AINVP(H,X,M,P,MX)
      DIMENSION X(MX,MX),P(MX,MX),H(1)
      M1=M-1
      P(1,1)=2.*(H(1)+H(2))
      P(3,1)=-H(1)/P(1,1)
      P(2,1)=-H(2)/P(1,1)
      DO 1 K=2,M1
      P(1,K)=H(K)*P(2,K-1)+2.*(H(K)+H(K+1))
      P(2,K)=-H(K+1)/P(1,K)
    1 P(3,K)=-H(K)*P(3,K-1)/P(1,K)
      P(1,M)=H(M)*P(2,M-1)+2.*(H(M)+H(1))
      P(2,M)=-H(1)/P(1,M)
      P(3,M)=-H(M)*P(3,M-1)/P(1,M)
      P(5,M)=1.
      DO 2 J=1,M1
      K=M-J
    2 P(5,K)=P(2,K)*P(5,K+1)+P(3,K)
      POM=H(1)*P(5,1)+H(M)*P(5,M1)+2.*(H(M)+H(1))
      P(4,1)=1./P(1,1)
      DO 3 L=1,M
      DO 4 K=2,M
      D=0.
      IF(L.EQ.K)D=1.
    4 P(4,K)=(D-H(K)*P(4,K-1))/P(1,K)
      P(6,M)=0.
      DO 5 J=1,M1
      K=M-J
    5 P(6,K)=P(2,K)*P(6,K+1)+P(4,K)
      IF(L.EQ.M)GO TO 7
      X(M,L)=(-H(1)*P(6,1)-H(M)*P(6,M1))/POM
      X(L,M)=X(M,L)
      DO 6 K=L,M1
      X(K,L)=P(5,K)*X(M,L)+P(6,K)
    6 X(L,K)=X(K,L)
      P(4,1)=0.
    3 CONTINUE
    7 X(M,M)=(1.-H(1)*P(6,1)-H(M)*P(6,M1))/POM
      RETURN
      END
C
C     *****************************************************************
C
