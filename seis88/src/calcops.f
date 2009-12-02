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


C CalComp-PostScript interface
C
C This file contains the CalComp plotting routines
C     PLOTS, PLOT, NEWPEN, SYMBOL and NUMBER
C supplemented with additional area filling routine
C     FILL
C coded in the ANSI X3.9-1978 FORTRAN77 standard full language.
C The graphical output is generated in the form of ASCII files coded in
C the PostScript Level 2 Language - Encapsulated PostScript File Format
C Version 3.0.
C
C Whereas the original CalComp routines are conformable to the ANSI
C X3.10-1966 FORTRAN standard, the dummy argument text of the subroutine
C SYMBOL is declared here as
C     CHARACTER*(*) TEXT
C in order to conform to the ANSI X3.9-1978 FORTRAN77 standard.  In this
C way, the subroutine SYMBOL is not conformal to the original CalComp
C specification.
C
C Possible input file - CalComp colour table file 'calcops.rgb':
C     The file is taken into account if exists in the current directory.
C     If there is no 'calcops.rgb' file, default colours are used.
C     The file is read by list-directed (free format) input, and
C     consists of lines defining individual colours.  Each line contains
C     four numbers:
C K,R,G,B
C     K...    Index of the colour to be defined.  Non-negative integer.
C             Colours greater than MCOLOR (see block data PLOTB below)
C             are not taken into account.
C     R...    Content of the red colour.  Real between 0 and 1.
C     G...    Content of the green colour.  Real between 0 and 1.
C     B...    Content of the blue colour.  Real between 0 and 1.
C
C OUTPUT PostScript files 'plot00.ps', 'plot01.ps', ..., 'plot99.ps':
C     Each invocation of subroutine PLOTS creates new output PostScript
C     file named 'plot**.ps', where ** is the smallest one of values
C     00, 01, 02, ..., 99, such that the corresponding file has not been
C     present in the current directory.
C
C Declaration of the common memory storage locations:
C     ------------------------------------------------------------------
      BLOCK DATA PLOTB
      INCLUDE 'calcops.inc'
      END
C     ------------------------------------------------------------------
C
C Date: 1996, March 13
C Coded by Ludek Klimes
C
C=======================================================================
C
      SUBROUTINE PLOTS(I1,I2,I3)
      INTEGER I1,I2,I3
C
C Input:
C     I1,I2,I3... Dummy parameters - ignored.
C No output.
C
C Common block /PLOTC/:
      INCLUDE 'calcops.inc'
C
C No subroutines and external functions required.
C
C Date: 1996, September 30
C Coded by Ludek Klimes
C
C-----------------------------------------------------------------------
C
C     Auxiliary storage locations:
      CHARACTER*9 FILEPS
      INTEGER IERR,K,I
      REAL RK,GK,BK
C
C.......................................................................
C
C     Colours:
C     Default white:
      R(0)=1.
      G(0)=1.
      B(0)=1.
C     Default blacks:
      DO 10 K=1,MCOLOR
        R(K)=0.
        G(K)=0.
        B(K)=0.
   10 CONTINUE
C     Other colours:
      OPEN(LUCFG,FILE='CALCOPS.RGB',STATUS='OLD',IOSTAT=IERR)
      IF(IERR.EQ.0) THEN
C       User-defined colours:
   11   CONTINUE
          K=-999
          READ(LUCFG,*,END=19) K,RK,GK,BK
          IF(K.LT.0) THEN
            GO TO 19
          ELSE IF(K.LE.MCOLOR) THEN
            R(K)=RK
            G(K)=GK
            B(K)=BK
          END IF
        GO TO 11
   19   CONTINUE
        CLOSE(LUCFG)
      ELSE
C       Default colours:
        R( 2)=1.00
        G( 2)=0.00
        B( 2)=0.00
        R( 3)=0.00
        G( 3)=0.90
        B( 3)=0.00
        R( 4)=0.00
        G( 4)=0.00
        B( 4)=1.00
        R( 5)=1.00
        G( 5)=0.90
        B( 5)=0.00
        R( 6)=0.00
        G( 6)=0.80
        B( 6)=0.90
        R( 7)=0.90
        G( 7)=0.00
        B( 7)=0.90
        R( 8)=0.90
        G( 8)=0.63
        B( 8)=0.50
        R( 9)=0.63
        G( 9)=0.63
        B( 9)=0.63
        R(10)=0.95
        G(10)=0.00
        B(10)=0.71
        R(11)=0.71
        G(11)=0.85
        B(11)=0.00
        R(12)=0.00
        G(12)=0.63
        B(12)=0.95
        R(13)=0.95
        G(13)=0.63
        B(13)=0.00
        R(14)=0.00
        G(14)=0.85
        B(14)=0.71
        R(15)=0.71
        G(15)=0.00
        B(15)=0.95
      END IF
C
C     Opening output PostScript file:
      FILEPS='plot00.ps'
      DO 28 I=0,99
        WRITE(FILEPS(5:6),'(2I1)') I/10,I-I/10*10
        OPEN(LUCFG,FILE=FILEPS,STATUS='NEW',ERR=21)
          GO TO 29
   21   CONTINUE
   28 CONTINUE
      PAUSE 'Error: Unable to open output PostScript file plot**.ps'
   29 CONTINUE
C
C     Writing prolog containing definitions of procedures to be used:
      WRITE(LUCFG,'(A)')
     * '%!PS-Adobe-3.0 EPSF-3.0'
     *,'%%BoundingBox: (atend)'
     *,'%%Creator: CALCOPS'
     *,'%%EndComments'
     *,'%%BeginProlog'
     *,'%'
     *,'% General definitions:'
     *,'/C {setrgbcolor} bind def'
     *,'/M {stroke moveto} bind def'
     *,'/L {lineto} bind def'
     *,'/S {stroke} bind def'
     *,'/F {/H exch def'
     *,'    /Helvetica findfont exch scalefont setfont} bind def'
C     Character spacing is increased by: A=H-stringwidth/N
C     The whole string is shifted by:    A/2-0.15*H
     *,'/T {dup rotate exch dup stringwidth pop 4 -1 roll div H sub neg'
     *,'    dup 2 div 0.15 H mul sub dup 0 rmoveto 3 1 roll'
     *,'    exch 0 exch ashow neg 0 rmoveto neg rotate} bind def'
     *,'%'
     *,'% Centred symbols:'
     *,'/Tb {/H0 exch def /H1 H0 2 div def /H2 H1 2 div def'
     *,'   /h0 H0 neg def /h1 H1 neg def /h2 H2 neg def dup rotate} def'
     *,'/Te {neg rotate} def /BD {bind def} def /R  {rlineto}  def'
     *,'/T00 {Tb  0 H1 R h1  0 R  0 h0 R H0  0 R  0 H0 R h1  0 R       '
     *,'                                                  0 h1 R Te} BD'
     *,'/T01 {Tb  0 H1 R h2  0 R h2 h2 R  0 h1 R H2 h2 R H1  0 R       '
     *,'                 H2 H2 R  0 H1 R h2 H2 R h2  0 R  0 h1 R Te} BD'
     *,'/T02 {Tb  0 H1 R h1 h1 h2 add  R H0  0 R h1 H1 H2 add  R       '
     *,'                                                  0 h1 R Te} BD'
     *,'/T03 {Tb  0 H1 R  0 h0 R  0 H1 R h1  0 R H0  0 R h1  0 R Te} BD'
     *,'/T04 {Tb h1 H1 R H0 h0 R h1 H1 R H1 H1 R h0 h0 R H1 H1 R Te} BD'
     *,'/T05 {Tb  0 H1 R h1 h1 R H1 h1 R H1 H1 R h1 H1 R  0 h1 R Te} BD'
     *,'/T06 {Tb  0 H1 R h1 h1 R H0  0 R h1 H1 R  0 h0 R  0 H1 R Te} BD'
     *,'/T07 {Tb H1 h1 R h0 H0 R H0  0 R h0 h0 R H1 H1 R         Te} BD'
     *,'/T08 {Tb h2  0 R H1  0 R h2  0 R h1 h1 R H0  0 R h0  0 R       '
     *,'                         H0 H0 R h0  0 R H0  0 R h1 h1 R Te} BD'
     *,'/T09 {Tb h1 H1 R H1 h1 R H1 H1 R h1 h1 R  0 h1 R  0 H1 R Te} BD'
     *,'/T10 {Tb H1 H1 R h2 h2 R h1  0 R h2 H2 R H2 h2 R  0 h1 R       '
     *,' h2 h2 R H2 H2 R H1  0 R H2 h2 R h2 H2 R  0 H1 R h2 h2 R Te} BD'
     *,'/T11 {Tb h1 H1 R H0 h0 R h1 H1 R H1 H1 R h0 h0 R H1 H1 R       '
     *,'          0 H1 R  0 h0 R  0 H1 R H1  0 R h0  0 R H1  0 R Te} BD'
     *,'/T12 {Tb H1 h1 R h0  0 R H0 H0 R h0  0 R H1 h1 R         Te} BD'
     *,'/T13 {Tb  0 H1 R  0 h0 R  0 H1 R                         Te} BD'
     *,'%%EndProlog'
     *,'%'
     *,'%%BeginSetup'
     *,'%595 0 translate  90 rotate % line for landscape'
     *,' 0.0 0.0 translate  1.00 dup scale'
     *,' 1.0 setlinewidth 1 setlinecap 1 setlinejoin'
     *,'%%EndSetup'
     *,'%'
C
C     CalComp plotting initialisation:
      STARTX=0.
      STARTY=0.
      XOLD=0.
      YOLD=0.
      HOLD=0.
      KOLOR=0
      B1=999.9
      B2=999.9
      B3=-99.9
      B4=-99.9
      CALL NEWPEN(1)
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE PLOT(XPAGE,YPAGE,IPEN)
      REAL    XPAGE,YPAGE
      INTEGER IPEN
C
C Input:
C     XPAGE,YPAGE... Coordinates of a point, in centimetres from the
C             current reference point (origin), of the position to which
C             the pen is to be moved.
C     IPEN... A signed integer which controls pen status (up or down)
C             and the origin definition:
C             IPEN=2... The pen is down during movement, thus drawing a
C               visible line.
C             IPEN=3... The pen is up during movement.
C             IPEN=-2 OR -3... A new origin is defined at the terminal
C               position after the movement is completed as if IPEN were
C               positive.
C             IPEN=999... Output device is closed.
C No output.
C
C Common block /PLOTC/:
      INCLUDE 'calcops.inc'
C
C No subroutines and external functions required.
C
C Date: 1996, September 30
C Coded by Ludek Klimes
C
C-----------------------------------------------------------------------
C
C     Auxiliary storage locations:
      REAL X,Y,XO,YO
C
C.......................................................................
C
C     Plotting the line:
      IF(IABS(IPEN).EQ.2) THEN
        X=SCALE*(STARTX+XPAGE)
        Y=SCALE*(STARTY+YPAGE)
        XO=SCALE*(STARTX+XOLD)
        YO=SCALE*(STARTY+YOLD)
        IF(-99.95.LT.X.AND.X.LT.999.95.AND.
     *     -99.95.LT.Y.AND.Y.LT.999.95) THEN
          WRITE(LUCFG,'(2(F5.1,1X),A)') X,Y,'L'
        ELSE
          WRITE(LUCFG,'(2(I5,1X),A)') NINT(X),NINT(Y),'L'
        END IF
        B1=AMIN1(B1,XO,X)
        B2=AMIN1(B2,YO,Y)
        B3=AMAX1(B3,XO,X)
        B4=AMAX1(B4,YO,Y)
      END IF
      IF(IPEN.NE.2) THEN
        X=SCALE*(STARTX+XPAGE)
        Y=SCALE*(STARTY+YPAGE)
        IF(-99.95.LT.X.AND.X.LT.999.95.AND.
     *     -99.95.LT.Y.AND.Y.LT.999.95) THEN
          WRITE(LUCFG,'(2(F5.1,1X),A)') X,Y,'M'
        ELSE
          WRITE(LUCFG,'(2(I5,1X),A)') NINT(X),NINT(Y),'M'
        END IF
      END IF
C
C     Moving the origin:
      IF(IPEN.GE.0) THEN
        XOLD=XPAGE
        YOLD=YPAGE
      ELSE
        STARTX=STARTX+XPAGE
        STARTY=STARTY+YPAGE
        XOLD=0.
        YOLD=0.
      END IF
C
C     Closing CalComp:
      IF(IPEN.GE.999) THEN
        WRITE(LUCFG,'(A)') 'S'
        WRITE(LUCFG,'(A)') '%%Trailer'
        IF(-99.95.LT.B1.AND.B1.LT.999.95.AND.
     *     -99.95.LT.B2.AND.B2.LT.999.95.AND.
     *     -99.95.LT.B3.AND.B3.LT.999.95.AND.
     *     -99.95.LT.B4.AND.B4.LT.999.95) THEN
          WRITE(LUCFG,'(A,4I4)')
     *              '%%BoundingBox:',NINT(B1),NINT(B2),NINT(B3),NINT(B4)
        ELSE
          WRITE(LUCFG,'(A,4I6)')
     *              '%%BoundingBox:',NINT(B1),NINT(B2),NINT(B3),NINT(B4)
        END IF
        WRITE(LUCFG,'(A)') 'showpage'
        WRITE(LUCFG,'(A)') '%%EOF'
C       WRITE(LUCFG,'(A)') CHAR(4)
        CLOSE(LUCFG)
      END IF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE NEWPEN(INP)
      INTEGER INP
C
C Input:
C     INP...  Number of the pen or colour index to be selected.
C No output.
C
C Common block /PLOTC/:
      INCLUDE 'calcops.inc'
C
C No subroutines and external functions required.
C
C Date: 1995, May 20
C Coded by Ludek Klimes
C
C-----------------------------------------------------------------------
C
      IF(INP.NE.KOLOR) THEN
        IF(0.LE.INP.AND.INP.LE.MCOLOR) THEN
          KOLOR=INP
        ELSE
          KOLOR=MCOLOR
        END IF
        WRITE(LUCFG,'(A,3(F4.2,1X),A)')
     *                               'S ',R(KOLOR),G(KOLOR),B(KOLOR),'C'
      END IF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SYMBOL(XPAGE,YPAGE,HEIGHT,TEXT,ANGLE,NCHAR)
      REAL    XPAGE,YPAGE,HEIGHT,ANGLE
      CHARACTER TEXT*(*)
      INTEGER NCHAR
C
C Input:
C     XPAGE,YPAGE... Coordinates, in centimetres, of the lower left-hand
C             corner of the first character to be produced.
C             Continuation occurs when XPAGE and YPAGE equals 999.
C     HEIGHT..Height, in centimetres, of the characters to be plotted.
C             The character width, including spacing, is normally the
C             same as the height.
C     TEXT... String containing the text to be plotted.
C     ANGLE...Angle, in degrees anticlockwise from the X-axis, at which
C             the text is to be plotted.
C     NCHAR...NCHAR.GT.0: number of characters to be drawn.
C             NCHAR.EQ.0: one character is to be drawn
C             NCHAR.LT.0: to plot a centred symbol no. ICHAR(TEXT(1:1)).
C               NCHAR.EQ.-1: the pen is up during the move.
C               NCHAR.EQ.-2: the pen is down during the move.
C No output.
C
C Common block /PLOTC/:
      INCLUDE 'calcops.inc'
C
C No subroutines and external functions required.
C
C Date: 1996, September 30
C Coded by Ludek Klimes
C
C-----------------------------------------------------------------------
C
C     Auxiliary storage locations:
      INTEGER I
      REAL X,Y,SX,SY,UX,UY,VX,VY,WX,WY
C
C     X,Y...  Coordinates.
C     SX,SY.. Scaled coordinates.
C     UX,UY...Text path vector.
C     VX,VY...Scaled text path vector.
C
C.......................................................................
C
      IF(HEIGHT.NE.HOLD) THEN
        IF(-99.95.LT.1.37*SCALE*HEIGHT.AND.
     *               1.37*SCALE*HEIGHT.LT.999.95) THEN
          WRITE(LUCFG,'(2(F5.1,1X),A)')
     *                                1.37*SCALE*HEIGHT,SCALE*HEIGHT,'F'
        ELSE
          WRITE(LUCFG,'(2(I5,1X),A)')
     *                    NINT(1.37*SCALE*HEIGHT),NINT(SCALE*HEIGHT),'F'
        END IF
        HOLD=HEIGHT
      END IF
C
      X=XPAGE
      Y=YPAGE
      IF(ABS(X).GT.998.) THEN
        X=XOLD
        Y=YOLD
      END IF
C
      UX= HEIGHT*COS(.0174533*ANGLE)
      UY= HEIGHT*SIN(.0174533*ANGLE)
      SX=SCALE*(STARTX+X)
      SY=SCALE*(STARTY+Y)
      IF(NCHAR.GE.0) THEN
C       Standard call - text:
        DO 1 I=MAX0(NCHAR,1),1,-1
          IF(TEXT(I:I).NE.' ') THEN
            GO TO 2
          END IF
    1   CONTINUE
        I=1
    2   CONTINUE
        IF(-99.95.LT.SX.AND.SX.LT.999.95.AND.
     *     -99.95.LT.SY.AND.SY.LT.999.95) THEN
          WRITE(LUCFG,'(2(F5.1,1X),A,I3,3A,I4,A)')
     *                 SX,SY,'M ',I,' (',TEXT(1:I),') ',NINT(ANGLE),' T'
        ELSE
          WRITE(LUCFG,'(2(I5,1X),A,I3,3A,I4,A)')
     *     NINT(SX),NINT(SY),'M ',I,' (',TEXT(1:I),') ',NINT(ANGLE),' T'
        END IF
        VX= SCALE*UX
        VY= SCALE*UY
        WX= VX*FLOAT(I)
        WY= VY*FLOAT(I)
        SX=SX-0.15*VX
        SY=SY-0.15*VY
        B1=AMIN1(B1,SX,SX+WX,SX-VY,SX+WX-VY)
        B2=AMIN1(B2,SY,SY+WY,SY+VX,SY+WY+VX)
        B3=AMAX1(B3,SX,SX+WX,SX-VY,SX+WX-VY)
        B4=AMAX1(B4,SY,SY+WY,SY+VX,SY+WY+VX)
        X=X+UX*FLOAT(NCHAR)
        Y=Y+UY*FLOAT(NCHAR)
      ELSE
C       Special call - centred symbol:
        VX= SCALE*UX/2.
        VY= SCALE*UY/2.
        B1=AMIN1(B1,SX+VX+VY,SX-VX+VY,SX+VX-VY,SX-VX-VY)
        B2=AMIN1(B2,SY+VY+VX,SY-VY+VX,SY+VY-VX,SY-VY-VX)
        B3=AMAX1(B3,SX+VX+VY,SX-VX+VY,SX+VX-VY,SX-VX-VY)
        B4=AMAX1(B4,SY+VY+VX,SY-VY+VX,SY+VY-VX,SY-VY-VX)
        I=MIN0(ICHAR(TEXT(1:1)),13)
        IF(NCHAR.EQ.-2) THEN
          IF(-99.95.LT.SX.AND.SX.LT.999.95.AND.
     *       -99.95.LT.SY.AND.SY.LT.999.95.AND.
     *       -99.95.LT.SCALE*HEIGHT.AND.SCALE*HEIGHT.LT.999.95) THEN
            WRITE(LUCFG,'(2(F5.1,1X),A,I4,1X,F5.1,A,I2.2)')
     *                        SX,SY,'L ',NINT(ANGLE),SCALE*HEIGHT,' T',I
          ELSE
            WRITE(LUCFG,'(2(I5,1X),A,I4,1X,I5,A,I2.2)')
     *      NINT(SX),NINT(SY),'L ',NINT(ANGLE),NINT(SCALE*HEIGHT),' T',I
          END IF
          SX=SCALE*(STARTX+XOLD)
          SY=SCALE*(STARTY+YOLD)
          B1=AMIN1(B1,SX)
          B2=AMIN1(B2,SY)
          B3=AMAX1(B3,SX)
          B4=AMAX1(B4,SY)
        ELSE
          IF(-99.95.LT.SX.AND.SX.LT.999.95.AND.
     *       -99.95.LT.SY.AND.SY.LT.999.95.AND.
     *       -99.95.LT.SCALE*HEIGHT.AND.SCALE*HEIGHT.LT.999.95) THEN
            WRITE(LUCFG,'(2(F5.1,1X),A,I4,F5.1,A,I2.2)')
     *                        SX,SY,'M ',NINT(ANGLE),SCALE*HEIGHT,' T',I
          ELSE
            WRITE(LUCFG,'(2(I5,1X),A,I4,1X,I5,A,I2.2)')
     *      NINT(SX),NINT(SY),'M ',NINT(ANGLE),NINT(SCALE*HEIGHT),' T',I
          END IF
        END IF
      END IF
      XOLD=X
      YOLD=Y
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE NUMBER (XPAGE,YPAGE,HEIGHT,FPN,ANGLE,NDEC)
      REAL XPAGE,YPAGE,HEIGHT,FPN,ANGLE
      INTEGER NDEC
C
C Input:
C     XPAGE,YPAGE... Coordinates, in centimetres, of the lower left-hand
C             corner of the first character to be produced.
C             Continuation occurs when XPAGE and YPAGE equals 999.
C     HEIGHT..Height, in centimetres, of the characters to be plotted.
C             The character width, including spacing, is normally the
C             same as the height.
C     FPN...  Floating point number to be plotted.
C     ANGLE...Angle, in degrees anticlockwise from the X-axis, at which
C             the number is to be plotted.
C     NDEC... Controls the precision of the conversion of the number
C             FPN.
C             NDEC.GE.0: number of decimal places to be drawn, after
C               rounding.
C             NDEC.EQ.-1: only the integer portion is to be plotted,
C               after rounding.
C             NDEC.LE.-2: -NDEC-1 digits are truncated from the integer
C               portion, after rounding.
C             The magnitude of NDEC should not exceed 9.
C No output.
C
C No subroutines and external functions required.
C
C Date: 1993, December 18
C Coded by Ludek Klimes
C
C-----------------------------------------------------------------------
C
C     Auxiliary storage locations:
      INTEGER N,ILP,I,J,K
      REAL X,Y,FPV,SAMEV
      PARAMETER (SAMEV=999.)
C
C     N...    Storage for (possibly modified) NDEC.
C     ILP...  Length of the integer part of the given number.
C     I...    Temporary storage.
C     J...    Loop variable.
C     K...    Digit to plot.
C     X,Y...  Coordinates.
C     FPV...  Storage for FPN and its decimal modules.
C
C.......................................................................
C
      X=XPAGE
      Y=YPAGE
      FPV=FPN
      N=MIN0(MAX0(-9,NDEC),9)
C
C     Minus sign:
      IF (FPV.LT.0) THEN
        CALL SYMBOL (X,Y,HEIGHT,'-',ANGLE,1)
        X=SAMEV
        Y=SAMEV
      END IF
C
C     To guarantee a correct rounding:
      IF (N.GE.0) THEN
        FPV=ABS(FPV)+(0.5*0.1**N)
      ELSE
        FPV=ABS(FPV)+(0.05*0.1**N)
      END IF
C
C     Integer part of the given number:
      I=INT(ALOG10(FPV)+1.0)
      IF(N.GE.-1) THEN
        ILP=I
      ELSE
        ILP=I+N+1
      END IF
      IF (ILP.LE.0) THEN
        CALL SYMBOL (X,Y,HEIGHT,'0',ANGLE,1)
        X=SAMEV
        Y=SAMEV
      ELSE
        DO 60 J=1,ILP
          K=FPV*10.**(J-I)
          CALL SYMBOL (X,Y,HEIGHT,CHAR(ICHAR('0')+K),ANGLE,1)
          FPV=FPV-(FLOAT(K)*10.**(I-J))
          X=SAMEV
          Y=SAMEV
   60   CONTINUE
      END IF
C
C     Decimal places:
      IF(N.GE.0) THEN
        CALL SYMBOL (X,Y,HEIGHT,'.',ANGLE,1)
        DO 70 J=1,N
          K=FPV*10.
          CALL SYMBOL(X,Y,HEIGHT,CHAR(ICHAR('0')+K),ANGLE,1)
          FPV=FPV*10.-FLOAT(K)
   70   CONTINUE
      END IF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE FILL(XPTS,YPTS,NPTS)
      INTEGER NPTS
      REAL    XPTS(NPTS),YPTS(NPTS)
C
C Subroutine to fill the area inside a given polygon with the colour
C specified by the last invocation of subroutine NEWPEN.
C
C Input:
C     XPTS,YPTS... Coordinates of vertices of the polygon to be filled
C             with the current colour specified by subroutine NEWPEN.
C     NPTS... Number of vertices of the polygon
C             and the origin definition:
C No output.
C
C Common block /PLOTC/:
      INCLUDE 'calcops.inc'
C
C No subroutines and external functions required.
C
C Date: 1996, September 30
C Coded by Ludek Klimes
C
C-----------------------------------------------------------------------
C
C     Auxiliary storage locations:
      INTEGER I
      REAL X,Y
C
C.......................................................................
C
      DO 10 I=1,NPTS
        X=SCALE*(STARTX+XPTS(I))
        Y=SCALE*(STARTY+YPTS(I))
        IF(I.EQ.1) THEN
          IF(-99.95.LT.X.AND.X.LT.999.95.AND.
     *       -99.95.LT.Y.AND.Y.LT.999.95) THEN
            WRITE(LUCFG,'(2(F5.1,1X),A)') X,Y,'M'
          ELSE
            WRITE(LUCFG,'(2(I5,1X),A)') NINT(X),NINT(Y),'M'
          END IF
        ELSE IF(I.LT.NPTS) THEN
          IF(-99.95.LT.X.AND.X.LT.999.95.AND.
     *       -99.95.LT.Y.AND.Y.LT.999.95) THEN
            WRITE(LUCFG,'(2(F5.1,1X),A)') X,Y,'L'
          ELSE
            WRITE(LUCFG,'(2(I5,1X),A)') NINT(X),NINT(Y),'L'
          END IF
        ELSE
          IF(-99.95.LT.X.AND.X.LT.999.95.AND.
     *       -99.95.LT.Y.AND.Y.LT.999.95) THEN
            WRITE(LUCFG,'(2(F5.1,1X),A)') X,Y,'L closepath fill'
          ELSE
            WRITE(LUCFG,'(2(I5,1X),A)') NINT(X),NINT(Y),
     *                                                'L closepath fill'
          END IF
        END IF
        B1=AMIN1(B1,X)
        B2=AMIN1(B2,Y)
        B3=AMAX1(B3,X)
        B4=AMAX1(B4,Y)
   10 CONTINUE
      RETURN
      END
C
C=======================================================================
C
