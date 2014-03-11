C  Copyright (c) 2003-2010 University of Florida
C
C  This program is free software; you can redistribute it and/or modify
C  it under the terms of the GNU General Public License as published by
C  the Free Software Foundation; either version 2 of the License, or
C  (at your option) any later version.

C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C  GNU General Public License for more details.

C  The GNU General Public License is included in this distribution
C  in the file COPYRIGHT.
      SUBROUTINE OUTPUT (MATRIX,ROWLOW,ROWHI,COLLOW,COLHI,ROWDIM,COLDIM,
     $                   NCTL)
C
C.......................................................................
C
C
C OUTPUT PRINTS A REAL*8 MATRIX IN FORMATTED FORM WITH NUMBERED ROWS
C
C AND COLUMNS.  THE INPUT IS AS FOLLOWS;
C
C        MATRIX(*,*).........MATRIX TO BE OUTPUT
C
C        ROWLOW..............ROW NUMBER AT WHICH OUTPUT IS TO BEGIN
C
C        ROWHI...............ROW NUMBER AT WHICH OUTPUT IS TO END
C
C        COLLOW..............COLUMN NUMBER AT WHICH OUTPUT IS TO BEGIN
C
C        COLHI...............COLUMN NUMBER AT WHICH OUTPUT IS TO END
C
C        ROWDIM..............ROW DIMENSION OF MATRIX(*,*)
C
C        COLDIM..............COLUMN DIMENSION OF MATRIX(*,*)
C
C        NCTL................CARRIAGE CONTROL FLAG; 1 FOR SINGLE SPACE
C                                                   2 FOR DOUBLE SPACE
C                                                   3 FOR TRIPLE SPACE
C
C        [ NCTL does not do what is claimed, and as far as I know
C          never has in the last decade. JDW 1/8/98. ]
C
C THE PARAMETERS THAT FOLLOW MATRIX ARE ALL OF TYPE INTEGER*4.  THE
C
C PROGRAM IS SET UP TO HANDLE 5 COLUMNS/PAGE WITH A 1P5D24.15 FORMAT FOR
C
C THE COLUMNS.  IF A DIFFERENT NUMBER OF COLUMNS IS REQUIRED, CHANGE
C
C FORMATS 1000 AND 2000, AND INITIALIZE KCOL WITH THE NEW NUMBER OF
C
C COLUMNS.
C
C AUTHOR;  NELSON H.F. BEEBE, QUANTUM THEORY PROJECT, UNIVERSITY OF
C          FLORIDA, GAINESVILLE
C
C REVISED;  FEBRUARY 26, 1971
C
C Revised to work with f90 : 1/8/98. JDW.
C
C.......................................................................
CSW      1
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER ROWLOW,ROWHI,COLLOW,COLHI,ROWDIM,COLDIM,BEGIN,KCOL
CSW      1
      REAL*8 MATRIX(ROWDIM,COLDIM)
CNSW     1
C     REAL   MATRIX(ROWDIM,COLDIM)
      CHARACTER*1 ASA,BLANK,CTL
      CHARACTER*6 COLUMN
      DIMENSION ASA(3)
      DATA COLUMN/'COLUMN'/,ASA/' ','0','-'/, BLANK/' '/
      DATA KCOL/4/
      DATA ZERO/0.D00/
      DO 10 I=ROWLOW,ROWHI
      DO 10 J=COLLOW,COLHI
      IF (MATRIX(I,J).NE.ZERO) GO TO 15
   10 CONTINUE
      WRITE (6,3000)
 3000 FORMAT ('0ZERO MATRIX.')
      GO TO 3
   15 CONTINUE
      CTL = BLANK
      IF ((NCTL.LE.3).AND.(NCTL.GT.0)) CTL = ASA(NCTL)
      IF (ROWHI.LT.ROWLOW) GO TO 3
      IF (COLHI.LT.COLLOW) GO TO 3
      LAST = MIN0(COLHI,COLLOW+KCOL-1)
      DO 2 BEGIN = COLLOW,COLHI,KCOL
      WRITE (6,1000) (COLUMN,I,I = BEGIN,LAST)
      DO 1 K = ROWLOW,ROWHI
      DO 4 I=BEGIN,LAST
      IF (MATRIX(K,I).NE.ZERO) GO TO 5
    4 CONTINUE
      GO TO 1
    5 WRITE (6,2000) CTL,K,(MATRIX(K,I), I = BEGIN,LAST)
    1 CONTINUE
    2 LAST = MIN0(LAST+KCOL,COLHI)
    3 RETURN
 1000 FORMAT (/,1X,16X,3(A6,I4,7X),(A6,I4))
 2000 FORMAT (A1,'ROW',I4,2X,4F17.11)
      END
