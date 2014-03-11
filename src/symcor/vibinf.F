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
c char*8  LABEL(NIRREP)
c integer ICENSUS(NATOM)
c integer IPTR(NATOM,IORDER)
c integer NBFATM(NATOM)
c integer ILCATM(NATOM)
c double  SCR(*)

c RECORDS
c get TYPE//'MEMB'
c get TYPE//'CHAR'
c get TYPE//'SYOP'
c get TYPE//'PERM'
c get TYPE//'LABL'
c get 'ATOMCHRG'
c get 'COORD   '
c get 'ORIENTMT'
c get 'LINEAR  '
c put 'ROTVECX '
c put 'ROTVECY '
c put 'ROTVECZ '

      SUBROUTINE VIBINF(NATOM,NIRREP,IORDER,COORD,VREF,
     &                  VIMAGE,CHAR,SYOP,LABEL,ICENSUS,IPTR,
     &                  NBFATM,ILCATM,SCR,TYPE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION COORD(3,NATOM),VREF(NATOM*3),VIMAGE(NATOM*3)
      DIMENSION CHAR(IORDER,NIRREP),SYOP(9*IORDER)
      CHARACTER*8 LABEL(NIRREP)
      DIMENSION ICENSUS(NATOM),IPTR(NATOM,IORDER)
      DIMENSION NBFATM(NATOM),ILCATM(NATOM),SCR(*)
      CHARACTER*4 TYPE

      DIMENSION IATNUM(100)

      COMMON /MACHSP/ IINTLN,IFLTLN,IINTFP,IALONE,IBITWD

      DATA ONE /1.0/

      CALL ZERO(VREF,3*NATOM)

      CALL IGETREC(20,'JOBARC',TYPE//'MEMB',NATOM,ICENSUS)
      CALL DGETREC(20,'JOBARC',TYPE//'CHAR',NIRREP*IORDER,CHAR)
      CALL DGETREC(20,'JOBARC',TYPE//'SYOP',9*IORDER,SYOP)
      CALL IGETREC(20,'JOBARC',TYPE//'PERM',NATOM*IORDER,IPTR)
      CALL DGETREC(20,'JOBARC',TYPE//'LABL',NIRREP,LABEL)
      CALL IGETREC(20,'JOBARC','ATOMCHRG',NATOM,IATNUM)
      CALL DGETREC(20,'JOBARC','COORD   ',3*NATOM,COORD)
      CALL DGETREC(20,'JOBARC','ORIENTMT',9,SCR)
      CALL IGETREC(20,'JOBARC','LINEAR  ',1,ILINEAR)

      IF (TYPE.EQ.'FULL') CALL TRNOPS(SYOP,SCR,IORDER)

c   o compute Rx, Ry, and Rz rotational normalization factors
      IOFF=0
      RX=0.0
      RY=0.0
      RZ=0.0
      DO IATOM=1,NATOM
         IF (IATNUM(IATOM).NE.0) THEN
            RX=RX+COORD(3,IATOM)*COORD(3,IATOM)+
     &            COORD(2,IATOM)*COORD(2,IATOM)
            RY=RY+COORD(3,IATOM)*COORD(3,IATOM)+
     &            COORD(1,IATOM)*COORD(1,IATOM)
            RZ=RZ+COORD(1,IATOM)*COORD(1,IATOM)+
     &            COORD(2,IATOM)*COORD(2,IATOM)
         END IF
         IOFF=IOFF+3
      END DO
      FX=ONE/SQRT(RX)
      FY=ONE/SQRT(RY)
      IF (ILINEAR.NE.0) THEN
         FZ=0.d0
      ELSE
         FZ=ONE/SQRT(RZ)
      END IF

      DO IATOM=1,NATOM
         NBFATM(IATOM)=3
         ILCATM(IATOM)=3*(IATOM-1)+1
      END DO

      ZNORM=ONE/DFLOAT(IORDER)

C FIRST COMPUTE REDUCIBLE REPRESENTATION FOR VIBRATIONS,
C TRANSLATIONS AND ROTATIONS AND DECOMPOSE IT

      DO IOP=1,IORDER

         CALL ZERO(VREF,3*NATOM)
         DO IPOS=1,3*NATOM
            IATOM=1+(IPOS-1)/3
            IF(IATNUM(IATOM).NE.0)VREF(IPOS)=ONE
         END DO
         CALL IMAGE(NATOM,3*NATOM,1,IOP,IPTR,NBFATM,ILCATM,
     &              VREF,VIMAGE,SCR,1,3*NATOM,SYOP,0)
         IOFF=1
         DO I=1,NATOM
            IF(IPTR(I,IOP).NE.I)CALL ZERO(VIMAGE(IOFF),3)
            IOFF=IOFF+3
         END DO
         SCR(3*NATOM+IOP)=XDOT(3*NATOM,VIMAGE,1,VREF,1)

c      o translations
         CALL IMAGE(NATOM,3*NATOM,1,IOP,IPTR,NBFATM,ILCATM,
     &              VREF,VIMAGE,SCR,1,3*NATOM,SYOP,1)
         SCR(3*NATOM+IORDER+IOP)=XDOT(3*NATOM,VIMAGE,1,VREF,1)*3.0

c      o Rx
         CALL ZERO(VREF,3*NATOM)
         IOFF=0
         DO IATOM=1,NATOM
            IF (IATNUM(IATOM).NE.0) THEN
               VREF(IOFF+2)=FX*COORD(3,IATOM)
               VREF(IOFF+3)=-FX*COORD(2,IATOM)
            END IF
            IOFF=IOFF+3
         END DO
         CALL DPUTREC(20,'JOBARC','ROTVECX ',3*NATOM,VREF)
         CALL IMAGE(NATOM,3*NATOM,1,IOP,IPTR,NBFATM,ILCATM,
     &              VREF,VIMAGE,SCR,1,3*NATOM,SYOP,0)
         ZTMPX=XDOT(3*NATOM,VIMAGE,1,VREF,1)

c      o Ry
         CALL ZERO(VREF,3*NATOM)
         IOFF=0
         DO IATOM=1,NATOM
            IF (IATNUM(IATOM).NE.0) THEN
               VREF(IOFF+1)=FY*COORD(3,IATOM)
               VREF(IOFF+3)=-FY*COORD(1,IATOM)
            END IF
            IOFF=IOFF+3
         END DO
         CALL DPUTREC(20,'JOBARC','ROTVECY ',3*NATOM,VREF)
         CALL IMAGE(NATOM,3*NATOM,1,IOP,IPTR,NBFATM,ILCATM,
     &              VREF,VIMAGE,SCR,1,3*NATOM,SYOP,0)
         ZTMPY=XDOT(3*NATOM,VIMAGE,1,VREF,1)

c      o Rz
         CALL ZERO(VREF,3*NATOM)
         IOFF=0
         DO IATOM=1,NATOM
            IF (IATNUM(IATOM).NE.0) THEN
               VREF(IOFF+1)=FZ*COORD(2,IATOM)
               VREF(IOFF+2)=-FZ*COORD(1,IATOM)
            END IF
            IOFF=IOFF+3
         END DO
         IF (ILINEAR.EQ.0) THEN
            CALL DPUTREC(20,'JOBARC','ROTVECZ ',3*NATOM,VREF)
         END IF
         CALL IMAGE(NATOM,3*NATOM,1,IOP,IPTR,NBFATM,ILCATM,
     &              VREF,VIMAGE,SCR,1,3*NATOM,SYOP,0)
         ZTMPZ=XDOT(3*NATOM,VIMAGE,1,VREF,1)

         SCR(3*NATOM+2*IORDER+IOP)=ZTMPX+ZTMPY+ZTMPZ

      END DO

1001  FORMAT(T3,'@VIBINF-I, Symmetries species for nuclear motions:')
1002  FORMAT(T3,'Irrep',T13,'Label ',T21,' Total ',T31,'Vibrations',
     &       T46,'Translations',T63,'Rotations' )
1000  FORMAT(T4,I2,T14,A,T22,F5.2,T33,F5.2,T49,F5.2,T65,F5.2)
      WRITE(6,1001)
      WRITE(6,1002)
      IOFF=3*NATOM
      DO IRREP=1,NIRREP
         XTOT=XDOT(IORDER,SCR(IOFF+1),1,CHAR(1,IRREP),1)*ZNORM
         XTRA=XDOT(IORDER,SCR(IOFF+IORDER+1),1,CHAR(1,IRREP),1)*ZNORM
         XROT=XDOT(IORDER,SCR(IOFF+2*IORDER+1),1,CHAR(1,IRREP),1)*ZNORM
         WRITE(6,1000)IRREP,LABEL(IRREP)(1:4),XTOT,XTOT-XTRA-XROT,
     &                XTRA,XROT
      END DO

      RETURN
      END

