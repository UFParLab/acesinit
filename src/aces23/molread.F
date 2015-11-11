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
      SUBROUTINE simple_INSPECT_MOL(fname, MAX_ATOMS, max_shells,
     &                natoms, nshells, nspc, cartesian, ITFCT, LNP1,
     &                lnpo, nfct, nufct, nbasis, NAOBASIS, nCFpS, 
     &                nPFpS, NAOATM, angmom, atomic_label,
     & 
     &                Iecp,Maxang_L,Maxjco_L,Nucstr_tmp,Nrcstr_tmp,
     &                Iqmstr_tmp,Jcostr_tmp,Jstrt_tmp,Jrs_tmp,
     &                Namat,Charge) 
C
C   Simple version of Ajith Perera's INSPECT_MOL routine.  This version 
C   scans the MOL file to determine the number of atoms.  It also assumes
C   no reordering of centers and all atoms are symmetry-unique.
c
c   Mark Ponton 10/2/03
C
C     ----INPUT ARGUMENTS----
c FNAME       = Unix file name of the MOL file.
C MAX_ATOMS   = Maximum number of atoms allowed.
C MAX_SHELLS  = Maximum number of shells
C CARTESIAN   = True for Cartesian basis choice 
C 
C    ----OUTPUT ARGUMENTS----- 
C
C NATOMS    = The total number of atoms. 
c nshells   = total number of shells
c nspc      = Array containing the number of shells per center.
C ITFCT   = Total number of primitive functions.
C LNP1    = Largest possible value for number of contracted functions for
C           all shells.
C LNPO    = Largest possible value for the product of number of primitives
C           of contracted functions for all shells.
C NFCT    = The total number of  primitive functions on each atom.
C NAOATM  = The total number of contracted functions on each atom.
C NUFCT   = The total number of primitives on each sym. unique atom
C NAOUATM = The total number of contracted functions on each sym. 
C           unique atom.
C NBASIS  = Total number of basis functions (contracted)
C NAOBASIS = Total number of basis functions in Cartesian coordinates.
c nCFpS   = Number of contracted functions per shell.
c nPFpS   = Number of primitive functions per shell.
c angmom  = Anuglar momentum of each (sub)shell.
c atomic_label = Array of unique integer ids for the atom of each shell.
c vnn     = Nuclear-nuclear repulsion energy.

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'dbugcom.h'
      include 'basis_info.h'
      include 'ecp_info.h'
C     
      CHARACTER*(*) FNAME
      CHARACTER*4 ATMNAM
      CHARACTER*4 NAMAT(MAX_ATOMS)
      CHARACTER*80 XLINE
      LOGICAL CARTESIAN, ECP
C     
      DIMENSION NFCT(MAX_ATOMS), NAOATM(MAX_ATOMS),
     &          NUFCT(MAX_ATOMS), NAOUATM(MAX_ATOMS),
     &          COORD(3)

      DIMENSION jcostr_tmp(MAX_ATOMS,Maxjco_L),
     &          nucstr_tmp(MAX_ATOMS,Maxang_L,Maxjco_L),
     &          nrcstr_tmp(MAX_ATOMS,Maxang_L,Maxjco_L),
     &          Jstrt_tmp(max_shells),Jrs_tmp(max_shells),
     &          iqmstr_tmp(MAX_ATOMS),Charge(Max_atoms)

      INTEGER NAOUATM2(MAX_ATOMS)
      INTEGER NAOBASIS, NAOTMP2, NP22
      INTEGER ISHL(10)
      integer idosph, idum(8)
      integer nspc(max_atoms), nCFpS(max_shells), nPFpS(max_shells)
      integer angmom(max_shells)
      integer atomic_label(max_shells)
      double precision vnn, r3, r5 
      double precision nuclear_nuclear_repulsion_energy 
      character*52  dumstring

      integer ihess, jhess, iatom, jatom 
C
C Open MOL file for basis set information. The MOL file is created by
C joda by processing user input file and basis set library.
C
      OPEN(UNIT=10, FILE=FNAME, FORM='FORMATTED', STATUS='OLD')
      REWIND(10)

      Ecp = .False.
      If (Iecp .GE. 1)  ECP = .TRUE.
C
C R
C     
C Read the first five lines. Information on those five lines 
C are not relevent in the presnt context. 
C
      read (10,'(a6,4x,9i5)') dumstring,(idum(i),i=1,8),icart
      if (icart .eq. 1) then
         cartesian = .true.
         if (dbg)
     *      print *,'*** MOL file contains Cartesian coordinates ***'
      else
         if (dbg)
     *      print *,'*** MOL file contains spherical coordinates ***'
         cartesian = .false.
      endif

      READ(10,'(A)') XLINE
      READ(10,'(A)') XLINE
      READ(10,'(A)') XLINE
      READ(10,'(A)') XLINE
C     
      LNP1   = 0
      LNPO   = 0
      LTNP1  = 0
      LTNPO  = 0
C
      natoms     = 0
      nshells    = 0
      indx_shell = 0

      DO 10 IATM = 1, MAX_ATOMS
         READ (10, '(A80)') XLINE
         if (xline(1:6) .eq. 'FINISH') go to 2000
C
         natoms = natoms + 1
         READ(xline, 1110) ZNUC, IJUNK, NSHL,
     *       (ISHL(I),I=1,NSHL)
 1110    FORMAT(F20.1,10I5)
         nspc(iatm) = 0
C
         READ(10,1115) ATMNAM,(COORD(I),I=1,3)
 1115    FORMAT(A4,3F20.12)

C ECP needs the charges as double precison.

         CHARGE(IATM) = ZNUC
C
c----------------------------------------------------------------------------
c   Save the geometry data 
c----------------------------------------------------------------------------

         call set_geometry(iatm, coord, znuc)
C
         NUFCT(IATM) = 0
         NAOTMP      = 0
         NAOTMP2     = 0
         shell       = 0
         sub_shell   = 0

C The ECP arrays added, Ajith Perera, 04/2012----

         IF (ECP) THEN
            IQMSTR_TMP(IATM) = NSHL
            NAMAT(IATM) = ATMNAM(1:2)
            DO IMOM = 1, NSHL
               JCOSTR_TMP(IATM, IMOM) = ISHL(IMOM)
            ENDDO
         ENDIF
C----

         DO 20 I = 1, NSHL
            NPT  = 0
            NAOT = 0
            iangmom = i - 1
C
            DO 21 I1 = 1, ISHL(I)
               nshells        = nshells + 1
               nspc(iatm)     = nspc(iatm) + 1
               angmom(nshells) = iangmom   ! same a. m. value for each subshell
               atomic_label(nshells) = iatm
C
               READ(10,1120) NP1, NAO
 1120          FORMAT(2I5)
C
               NPT  = NPT  + NP1
               NAOT = NAOT + NAO
C
C The ECP arrays added, Ajith Perera, 04/2012----
C
               IF (ECP) THEN
                  NUCSTR_TMP(IATM,I,I1) = NP1
                  NRCSTR_TMP(IATM,I,I1) = NAO
               ENDIF
C-------
               IF (CARTESIAN) THEN 
                  NP2 = I*(I + 1)/2 
                  NP22 = NP2
               ELSE
                  NP2 = 2*I - 1
                  NP22 = I*(I + 1)/2
               ENDIF
C
               NAOTMP = NAOTMP + NP2*NAO
               NAOTMP2 = NAOTMP2 + NP22 * NAO
               NUFCT(IATM) = NUFCT(IATM) + NP2*NP1
               nPFpS(nshells) = np1
               nCFpS(nshells) = nao
C
               NLN = (NAO-3)/4
               IF ((NAO-3) .GT. (NLN*4)) NLN = NLN + 1
               NLN = (NLN + 1)*NP1
C
               DO 30 J=1,NLN
                  READ(10,'(A)') XLINE
 30            CONTINUE
C
               IF(NPT .GT. LNP1) THEN
                  IF(NPT .GT. LTNP1) LTNP1 = NPT
               ENDIF
C
               ITMP = NPT*NAOT
               IF(ITMP .GT. LNPO) THEN
                  IF(ITMP .GT. LTNPO) LTNPO = ITMP
               ENDIF
C
 21         CONTINUE
 20      CONTINUE
C
         IF (LTNP1 .GT. LNP1) LNP1 = LTNP1
         IF (LTNPO .GT. LNPO) LNPO = LTNPO
C
         NAOUATM(IATM) = NAOTMP
         NAOUATM2(IATM) = NAOTMP2
C
 10   CONTINUE
C     
 2000 continue
      iuatms = natoms
      ITFCT = 0
      DO 110 IATM = 1, IUATMS
            ITFCT = ITFCT + NUFCT(IATM)
 110  CONTINUE
C     
C Fill out NFCT, NAOATM and NMOMFCT for all atoms.
C
      ICNT   = 0
      NBASIS = 0
      NAOBASIS = 0
      DO 1011 II = 1, IUATMS
C
C
            ICNT = ICNT + 1
            NFCT(ICNT) = NUFCT(II)
            NAOATM(ICNT)= NAOUATM(II)
            NBASIS = NBASIS + NAOUATM(II)
            NAOBASIS = NAOBASIS + NAOUATM2(II)
C
 1011 CONTINUE
C
C The ECP arrays added, Ajith Perera, 04/2012----

      JSTRT_TMP(1) = 0
      JRS_TMP(1)   = 0

      IF (ECP) THEN
         DO ISHELL = 1, NSHELLS
            JSTRT_TMP(ISHELL+1) = JSTRT_TMP(ISHELL) + NPFPS(ISHELL)
            JRS_TMP(ISHELL+1)   = JRS_TMP(ISHELL) +   NPFPS(ISHELL)*
     &                            NCFPS(ISHELL)
         ENDDO
      ENDIF
C-------
C Compute the Nuclear-Nuclear repulsion energy
C --------------------------------------------
C
c     vnn = nuclear_nuclear_repulsion_energy(natoms)

C Write the arrays used in ECP integrals to the ecp_info.h to
C retrive them in aces_setup.F to write back to the .dat file.

      Do Ishell = 1, Nshells
         Jstrt(Ishell) = Jstrt_tmp(Ishell)
         Jrs(Ishell)   = Jrs_tmp(Ishell)
      Enddo

      Max_sub_shells = 0

      Do Iatm = 1, Natoms 

         Iqmstr(iatm) =  Iqmstr_tmp(iatm)
          
         Do I = 1, Iqmstr_tmp(iatm)
            Jcostr(iatm, i) = Jcostr_tmp(iatm,i)
            Max_sub_shells = Max(Max_sub_shells, Jcostr_tmp(iatm,i))
            Do i1 = 1,  Jcostr_tmp(iatm, i)
               Nucstr(iatm,i,i1) = Nucstr_tmp(iatm,i,i1)
               Nrcstr(iatm,i,i1) = Nrcstr_tmp(iatm,i,i1)
            Enddo
         Enddo
      Enddo

C#ifdef _DEBUG_LVL0
      Write(6,"(a)") "The Iqmstr"
      Write(6,"(10(1x,I4))") (Iqmstr(i), i=1, natoms)
      Write(6,"(a)") "The jcostr"
      Do i=1, Natoms 
        Write(6,"(10(1x,I4))") (jcostr(i,j),j=1,Maxjco)
      Enddo
      Write(6,"(a)") "The Nucstr"
      Do i=1, natoms 
        Write(6,"(10(1x,I4))") ((Nucstr(i,j,k),j=1,4),k=1,Maxjco)
      Enddo
      Write(6,"(a)") "The Nrcstr"
      Do i=1, Natoms 
        Write(6,"(10(1x,I4))") ((Nrcstr(i,j,k),j=1,4),k=1,Maxjco)
      Enddo
      Write(6,"(a)") "The jstrt"
      Write(6,"(10(1x,I4))") (Jstrt(i), i=1, Nshells)
      Write(6,"(a)") "The jrs"
      Write(6,"(10(1x,I4))") (Jrs(i), i=1, Nshells)
      Write(6,*)
C#endif
      close(10)
      RETURN
      END

      SUBROUTINE READ_BASIS_INFO(FNAME, IUATMS, NATOMS, NPOP, IREORDER,
     &                           CARTESIAN, ITFCT, LNP1, LNPO,
     &                           NFCT, NBASIS, ALPHA, IXALPHA,
     &                           PCOEFF, IXPCOEF,
     &                           ATMNAM, COORD,Atom_COORDS,
     &                           ALPHA_NORM,NAOATM)
C
C     ----INPUT ARGUMENTS----
C
C FNAME     = Unix file name of the MOL file.
C IUATMS    = The number of symmetry unique atoms.
C NATOMS    = The total number of atoms.
C IREORDER  = If there is any reordering of centers, this should
C             give the correspondence. Most often this is a unit vector.
C CARTESIAN = True for Cartesian basis choice.
C NPOP      = number of symmetry equivalent atom for given
C             symmetry unique center.
C NAOATM    = The total number of contracted functions on each atom.
C ITFCT     = Total number of primitive functions.
C LNP1      = Largest value for number of contracted functions for all
C             shells.
C LNPO      = Largest possible value for the product of number of primitives
C             of contracted functions for all shells.
C NFCT      = The total number of  primitive functions on each atom.
C
C     ----OUTPUT ARGUMENTS------
C
C ALPHA     = Exponents for basis functions
C IXALPHA   = Index array, each entry is the beginning of alpha's for the 
C             shell.
C PCOEFF    = The contraction coefficients.
C IXPCOEF   = Index array for PCOEF, each entry is the beginning of
C             pcoeff's for the shell.
C ATMNAM    = Array of Atom names
C COORD     = (x,y,z) coordinate of each shell.
C
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      include 'basis_info.h'
C     
      CHARACTER*(*) FNAME
      CHARACTER*80 XLINE
      CHARACTER*4 ATMNAM(NATOMS)
      LOGICAL CARTESIAN
C     
      DIMENSION NPOP(IUATMS),IREORDER(NATOMS), NFCT(NATOMS), 
     &          PCOEFF(ITFCT*NBASIS), CNAOATM(NATOMS),
     &          ALPHA(ITFCT), 
     &          SALPHA(LNP1), SPCOEF(LNPO), SALPHA_NORM(LNP1),
     &          COORD(3,*), Atom_coords(3,Natoms),
     &          ALPHA_NORM(ITFCT)

      INTEGER IXALPHA(*), IXPCOEF(*)
c     INTEGER ntot_alpha, ntot_pcoef, ntot_shells  
 
      DOUBLE PRECISION X, Y, Z 
      INTEGER ISHL(10)

      integer nxt_alpha, nxt_pcoef
C     
      PI=DATAN(1.0D+00)*4.D+00
      PICNST=(0.5D+00/PI)**0.75D+00
C
C Open MOL file for basis set information. The MOL file is created by 
C joda by processing user input file and basis set library.
C
      OPEN(UNIT=10, FILE=FNAME, FORM='FORMATTED', STATUS='OLD')
      REWIND(10)
C
C For the prsent puupose we can ignore the first 5 line of the MOL file.
C
      READ(10,'(A)') XLINE
      READ(10,'(A)') XLINE
      READ(10,'(A)') XLINE
      READ(10,'(A)') XLINE
      READ(10,'(A)') XLINE

      ntot_alpha = 0
      ntot_pcoef = 0  
      ntot_shells = 0 
C
      INI    = 1
      MAXANG = 0

      ishell = 0
      nxt_alpha = 1
      nxt_pcoef = 1 

      DO 10 IATM = 1, IUATMS
C
         READ(10,1000) ZNUC, IJUNK, NSHL,(ISHL(I),I=1,NSHL)
 1000    FORMAT(F20.1,10I5)
C
         READ(10,1010) ATMNAM(IATM), X, Y, Z
#ifdef _DEBUG_LVL0

         write(6,*) iatm, ATMNAM(IATM), x, y, z 
#endif
 1010    FORMAT(A4,3F20.12)
C     
         IDGOFF = 0
         IOFF   = 0
         JOFF   = 0
         NUNQSHL= 0

C-The ECP arrays added, Ajith Perera, 04/2012----

         Atom_COORDS(1,IATM) = X
         Atom_COORDS(2,IATM) = Y
         Atom_COORDS(3,IATM) = Z
C---
         DO 28 LL = 1, NSHL
C
            NPT=0
            NAOT=0
            ntot_shells = ntot_shells + 1 
C
            DO 30 II1 = 1, ISHL(LL)
               READ(10,1120) NP1, NAO
C
               ishell = ishell + 1
               coord(1, ishell) = x
               coord(2, ishell) = y
               coord(3, ishell) = z
               NPT  = NPT + NP1
               NAOT = NAOT + NAO
C
               IF (CARTESIAN) THEN
                  NP2 = LL*(LL + 1)/2
               ELSE
                  NP2 = 2*LL - 1
               ENDIF
C
               DO 32 I = 1, NP1
                  READ(10,1060) SALPHA(I),(SPCOEF((J-1)*NP1+I),J=1,NAO)

                  SALPHA_NORM(I) = (4.0D+00*SALPHA(I))**
     &                             (0.50D+00*DBLE(LL)+
     &                             0.25D+00)*PICNST
 32            CONTINUE
C     
C Renormalize the atomic orbitals. Multiply the renormalized
C coefficients by the appropriate normalization constants.
C     
               DO 34 INAO = 1, NAO
C
                  SUM = 0.D+00
C
                  DO 36 I = 1, NP1
                     DO 37 J = 1, I
C
                        AI=SALPHA(I)
                        AJ=SALPHA(J)
C
                        TMP=SPCOEF((INAO-1)*NP1+I)*SPCOEF((INAO-1)*
     &                      NP1+J)*(2.0D+00*DSQRT(AI*AJ)/
     &                     (AI+AJ))**(REAL(LL)+0.5D+00)
C
                        SUM = SUM + TMP
                        IF(I .NE. J) SUM = SUM + TMP
C
 37                  CONTINUE
 36               CONTINUE
C
                  XNORM=1.D+00/DSQRT(SUM)
C
                  DO 38 I = 1, NP1


CSSS                    SPCOEF((INAO-1)*NP1+I)= SPCOEF((INAO-1)*NP1+I)*
CSSS     &                                       XNORM*PICNST*
CSSS     &                                       (4.D+00*SALPHA(I))**
CSSS     &                                       (0.5D+00*REAL(LL)+
CSSS     &                                        0.25D+00)

                     SPCOEF((INAO-1)*NP1+I)= SPCOEF((INAO-1)*NP1+I)*
     &                                       XNORM
 38               CONTINUE
 34            CONTINUE
C     
C Place the alpha's and coefficients in their appropriate place in 
C their respective matrices, ALPHA and PCOEFF.
C
CCCCC               DO 40 IPOP = INI, INI + NPOP(IATM) - 1
C
CCCCC                  IATMOFF = 0
CCCCC                  JATMOFF = 0
C
CCCCC                  DO 43 ITMP = 1, IREORDER(IPOP)-1
CCCCC                     IATMOFF = IATMOFF + NFCT(ITMP)
CCCCC                     JATMOFF = JATMOFF + NAOATM(ITMP)*ITFCT
CCCCC 43               CONTINUE
C
CCCCC                  DO 45 I = 1, NP1
CCCCC                     JSHOFF=0
CCCCC                     DO 46 I1 = 1, NP2
CCCCC                        ALPHA(I+IOFF+IATMOFF+(I1-1)*NP1) = SALPHA(I)
C
CCCCC                        DO 57 J=1,NAO
CCCCC                           PCOEFF(JATMOFF+(JOFF+JSHOFF)*ITFCT+I+IOFF+
CCCCC     $                     IATMOFF+(I1-1)*NP1 )= SPCOEF((J-1)*NP1+I)
CCCCC                           JSHOFF=JSHOFF+1
C
CCCCC 57                     CONTINUE
CCCCC 46                  CONTINUE
CCCCC 45               CONTINUE
CCCCC 40            CONTINUE
C
CCCCC               IOFF=IOFF+NP1*NP2
CCCCC               JOFF=JOFF+NP2*NAO
C

c---------------------------------------------------------------------------
c   Move alpha's and pcoef's into their packed return arrays.
c   Save pointers in the ixalpha, ixpcoef arrays so the data may be
c   more easily referenced later.
c---------------------------------------------------------------------------

               ixalpha(ishell) = nxt_alpha
               do i = 1, np1
                  alpha(nxt_alpha+i-1)      = salpha(i)
                  alpha_norm(nxt_alpha+i-1) = salpha_norm(i)
                  ntot_alpha = ntot_alpha + 1 
               enddo
       
               nxt_alpha = nxt_alpha + np1

               ixpcoef(ishell) = nxt_pcoef
               do j = 1, nao
               do i = 1, np1
                  pcoeff(nxt_pcoef) = spcoef((j-1)*np1+i)
                  nxt_pcoef         = nxt_pcoef + 1
                  ntot_pcoef        = ntot_pcoef + 1 
               enddo
               enddo

 30         CONTINUE
 28      CONTINUE
C
         INI = INI + NPOP(IATM)
         IF(NSHL .GT. MAXANG) MAXANG = NSHL
 10   CONTINUE
C
 1120 FORMAT(2I5)
 1060 FORMAT(4F18.10)

c---------------------------------------------------------------------------
c   Write the basis info out to the initialization file.   
c---------------------------------------------------------------------------

c     ntot_alpha    mumber of exponents 
c     ntot_pcoef    number of exponentcients  
c     ntot_shells   number of shells 
c     alpha         array of exponents 
c     alpha_norm    expoent dependent normalization factor
c     pcoeff        array of coefficients 
c     ixalpha       array indicating the beginning index for each alpha/shell    
c     ixcoeff       array indicating the beginning index for each pcoeff/shell    
#ifdef _DEBUG_LVL0
      write(6,*) ' Total number of alphas', ntot_alpha 
      write(6,*) ' Total number of pcoeff', ntot_pcoef 
      write(6,*) ' Total number of shells', ntot_shells  
#endif

      do n = 1, ntot_shells 
         tixalpha(n) = ixalpha(n) 
         tixpcoeff(n) = ixpcoef(n) 
      enddo 
      do i = 1, ntot_alpha
         talpha(i)      = alpha(i)
         talpha_norm(i) = alpha_norm(i)
      enddo
      do i = 1, ntot_pcoef  
         tpcoeff(i) = pcoeff(i) 
      enddo 
      do i = 1, 3 
      do n = 1, ntot_shells 
         tcoord(i,n) = coord(i,n) 
      enddo 
      enddo 

#ifdef _DEBUG_LVL0
      Write(*,*) atom_coords
      write(6,*) ' IXALPHAs '
      write(6,"(5(1x,I4))")    (tixalpha(i), i=1,ntot_shells)
      write(6,*) ' ALPHAs '
      write(6,"(5(1x,F15.8))") (Alpha(i), i=1,ntot_alpha)
      write(6,*) ' ALPHA NORMS '
      write(6,"(5(1x,F15.8))") (tAlpha_norm(i), i=1,ntot_alpha)
      write(6,*) ' PCoeffs '
      write(6,"(5(1x,F15.8))") (tpcoeff(i), i=1,ntot_pcoef)
      write(6,*) 'Shell coordinates '
      do i=1,3
      write(6,"(3(1x,F15.8))") (tcoord(i,n), n=1,3*ntot_shells)
      enddo 
#endif
C
      close(10)
      RETURN
      END

