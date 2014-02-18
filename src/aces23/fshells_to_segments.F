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
      subroutine fshells_to_segments(mx_seg_size, segs, nsegs)
c---------------------------------------------------------------------------
c   Fit the shells into index segments, based on the maximum segment size.
c
c   If the "atom_based" flag is .true., we will use a segmentation scheme 
c   that forces the segments to occur on atomic boundaries.  Otherwise,
c   the shells are arranged for most efficient packing within the segment
c   boundaries.
c---------------------------------------------------------------------------
      implicit none
      include 'int_gen_parms.h'
      include 'frag_def.h'

      integer mx_seg_size, nsegs
      integer segs(nshells)
      integer last_frag 

      integer i, j, ishell, jshell, iatom, jatom, last_shell(100), 
     *        nsh, number, number0, sum, sumT 

      integer fseg(100) 

c     logical frag_calc 
c     integer nfrags, natoms_frag, watom_frag 
c     common /FRAGDEF/frag_calc, nfrags, natoms_frag(100), 
c    *                watom_frag(100,100) 

c     integer fragAO, n_occ, n_aocc, n_bocc, n_virt, n_avirt, n_bvirt
c     common /FORBS/fragAO(100), n_occ(100), n_aocc(100), n_bocc(100), 
c    *              n_virt(100), n_avirt(100), n_bvirt(100) 


      nsegs = 0 
      do i = 1, nfrags 
         sum   = 0 
         sumT  = 0 
         number = fragAO(i)   
         if (fragAO(i) .gt. mx_seg_size) then  
             number = fragAO(i)/mx_seg_size + 1 
             number = fragAO(i)/number 
             number = mx_seg_size !/2 
             number0 = number 
         endif 
         if (fragAO(i) .le. mx_seg_size) then  
             number = fragAO(i)/mx_seg_size + 1 
             number = fragAO(i) 
             number0 = number 
         endif 

c        if (fragAO(i) .ne. mx_seg_size) then
c            number = fragAO(i)/mx_seg_size + 1
c            number = fragAO(i)/number
c        endif
c        number = mx_seg_size/2
c        number0 = number

      do j = 1, natoms_frag(i) 
         jatom = watom_frag(i,j) 
      do ishell = 1, nshells 
         iatom = atom(ishell) 
         if (iatom .eq. jatom)then 

            if (ishell .eq. 1) sum = sum + end_nfps(ishell)  
            if (ishell .gt. 1) sum = sum + end_nfps(ishell) 
     *                                   - end_nfps(ishell-1) 
            if (ishell .eq. 1) sumT= sumT+ end_nfps(ishell)  
            if (ishell .gt. 1) sumT= sumT+ end_nfps(ishell) 
     *                                   - end_nfps(ishell-1) 

            if (sum .eq. fragAO(i)) then 
               nsegs = nsegs + 1 
               segs(nsegs) = end_nfps(ishell) 
               fseg(nsegs) = i 
               sum = 0 
               go to 10 
            endif 

            if (sumT .eq. fragAO(i)) then 
               nsegs = nsegs + 1 
               segs(nsegs) = end_nfps(ishell) 
               fseg(nsegs) = i 
               sum = 0 
               go to 10 
            endif 
            if ((sum .lt. fragAO(i)) .and. 
c    *          (sum .le. mx_seg_size) .and.   
     *          (sum .ge. number)) then  
               nsegs = nsegs + 1 
               segs(nsegs) = end_nfps(ishell) 
               fseg(nsegs) = i 
               sum = 0 
            endif 

         endif 
      enddo 
      enddo 
10       continue 
      enddo 

      write(6,*) ' ATOMS in SHELLS with ', nfrags, 'FRAGMENTS' 
      do i = 1, nshells 
         write(6,*) i, atom(i) 
      enddo 

      write(6,*)' AO segments ', nsegs  
      do i = 1, nsegs 
         write(6,*) i, segs(i), fseg(i) 
         w_frag_AO(i) = fseg(i)   
      enddo 
      return
      end
