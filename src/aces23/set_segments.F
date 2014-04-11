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
      subroutine set_naocc_segs(nocc, nocc_orbitals, mx_occ_seg_size) 
      implicit none 
      include 'frag_def.h'
      integer nocc, nocc_orbitals, mx_occ_seg_size
      integer i, j, start, nsegs    
      integer n_frag, occ_frag(maxfrag), nocc_frag(maxfrag)

c As a test just set occ fragmants 

      n_frag = nfrags 
      if (nfrags .eq. 1) occ_frag(1) = nocc_orbitals 
      if (nfrags .gt. 1) then 
         do i = 1, n_frag 
            occ_frag(i) = n_aocc(i)  
         enddo 
      endif 

c Determine the effective mx_occ_seg_size 

      nsegs = nocc_orbitals/mx_occ_seg_size 
      write(6,*) ' VFL nsegs - ', nsegs, mx_occ_seg_size 
      if (nsegs*mx_occ_seg_size .ne. nocc_orbitals) nsegs = nsegs + 1 
      mx_occ_seg_size = nocc_orbitals/nsegs 
      write(6,*) ' VFL nsegs - ', nsegs, mx_occ_seg_size 

100   continue 
      nocc = 0 
      do i = 1, n_frag 
         if (mx_occ_seg_size .ge. occ_frag(i)) then 
            nocc         = nocc + 1 
            nocc_frag(i) = 1 
            bocc_frag(i,nocc_frag(i)) = 1 
            eocc_frag(i,nocc_frag(i)) = bocc_frag(i,nocc_frag(i)) 
     *                                + occ_frag(i) - 1  
         else  
            nocc_frag(i) = occ_frag(i)/mx_occ_seg_size 
            if (nocc_frag(i)*mx_occ_seg_size .lt. occ_frag(i)) 
     *          nocc_frag(i) = nocc_frag(i) + 1 
            nocc = nocc + nocc_frag(i) 

            do j = 1, nocc_frag(i) 
               if (j .eq. 1) then 
                  bocc_frag(i,j) = 1 
                  eocc_frag(i,j) = bocc_frag(i,j) + mx_occ_seg_size - 1  
               endif 

               if (j .gt. 1) then 
                  bocc_frag(i,j) = eocc_frag(i,j-1) + 1 
                  eocc_frag(i,j) = bocc_frag(i,j) + mx_occ_seg_size - 1  
               endif 
            enddo 

            if (eocc_frag(i,nocc_frag(i)) .gt. occ_frag(i)) 
     *         eocc_frag(i,nocc_frag(i)) = occ_frag(i) 

         endif 

      enddo 
c     if (nocc .gt. nsegs) then 
c        mx_occ_seg_size = mx_occ_seg_size + 1 
c        go to 100 
c     endif 

      write(6,*) ' NaOCC = ', nocc 
      start = 0 
      do i = 1, n_frag 
         n_aocc(i) = nocc_frag(i) 
         write(6,*) '    ', i, n_aocc(i)
         do j = 1, nocc_frag(i)
            baocc_frag(i,j) = bocc_frag(i,j) + start  
            eaocc_frag(i,j) = eocc_frag(i,j) + start  
            write(6,*) '          ', baocc_frag(i,j), eaocc_frag(i,j) 
         enddo 
         start = start + (eaocc_frag(i,nocc_frag(i)) -
     *                    baocc_frag(i,1) + 1)   
      enddo 


      end

      subroutine set_navirt_segs(nvirt,nocc_orbitals,nvirt_orbitals,
     *                           mx_virt_seg_size) 
      implicit none 
      include 'frag_def.h'
      integer nvirt, nocc_orbitals, nvirt_orbitals, mx_virt_seg_size
      integer i, j, start, nsegs    
      integer n_frag, virt_frag(maxfrag), nvirt_frag(maxfrag) 

c As a test just set virt fragmants 

      n_frag = nfrags 
      if (nfrags .eq. 1) virt_frag(1) = nvirt_orbitals 
      if (nfrags .gt. 1) then 
         do i = 1, n_frag 
            virt_frag(i) = n_avirt(i)  
         enddo 
      endif 

c Determine the effective mx_occ_seg_size 

      nsegs = nvirt_orbitals/mx_virt_seg_size 
      write(6,*) ' VFL nsegs - ', nsegs, mx_virt_seg_size 
      if (nsegs*mx_virt_seg_size .ne. nvirt_orbitals) nsegs = nsegs + 1 
      mx_virt_seg_size = nvirt_orbitals/nsegs 
      write(6,*) ' VFL nsegs - ', nsegs, mx_virt_seg_size 

100   continue 

      nvirt = 0 
      do i = 1, n_frag 
         if (mx_virt_seg_size .ge. virt_frag(i)) then 
            nvirt         = nvirt + 1 
            nvirt_frag(i) = 1 
            bvirt_frag(i,nvirt_frag(i)) = 1 
            evirt_frag(i,nvirt_frag(i)) = bvirt_frag(i,nvirt_frag(i)) 
     *                                + virt_frag(i) - 1  
         else  
            nvirt_frag(i) = virt_frag(i)/mx_virt_seg_size 
            if (nvirt_frag(i)*mx_virt_seg_size .lt. virt_frag(i)) 
     *          nvirt_frag(i) = nvirt_frag(i) + 1 
            nvirt = nvirt + nvirt_frag(i) 

            do j = 1, nvirt_frag(i) 
               if (j .eq. 1) then 
                  bvirt_frag(i,j) = 1 
                  evirt_frag(i,j) = bvirt_frag(i,j) + mx_virt_seg_size-1  
               endif 

               if (j .gt. 1) then 
                  bvirt_frag(i,j) = evirt_frag(i,j-1) + 1 
                  evirt_frag(i,j) = bvirt_frag(i,j) + mx_virt_seg_size-1  
               endif 
            enddo 

            if (evirt_frag(i,nvirt_frag(i)) .gt. virt_frag(i)) 
     *         evirt_frag(i,nvirt_frag(i)) = virt_frag(i) !nocc_orbitals 

         endif 

      enddo 
c     if (nvirt .gt. nsegs) then 
c        mx_virt_seg_size = mx_virt_seg_size + 1 
c        go to 100 
c     endif 


      write(6,*) ' NaVIRT = ', nvirt, n_frag, virt_frag(1)  
      start = nocc_orbitals 
      do i = 1, n_frag 
         n_avirt(i) = nvirt_frag(i) 
         write(6,*) '    ', i, n_avirt(i)
         do j = 1, nvirt_frag(i)
            bavirt_frag(i,j) = bvirt_frag(i,j) + start  
            eavirt_frag(i,j) = evirt_frag(i,j) + start  
            write(6,*) '          ', bavirt_frag(i,j), eavirt_frag(i,j) 
         enddo 
         start = eavirt_frag(i,nvirt_frag(i)) 
      enddo 


      end

      subroutine set_nbocc_segs(nocc, nocc_orbitals, mx_occ_seg_size) 
      implicit none 
      include 'frag_def.h'
      integer nocc, nocc_orbitals, mx_occ_seg_size
      integer i, j, start, nsegs    
      integer n_frag, occ_frag(maxfrag), nocc_frag(maxfrag) 

c As a test just set occ fragmants 

      n_frag = nfrags 
      if (nfrags .eq. 1) occ_frag(1) = nocc_orbitals 
      if (nfrags .gt. 1) then 
         do i = 1, n_frag 
            occ_frag(i) = n_bocc(i)  
         enddo 
      endif 

      nsegs = nocc_orbitals/mx_occ_seg_size 
      write(6,*) ' VFL nsegs - ', nsegs, mx_occ_seg_size 
      if (nsegs*mx_occ_seg_size .ne. nocc_orbitals) nsegs = nsegs + 1 
      mx_occ_seg_size = nocc_orbitals/nsegs 
      write(6,*) ' VFL nsegs - ', nsegs, mx_occ_seg_size 

100   continue 

      nocc = 0 
      do i = 1, n_frag 
         if (mx_occ_seg_size .ge. occ_frag(i)) then 
            nocc         = nocc + 1 
            nocc_frag(i) = 1 
            bocc_frag(i,nocc_frag(i)) = 1 
            eocc_frag(i,nocc_frag(i)) = bocc_frag(i,nocc_frag(i)) 
     *                                + occ_frag(i) - 1  
         else  
            nocc_frag(i) = occ_frag(i)/mx_occ_seg_size 
            if (nocc_frag(i)*mx_occ_seg_size .lt. occ_frag(i)) 
     *          nocc_frag(i) = nocc_frag(i) + 1 
            nocc = nocc + nocc_frag(i) 

            do j = 1, nocc_frag(i) 
               if (j .eq. 1) then 
                  bocc_frag(i,j) = 1 
                  eocc_frag(i,j) = bocc_frag(i,j) + mx_occ_seg_size - 1  
               endif 

               if (j .gt. 1) then 
                  bocc_frag(i,j) = eocc_frag(i,j-1) + 1 
                  eocc_frag(i,j) = bocc_frag(i,j) + mx_occ_seg_size - 1  
               endif 
            enddo 

            if (eocc_frag(i,nocc_frag(i)) .gt. occ_frag(i)) 
     *         eocc_frag(i,nocc_frag(i)) = occ_frag(i) 

         endif 

      enddo 
c     if (nocc .gt. nsegs) then 
c        mx_occ_seg_size = mx_occ_seg_size + 1 
c        go to 100 
c     endif 

      write(6,*) ' NbOCC = ', nocc 
      start = 0 
      do i = 1, n_frag 
         n_bocc(i) = nocc_frag(i) 
         write(6,*) '    ', i, n_bocc(i)
         do j = 1, nocc_frag(i)
            bbocc_frag(i,j) = bocc_frag(i,j) + start  
            ebocc_frag(i,j) = eocc_frag(i,j) + start  
            write(6,*) '          ', bbocc_frag(i,j), ebocc_frag(i,j) 
         enddo 
         start = start + (ebocc_frag(i,nocc_frag(i)) -
     *                    bbocc_frag(i,1) + 1)   
      enddo 

      end

      subroutine set_nbvirt_segs(nvirt,nocc_orbitals,nvirt_orbitals,
     *                           mx_virt_seg_size) 
      implicit none 
      include 'frag_def.h'
      integer nvirt, nocc_orbitals, nvirt_orbitals, mx_virt_seg_size
      integer i, j, start, nsegs    
      integer n_frag, virt_frag(maxfrag), nvirt_frag(maxfrag) 

c As a test just set virt fragmants 

      n_frag = nfrags 
      if (nfrags .eq. 1) virt_frag(1) = nvirt_orbitals 
      if (nfrags .gt. 1) then 
         do i = 1, n_frag 
            virt_frag(i) = n_bvirt(i)  
         enddo 
      endif 

c Determine the effective mx_occ_seg_size 

      nsegs = nvirt_orbitals/mx_virt_seg_size 
      write(6,*) ' VFL nsegs - ', nsegs, mx_virt_seg_size 
      if (nsegs*mx_virt_seg_size .ne. nvirt_orbitals) nsegs = nsegs + 1 
      mx_virt_seg_size = nvirt_orbitals/nsegs 
      write(6,*) ' VFL nsegs - ', nsegs, mx_virt_seg_size 

100   continue 

      nvirt = 0 
      do i = 1, n_frag 
         if (mx_virt_seg_size .ge. virt_frag(i)) then 
            nvirt         = nvirt + 1 
            nvirt_frag(i) = 1 
            bvirt_frag(i,nvirt_frag(i)) = 1 
            evirt_frag(i,nvirt_frag(i)) = bvirt_frag(i,nvirt_frag(i)) 
     *                                + virt_frag(i) - 1  
         else  
            nvirt_frag(i) = virt_frag(i)/mx_virt_seg_size 
            if (nvirt_frag(i)*mx_virt_seg_size .lt. virt_frag(i)) 
     *          nvirt_frag(i) = nvirt_frag(i) + 1 
            nvirt = nvirt + nvirt_frag(i) 

            do j = 1, nvirt_frag(i) 
               if (j .eq. 1) then 
                  bvirt_frag(i,j) = 1 
                  evirt_frag(i,j) = bvirt_frag(i,j) + mx_virt_seg_size-1  
               endif 

               if (j .gt. 1) then 
                  bvirt_frag(i,j) = evirt_frag(i,j-1) + 1 
                  evirt_frag(i,j) = bvirt_frag(i,j) + mx_virt_seg_size-1  
               endif 
            enddo 

            if (evirt_frag(i,nvirt_frag(i)) .gt. virt_frag(i)) 
     *         evirt_frag(i,nvirt_frag(i)) = virt_frag(i) !+nocc_orbitals 
         endif 

      enddo 
c     if (nvirt .gt. nsegs) then 
c        mx_virt_seg_size = mx_virt_seg_size + 1 
c        go to 100 
c     endif 

      write(6,*) ' NbVIRT = ', nvirt 
      start = nocc_orbitals 
      do i = 1, n_frag 
         n_bvirt(i) = nvirt_frag(i) 
         write(6,*) '    ', i, n_bvirt(i)
         do j = 1, nvirt_frag(i)
            bbvirt_frag(i,j) = bvirt_frag(i,j) + start  
            ebvirt_frag(i,j) = evirt_frag(i,j) + start  
            write(6,*) '          ', bbvirt_frag(i,j), ebvirt_frag(i,j) 
         enddo 
         start = ebvirt_frag(i,nvirt_frag(i)) 
      enddo 

      end

      subroutine set_frag_segs() 
      implicit none 
      include 'frag_def.h'
      integer i, j, n 

      write(6,*) ' Defining FRAGMENT lists' 

c Determine the AO frag segment list 

c Determine the occupied frag segment list 

c Determine the AO frag segment list 

      write(6,*) ' OCC list ' 

      n = 0 
      do i = 1, nfrags 
      do j = 1, n_occ(i) 
         n = n + 1 
         w_frag_occ(n) = i 
         write(6,*) n, w_frag_occ(n) 
      enddo 
      enddo 

      write(6,*) ' OCCA list ' 

      n = 0 
      do i = 1, nfrags 
      do j = 1, n_aocc(i) 
         n = n + 1 
         w_frag_aocc(n) = i 
         write(6,*) n, w_frag_aocc(n) 
      enddo 
      enddo 

      write(6,*) ' OCCB list ' 

      n = 0 
      do i = 1, nfrags 
      do j = 1, n_bocc(i) 
         n = n + 1 
         w_frag_bocc(n) = i 
         write(6,*) n, w_frag_bocc(n) 
      enddo 
      enddo 

c Determine the virtual frag segment list 

      write(6,*) ' VIRT list ' 

      n = 0 
      do i = 1, nfrags 
      do j = 1, n_virt(i) 
         n = n + 1 
         w_frag_virt(n) = i 
         write(6,*) n, w_frag_virt(n) 
      enddo 
      enddo 

      write(6,*) ' VIRTA list ' 

      n = 0 
      do i = 1, nfrags 
      do j = 1, n_avirt(i) 
         n = n + 1 
         w_frag_avirt(n) = i 
         write(6,*) n, w_frag_avirt(n) 
      enddo 
      enddo 

      write(6,*) ' VIRTB list ' 

      n = 0 
      do i = 1, nfrags 
      do j = 1, n_bvirt(i) 
         n = n + 1 
         w_frag_bvirt(n) = i 
         write(6,*) n, w_frag_bvirt(n) 
      enddo 
      enddo 


      end 
