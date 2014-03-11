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

c This function returns the first sensical integral value of the string, sz.
c It ignores whitespace before the first character and between the sign,
c if any, and the first digit. WARNING, it will return zero if the string
c is empty.

      integer function atoi(sz)
      implicit none

      character*(*) sz

      integer fnblnk, f_strpbrk, iachar

      integer ndx, iLength
      integer iInt, iTmp
      logical bNeg, bTmp

c   o quit if the string is empty
      ndx = fnblnk(sz)
      if (ndx.eq.0) then
         atoi = 0
         return
      end if
      iLength = len(sz)

c   o iInt will be the local atoi value
      iInt = 0

c   o record a sign
      bNeg = (sz(ndx:ndx).eq.'-')
      iTmp = f_strpbrk('-+',sz(ndx:ndx))
      if (iTmp.gt.0) then
         if (ndx.eq.iLength) then
            atoi = 0
            return
         end if
         iTmp = fnblnk(sz(ndx+1:))
         ndx  = ndx + iTmp
      end if

c   o scan the digits
      bTmp = .true.
      do while (bTmp)
         iTmp = iachar(sz(ndx:ndx)) - 48
         if (iTmp.ge.0.and.iTmp.le.9) then
            iInt = 10*iInt + iTmp
            ndx  = ndx + 1
            bTmp = (ndx.le.iLength)
         else
            bTmp = .false.
         end if
      end do

C   o negate the result if necessary and assign atoi
      if (bNeg) iInt = -iInt
      atoi = iInt

      return
      end

