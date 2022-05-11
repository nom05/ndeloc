! ** read/wfninfo.f >>
!
!  Copyright (c) 2022  Nicolás Otero Martínez - Marcos Mandado Alonso

!  This file is part of the NDELOC program available in:
!      https://github.com/nom05/ndeloc
!
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.


      subroutine wfninfo(iwfn,nmo,nprim,nat,nheavy,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical      debug
      character*80 lin1
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
c     dimension cm(3),dipole(3),vtemp(3),uvec(9)
C***********************************************************************
c     rnucchar(0)= 0.0
C***********************************************************************
      if (debug) print *,'In SUBROUTINE wfninfo :',iwfn,nmo,nprim,nat,nh
     .eavy,debug
C
      read (iwfn,'(/,16x,i7,13x,i7,13x,i7)') nmo,nprim,nat
      if (debug) print *,nmo,' MOs',nprim,' gaussians',nat,' atoms'
C >>> Coordinates,charges<<<
      nheavy = 0
      do i = 1,nat
         read (iwfn,'(70X,i3)') iq
         if (iq.GT.1) nheavy = nheavy+1
      enddo !! i = 1,nat
C
      if (debug) print *,'***** END *** wfninfo ***'
      return
      end

