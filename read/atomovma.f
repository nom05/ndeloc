! ** read/atomovma.f >>
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


      subroutine atomovma(iint,nmo,nprim,nat,iat,iatt,pop,ss,iword,
     .                                                            debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical      debug
      character*3  chtemp
      character*20 text
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension pop(iatt)
      dimension  ss(iatt,nmo,nmo)
C***********************************************************************
c     rnucchar(0)= 0.0
C***********************************************************************
      if (debug) print *,'In SUBROUTINE atomovma :',iint,nmo,nprim,nat,i
     .at,iatt,'pop',iword,debug
      if (debug) print *,'                         ,iint,nmo,nprim,nat,i
     .at,iatt, pop ,iword,debug'
C
      text = 'RESULTS OF THE INTEG'
      call sudgfchk(text,20,iint,icde,0,jjj,debug)
      if (jjj.NE.0) stop ' ** PROBLEM while the atomic over
     .lapping matrix was read'
      if (iword.EQ.2) then
         read (iint,*) chtemp,pop(iat)
      elseif (iword.EQ.3) then
         read (iint,*) chtemp,chtemp,pop(iat)
      else
         stop '** WRONG NUMBER OF WORKS TO BE READ IN AT OV FILE **'
      endif !! (iword.EQ.2) then
      text = 'The Atomic Overlap M'
      call sudgfchk(text,20,iint,icde,1,jjj,debug)
      if (jjj.NE.0) stop ' ** PROBLEM while the atomic over
     .lapping matrix was read'
      read (iint,'(2(/),a)') chtemp
      do i=1,nmo
         read (iint,'(8e20.12)') (ss(iat,i,j),j=1,i)
         do j = 1,i
            ss(iat,j,i) = ss(iat,i,j)
         enddo !! j = 1,i
      enddo !! i=1,nmo
      if (debug) print *,'first,last atom ov mat value = ',ss(iat,1,1),s
     .s(iat,nmo,nmo)
C
      if (debug) print *,'***** END *** atomovma ***'
      return
      end

