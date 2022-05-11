! ** combinatorics/nbinom.f >> Binomial coefficient
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

      function nbinom(iup,idown,nmax,debug)
      implicit real*8 (a-h,o-z)
      logical   debug
      real*8,dimension(:,:),allocatable :: c
      allocate(c(0:nmax,0:nmax))
      if (debug) print *,'function nbinom=',iup,idown,nmax,debug
      c = 0
      c(0,0) = 1
      c(1,0) = 1
      c(1,1) = 1
      do n = 2,nmax
         c(n,0) = 1
         c(n,n) = 1
         do m = 1,n-1
            c(n,m) = c(n-1,m-1) + c(n-1,m)
            if (debug) print *,n,m,c(n,m)
         enddo !! m = 1,n-1
      enddo !! n = 2,nmax
      nbinom = int(c(iup,idown))
      if (debug) print *,'binomial coeff=',c(iup,idown)
      if (debug) print *,' ** END function nbinom **'
      return
      end
C=======================================================================
C

