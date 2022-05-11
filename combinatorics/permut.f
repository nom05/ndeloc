! ** combinatorics/permut.f **
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

      subroutine permut(iperm,n,matper) 
C
C ... This subroutine calculates n-1 permutations without repetition
C
      integer n,i
      logical nextp
      dimension matper(iperm,n)
      integer,dimension(:),allocatable :: a

      allocate(a(n-1))
C
      do i = 1,n-1
         a(i) = i
      enddo !! i = 1,n
      idi = 0
   10 idi = idi+1
      do i = 1,n-1
         matper(idi,i+1) = matper(1,a(i)+1)
      enddo !! i = 1,n-1
      if (nextp(n-1,a)) goto 10
C
      return
      end
C=======================================================================
C

