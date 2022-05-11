! ** read/p1fromgauss.f90 >>
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


subroutine p1fromgauss(inp,ncf,S)

!     use commonmod ,only : ndimtriang
      
      implicit none
      integer,intent(in) :: inp
      integer,intent(in) :: ncf
      integer            :: istart,irow,l,ir,ierr
      integer,parameter  :: numcol=5
      real(kind=8)       :: S(ncf*(ncf+1)/2)
!       
      do istart = 1,ncf,numcol
        read (inp,*)
        do irow = istart,ncf
           l  = min(irow,istart+numcol-1)
           ir = irow*(irow-1)/2
           read (inp,'(6X,5(F14.8))',iostat=ierr) S(ir+istart:ir+l)
           if (ierr.NE.0) stop 'Error 1 in p1fromgauss'
        enddo !! irow = istart,ncf
      enddo !! istart = 1,ncf

end subroutine p1fromgauss
