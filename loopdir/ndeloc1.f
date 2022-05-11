! ** loopdir/ndeloc1.f >>
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


      subroutine ndeloc1(nindex,igoto,iout,pop,nato)
      implicit real*8 (a-h,o-z)
      character*5  numero
      character*22 formato
      dimension    pop(*)
C
      if (nindex.GT.igoto) then
         write(*,*) '  >> NOT IMPLEMENTED <<'
      else
         numero = '     '
         write(numero,'(I5)') nato
         formato='('//trim(numero(verify(numero,' '):5))//'(1X,1PE10.3))
     .'
         write(iout,'("POPULATIONS:",/)')
         write(iout,formato) (pop(i),i=1,nato)
         write(iout,'(/)')
         write(iout,'(1PE10.3)') (pop(i),i=1,nato)
      endif !! (nindex.GT.igoto) then
C 
      return
      end
