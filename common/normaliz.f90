! ** common/normaliz.f90 >>
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

!!!---------------- Normalize deloc index
real(8) function rnormaliz(a,i)
   !Normalized multicenter bond order, see Delocalization Aromaticity Index for Large Rings DOI: 10.1039/C6CP00636A
   !When it is negative, first obtain **(1/n) using its absolute value, then multiply it by -1
   implicit none 
   real(kind=8),intent(in) :: a  
   integer     ,intent(in) :: i  
   rnormaliz = a/abs(a) * (abs(a)**(dble(1)/dble(i)))
end function rnormaliz
