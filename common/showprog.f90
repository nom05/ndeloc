! ** common/showprog.f90 >>
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


subroutine showprog(inow,nall)
   integer,intent(in):: inow,nall
   integer           :: itmp=0
   character         :: c80tmp*80
   iprog               = int(dfloat(inow)/nall*50)
   c80tmp              = '     Progress: ['
   if (iprog.GT.1) c80tmp(17:62) = repeat('-',iprog-2)
   if (mod(iprog,2).GT.0) then
      c80tmp(15+iprog:16+iprog) = ' C'
   else
      c80tmp(15+iprog:16+iprog) = 'c'
   endif !! (mod(iprog,2).GT.0) then
   c80tmp(17+iprog:62) = repeat('o',50-iprog)
   c80tmp(63:63)       = ']'
   write(c80tmp(64:),"(f8.2,' %')") dfloat(inow)/nall*100
   itmp = itmp+1
   if (itmp.EQ.1) c80tmp(79:79)='-'
   if (itmp.EQ.2) c80tmp(79:79)='\'
   if (itmp.EQ.3) c80tmp(79:79)='|'
   if (itmp.EQ.4) then
      c80tmp(79:79)='/'
      itmp=0
   endif
   write(*,'(2(A))',advance='no') trim(c80tmp),char(13)
   if (inow>=nall) write(*,*)
end subroutine showprog
