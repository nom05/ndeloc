! ** common/openfile.f >>
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

      subroutine openfile(filetext,iout,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical       debug,filex
      character*100 filetext
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
C     dimension options(6)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE openfile :',trim(filetext),iout,
     .debug
C
      inquire(file=trim(filetext),exist=filex)
      if (.NOT.filex) then
          write(*,'(2x,"requested file does not exists: ",a)') trim(file
     .text)
          call getarg (0,filetext)
          write(*,'("Usage: ",a," input-file (without extension)")') tri
     .m(filetext)
          include '../descr.h'     !! SHORT DESCRIPTION
          stop
      endif !! (filex) then
      open(unit=iout,file=trim(filetext),status='old',form='formatted')
      if (debug) print *,'+++ Opening :',trim(filetext),'+'
C
       return
       end
C=======================================================================
C

