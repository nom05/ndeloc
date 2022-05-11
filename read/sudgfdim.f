! ** read/sudgfdim.f >>
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


      subroutine sudgfdim(ndim,text,iout,icde,irw,jjj,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      character line*80,text*20
      logical   debug
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
      if (debug) print *,'In SUBROUTINE sudgfchk:',ndim,'"',text,'"',iou
     .t,icde,irw,jjj,debug
      icde = 0
      jjj  = 0
      if (irw.eq.0) rewind(iout)
      do
        read (iout,'(a)',iostat=iii) line
        if (iii.ne.0) then
          jjj = 1
          return
        endif
        if (index(line,text(1:ndim)).gt.0) then
           if (debug) print *,'+',line,'+',text(1:ndim),'+'
           exit
        endif
      enddo
      icde = 1
C
      if (debug) print *,'***** END *** sudgfchk ***'
      return
      end

