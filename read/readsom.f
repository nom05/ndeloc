! ** read/readsom.f >>
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



      subroutine readsom(isom,lpi,npi,moc,linear,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical       lpi,debug,linear
      character*3   textito
      character*20  text
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension moc(*)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE readsom: ',isom,lpi,npi,'moc',
     .                                             linear,debug
C
      rewind(isom)
      if (linear) then
         textito = 'eje'
      else
         textito = 'pla'
      endif !! (linear) then
      if (lpi) then
         text = 'PI   respecto al '//textito(1:3)
      else
         text = 'SIGMA respecto al '//textito(1:2)
      endif !! (lpi) then
      do i = 1,npi
            call sudgfchk(text,20,isom,icde,1,jjj,debug)
            if (jjj.EQ.0) then
               backspace(isom)
               read (isom,'(3X,I3)') moc(i)
            endif !! (jjj.EQ.0) then
      enddo
      if (debug) print *,'PI MOs : ',(moc(i),i=1,npi)
C
      return
      end
