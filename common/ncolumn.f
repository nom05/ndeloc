! ** common/ncolumn.f >>
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

      function ncolumn(nome,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      character,intent(in):: nome*(*)
      logical debug,filex
C
C----------------------------------------------------------------------
c     include '../elements.h'
C----------------------------------------------------------------------
c     common /names/ filwfn,filinp,filout,filw,filsom
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
c     dimension  elem(18)
C----------------------------------------------------------------------
c     data elem/'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
c    .                    'Na','Mg','Al','Si','P ','S ','Cl','Ar'/
C----------------------------------------------------------------------
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In FUNCTION ncolumn: +',trim(nome),'+',debug
C
      ncolumn = 0
      filex = .FALSE. 
      j = len(trim(nome))
      do i = 1,j
         if (nome(i:i).EQ.' ') then
            if (filex) ncolumn = ncolumn+1
            filex = .FALSE. 
         else
            filex = .TRUE.
         endif !! (nome(i:i).NE.' ') then
      enddo !! i = 1,len(trim(nome))
      if (filex) ncolumn = ncolumn+1
      if (debug) print *,'ncolumn = ',ncolumn
C
      return
      end
C
C=======================================================================
