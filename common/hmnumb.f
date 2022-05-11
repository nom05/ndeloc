! ** common/hmnumb.f >>
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

      subroutine hmnumb(nome,iato,ifrag,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical        debug,filex
      character*7    charint
      character*1000 nome
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
C     dimension      moc(imos)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE hmnumb:',trim(nome),iato,ifrag,d
     .ebug
      if (debug) print *,'                            nome ,iato,ifrag,d
     .ebug'
C
      iato  = 0
      ifrag = 0
      j     = 1
      charint = '       '
      if (len(trim(nome(1:index(nome,' ')))).EQ.1000) stop ' ** FIELD to
     .o long **'
      do i = 1,len(trim(nome(1:index(nome,' '))))
         if (nome(i:i).EQ.',') then
            iato  = iato+1
            ifrag = ifrag+1
            filex = .TRUE.
         else
            filex = .FALSE.
            charint(j:j) = nome(i:i)
            j = j+1
         endif !! (nome(i:i).EQ.',') then
         if (filex.OR.i.EQ.len(trim(nome(1:index(nome,' '))))) then
            j = 1
            if (debug) print *,'Processing numbers... ',trim(charint)
            if (index(trim(charint),'-').GT.0) then
               read (charint(1:index(trim(charint),'-')-1),*,
     .                                                     iostat=iii) k
                  if (iii.NE.0) stop ' ** PROBLEM while a number was rea
     .d'
                  read (charint(index(trim(charint),'-')+1:7),*,
     .                                                     iostat=iii) n
                  if (iii.NE.0) stop ' ** PROBLEM while a number was rea
     .d'
                  if (debug) print *,' range: ',k,n
                  iato = iato+n-k
               else
                  read (charint(1:7),*,iostat=iii) k
                  if (iii.NE.0) stop ' ** PROBLEM while a number was rea
     .d'
                  if (debug) print *,k
               endif !! (index(trim(charint),'-').GT.0) then
               charint = '       '
            endif !! (filex) then
         enddo !! i = 1,len(trim(nome(1:index(nome,' '))))
         ifrag = ifrag+1
         iato  =  iato+1
C
       return
       end
C=======================================================================
C

