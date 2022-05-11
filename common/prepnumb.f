! ** common/prepnumb.f >>
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


      subroutine prepnumb(imos,moc    ,nome,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical        debug
      character*7    charint,charintd
      character*1000 nome
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension      moc(imos)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE prepnumb:',imos,'moc,nome',debug
      if (debug) print *,'                         imos, moc,nome ,debug
     .'
C
      do i = 1,1000
         nome(i:i) = ' '
      enddo !! i = 1,1000
      j = 0
      n = 0
      do i = 1,imos
         if (moc(i)-j.NE.n) then
            if (debug) print *,moc(i),moc(i)-j,n
            n = moc(i)
            write(charint ,'(I7)') n
            if (i.NE.1) then
               if (j.EQ.1) then
                  nome = nome(1:len(trim(nome)))//
     .                                                             ','//
     .                          trim(charint (verify(charint ,' '):7))
               else
                  write(charintd,'(I7)') moc(i-1)
                  nome = nome(1:len(trim(nome)))//
     .                                                             '-'//
     .                          trim(charintd(verify(charintd,' '):7))//
     .                                                             ','//
     .                          trim(charint (verify(charint ,' '):7))
               endif !! (j.EQ.1) then
            else
                  nome = nome(1:len(trim(nome)))//
     .                              trim(charint(verify(charint,' '):7))
            endif !! (i.NE.1) then
            j = 1
            if (debug) print *,trim(nome)
         else
            j = j+1
            if (i.EQ.imos) then
                  write(charint ,'(I7)') moc(i)
                  nome = nome(1:len(trim(nome)))//
     .                                                             '-'//
     .                          trim(charint (verify(charint ,' '):7))  
            endif !! (i.EQ.imos) then
         endif !! (moc(i)-j.NE.n) then
      enddo !! i = 1,imos
      if (debug) print *,'numbers= "',trim(nome),'"'
C
      return
      end
C=======================================================================
C

