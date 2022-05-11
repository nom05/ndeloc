! ** common/procnumb.f >>
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


      subroutine procnumb(nome,kk,imos,ifrmo,moc    ,imo,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical        debug
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
      dimension      moc(imos)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE procnumb:',trim(nome),imos,ifrmo
     .,'moc',imo,debug
      if (debug) print *,'                         trim(nome),imos,ifrmo
     ., moc ,imo,debug'
C
      nome(kk:kk) = ','
      imo = 0
      j   = 1
      do i = 1,ifrmo
         if (debug) print *,'j=',j,',j+1=',index(nome(j:kk),',')-2+j
         read (nome(j:index(nome(j:kk),',')-2+j),*,iostat=iii) 
     .                                                           charint
         if (debug) print *,charint
         if (iii.NE.0) stop ' ** PROBLEM while a number was read'
         if (index(trim(charint),'-').GT.0) then
            imo = imo+1
            read (charint(1:index(trim(charint),'-')-1),*,
     .                                            iostat=iii) moc(imo)
            if (debug) print *,imo,moc(imo)
            if (iii.NE.0) stop ' ** PROBLEM while a number was read'
            read (charint(index(trim(charint),'-')+1:7),*,
     .                                                 iostat=iii) ntemp
            if (iii.NE.0) stop ' ** PROBLEM while a number was read'
            nn = imo
            do l = 1,ntemp-moc(nn)
               imo = imo+1
               moc(imo) = moc(nn)+l
               if (debug) print *,imo,moc(imo),' gen'
            enddo !! l = 1,ntemp-iatomat(n)
         else
            imo = imo+1
            read (charint(1:7),*,iostat=iii)     moc(imo)
            if (debug) print *,imo,moc(imo)
         endif !! (index(trim(charint),'-').GT.0) then
         j = index(nome(j:kk),',')+j
      enddo !! i = 1,ifrmo
C
       return
       end
C=======================================================================
C

