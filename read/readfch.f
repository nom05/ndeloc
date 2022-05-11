! ** read/readfch.f >>
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



      subroutine readfch(ifch,nmo,nprim,nat,debug,xx,yy,zz,nheavy,iq,
     .                                                      luhf,iheavy)
C
C Read all necessary data from the fchk file
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical      debug,luhf
      character*20 text
      character*80 line
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension xx(*),yy(*),zz(*)
      dimension iq(*),iheavy(*)
C***********************************************************************
      luhf = .FALSE.
C***********************************************************************
      if (debug) print *,'In SUBROUTINE readfch :',ifch,nmo,nprim,nat,de
     .bug,'xx,yy,zz',nheavyt,'iq',luhf,'iheavy'
      if (debug) print *,'                         ifch,nmo,nprim,nat,de
     .bug, xx,yy,zz ,nheavyt, iq ,luhf, iheavy'
C
      nhebac = nheavy
      rewind(ifch)
C >> Read if the calc is R or U     <<
      read (ifch,'(/,a)') text
      luhf = (text(11:11).EQ.'u'.OR.text(11:11).EQ.'U')
      if (debug) print *,' ** UHF calc: ',luhf,'**'
C >> Number of atoms/Detect # Z > 1 <<
      text = 'Atomic numbers      '
      call sudgfchk(text,20,ifch ,iok,0,jjj,debug)
      if (iok.eq.1) then
         read (ifch,'(6I12)',iostat=iii) (iq(j),j=1,nat)
         if (iii.ne.0) stop ' ** PROBLEM while the fchk  file was read'
         if (debug) print *,'iq=',(iq(i),i=1,nat)
         nheavy = 0
         do j = 1,nat
            if (iq(j).GT.1) then
               nheavy         = nheavy+1
               iheavy(nheavy) = j
            endif !! (iq(j).GT.1) then
         enddo !! j = 1,nat
      else
         stop ' ** PROBLEM while the fchk  file was read'
      endif
      if (debug) print *,'iheavy=',(iheavy(i),i=1,nheavy)
      if (debug) print *,'nheavy=',nheavy,nhebac,'=nhebac'
      if(nhebac.NE.nheavy) stop ' ** PROBLEM WITH HEAVY ATOMS DETECTION'
C >> Cartesian coordinates          <<
      text = 'Current cartesian co'
      call sudgfchk(text,20,ifch ,iok,0,jjj,debug)
      if (iok.eq.1) then
         read (ifch,'(5E16.8)',iostat=iii) (xx(j),yy(j),zz(j),j=1,nat)
         if (iii.ne.0) stop ' ** PROBLEM while the fchk  file was read'
      else
         stop ' ** PROBLEM while the fchk  file was read'
      endif
      if (debug) then
         print *,trim(text)
         print '(I3,3F12.3)',(i,xx(i),yy(i),zz(i),i=1,nat)
      endif !! (debug) then
C
      if (debug) print *,'***** END *** readfch ***'
      return
      end
