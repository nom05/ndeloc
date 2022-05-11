! ** output/ringhost.f >>
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


C-----------------------------------------------------------------------
C
      subroutine ringhost(nat,ntot   ,nval  ,nindex,xx,yy,zz,matindx
     .                                                      xyzgc,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical     debug
c     character*3 atemp
C
C----------------------------------------------------------------------
c     common /names/  filwfn,filinp,filout,filw,filsom
C----------------------------------------------------------------------
      include '../cnstants.h'    !! Constants
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension        xx(nat)        , yy(nat) , zz(nat)
      dimension     xyzgc(3,nval)
      dimension   matindx(ntot,nindex)
C=======================================================================
C
C***********************************************************************
c     lloc = .FALSE.
C***********************************************************************
      if (debug) print *,'In SUBROUTINE ringhost:',nat,ntot   ,nval  ,ni
     .ndex,'xx,yy,zz ,matindx, xyzgc',debug
      if (debug) print *,'                         nat,ntot   ,nval  ,ni
     .ndex, xx,yy,zz ,matindx, xyzgc ,debug'
C
C >> CALC Geometrical center            :
      xyzgc = 0.0
      do i = 1,nval
         do j = 1,nindex
            xyzgc(1,i) = xyzgc(1,i)+xx(matindx(i,j))
            xyzgc(2,i) = xyzgc(2,i)+yy(matindx(i,j))
            xyzgc(3,i) = xyzgc(3,i)+zz(matindx(i,j))
         enddo !! j = 1,nindex
      enddo !! i = 1,nval
      xyzgc = xyzgc*bohr2a/nindex
      if (debug) then
         do i = 1,nval
            print '(A,I4,A,3(F10.4))','Geom c',i,'=',
     .                                                (xyzgc(j,i),j=1,3)
         enddo !! i = 1,nindex
      endif !! (debug) then
C
      if (debug) print *,'***** END *** ringhost ***'
      return
      end
C=======================================================================
C
