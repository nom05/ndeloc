! ** read/mwfn.f >>
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


      subroutine          mwfn(imwfn,nmo,nat,laom,lbom,iatt,iatoma ,pop
     ,                                                        ,ss,debug)
C
C Read all necessary data from a AOM.txt file
C
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical      debug,laom,lbom
      integer ndimtriang
      character*5  charint
      character*20 text
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension iatoma(iatt)
      dimension    pop(iatt)
      dimension     ss(iatt,nmo,nmo)
      real*8,dimension(:),allocatable :: arratmp
C***********************************************************************
c     rnucchar(0)= 0.0
C***********************************************************************
      if (debug) print *,'In SUBROUTINE mwfn:',imwfn,nmo,nat,laom,lbom
     ,,iatt,iatoma,'pop,ss',debug
      if (debug) print *,'                     imwfn,nmo,nat,laom,lbom
     ,,iatt,iatoma, pop,ss ,debug'

      allocate(arratmp(nmo*(nmo+1)/2))

      rewind(imwfn)
      text(:) = ''
      if (laom.AND..NOT.lbom) then
         text    = 'rlap matrix of'
      else if (.NOT.laom.AND.lbom) then
         text    = 'atrix of basin'
      else
         stop 'AOM/BOM detectation failed'
      endif !! (laom.AND..NOT.lbom) then
      ii      = len_trim(text)
      nmodim  = nmo*(nmo+1)/2
      do i = 1,iatt
         write(charint,'(I5)') iatoma(i)
         text = text(:ii)//' '//charint
         if (debug) print '("finding +",A,"+")',text
         call sudgfchk(text,20,imwfn,icde,1,jjj,debug)
         if (jjj.NE.0) stop ' ** PROBLEM while the overlap matrix was re
     .ad'
         call p1fromgauss(imwfn,nmo,arratmp(:nmo*(nmo+1)/2))
C$omp parallel default(none) shared ( nmo,ss,arratmp,i ) private (j,k)
C$omp    do
         do j = 1,nmo
            ss(i,j,j) = arratmp(j*(j+1)/2)
            do k = 1,j-1
               ss(i,j,k) = arratmp(j*(j-1)/2+k)
               ss(i,k,j) = ss(i,j,k)
            enddo !! k = 1,j-1
         enddo !! j = 1,nmo
C$omp    end do
C$omp end parallel

         if (debug) 
     .      write(*,'("# atom/basin:",I4,1X,F14.8,A,F14.8,/
     ,                                 ,17X,F14.8,A,F14.8)')
     .         i,ss(i,1  ,1),'=ss(i,1  ,1) ss(i,1  ,nmo)=',ss(i,1  ,nmo)
     .          ,ss(i,nmo,1),'=ss(i,nmo,1) ss(i,nmo,nmo)=',ss(i,nmo,nmo)
      enddo !! i = 1,iatt

      return
      end
