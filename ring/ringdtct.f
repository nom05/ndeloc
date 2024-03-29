! ** ring/ringdtct.f >>
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


!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine    ringdtct(ncomb,ncomb2,nato,nindex,xx,yy,zz,mxring,
     .                         nproc,iamat,bonddist,nring,matring,debug)
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)

      include 'omp_lib.h'

      logical        debug,IFAULT
      character*7    charintu,charintd
      character*15   charreal
C
C-----------------------------------------------------------------------
      common /ttime/  start,qtime1,qtime2,rtime1,rtime2
C=======================================================================
C
      dimension      xx(nato),yy(nato),zz(nato)
      dimension      iamat(nato)
      dimension      matring(mxring,nindex)

      real*8,allocatable,dimension(:) :: dist
      integer,allocatable,dimension(:) :: matats,indx2
      integer,allocatable,dimension(:,:) :: matcomb2
C
C=======================================================================
C
c     external       looporb6
C
C=======================================================================
C
      if (debug) print *,'In SUBROUTINE ringdtct:',ncomb,ncomb2,nato,nin
     .dex,xx(nato),yy(nato),zz(nato),mxring,bonddist,nring,'matring',deb
     .ug,nproc
      if (debug) print *,'                         ncomb,ncomb2,nato,nin
     .dex,xx(nato),yy(nato),zz(nato),mxring,bonddist,nring, matring ,deb
     .ug,nproc'

      allocate(dist(ncomb2),matats(nato),indx2(ncomb2))
      allocate(matcomb2(ncomb2,2))
C
C >>> Generate all 2-comp combs <<<
      do i = 1,nato
         matats(i) = i
      enddo !! i = 1,nato
      call       ALLNR(nato,2,matats,ncomb2,matcomb2,IFAULT)
      if (IFAULT) stop ' ** Problem while the combinations were created 
     .**'
C >>> Average dist vector<<<
      do i = 1,ncomb2
         dist(i) = calcdist(i,2,ncomb2,matcomb2,xx,yy,zz,debug)
      enddo !! i = 1,ncomb
      if (debug) then
         print '("(",2I4,F12.6,")")',((matcomb2(i,j),j=1,2),dist(i),i=
     .1,ncomb2)
      endif !! (debug) then
C >>> Sort dist vector   <<<
      qtime1 = omp_get_wtime()
c     call       indexx(ncomb2,dist,indx2)
      call indexxabs(dist,indx2,ncomb2,.FALSE.,nproc,debug)
      qtime2 = omp_get_wtime()
      write(charreal,'(F15.2)') qtime2-qtime1
      write(*,'("  >> qcksort spent ",8("."),X,a," s")')  
     .                           trim(charreal(verify(charreal,' '):15))
      if (debug) then
         print *,'++++++++++++++ SORT+++++++++++++'
         print '("(",2I4,F12.6,")")',((matcomb2(indx2(i),j),j=1,2),dist(
     .indx2(i)),i=1,ncomb2)
      endif !! (debug) then
C >>> Detect last bond   <<<
      write(charreal,'(F15.3)') bonddist
      write(*,'("  >> Bond dist cut-off ",4("."),X,a," A")')  
     .                           trim(charreal(verify(charreal,' '):15))
      nbond = 0
      do i = 1,ncomb2
         if (dist(indx2(i)).GT.bonddist) exit
         nbond = nbond+1
      enddo !! i = 1,ncomb
      write(charintu,'(I7)') nbond
      write(*,'("  >> # bonds ",14("."),X,a)') 
     .                            trim(charintu(verify(charintu,' '):7))
      write(charreal,'(F15.3)') dist(indx2(nbond))
      write(*,'("  >> Frontier dists in A ",2("."),$)')  
      do i = nbond-2,nbond+1
         write(charreal,'(F15.3)') dist(indx2(i))
         write(charintu,'(I7)') iamat(matcomb2(indx2(i),1))
         write(charintd,'(I7)') iamat(matcomb2(indx2(i),2))
         write(*,'(" d(",a,",",a,")=",a,$)')  
     .                           trim(charintu(verify(charintu,' '):7)),
     .                           trim(charintd(verify(charintd,' '):7)),
     .                           trim(charreal(verify(charreal,' '):15))
      enddo !! i = nbond-2,nbond+2
      write(charintu,'(I7)') iamat(matcomb2(indx2(nbond+2),1))
      write(charintd,'(I7)') iamat(matcomb2(indx2(nbond+2),2))
      write(charreal,'(F15.3)') dist(indx2(nbond+2))
      write(*,'(" d(",a,",",a,")=",a  )')  
     .                           trim(charintu(verify(charintu,' '):7)),
     .                           trim(charintd(verify(charintd,' '):7)),
     .                           trim(charreal(verify(charreal,' '):15))
      write(*,'(28X,"(3 last atom pairs used in ring detection process t
     .ogether with 2 first unused ones)")')
C
      rtime1 = omp_get_wtime()
      call       connect(nbond,ncomb2,ncomb,nindex,nato,matcomb2,indx2,
     .                             xx,yy,zz,mxring,nring,matring,debug)
      rtime2 = omp_get_wtime()
      write(charreal,'(F15.2)') rtime2-rtime1
      write(*,'("  >> Connec part spent ",4("."),X,a," s")')  
     .                           trim(charreal(verify(charreal,' '):15))
C
      if (debug) print *,'***** END *** ringdtct'
      return
      end

