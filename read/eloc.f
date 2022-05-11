! ** read/eloc.f >>
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


      subroutine          eloc(ieloc,nmo,nat,iatt,iatoma ,pop,ss,debug)
C
C Read all necessary data from a eloc file
C
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical      debug
      character*4  charint,charintd
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
      real*8,dimension(:),allocatable :: ptmp
C***********************************************************************
c     rnucchar(0)= 0.0
C***********************************************************************
      if (debug) print *,'In SUBROUTINE eloc:',ieloc,nmo,nat,iatt,iatoma
     .,'pop,ss',debug
      if (debug) print *,'                     ieloc,nmo,nat,iatt,iatoma
     ., pop,ss ,debug'

      allocate(ptmp(nat))
C
      rewind(ieloc)
      text = 'orb1,orb2 = '
      ilong = 12
      do iom = 1,nmo
         write(charint,'(I4)') iom
         if (debug) print '("orb ",A," >> ",2X,$)',trim(charint)
         do jom = 1,iom
c        do jom = 1,nmo
            write(charintd,'(I4)') jom
            text = text(1:ilong)//trim(charint)//trim(charintd)
            if (debug) print '("+",A,"+",$)',text
            call sudgfchk(text,len_trim(text),ieloc,icde,1,jjj,debug)
            if (jjj.NE.0) stop ' ** PROBLEM while the atomic overlapping
     . matrix was read'
            read (ieloc,'(/,36X,I12)') nmotmp
            if (nmotmp.NE.nmo) then
               write (*,'(A," (nmo from eloc = ",I5," != ",I5," = nmo)")
     .') ' ** PROBLEM while the atomic overlapping matrix was read',nmot
     .mp,nmo
               stop
            endif !! (nmotmp.NE.nmo) then
c           text = '   #  Z    TR(Pat*S)'
c           call sudgfchk(text,20,ieloc,icde,1,jjj,debug)
c           if (jjj.NE.0) stop ' ** PROBLEM while the atomic overlapping
c    . matrix was read'
c           do inat = 1,nat
c              read (ieloc,'(9X,F14.10)') ptmp(inat)
c           enddo !! inat = 1,nat
c           do inat = 1,iatt
c              pop(inat) = ptmp(iatoma(inat))
c           enddo !! inat = 1,iatt
            text = 'corrected stockholde'
            call sudgfchk(text,20,ieloc,icde,1,jjj,debug)
            if (jjj.NE.0) stop ' ** PROBLEM while the atomic overlapping
     . matrix was read'
            read (ieloc,'(A)') charintd
            do inat = 1,nat
               read (ieloc,'(41X,E25.12)') ptmp(inat)
            enddo !! inat = 1,nat
            do inat = 1,iatt
               ss(inat,iom,jom) = -ptmp(iatoma(inat))
               ss(inat,jom,iom) =  ss(inat,iom,jom)
            enddo !! inat = 1,iatt
            if (debug) print *,' ++>> ss(1,',iom,jom,')= ',ss(1,iom,jom)
            text = 'orb1,orb2 = '
         enddo !! jom = 1,nmo
      enddo !! iom = 1,nmo
      if (debug) print *,' <*> eloc finished'
      if (debug) then
         do i = 1,iatt
            print *,'first,last atom ov mat value = ',ss(i,1,1),ss(i,nmo
     .,nmo)
         enddo !! i = 1,iatt
      endif !! (debug) then
C
      if (debug) print *,'***** END *** eloc     ***'
C
      return
      end

