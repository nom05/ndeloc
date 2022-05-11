! ** loopdir/ndeloc2.f >>
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
      subroutine ndeloc2(cloc,deloc,ndeloc,nato,iamat  ,nmo,itmo,moc,s ,
     .                                            nproc,luhf,locc,debug)
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical        debug,luhf,locc
C
C=======================================================================
C
      dimension      iamat(*),moc(*)
      dimension      deloc(*),cloc(*)
      real*8,dimension(:,:),allocatable :: f
      dimension      s(nato,nmo,nmo)
C
C=======================================================================
C
      if (debug) print *,'In SUBROUTINE ndeloc2:','cloc',' deloc',ndeloc
     .,nato,iamat(nato),nmo,itmo,moc(itmo),'s',nproc,luhf,locc,debug
      if (debug) print *,'                         cloc ,  deloc ,ndeloc
     .,nato,iamat(nato),nmo,itmo,moc(itmo), s ,nproc,luhf,locc,debug'
C
      allocate(f(nato,nato))
      iac = 0
      f   = 0.0
C
      do iat = 1,nato
C$OMP PARALLEL DO DEFAULT(none) NUM_THREADS(nproc)
C$OMP&                        PRIVATE(jat,iom,jom)
C$OMP&           SHARED(iat,luhf,nato,itmo,s,locc)
C$OMP&                              REDUCTION(+:f)
         do jat = iat,nato
            do iom = 1,itmo
               do jom = 1,itmo
                  f(iat,jat) = f(iat,jat)+s(iat,moc(iom),moc(jom))
     .                                   *s(jat,moc(iom),moc(jom))
               enddo !! jom = 1,itmo
            enddo !! iom = 1,itmo
            f(iat,jat) = f(iat,jat)*2
            if (.NOT.luhf) f(iat,jat) = f(iat,jat)*2
            f(jat,iat) = f(iat,jat)
         enddo !! jat = iat,nato
C$OMP END PARALLEL DO
         do jat = iat+1,nato
            iac        = iac+1
            deloc(iat+(jat-1)*(jat-2)/2) = f(iat,jat)
         enddo !! jat = iat,nato-1
         f(iat,iat) = f(iat,iat)/2
         cloc(iat)  = f(iat,iat)
      enddo !! iat = 1,nato
      if (debug) then
         do iat = 1,nato
            do jat = iat,nato
               print *,'f(',iat,',',jat,')=',f(iat,jat)
            enddo !! jat = i,nato
         enddo !! iat = 1,nato
         print *,'iac=',iac,ndeloc,'=ndeloc'
      endif !! (debug) then
      if (iac.NE.ndeloc) stop ' ** An error has been found: iac != ndelo
     .c'
C
      if (debug) print *,'***** END *** ndeloc2 ***'
      return
      end
