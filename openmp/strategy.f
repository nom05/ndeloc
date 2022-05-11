! ** openmp/strategy.f >>
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


      subroutine strategy(nbndcen,nproc,runparal,nmo,icodepar,icolapse,
     .                                                      iperm,debug)
         implicit none
         integer,intent(in) :: nbndcen,nproc,nmo
         logical,intent(in) :: debug
         integer            :: iperm,ft,icodepar,icolapse
         logical            :: runparal
!
         iperm        = ft(nbndcen-1)
!
! Choose parallelization strategy
!  -- icodepar    = 0 -> Serial
!     icodepar    = 1 -> Giambiagi + Each addend of Ponec
!     icodepar    = 2 -> Giambiagi + Ponec permutations
!  -- icolapse    = 1 -> Nothing to do
!     icolapse    = 2 -> Collapse two loops
!
         if (nproc.EQ.1 .OR. .NOT.runparal) then
            icodepar    = 0
            icolapse    = 1
         else
            icodepar    = 1
            icolapse    = 1
            if (iperm.GE.nproc) icodepar = 2
            if (nproc.GT.nmo  ) icolapse = 2 
            if (nmo.NE.nproc.AND.real(mod(nmo,nproc)).LT.real(nproc)/2.)
     .                          icolapse = 2
         endif !! (nproc.EQ.1) then
         write(*,'("  >> Parallel. strategy ...",2(X,I1))') 
     .                                                 icodepar,icolapse
      end subroutine strategy
