! ** sort/indexxabs.f90 >>
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


! PROGRAM TO TEST SORT MODULE
subroutine indexxabs(B,matsort,nA,labs,nproc,debug)
! USED MODULES
!use types
!use sort
!use, intrinsic :: iso_fortran_env, only: int64
use mo_quicksort

implicit none

integer (kind=4)                          ,intent( in) :: nA
integer (kind=4),            dimension(nA),intent(out) :: matsort
integer (kind=4),optional                 ,intent( in) :: nproc
real    (kind=8),allocatable,dimension( :)             :: A
real    (kind=8),            dimension(nA),intent( in) :: B
integer                                                :: count1,count2,rate &
                                                       , i,nprocdef=1
logical         ,optional                 ,intent( in) :: debug,labs
logical                                                :: debugdef=.FALSE. &
                                                        , labsdef=.FALSE.
    if (present(debug)) debugdef = debug
    if (present(labs )) labsdef  = labs
    if (present(nproc)) nprocdef = nproc
    if (debug) PRINT *,'... in QUICKSORT(nA,abs,nproc) ...',nA,labs,nproc

    if (debug) call system_clock(count1,rate)
    allocate (A(nA))
    if (labsdef) then
       A = abs(B)
    else
       A = B
    endif !! (labsdef) then
    call qsortmp(A,matsort,nproc)
    if (debug) call system_clock(count2)

    if (debug) then
       PRINT *,"       First and last in sorted list"
       write (*,'(8X,2(G13.4,1X,I6))') B(matsort(1)),matsort(1), B(matsort(nA)),matsort(nA)
       write (*,'(8X,A,2X,G12.4)') "Execution time in seconds:",real(count2-count1)/real(rate)
    endif !! (debug) then
    deallocate(A)

end subroutine indexxabs
