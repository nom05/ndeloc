! PROGRAM TO TEST SORT MODULE
subroutine indexxabs(B,matsort,nA,labs,nproc,debug)
! USED MODULES
use types
use sort
use, intrinsic :: iso_fortran_env, only: int64

implicit none

integer (kind=4)                          ,intent( in) :: nA
integer (kind=int64)                                   :: nA64
integer (kind=4),            dimension(nA),intent(out) :: matsort
integer (kind=4),optional                 ,intent( in) :: nproc
type    (group ),allocatable,dimension( :)             :: A
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

    allocate (A(nA))

    nA64 = int8(nA)
    do i = 1, nA
        A(i)%order = i
    end do
    if (labsdef) then
       A%value = abs(B)
    else
       A%value = B
    endif !! (labsdef) then
    if (debug) call system_clock(count1,rate)
    call MTSort(A,nA64,"Ascending",nprocdef,debugdef)
    matsort = A%order
    if (debug) call system_clock(count2)

    if (debug) then
       PRINT *,"       First and last in sorted list"
       write (*,'(8X,2(G13.4,1X,I6))') B(A(1)%order),A(1)%order, B(A(nA)%order),A(nA)%order
       write (*,'(8X,A,2X,G12.4)') "Execution time in seconds:",real(count2-count1)/real(rate)
    endif !! (debug) then
    deallocate(A)

end subroutine indexxabs
