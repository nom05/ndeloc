! Better OpenMP multi-threaded sort.
! Sorts derived data types
! Sorts equal parts of the input array on N number of threads.
! Uses both quicksort & insertion sort.  In quicksort, when the array size is less than a defined limit insertion sort is used
! instead, as it is faster for small arrays.
! After the initial sort, it does a bottom-up merge sort to combine the sorted chunks into a single sorted list.  The code will
! merge multiple chunks are once, and uses multiple threads for each merge.  For example, when run with 4 threads all 4 will be
! used to sort 4 chunks.  Then each set of 2 chunks will use 2 threads to merge together to form  2 larger chunks.  Finally,
! 4 threads will merge the 2 chunks together.
! Optimally uses power of 2 number of threads (2, 4, 8, etc).  The initial sort can use non-power of 2 threads, but not the merge.
! Uses double the memory of input array A as temp storage.
! Note: Subroutines use explicit size arrays instead of allocatable arrays.  The code runs faster this way.  Segmentation fault
! may occur for large arrays (i.e. 1 billion+ elements).  If a problem occurs either change to allocatable arrays or set memory
! stack size to unlimited.  In Linux the BASH command is "ulimit -s unlimited"
module types

use, intrinsic :: iso_fortran_env, only: int64, real32, int32

type group
    real (kind=real32) :: value
    integer (kind=int64) :: order
end type group

end module types

module sort

use, intrinsic :: iso_fortran_env, only: int64, real32, int32

implicit none

public :: MTSort
private :: QuickSortA, QuickSortD, InsertionSortA, InsertionSortD, Merge8A, Merge8D

contains

! Main sorting subroutine
subroutine MTSort(A, nA, direction, nt, debug)

! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use omp_lib
use types

! DUMMY ARGUMENTS
! nA:  size of input array a
! a: input data to be sorted
! direction: A = ascending, D = descending
! nt: number of threads, if omitted then uses all available
integer (kind=int64), intent(in) :: nA
type (group), dimension(nA), intent(in out) :: A
character, intent(in) :: direction
integer, optional, intent(in) :: nt

! LOCAL VARIABLES
! A2: temp storage for partially sorted copies of A.  Program toggles between using arrays A and A2 as input and output to
!   progressively sort the data.  The final sorted data is in array A.
! nt1: number of threads available.  Either a copy of input variable nt or omp_get_max_threads() if nt is not present.
! nt2: largest power of 2 number of threads (i.e. 2,4,8,16...)
! nt3: number of threads for each merge job.
! chunks: number of sort chunks for multithreaded quicksort.  Uses chunks_per_thread for each thread.  Using more smaller chunks
!      helps load balancing.
! swap_flag: flag used to toggle between array a and array a2
! t: thread number
! s, f: Chunk indexes (start and finish) for each thread used by the initial quicksort/insertion sort combo.
! i, j: loop counters to merge sorted chunks together in order For example, with 4 threads first merge chunks 1 & 2 and 3 & 4 at the
!       same time.  Next merge the previously made 1+2 chunk and 3+4 chunk to generate the completely sorted result.
! levels: Log(base 2) of nt2 (i.e 8 threads has 3 levels 2**3).  Extent of outer merge loop (level_loop).
! step: number of threads available for each merge.
! gap: number of pieces between left and right chunks to be merged.
! span: number of pieces in each left and right chunk.  As level_loop progresses each left and right chunk spans multiple chunks.
! l1, l2, r1, r2: index locations of left and right chunks to be merged.  1 is start, 2 is finish.
! left_part: temp array half the size of chunks being merged
! insert_limit: array size limit when quicksort changes to insertion sort.  50 is a good value for small data types.  As derived
!       derived type increases in bytes the insert_limit should become lower.
! chunks_per_thread: number of work chunks for each thread.  More chunks improve load balancing.  However, too many can be slow.
!       Quicksort is more efficient than then merge.  For quicksort 1 is optimal. For merge sort 64 is optimal.  Must be a power of
!       2 (1, 2, 4, 8...)
! s2, f2: start and finish of sections of array a2 to be copied into array a
! verbose: T means to output messages, F means to output no messages
type (group), dimension(:), allocatable :: A2
integer :: nt1
integer :: nt2
integer :: nt3
integer :: chunks
logical :: swap_flag
integer :: t
integer (kind=int64), dimension(:), allocatable :: s, f
integer :: i, j
integer :: levels
integer :: step
integer :: gap
integer :: span
integer (kind=int64) :: l1, l2, r1, r2
!type (group), dimension(:), allocatable :: left_part    ! temp array for left half of array to be sorted
integer, parameter :: insert_limit = 50
integer :: chunks_per_thread = 1
!integer (kind=int64) :: s2, f2
logical            :: verbose    = .true.
logical,optional   :: debug

! ABSTRACT INTERFACE (used by procedure pointer)
abstract interface ! explicit interface block to make sure procedure pointers are given correct arguments

    subroutine QuickSort_Interface(A,nA,i_limit)
        use, intrinsic :: iso_fortran_env, only: int64, real32
        use types
        integer (kind=int64), intent(in) :: nA
        type (group), dimension(nA), intent(in out) :: A
        integer, intent(in) :: i_limit
    end subroutine QuickSort_Interface

    subroutine Merge_Interface(A,nA,B,nB,C,nC,nt)
        use, intrinsic :: iso_fortran_env, only: int64
        use types
        integer (kind=int64), intent(in) :: nA, nB, nC
        type (group), dimension(nA), intent(in) :: A
        type (group), dimension(nB), intent(in) :: B
        type (group), dimension(nC), intent(out) :: C
        integer, intent(in) :: nt
    end subroutine Merge_Interface

end interface

! PROCEDURE POINTER
procedure (QuickSort_Interface), pointer :: QuickSort  ! which sort to use (ascending or descending)
procedure (Merge_Interface), pointer :: Merge8         ! which merge to use (ascending or descending)

    ! POINT TO CORRECT SORT & MERGE PROCEDURES
    if (direction == "A" .or. direction == "a") then
        QuickSort => QuickSortA
        Merge8 => Merge8A_mt
    else if (direction == "D" .or. direction == "d") then
        QuickSort => QuickSortD
        Merge8 => Merge8D_mt
    else
        write (*,*) "ERROR: Invalid sort direction: ", direction
        write (*,*) "Valid options are A and D for ascending and descending sort"
        write (*,*) "Can not sort."
        return
    end if

    if (present(debug)) verbose = debug
    ! FIND AVAILABLE THREADS
    nt1 = 1  ! default to 1 thread in not using openmp
    !$ nt1 = omp_get_max_threads()  ! get max threads available if using OpenMP
!   if (nt1 == 1) then
!       write (*,*) "WARNING: Multi-threaded sort requested, but either system has only one CPU core or OpenMP is not enabled."
!   end if
    if (present(nt)) then
         nt1 = nt
    end if

    multithread: if (nA < 10 .or. nt1 == 1) then ! Single-threaded

        if (verbose) write (*,'(15X,A)') "Single threaded"
        call QuickSort(A, nA, insert_limit)

    else multithread ! PARALLEL SORT

        ! THREAD AND CHUNK CALCULATIONS
        nt2 = 2 ** int(log(real(nt1))/log(2.0)) ! get largest power of 2 number of threads (i.e. 2,4,8,16...)
        chunks = nt2 * chunks_per_thread           ! set number of smaller work chunks to increase load balancing
        levels = nint(log(real(chunks))/log(2.0))  ! set number of levels of merging
        swap_flag = .true.
        ! Make adjustments to ensure results end in array A for 2, 8, 32... threads and avoid an extra data copy.
        if (mod(levels,2) /= 0) then
            if (chunks_per_thread == 1) then    ! Ensure results end in array A by doubling chunks and possibly doubling threads
                ! If no thread count specified then double threads if more threads available.  This may result in more than one
                ! thread per core, but execution is faster.
                if (.not. present(nt) .and. nt1 > nt2) then
                    nt2 = nt2 * 2
                    nt1 = nt2
                end if
                chunks_per_thread = chunks_per_thread * 2
                chunks = chunks * 2
                levels = levels + 1
            else
                chunks_per_thread = chunks_per_thread / 2
                chunks = chunks / 2
                levels = levels - 1
            end if
         end if
        if (verbose) then
            write (*,"(A,I3)") "Threads used =", nt1
            write (*,"(A,I5)") "Chunks =", chunks
            if (nt2 /= nt1) write (*,"(A,I3,a)") "Only efficiently using",nt2," threads."
        end if

        allocate (s(chunks),f(chunks))        ! allocate start and finish locations of chunks
        allocate (A2(nA))                     ! allocate copy of A for temporary storage in merging

        ! SORT PIECES
        !$omp parallel do &
        !$omp num_threads(nt1) &
        !$omp private(t) &
        !$omp shared(A, A2, nA, s, f, nt2, chunks, swap_flag)
        do i = 1, chunks
            s(i) = nA * (i-1) / chunks + 1     ! start
            f(i) = nA * i / chunks             ! finish
            call QuickSort(a(s(i):f(i)), &    ! section to be sorted
            &               f(i)-s(i)+1, &    ! size of section
            &               insert_limit)     ! Insertion sort limit (50 is a good)
        end do
        !$omp end parallel do

        ! MERGE SORTED CHUNKS
        level_loop: do i = levels, 1, -1
            step = 2 ** (levels - i + 1)
            gap = 2 ** (levels - i)
            span = 2 ** (levels - i) - 1
            nt3 = max(2**(levels-i+1)/chunks_per_thread,1)                ! threads available for each merge
            !$omp parallel do &
            !$omp num_threads(nt2/nt3) &
            !$omp private (l1, l2, r1, r2) &
            !$omp shared (A, A2, chunks, nt3, s, f, gap, span, step, swap_flag, nA, i)
            merge_loop: do j = 1, chunks, step
                l1 = s(j)
                l2 = f(j+span)
                r1 = s(j+gap)
                r2 = f(j+gap+span)

                ! MERGE 2 CHUNKS
                ! Merges sorted chunks.  Toggles input and output between arrays A and A2.  The duplicate memory is needed for the
                ! multithreaded merge.  Provisions are made above to ensure final output is in array A.
                if (swap_flag) then
                    call Merge8(A(l1:l2), &                 ! left part
                    &           l2-l1+1, &                  ! size of left part
                    &           A(r1:r2), &                 ! right part
                    &           r2-r1+1, &                  ! size of right part
                    &           A2(l1:r2), &                ! output array
                    &           r2-l1+1, &                  ! size of output array
                    &           nt3)                        ! number of threads
                else
                    call Merge8(A2(l1:l2), &                ! left part
                    &           l2-l1+1, &                  ! size of left part
                    &           A2(r1:r2), &                ! right part
                    &           r2-r1+1, &                  ! size of right part
                    &           A(l1:r2), &                 ! output array
                    &           r2-l1+1, &                  ! size of output array
                    &           nt3)                        ! number of threads
                end if

            enddo merge_loop
            !$omp end parallel do
            swap_flag = .not. swap_flag   ! toggle swap_flag to swap between array A and A2 as input and output arrays.
        enddo level_loop

    end if multithread

end subroutine MTSort






! Ascending Quicksort/Insertion Sort Combo for derived type
recursive subroutine QuickSortA(A,nA,limit)
! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use types

! DUMMY ARGUMENTS
integer (kind=int64), intent(in) :: nA
type (group), dimension(nA), intent(in out) :: A
integer, intent(in) :: limit

! LOCAL VARIABLES
integer (kind=int64) :: left, right
real (kind=real32) :: pivot
real (kind=real32) :: temp
type (group) :: temp2
integer (kind=int64) :: marker
integer (kind=int64) :: eighth
real (kind=real32), dimension(9) :: sample

    if (nA > 1) then
        if (nA > limit) then ! Do quicksort for large groups
            ! ************************
            ! 9-SAMPLE PIVOT METHOD
            eighth = nA/8
!            sample = a(1:nA:eighth)

            sample(1) = a(1)%value
            sample(2) = a(1 + eighth)%value
            sample(3) = a(1 + eighth*2)%value
            sample(4) = a(1 + eighth*3)%value
            sample(5) = a(1 + eighth*4)%value
            sample(6) = a(1 + eighth*5)%value
            sample(7) = a(1 + eighth*6)%value
            sample(8) = a(1 + eighth*7)%value
            sample(9) = a(nA)%value
            ! Sort Network for N=9, using Batcher's Merge-Exchange. Skip some steps because I only care about the median (5)
            if (sample(1) > sample(9)) then; temp = sample(1); sample(1) = sample(9); sample(9) = temp; end if
            if (sample(1) > sample(5)) then; temp = sample(1); sample(1) = sample(5); sample(5) = temp; end if
            if (sample(2) > sample(6)) then; temp = sample(2); sample(2) = sample(6); sample(6) = temp; end if
            if (sample(3) > sample(7)) then; temp = sample(3); sample(3) = sample(7); sample(7) = temp; end if
            if (sample(4) > sample(8)) then; temp = sample(4); sample(4) = sample(8); sample(8) = temp; end if
            if (sample(5) > sample(9)) then; temp = sample(5); sample(5) = sample(9); sample(9) = temp; end if
            if (sample(1) > sample(3)) then; temp = sample(1); sample(1) = sample(3); sample(3) = temp; end if
            if (sample(2) > sample(4)) then; temp = sample(2); sample(2) = sample(4); sample(4) = temp; end if
            if (sample(5) > sample(7)) then; temp = sample(5); sample(5) = sample(7); sample(7) = temp; end if
            if (sample(6) > sample(8)) then; temp = sample(6); sample(6) = sample(8); sample(8) = temp; end if
            if (sample(3) > sample(9)) then; temp = sample(3); sample(3) = sample(9); sample(9) = temp; end if
            if (sample(3) > sample(5)) then; temp = sample(3); sample(3) = sample(5); sample(5) = temp; end if
            if (sample(4) > sample(6)) then; temp = sample(4); sample(4) = sample(6); sample(6) = temp; end if
            if (sample(7) > sample(9)) then; temp = sample(7); sample(7) = sample(9); sample(9) = temp; end if
            if (sample(1) > sample(2)) then; temp = sample(1); sample(1) = sample(2); sample(2) = temp; end if
            if (sample(3) > sample(4)) then; temp = sample(3); sample(3) = sample(4); sample(4) = temp; end if
            if (sample(5) > sample(6)) then; temp = sample(5); sample(5) = sample(6); sample(6) = temp; end if
            if (sample(7) > sample(8)) then; temp = sample(7); sample(7) = sample(8); sample(8) = temp; end if
            if (sample(2) > sample(9)) then; temp = sample(2); sample(2) = sample(9); sample(9) = temp; end if
            if (sample(2) > sample(5)) then; temp = sample(2); sample(2) = sample(5); sample(5) = temp; end if
            if (sample(4) > sample(7)) then; temp = sample(4); sample(4) = sample(7); sample(7) = temp; end if
           !if (sample(6) > sample(9)) then; temp = sample(6); sample(6) = sample(9); sample(9) = temp; end if ! skipped
           !if (sample(2) > sample(3)) then; temp = sample(2); sample(2) = sample(3); sample(3) = temp; end if ! skipped
            if (sample(4) > sample(5)) then; temp = sample(4); sample(4) = sample(5); sample(5) = temp; end if
           !if (sample(6) > sample(7)) then; temp = sample(6); sample(6) = sample(7); sample(7) = temp; end if ! skipped
           !if (sample(8) > sample(9)) then; temp = sample(8); sample(8) = sample(9); sample(9) = temp; end if ! skipped
            pivot = sample(5)
            ! ************************
            left = 0
            right = nA + 1
            do while (left < right)
                right = right - 1
                do while (A(right)%value > pivot)
                    right = right - 1
                end do
                left = left + 1
                do while (A(left)%value < pivot)
                    left = left + 1
                end do
                if (left < right) then
                    temp2 = A(left)
                    A(left) = A(right)
                    A(right) = temp2
                end if
            end do

            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if

            call QuickSortA(A(:marker-1),marker-1,limit)
            call QuickSortA(A(marker:),nA-marker+1,limit)

        else
            call InsertionSortA(A,nA)    ! Insertion sort for small groups is faster than Quicksort
        end if
    end if

end subroutine QuickSortA

subroutine InsertionSortA(A,nA)
! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use types

! DUMMY ARGUMENTS
integer (kind=int64), intent(in) :: nA
type (group), dimension(nA), intent(in out) :: A

! LOCAL VARIABLES
type (group) :: temp
integer (kind=int64) :: i, j

    outter: do i = 2, nA
        j = i - 1
        temp = A(i)
        inner: do
            if (j == 0) exit inner
            if (a(j)%value <= temp%value) exit inner
            A(j+1) = A(j)
            j = j - 1
        end do inner
        a(j+1) = temp
    end do outter

end subroutine InsertionSortA






! Descending Quicksort/Insertion Sort Combo for derived type
recursive subroutine QuickSortD(A,nA,limit)
! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use types

! DUMMY ARGUMENTS
integer (kind=int64), intent(in) :: nA
type (group), dimension(nA), intent(in out) :: A
integer, intent(in) :: limit

! LOCAL VARIABLES
integer (kind=int64) :: left, right
real (kind=real32) :: pivot
real (kind=real32) :: temp
type (group) :: temp2
integer (kind=int64) :: marker
integer (kind=int64) :: eighth
real (kind=real32), dimension(9) :: sample

    if (nA > 1) then
        if (nA > limit) then ! Do quicksort for large groups
            ! ************************
            ! 9-SAMPLE PIVOT METHOD
            eighth = nA/8
!            sample = a(1:nA:eighth)
            sample(1) = a(1)%value
            sample(2) = a(1 + eighth)%value
            sample(3) = a(1 + eighth*2)%value
            sample(4) = a(1 + eighth*3)%value
            sample(5) = a(1 + eighth*4)%value
            sample(6) = a(1 + eighth*5)%value
            sample(7) = a(1 + eighth*6)%value
            sample(8) = a(1 + eighth*7)%value
            sample(9) = a(nA)%value
            ! Sort Network for N=9, using Batcher's Merge-Exchange. Skip some steps because I only care about the median (5)
            if (sample(1) > sample(9)) then; temp = sample(1); sample(1) = sample(9); sample(9) = temp; end if
            if (sample(1) > sample(5)) then; temp = sample(1); sample(1) = sample(5); sample(5) = temp; end if
            if (sample(2) > sample(6)) then; temp = sample(2); sample(2) = sample(6); sample(6) = temp; end if
            if (sample(3) > sample(7)) then; temp = sample(3); sample(3) = sample(7); sample(7) = temp; end if
            if (sample(4) > sample(8)) then; temp = sample(4); sample(4) = sample(8); sample(8) = temp; end if
            if (sample(5) > sample(9)) then; temp = sample(5); sample(5) = sample(9); sample(9) = temp; end if
            if (sample(1) > sample(3)) then; temp = sample(1); sample(1) = sample(3); sample(3) = temp; end if
            if (sample(2) > sample(4)) then; temp = sample(2); sample(2) = sample(4); sample(4) = temp; end if
            if (sample(5) > sample(7)) then; temp = sample(5); sample(5) = sample(7); sample(7) = temp; end if
            if (sample(6) > sample(8)) then; temp = sample(6); sample(6) = sample(8); sample(8) = temp; end if
            if (sample(3) > sample(9)) then; temp = sample(3); sample(3) = sample(9); sample(9) = temp; end if
            if (sample(3) > sample(5)) then; temp = sample(3); sample(3) = sample(5); sample(5) = temp; end if
            if (sample(4) > sample(6)) then; temp = sample(4); sample(4) = sample(6); sample(6) = temp; end if
            if (sample(7) > sample(9)) then; temp = sample(7); sample(7) = sample(9); sample(9) = temp; end if
            if (sample(1) > sample(2)) then; temp = sample(1); sample(1) = sample(2); sample(2) = temp; end if
            if (sample(3) > sample(4)) then; temp = sample(3); sample(3) = sample(4); sample(4) = temp; end if
            if (sample(5) > sample(6)) then; temp = sample(5); sample(5) = sample(6); sample(6) = temp; end if
            if (sample(7) > sample(8)) then; temp = sample(7); sample(7) = sample(8); sample(8) = temp; end if
            if (sample(2) > sample(9)) then; temp = sample(2); sample(2) = sample(9); sample(9) = temp; end if
            if (sample(2) > sample(5)) then; temp = sample(2); sample(2) = sample(5); sample(5) = temp; end if
            if (sample(4) > sample(7)) then; temp = sample(4); sample(4) = sample(7); sample(7) = temp; end if
           !if (sample(6) > sample(9)) then; temp = sample(6); sample(6) = sample(9); sample(9) = temp; end if ! skipped
           !if (sample(2) > sample(3)) then; temp = sample(2); sample(2) = sample(3); sample(3) = temp; end if ! skipped
            if (sample(4) > sample(5)) then; temp = sample(4); sample(4) = sample(5); sample(5) = temp; end if
           !if (sample(6) > sample(7)) then; temp = sample(6); sample(6) = sample(7); sample(7) = temp; end if ! skipped
           !if (sample(8) > sample(9)) then; temp = sample(8); sample(8) = sample(9); sample(9) = temp; end if ! skipped
            pivot = sample(5)
            ! ************************

            left = 0
            right = nA + 1
            do while (left < right)
                right = right - 1
                do while (A(right)%value < pivot)
                    right = right - 1
                end do
                left = left + 1
                do while (A(left)%value > pivot)
                    left = left + 1
                end do
                if (left < right) then
                    temp2 = A(left)
                    A(left) = A(right)
                    A(right) = temp2
                end if
            end do

            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if

            call QuickSortD(A(:marker-1),marker-1,limit)
            call QuickSortD(A(marker:),nA-marker+1,limit)

        else
            call InsertionSortD(A,nA)    ! Insertion sort for small groups is faster than Quicksort
        end if
    end if

end subroutine QuickSortD

subroutine InsertionSortD(A,nA)
! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use types

! DUMMY ARGUMENTS
integer (kind=int64), intent(in) :: nA
type (group), dimension(nA), intent(in out) :: A

! LOCAL VARIABLES
type (group) :: temp
integer (kind=int64) :: i, j

    outter: do i = 2, nA
        j = i - 1
        temp = A(i)
        inner: do
            if (j == 0) exit inner
            if (a(j)%value >= temp%value) exit inner
            A(j+1) = A(j)
            j = j - 1
        end do inner
        a(j+1) = temp
    end do outter

end subroutine InsertionSortD









! MULTI-THREAD MERGE
! Divides array a into nt number of parts.  Lookup matching values in array b.  Merges the parts together into new memory array c.
! Ascending sort
subroutine Merge8a_mt(A,nA,B,nB,C,nC,nt)

! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use types
use omp_lib, only: omp_get_thread_num

! DUMMY ARGUMENTS
integer (kind=int64), intent(in) :: nA, nB, nC                      ! Size of arrays.  Normal usage: nA+nB = nC
type (group), dimension(nA), intent(in) :: A     ! left part
type (group), dimension(nB), intent(in) :: B     ! right part
type (group), dimension(nC), intent(out) :: C    ! sorted output must be new memory (not overlapping a or b)
integer, intent(in) :: nt                                           ! number of threads

! LOCAL VARIABLES
integer :: t                                             ! thread number
integer (kind=int64), dimension(0:nt) :: dividerA        ! section divider of array a
integer (kind=int64), dimension(0:nt) :: dividerB        ! section divider of array b
integer (kind=int64), dimension(0:nt) :: dividerC        ! section divider of array c
real (kind=real32) :: divider_value                      ! value of section dividers
integer (kind=int64) :: nA_part, nB_part                 ! number of left and right elements of group
integer (kind=int64) :: nC_part                          ! number of sorted elements in group

t=1
!$ call omp_set_nested(.true.)

    ! initialize first and last divider
    dividerA(0) = 0
    dividerB(0) = 0
    dividerB(nt) = nB
    dividerC(0) = 0
    dividerC(nt) = nC

    !$omp parallel &
    !$omp num_threads(nt) &
    !$omp default(none) &
    !$omp private (t, divider_value, nA_part, nB_part, nC_part) &
    !$omp shared (a, nA, b, nB, c, nC, nt, dividerA, dividerB, dividerC)

    !$ t = omp_get_thread_num() + 1  ! add 1 to make first thread 1 instead of 0
    dividerA(t) = nA * t / nt   ! starting value for dividerA.  Will change if not last in group.
    if (t < nt) then
        if (a(dividerA(t))%value == a(dividerA(t)-1)%value) then
            ! Get new DividerA because starting value is in group, and not the last in the group.
            dividerA(t) = LookupAscending(a,nA,a(dividerA(t))%value) - 1
        end if
    end if
    ! POSSIBILITIES
    ! dividerA is zero: Previous groups took all A values (same number extends from one dividerA to the end of A)
    ! dividerA if equal to nA: All values after dividerA starting value are same for the rest of A.
    ! dividerA is between 0 and nA: There are values that exceed the dividerA starting value (normal operation)

    divider_value = a(dividerA(t))%value
    if (t < nt) then
        ! find closest match that is just less than divider_value in array b
        dividerB(t) = LookupAscending(b,nB,divider_value) - 1
        dividerC(t) = dividerA(t) + dividerB(t)
    end if
    !$omp barrier   ! make sure all divider values are calculated because each thread needs previous thread's divider values

    nA_part = dividerA(t) - (dividerA(t-1)+1) + 1
    nB_part = dividerB(t) - (dividerB(t-1)+1) + 1
    nC_part = dividerC(t) - (dividerC(t-1)+1) + 1

! POSSIBILITIES
! 1. nA_part == 0: Caused by dividerA being equal to nA.  This happens when all values in A beyond starting value of dividerA are
!    equal.  After first thread, copy none of A-part and all of B-part into C.  First thead does normal (3) operation with all of A
!    and a part of B (nA_part is not equal to 0 for the first thread, only for all threads greater than one)
! 2. nB_part == 0: Happens if Lookup finds no value in B that is less than divider_value in A.  Copy A-part into C.
! 3. Neither nA_part or nB_part are equal to 0.  Merge A-part and B-part (normal operation).

    if (nA_part == 0) then      ! possibility 1
        if (nB_part > 0) C(dividerC(t-1)+1:dividerC(t-1)+nB_part) = B(dividerB(t-1)+1:dividerB(t)) ! copy only B-part
    else if (nB_part == 0) then ! possibility 2
        C(dividerC(t-1)+1:dividerC(t)) = A(dividerA(t-1)+1:dividerA(t)) ! copy only A-part
    else                        ! possibility 3
        call Merge8a(   A(dividerA(t-1)+1:dividerA(t)), nA_part, &   ! A-part
        &               B(dividerB(t-1)+1:dividerB(t)), nB_part, &   ! B-part
        &               C(dividerC(t-1)+1:dividerC(t)), nC_part)     ! sorted part
    end if

    !$omp end parallel

end subroutine Merge8a_mt

! find first location where value is exceeded in array a.  Array a must be sorted in ascending order.  Uses binary search.
function LookupAscending(a,nA,value)

! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use types

! DUMMY ARGUMENTS
integer (kind=int64) :: LookupAscending,unidad=1
integer (kind=int64), intent(in) :: nA         ! Size of a
type (group), dimension(nA), intent(in) :: a
real (kind=real32), intent(in) :: value

! LOCAL VARIABLES
integer (kind=int64) :: half ! half the distance between binary searches

    if (a(1)%value > value) then
        ! Note: If using this function for other purposes, you may want to set LookupAscending to zero when the first value exceeds
        ! the value to make the result distinct from the first value being the match, but returning that value causes the
        ! multi-threaded merge to fail.
        LookupAscending = 1    ! first value exceeds
    else
        if (a(nA)%value <= value) then
            LookupAscending = nA + 1 ! last value is too small
        else    ! value is in between.  Find it.
            LookupAscending = (nA+1)/2    ! starting location in middle
            half = LookupAscending
            do
                half = (half+1) / 2
                if (a(LookupAscending)%value > value) then
                    if (a(LookupAscending-1)%value <= value) then
                        exit    ! found
                    else
                        LookupAscending = max(LookupAscending - half, unidad) ! move down half
                    end if
                else
                    LookupAscending = min(LookupAscending + half, nA) ! move up half
                end if
            end do
        end if
    end if

end function LookupAscending

! Ascending merge (merges 2 ascending sorted lists into 1 ascending sorted list)
subroutine Merge8a(a,nA,b,nB,c,nC)

! USED MODULES
use types
use, intrinsic :: iso_fortran_env, only: int64

! DUMMY ARGUMENTS
integer(kind=int64), intent(in) :: nA, nB, nC         ! Size of arrays.  Normal usage: nC = nA+nB
!   type (group), dimension(:), intent(in) :: A ! Make sure A is a copy of any subset of C or sort overwrites as it goes
!   type (group), dimension(:), intent(in) :: B        ! B overlays C(nA+1:nC)
!   type (group), dimension(:), intent(out) :: C
!    ! ***NOTE: USING EXPLICIT ARRAY SIZES AS SHOW BELOW SOMETIMES CAUSES SEGMENTATION FAULTS WITH LARGE ARRAY SIZES
!   automatic arrays are faster than assumed size arrays.  Switch to above variable declarations if problems happen.
type (group), dimension(nA), intent(in) :: A  ! left part
type (group), dimension(nB), intent(in) :: B ! right part
type (group), dimension(nC), intent(out) :: C ! output array
! Note: for single-threaded merge, such as the call from MergeSort, array a is a copy of the left part of array c.  Also, B overlays
! array c(nA+1:nC).  As multi-threaded merge array a and b are parts of the same input array.  Multi-threaded usage also requires
! array c to be different memory not overlapping arrays a or b.

! LOCAL VARIABLES
integer(kind=int64) :: i, j, k

    i = 1; j = 1; k = 1
    do
        if (i > nA .or. j > nB) exit
        if (A(i)%value <= B(j)%value) then
            C(k) = A(i)
            i = i + 1
        else
            C(k) = B(j)
            j = j + 1
        end if
        k = k + 1
    end do
    if (i <= nA) then
        C(k:) = A(i:)
        return
    end if
    if (j <= nB) C(k:) = B(j:)    ! This statement is only necessary for multi-threaded merge

end subroutine Merge8a










! MULTI-THREAD MERGE
! Divides array a into nt number of parts.  Lookup matching values in array b.  Merges the parts together into new memory array c.
! Ascending sort
subroutine Merge8d_mt(A,nA,B,nB,C,nC,nt)

! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use types
use omp_lib, only: omp_get_thread_num

! DUMMY ARGUMENTS
integer (kind=int64), intent(in) :: nA, nB, nC                      ! Size of arrays.  Normal usage: nA+nB = nC
type (group), dimension(nA), intent(in) :: A     ! left part
type (group), dimension(nB), intent(in) :: B     ! right part
type (group), dimension(nC), intent(out) :: C    ! sorted output must be new memory (not overlapping a or b)
integer, intent(in) :: nt                                           ! number of threads

! LOCAL VARIABLES
integer :: t                                             ! thread number
integer (kind=int64), dimension(0:nt) :: dividerA        ! section divider of array a
integer (kind=int64), dimension(0:nt) :: dividerB        ! section divider of array b
integer (kind=int64), dimension(0:nt) :: dividerC        ! section divider of array c
real (kind=real32) :: divider_value                      ! value of section dividers
integer (kind=int64) :: nA_part, nB_part                    ! number of left and right elements of group
integer (kind=int64) :: nC_part                          ! number of sorted elements in group

t=1
!$ call omp_set_nested(.true.)

    ! initialize first and last divider
    dividerA(0) = 0
    dividerB(0) = 0
    dividerB(nt) = nB
    dividerC(0) = 0
    dividerC(nt) = nC

    !$omp parallel &
    !$omp num_threads(nt) &
    !$omp default(none) &
    !$omp private (t, divider_value, nA_part, nB_part, nC_part) &
    !$omp shared (a, nA, b, nB, c, nC, nt, dividerA, dividerB, dividerC)

    !$ t = omp_get_thread_num() + 1  ! add 1 to make first thread 1 instead of 0
    dividerA(t) = nA * t / nt   ! starting value for dividerA.  Will change if not last in group.
    if (t < nt) then
        if (a(dividerA(t))%value == a(dividerA(t)+1)%value) then
            ! Get new DividerA because starting value is in group, and not the last in the group.
            dividerA(t) = min(LookupDescending(a, nA, a(dividerA(t))%value), nA)
        end if
    end if

    ! POSSIBILITIES
    ! dividerA is zero: Previous groups took all A values (same number extends from one dividerA to the end of A)
    ! dividerA if equal to nA: All values after dividerA starting value are same for the rest of A.
    ! dividerA is between 0 and nA: There are values that exceed the dividerA starting value (normal operation)

    divider_value = a(dividerA(t))%value
    if (t < nt) then
        ! find closest match that is just less than divider_value in array b
        dividerB(t) = LookupDescending(b, nB, divider_value)
        dividerC(t) = dividerA(t) + dividerB(t)
    end if
    !$omp barrier   ! make sure all divider values are calculated because each thread needs previous thread's divider values

    nA_part = dividerA(t) - (dividerA(t-1)+1) + 1
    nB_part = dividerB(t) - (dividerB(t-1)+1) + 1
    nC_part = dividerC(t) - (dividerC(t-1)+1) + 1

! POSSIBILITIES
! 1. nA_part == 0: Caused by dividerA being equal to nA.  This happens when all values in A beyond starting value of dividerA are
!    equal.  After first thread, copy none of A-part and all of B-part into C.  First thead does normal (3) operation with all of A
!    and a part of B (nA_part is not equal to 0 for the first thread, only for all threads greater than one)
! 2. nB_part == 0: Happens if Lookup finds no value in B that is less than divider_value in A.  Copy A-part into C.
! 3. Neither nA_part or nB_part are equal to 0.  Merge A-part and B-part (normal operation).

    if (nA_part == 0) then      ! possibility 1
        if (nB_part > 0) C(dividerC(t-1)+1:dividerC(t-1)+nB_part) = B(dividerB(t-1)+1:dividerB(t)) ! copy only B-part
    else if (nB_part == 0) then ! possibility 2
        C(dividerC(t-1)+1:dividerC(t)) = A(dividerA(t-1)+1:dividerA(t)) ! copy only A-part
    else                        ! possibility 3
        call Merge8d(   A(dividerA(t-1)+1:dividerA(t)), nA_part, &   ! A-part
        &               B(dividerB(t-1)+1:dividerB(t)), nB_part, &   ! B-part
        &               C(dividerC(t-1)+1:dividerC(t)), nC_part)     ! sorted part

    end if
    !$omp end parallel

end subroutine Merge8d_mt

! find last location where value is not exceeded in array a.  Array a must be sorted in descending order.  Uses binary search.
! group type array
function LookupDescending(a,nA,value)

! USED MODULES
use, intrinsic :: iso_fortran_env, only: int64, real32
use types

! DUMMY ARGUMENTS
integer (kind=int64) :: LookupDescending,unidad=1
integer (kind=int64), intent(in) :: nA         ! Size of a
type (group), dimension(nA), intent(in) :: a
real (kind=real32), intent(in) :: value

! LOCAL VARIABLES
integer (kind=int64) :: half ! half the distance between binary searches

    if (a(1)%value < value) then
        LookupDescending = 0     ! first value is too small
    else
        if (a(nA)%value >= value) then
            ! Note: If using this function for other purposes, you may want to set LookupAscending to nA+1 when the last value
            ! exceeds the value to make the result distinct from the last value being the match, but returning that value causes the
            ! multi-threaded merge to fail.
            LookupDescending = nA  ! last value is too big
        else    ! value is in between.  Find it.
            LookupDescending = (nA+1)/2    ! starting location in middle
            half = LookupDescending
            do
                half = (half+1) / 2
                if (a(LookupDescending)%value < value) then
                    if (a(LookupDescending-1)%value >= value) then
                        LookupDescending = LookupDescending - 1 ! go down one value to get last one that doesn't exceed value
                        exit    ! found
                    else
                        LookupDescending = max(LookupDescending - half, unidad) ! move down half
                    end if
                else
                    LookupDescending = min(LookupDescending + half, nA) ! move up half
                end if
            end do
        end if
    end if

end function LookupDescending

! Descending merge (merges 2 descending sorted lists into 1 descending sorted list)
subroutine Merge8d(a,nA,b,nB,c,nC)

! USED MODULES
use types
use, intrinsic :: iso_fortran_env, only: int64

! DUMMY ARGUMENTS
integer(kind=int64), intent(in) :: nA, nB, nC         ! Size of arrays.  Normal usage: nC = nA+nB
!   type (group), dimension(:), intent(in) :: A ! Make sure A is a copy of any subset of C or sort overwrites as it goes
!   type (group), dimension(:), intent(in) :: B        ! B overlays C(nA+1:nC)
!   type (group), dimension(:), intent(out) :: C
!    ! ***NOTE: USING EXPLICIT ARRAY SIZES AS SHOW BELOW SOMETIMES CAUSES SEGMENTATION FAULTS WITH LARGE ARRAY SIZES
!   automatic arrays are faster than assumed size arrays.  Switch to above variable declarations if problems happen.
type (group), dimension(nA), intent(in) :: A
type (group), dimension(nB), intent(in) :: B
type (group), dimension(nC), intent(out) :: C
! Note: for single-threaded merge, such as the call from MergeSort, array A is a copy of the left part of array c.  Also, B overlays
! array c(nA+1:nC).  As multi-threaded merge array a and b are parts of the same input array.  Multi-threaded usage also requires
! array c to be different memory not overlapping arrays a or b.

! LOCAL VARIABLES
integer(kind=int64) :: i, j, k

    i = 1; j = 1; k = 1
    do
        if (i > nA .or. j > nB) exit
        if (A(i)%value >= B(j)%value) then
            C(k) = A(i)
            i = i + 1
        else
            C(k) = B(j)
            j = j + 1
        end if
        k = k + 1

    end do
    if (i <= nA) then
        C(k:) = A(i:)
        return
    end if
    if (j <= nB) C(k:) = B(j:)    ! THIS STATEMENT IS NOT NECESSARY IN SINGLE THREADED MERGE

end subroutine Merge8d

end module sort
