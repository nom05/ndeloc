      subroutine permut(iperm,n,matper) 
C
C ... This subroutine calculates n-1 permutations without repetition
C
      integer n,i,a
      logical nextp
      dimension a(n-1),matper(iperm,n)
C
      do i = 1,n-1
         a(i) = i
      enddo !! i = 1,n
      idi = 0
   10 idi = idi+1
      do i = 1,n-1
         matper(idi,i+1) = matper(1,a(i)+1)
      enddo !! i = 1,n-1
      if (nextp(n-1,a)) goto 10
C
      return
      end
C=======================================================================
C

