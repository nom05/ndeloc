!!!---------------- Calculate factorial
      integer function ft(i)
        implicit none
        integer :: i,j
        ft = 1
        do j = 2,i
           ft = ft*j
        end do
      end function
