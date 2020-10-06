!!!---------------- Normalize deloc index
real(8) function rnormaliz(a,i)
   !Normalized multicenter bond order, see Delocalization Aromaticity Index for Large Rings DOI: 10.1039/C6CP00636A
   !When it is negative, first obtain **(1/n) using its absolute value, then multiply it by -1
   implicit none 
   real(kind=8),intent(in) :: a  
   integer     ,intent(in) :: i  
   rnormaliz = a/abs(a) * (abs(a)**(dble(1)/dble(i)))
end function rnormaliz
