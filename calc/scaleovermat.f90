subroutine     scaleovermat(luhf,nmo,iato,pel,ss)

   implicit none

   integer       ,intent(in) :: nmo,iato
   integer                   :: i,j,itmp
   real( kind=8 ),intent(in) ::   pel(nmo)
   real( kind=8 )            :: moset(nmo)
   real( kind=8 )            ::    ss(iato,nmo,nmo)
   logical       ,intent(in) :: luhf

   moset = pel
   itmp  = 1
   if (.NOT.luhf) itmp = 2
!! >> Firstly, negative occupation numbers (generally very small values) will be converted to zero to avoid problems:
!$omp parallel default (none) shared(moset,ss,nmo,itmp) private(i,j)
  !$omp workshare
   where (moset<0.) moset = 0.
   moset = sqrt(moset/itmp)
  !$omp end workshare
  !$omp do
   do i = 1,nmo
     do j = 1,nmo
        ss(:,i,j) = ss(:,i,j)*moset(i)*moset(j)
     enddo !! j = 1,nmo
   enddo !! i = 1,nmo
  !$omp end do
!$omp end parallel

end subroutine scaleovermat
