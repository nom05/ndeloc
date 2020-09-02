      subroutine ndeloc1(nindex,igoto,iout,pop,nato)
      implicit real*8 (a-h,o-z)
      character*5  numero
      character*22 formato
      dimension    pop(*)
C
      if (nindex.GT.igoto) then
         write(*,*) '  >> NOT IMPLEMENTED <<'
      else
         numero = '     '
         write(numero,'(I5)') nato
         formato='('//trim(numero(verify(numero,' '):5))//'(1X,1PE10.3))
     .'
         write(iout,'("POPULATIONS:",/)')
         write(iout,formato) (pop(i),i=1,nato)
         write(iout,'(/)')
         write(iout,'(1PE10.3)') (pop(i),i=1,nato)
      endif !! (nindex.GT.igoto) then
C 
      return
      end
