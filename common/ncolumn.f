      function ncolumn(nome,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      character,intent(in):: nome*(*)
      logical debug,filex
C
C----------------------------------------------------------------------
c     include '../elements.h'
C----------------------------------------------------------------------
c     common /names/ filwfn,filinp,filout,filw,filsom
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
c     dimension  elem(18)
C----------------------------------------------------------------------
c     data elem/'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
c    .                    'Na','Mg','Al','Si','P ','S ','Cl','Ar'/
C----------------------------------------------------------------------
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In FUNCTION ncolumn: +',trim(nome),'+',debug
C
      ncolumn = 0
      filex = .FALSE. 
      j = len(trim(nome))
      do i = 1,j
         if (nome(i:i).EQ.' ') then
            if (filex) ncolumn = ncolumn+1
            filex = .FALSE. 
         else
            filex = .TRUE.
         endif !! (nome(i:i).NE.' ') then
      enddo !! i = 1,len(trim(nome))
      if (filex) ncolumn = ncolumn+1
      if (debug) print *,'ncolumn = ',ncolumn
C
      return
      end
C
C=======================================================================
