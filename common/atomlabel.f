      function atomlabel(iato,nato,iq,nzeros,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical       debug
      character*7   charint
      character*10  formatnu
      character*15  atomlabel,numero
C
C----------------------------------------------------------------------
      include '../elements.h'
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
      if (debug) print *,'In FUNCTION atomlabel:',iato,nato,iq,nzeros,de
     .bug
C
      if (iq.LT.0) stop ' ** Z < 1 or not basin !! ** ' !! zero for basins
      rfact = 10.0**nzeros
c1    if (nato.GE.int(rfact)) then
c        rfact  = rfact*10.0
c        nzeros = nzeros+1
c        goto 1
c     endif !! (nato.GE.int(rfact)) then
      atomlabel(:) = ''
      numero(:)    = ''
      write(charint,'(I7)') nzeros+2
      formatnu  = '(F'//trim(charint(verify(charint,' '):7))
      write(charint,'(I7)') nzeros
      formatnu  = trim(formatnu)//'.'//
     .                         trim(charint(verify(charint,' '):7))//')'
      if (debug) print *,'FORMAT FOR ATOMLABEL = ',formatnu,' ELEMENT = 
     .'                                                        ,elem(iq)
      write(numero,formatnu) real(iato)/rfact
      itemp=verify(numero,' ',.TRUE.)
      atomlabel = elem(iq)(1:index(elem(iq),' ')-1)//trim(numero(scan(nu
     .mero,'.')+1:itemp))
      if (debug) print *,atomlabel
C
      return
      end
C=======================================================================
C

