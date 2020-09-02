
      subroutine readsom(isom,lpi,npi,moc,linear,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical       lpi,debug,linear
      character*3   textito
      character*20  text
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension moc(*)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE readsom: ',isom,lpi,npi,'moc',
     .                                             linear,debug
C
      rewind(isom)
      if (linear) then
         textito = 'eje'
      else
         textito = 'pla'
      endif !! (linear) then
      if (lpi) then
         text = 'PI   respecto al '//textito(1:3)
      else
         text = 'SIGMA respecto al '//textito(1:2)
      endif !! (lpi) then
      do i = 1,npi
            call sudgfchk(text,isom,icde,1,jjj,debug)
            if (jjj.EQ.0) then
               backspace(isom)
               read (isom,'(3X,I3)') moc(i)
            endif !! (jjj.EQ.0) then
      enddo
      if (debug) print *,'PI MOs : ',(moc(i),i=1,npi)
C
      return
      end
