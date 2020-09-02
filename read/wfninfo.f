      subroutine wfninfo(iwfn,nmo,nprim,nat,nheavy,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical      debug
      character*80 lin1
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
c     dimension cm(3),dipole(3),vtemp(3),uvec(9)
C***********************************************************************
c     rnucchar(0)= 0.0
C***********************************************************************
      if (debug) print *,'In SUBROUTINE wfninfo :',iwfn,nmo,nprim,nat,nh
     .eavy,debug
C
      read (iwfn,'(/,16x,i7,13x,i7,13x,i7)') nmo,nprim,nat
      if (debug) print *,nmo,' MOs',nprim,' gaussians',nat,' atoms'
C >>> Coordinates,charges<<<
      nheavy = 0
      do i = 1,nat
         read (iwfn,'(70X,i3)') iq
         if (iq.GT.1) nheavy = nheavy+1
      enddo !! i = 1,nat
C
      if (debug) print *,'***** END *** wfninfo ***'
      return
      end

