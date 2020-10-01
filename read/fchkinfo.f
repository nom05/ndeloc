      subroutine fchkinfo(ifchk,nmo,nfunc,nat,nheavy,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical      debug,luhf
      character*20 text
      character*80 line
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension itemp(6),dipole(3),vtemp(3),uvec(9)
C***********************************************************************
      luhf = .FALSE.
C***********************************************************************
      if (debug) print *,'In SUBROUTINE fchkinfo :',ifchk,nmo,nfunc,nat,
     .nheavy,debug
C
      rewind(ifchk)
C >> Determine R/U <<
      read(ifchk,'(a)') line ! name
      read(ifchk,'(a)') line ! level + basisset ??
      if     (line(11:11).eq.'R') then
      elseif (line(11:11).eq.'U') then
             luhf  = .true.
      endif
C >> Number of basis func <<
      text = 'Number of basis func'
      call sudgfchk(text,20,ifchk,iok,0,jjj,debug)
      if (iok.eq.1) then
         backspace(ifchk)
         read (ifchk,'(49X,I12)') nfunc
      else
        stop 'FATAL ERROR: Number of basis functions not found'
      endif
C >> Number of atoms/Detect # Z > 1 <<
      text = 'Atomic numbers      '
      call sudgfchk(text,20,ifchk,iok,0,jjj,debug)
      if (iok.eq.1) then
         backspace(ifchk)
         read (ifchk,'(49X,I12)') nat
         nheavy = 0
         do i = 1,nat/6
            read (ifchk,'(6I12)') itemp
            do j = 1,6
               if (itemp(j).GT.1) nheavy = nheavy+1
            enddo !! j = 1,6
         enddo !! i = 1,nat/6
         itemp = 0
         if (mod(nat,6).GT.0) then
            read (ifchk,'(6I12)') itemp
            do j = 1,mod(nat,6)
               if (itemp(j).GT.1) nheavy = nheavy+1
            enddo !! j = 1,mod(nat,6)
         endif !! (mod(nat,6).GT.0) then
      else
         stop 'FATAL ERROR: Number of atoms not found'
      endif
C >> Number of electrons <<
      text = 'Number of electrons '
      call sudgfchk(text,20,ifchk,iok,0,jjj,debug)
      if (iok.eq.1) then
         backspace(ifchk)
         read (ifchk,'(49X,I12)') nmo
         if (.NOT.luhf) nmo = nmo/2
      else
         stop 'FATAL ERROR: Number of atoms not found'
      endif
      if (debug) print *,nmo,' MOs',nfunc,' basis funct',nat,' atoms'
C
      if (debug) print *,'***** END *** fchkinfo ***'
      return
      end
