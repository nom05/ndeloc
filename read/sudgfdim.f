      subroutine sudgfdim(ndim,text,iout,icde,irw,jjj,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      character line*80,text*20
      logical   debug
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
C     dimension options(6)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE sudgfchk:',ndim,'"',text,'"',iou
     .t,icde,irw,jjj,debug
      icde = 0
      jjj  = 0
      if (irw.eq.0) rewind(iout)
      do
        read (iout,'(a)',iostat=iii) line
        if (iii.ne.0) then
          jjj = 1
          return
        endif
        if (index(line,text(1:ndim)).gt.0) then
           if (debug) print *,'+',line,'+',text(1:ndim),'+'
           exit
        endif
      enddo
      icde = 1
C
      if (debug) print *,'***** END *** sudgfchk ***'
      return
      end

