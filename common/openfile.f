      subroutine openfile(filetext,iout,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical       debug,filex
      character*100 filetext
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
      if (debug) print *,'In SUBROUTINE openfile :',trim(filetext),iout,
     .debug
C
      inquire(file=trim(filetext),exist=filex)
      if (.NOT.filex) then
          write(*,'(2x,"requested file does not exists: ",a)') trim(file
     .text)
          call getarg (0,filetext)
          write(*,'("Usage: ",a," input-file (without extension)")') tri
     .m(filetext)
          include '../descr.h'     !! SHORT DESCRIPTION
          stop
      endif !! (filex) then
      open(unit=iout,file=trim(filetext),status='old',form='formatted')
      if (debug) print *,'+++ Opening :',trim(filetext),'+'
C
       return
       end
C=======================================================================
C

