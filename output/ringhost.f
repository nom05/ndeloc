C-----------------------------------------------------------------------
C
      subroutine ringhost(nat,ntot   ,nval  ,nindex,xx,yy,zz,matindx
     .                                                      xyzgc,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical     debug
c     character*3 atemp
C
C----------------------------------------------------------------------
c     common /names/  filwfn,filinp,filout,filw,filsom
C----------------------------------------------------------------------
      include '../cnstants.h'    !! Constants
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension        xx(nat)        , yy(nat) , zz(nat)
      dimension     xyzgc(3,nval)
      dimension   matindx(ntot,nindex)
C=======================================================================
C
C***********************************************************************
c     lloc = .FALSE.
C***********************************************************************
      if (debug) print *,'In SUBROUTINE ringhost:',nat,ntot   ,nval  ,ni
     .ndex,'xx,yy,zz ,matindx, xyzgc',debug
      if (debug) print *,'                         nat,ntot   ,nval  ,ni
     .ndex, xx,yy,zz ,matindx, xyzgc ,debug'
C
C >> CALC Geometrical center            :
      xyzgc = 0.0
      do i = 1,nval
         do j = 1,nindex
            xyzgc(1,i) = xyzgc(1,i)+xx(matindx(i,j))
            xyzgc(2,i) = xyzgc(2,i)+yy(matindx(i,j))
            xyzgc(3,i) = xyzgc(3,i)+zz(matindx(i,j))
         enddo !! j = 1,nindex
      enddo !! i = 1,nval
      xyzgc = xyzgc*bohr2a/nindex
      if (debug) then
         do i = 1,nval
            print '(A,I4,A,3(F10.4))','Geom c',i,'=',
     .                                                (xyzgc(j,i),j=1,3)
         enddo !! i = 1,nindex
      endif !! (debug) then
C
      if (debug) print *,'***** END *** ringhost ***'
      return
      end
C=======================================================================
C
