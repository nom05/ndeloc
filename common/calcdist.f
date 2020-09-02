      function calcdist(n,nindex,ncomb,matcomb,xx,yy,zz,debug)
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical    debug
C
C----------------------------------------------------------------------
c     common /names/  filwfn,filinp,filout,filw,filsom
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension vec(3)
      dimension matcomb(ncomb,nindex)
      dimension xx(*),yy(*),zz(*)
C=======================================================================
C
c     external ndelocma
C***********************************************************************
c     nindex = 6
C***********************************************************************
C
C >>> Geometrical center <<<
      xgc = 0
      ygc = 0
      zgc = 0
      do i = 1,nindex
         xgc = xgc + xx(matcomb(n,i))/2
         ygc = ygc + yy(matcomb(n,i))/2
         zgc = zgc + zz(matcomb(n,i))/2
      enddo !! i = 1,nindex
      calcdist = 0.0
      do i = 1,nindex
         vec(1) = xgc  - xx(matcomb(n,i))
         vec(2) = ygc  - yy(matcomb(n,i))
         vec(3) = zgc  - zz(matcomb(n,i))
         calcdist   = calcdist + sqrt(dot_product(vec,vec))/nindex
      enddo !! i = 1,nindex
C
      return
      end

