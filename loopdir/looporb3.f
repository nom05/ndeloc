C=======================================================================
C
      subroutine       looporb3(idi,idel,nato,nmo,ndeloc,iperm,nindex,
     .                                    itmo,matper,moc,s,res  ,debug)
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical debug
C
C=======================================================================
C
      dimension moc(*)
      dimension matper(nindex)
      dimension res(ndeloc)
      dimension s(nato,nmo,nmo)
C
C=======================================================================
C
      do iom = 1,itmo                         !! 1
         do jom = 1,itmo                      !! 2
            do kom = 1,itmo                   !! 3
C >>>     Deloc Calculation <<<
       res(idel) = res(idel) 
     +                        + s(matper(1),moc(iom),moc(jom)) !! 1
     +                        * s(matper(2),moc(jom),moc(kom)) !! 2
     +                        * s(matper(3),moc(kom),moc(iom)) !! 3
C >>> END Deloc Calculation <<<
            enddo !! kom = 1,itmo             !! 3
         enddo !! jom = 1,itmo                !! 2
      enddo !! iom = 1,itmo                   !! 1
C
      return
      end
C=======================================================================
C
