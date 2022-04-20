C=======================================================================
C
      subroutine       looporb6(nato,nmo,nindex,
     .                    nproc,icodepar,itmo,matper,moc,s,result,debug)
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical debug
C
C=======================================================================
C
      dimension moc(*)
      dimension matper(nindex)
      dimension s(nato,nmo,nmo)
C
C=======================================================================
C
      result = 0.
C$OMP PARALLEL DO default(none) num_threads(nproc)
C$OMP&                 if(icodepar.EQ.1)
C$OMP&            private(iom,jom,kom,lom,mom,nom)
C$OMP&             shared(itmo,s,matper,moc)
C$OMP&          reduction(+:result)
      do iom = 1,itmo                         !! 1
         do jom = 1,itmo                      !! 2
            do kom = 1,itmo                   !! 3
               do lom =1,itmo                 !! 4
                  do mom =1,itmo              !! 5
                     do nom =1,itmo           !! 6
C >>>     Deloc Calculation <<<
       result    = result    
     +                        + s(matper(1),moc(iom),moc(jom)) !! 1
     +                        * s(matper(2),moc(jom),moc(kom)) !! 2
     +                        * s(matper(3),moc(kom),moc(lom)) !! 3
     +                        * s(matper(4),moc(lom),moc(mom)) !! 4
     +                        * s(matper(5),moc(mom),moc(nom)) !! 5
     +                        * s(matper(6),moc(nom),moc(iom)) !! 6
C >>> END Deloc Calculation <<<
                     enddo !! nom =1,itmo     !! 6
                  enddo !! mom =1,itmo        !! 5
               enddo !! lom =1,itmo           !! 4
            enddo !! kom = 1,itmo             !! 3
         enddo !! jom = 1,itmo                !! 2
      enddo !! iom = 1,itmo                   !! 1
C$OMP END PARALLEL DO

      return
      end
C=======================================================================
C
