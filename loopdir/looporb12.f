! ** loopdir/looporb12.f >>
!
!  Copyright (c) 2022  Nicolás Otero Martínez - Marcos Mandado Alonso

!  This file is part of the NDELOC program available in:
!      https://github.com/nom05/ndeloc
!
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.


C=======================================================================
C
      subroutine       looporb12(nato,nmo,nindex,
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
C$OMP&            private(iom,jom,kom,lom,mom,nom,ii,ij,ik,il,im,ji)
C$OMP&             shared(itmo,s,matper,moc)
C$OMP&          reduction(+:result)
      do iom = 1,itmo                                              !! 1
         do jom = 1,itmo                                           !! 2
            do kom = 1,itmo                                        !! 3
               do lom =1,itmo                                      !! 4
                  do mom =1,itmo                                   !! 5
                     do nom =1,itmo                                !! 6
                        do ii =1,itmo                              !! 7
                           do ij =1,itmo                           !! 8
                              do ik =1,itmo                        !! 9
                                 do il =1,itmo                     !!10
                                    do im =1,itmo                  !!11
                                       do ji =1,itmo               !!12
C >>>     Deloc Calculation <<<
       result    = result    
     +                        + s(matper( 1),moc(iom) ,moc(jom) )  !! 1
     +                        * s(matper( 2),moc(jom) ,moc(kom) )  !! 2
     +                        * s(matper( 3),moc(kom) ,moc(lom) )  !! 3
     +                        * s(matper( 4),moc(lom) ,moc(mom) )  !! 4
     +                        * s(matper( 5),moc(mom) ,moc(nom) )  !! 5
     +                        * s(matper( 6),moc(nom) ,moc(ii)  )  !! 6
     +                        * s(matper( 7),moc(ii)  ,moc(ij)  )  !! 7
     +                        * s(matper( 8),moc(ij)  ,moc(ik)  )  !! 8
     +                        * s(matper( 9),moc(ik)  ,moc(il)  )  !! 9
     +                        * s(matper(10),moc(il)  ,moc(im)  )  !!10
     +                        * s(matper(11),moc(im)  ,moc(ji)  )  !!11
     +                        * s(matper(12),moc(ji)  ,moc(iom) )  !!12
C >>> END Deloc Calculation <<<
                                       enddo !! ji =1,itmo         !!12
                                    enddo !! im =1,itmo            !!11
                                 enddo !! il =1,itmo               !!10
                              enddo !! ik =1,itmo                  !! 9
                           enddo !! ij =1,itmo                     !! 8
                        enddo !! ii =1,itmo                        !! 7
                     enddo !! nom =1,itmo                          !! 6
                  enddo !! mom =1,itmo                             !! 5
               enddo !! lom =1,itmo                                !! 4
            enddo !! kom = 1,itmo                                  !! 3
         enddo !! jom = 1,itmo                                     !! 2
      enddo !! iom = 1,itmo                                        !! 1
C$OMP END PARALLEL DO

      return
      end
C=======================================================================
C
