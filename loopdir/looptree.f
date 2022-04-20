      subroutine    looptree(nato,nmo,nindex,itmo,nproc
     .                      ,icodepar ,matnperm,moc,s,deloc,debug)
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical   debug
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension matnperm(nindex)
      dimension moc(itmo)
      dimension s(nato,nmo,nmo)
C
C=======================================================================
C
      external       looporb3
      external       looporb4
      external       looporb5
      external       looporb6
      external       looporb7
      external       looporb8
      external       looporb9
      external       looporb10
      external       looporb11
      external       looporb12
      external       looporb13
      external       looporb14
C
C=======================================================================
C
C***********************************************************************
c     rnucchar(0)= 0.0
C***********************************************************************
      if (debug) print *,'In SUBROUTINE looptree'
C
C NINDEX      1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 -10- 11- 12- 13- 14
      goto (101,102,103,104,105,106,107,108,109,110,111,112,113,114)
     .                                                            nindex
C
 101  continue
      stop ' ** NOT IMPLEMENTED **'
      goto 100
C
 102  continue
      stop ' ** NOT IMPLEMENTED **'
      goto 100
C
 103  continue
            call       looporb3(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 104  continue
            call       looporb4(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 105  continue
            call       looporb5(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 106  continue
            call       looporb6(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 107  continue
            call       looporb7(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 108  continue
            call       looporb8(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 109  continue
            call       looporb9(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 110  continue
            call       looporb10(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C 
 111  continue
            call       looporb11(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 112  continue
            call       looporb12(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 113  continue
            call       looporb13(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 114  continue
            call       looporb14(nato,nmo,nindex,
     .                  nproc,icodepar,itmo,matnperm,moc,s,deloc,debug)
      goto 100
C
 100  continue
      if (debug) print *,'***** END *** looptree ***'
      return
      end

