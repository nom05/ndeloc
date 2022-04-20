      subroutine readwfn(iwfn,nmo,nprim,nat,debug,xx,yy,zz,nheavy,iq,
     .                                          pel,iheavy,luhf,lpostHF)
C
C Read wfn file
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical     debug,luhf,lpostHF
      character*10 codigo
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension xx(nat),yy(nat),zz(nat),pel(nmo)
      dimension iq(nat),iheavy(nat)
      real*8,allocatable,dimension(:) :: eom
      real*8,allocatable,dimension(:,:) :: c
      real*8,allocatable,dimension(:) :: ex
      integer,allocatable,dimension(:) :: ic,it
C***********************************************************************
      luhf    = .FALSE.
      lpostHF = .FALSE.!! Post-HF method (occ. numbers are not integers)
C***********************************************************************
      if (debug) print *,'In SUBROUTINE readwfn :',iwfn,nmo,nprim,debug,
     .'xx,yy,zz',nheavy,'iq,iheavy',luhf,'ic,it,ex,pel,c',lpostHF
      if (debug) print *,'                         iwfn,nmo,nprim,debug,
     . xx,yy,zz ,nheavy, iq,iheavy ,luhf, ic,it,ex,pel,c,lpostHF'

      allocate(eom(nmo),c(nmo,nprim),ex(nprim))
      allocate(ic(nprim),it(nprim))
C
      rewind(iwfn)
C >>> NMO,NPRIM,NAT      <<<
      read (iwfn,'(/,16x,i7,13x,i7,13x,i7)',err=52,end=52) nmo,nprim,nat
      if (debug) print *,'nmo,nprim,nat=',nmo,nprim,nat
C >>> Coordinates,charges<<<
      do i = 1,nat
         read (iwfn,'(24x,3f12.8,10x,i3)') xx(i),yy(i),zz(i),iq(i) 
      enddo !! i = 1,nat
      if (debug) print '(a,3(F9.3),I4)','coords,charge=',(xx(i),yy(i),zz
     .(i),iq(i),i=1,nat)
C >>> Detect HEAVY Atoms <<<
      nheavy = 0
      do i = 1,nat
         if (iq(i).GT.1) then
            nheavy         = nheavy+1
            iheavy(nheavy) = i
         endif !! (iq(i).GT.1) then
      enddo !! i = 1,nat
      if (debug) print *,'heavy atoms=',(iheavy(i),i=1,nheavy)
C >>> Prim centers       <<<
      read (iwfn,'(20x,20i3)',err=52,end=52) (ic(j),j=1,nprim)
c     read (iwfn,'(20x,20i3)',err=52,end=52) ((ic(j),j=k,k+19),k=1,nprim,20)
      if (debug) print *,'last prim center=',ic(nprim)
C >>> Prim types         <<<
      read (iwfn,'(20x,20i3)',err=52,end=52) (it(j),j=1,nprim)
c     read (iwfn,'(20x,20i3)',err=52,end=52) ((it(j),j=k,k+19),k=1,nprim,20)
      if (debug) print *,'last prim type  =',it(nprim)
C >>> Prim exponents     <<<
      read (iwfn,'(10x,5d14.7)',err=52,end=52) (ex(j),j=1,nprim)
c     read (iwfn,'(10x,5d14.7)',err=52,end=52) ((ex(j),j=k,k+4),k=1,nprim,5)
      if (debug) print *,'last prim expo  =',ex(nprim)
C >>> Occup n,MO E,MO Coef<<<
      if (debug) write (*,'(" MO read (first,last) :  ",$)')
      do j=1,nmo
c        read(iwfn,'(34X,F13.7)',err=52,end=52) pel(j)
         read (iwfn,'(38X,f9.7,15x,f12.7)') pel(j),eom(j)
         read (iwfn,'(5d16.8)',err=52,end=52) (c(j,m),m=1,nprim)
         if (debug) write (*,'(I5,"(",d16.8,",",d16.8,")",$)') j,c(j,1)
     .,c(j,nprim)
      enddo !! j=1,nmo
      if (debug) print '(/,a,F9.3)','last occup numb=',pel(nmo)
C >>> Detect UHF
      if (nint(pel(1)).NE.2) then
         luhf  = .TRUE.
         select case(nmo)
           case(1) !! Hydrogen
              ialfa = 1
           case(2) !! Helium
              if (eom(1).EQ.eom(2)) then !! ground state obtained as unrestricted calc
                 ialfa = 1
              else
                 ialfa = 2
              endif !! (eom(1).EQ.eom(2)) then
           case default
              do i = 2,nmo
                 if (eom(i).LT.eom(i-1)) exit
              enddo !! i = 2,nmo
              ialfa=i-1
         end select
         if (debug) write (*,'(10X,"UHF calc. ORB. E(n_alpha)=",F11.6,5
     .X,"ORB. E(n_alpha+1)=",F11.6)') eom(ialfa),eom(ialfa+1)
      endif
C >>> Detect Multideterminant wf
      lpostHF = any(ceiling(pel).GT.floor(pel))
      if (debug) PRINT *,'lpostHF(any(ceiling(pel).GT.floor(pel)))='
     ,                                                          ,lpostHF
C >>> El E, virial        <<<
      read (iwfn,'(/,a)') codigo
      backspace(iwfn)
      if (index(trim(codigo),'TOTAL').GT.0) then
         print *,'  ** G09  > A.02 **'
         read (iwfn,'(a10,5x,f22.12,18x,f13.8)',err=52,end=52) codigo
     .,enerx,virial
      else
         if (debug) print *,'  ** G09 <= A.02 **'
         read (iwfn,'(a10,7x,f20.12,18x,f13.8)',err=52,end=52) codigo
     .,enerx,virial
      endif
      if (debug) print *,'****WFN   OK****'
      goto 53
C
 52   stop 'Fatal error while reading .wfn file'
C
 53   continue
C
      if (debug) print *,'***** END *** readwfn ***'
      return
      end

