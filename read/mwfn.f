      subroutine          mwfn(imwfn,nmo,nat,iatt,iatoma ,pop,ss,debug)
C
C Read all necessary data from a AOM.txt file
C
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical      debug
      integer ndimtriang
      character*5  charint
      character*20 text
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension iatoma(iatt)
      dimension    pop(iatt),arratmp(nmo*(nmo+1)/2)
      dimension     ss(iatt,nmo,nmo)
C***********************************************************************
c     rnucchar(0)= 0.0
C***********************************************************************
      if (debug) print *,'In SUBROUTINE mwfn:',imwfn,nmo,nat,iatt,iatoma
     .,'pop,ss',debug
      if (debug) print *,'                     imwfn,nmo,nat,iatt,iatoma
     ., pop,ss ,debug'
      rewind(imwfn)
      text(:) = ''
      text    = 'lap matrix of'
      ii      = len_trim(text)
      nmodim  = nmo*(nmo+1)/2
      do i = 1,iatt
         write(charint,'(I5)') iatoma(i)
         text = text(:ii)//' '//charint//'('
         if (debug) print '("finding +",A,"+")',text
         call sudgfchk(text,imwfn,icde,1,jjj,debug)
         if (jjj.NE.0) stop ' ** PROBLEM while the atomic overlapping ma
     .trix was read'
         call p1fromgauss(imwfn,nmo,arratmp(:nmo*(nmo+1)/2))
C$omp parallel default(none) shared ( nmo,ss,arratmp,i ) private (j,k)
C$omp    do
         do j = 1,nmo
            ss(i,j,j) = arratmp(j*(j+1)/2)
            do k = 1,j-1
               ss(i,j,k) = arratmp(j*(j-1)/2+k)
               ss(i,k,j) = ss(i,j,k)
            enddo !! k = 1,j-1
         enddo !! j = 1,nmo
C$omp    end do
C$omp end parallel

         if (debug) 
     .      write(*,'("# atom/basin:",I4,1X,F14.8,A,F14.8,/
     ,                                 ,17X,F14.8,A,F14.8)')
     .         i,ss(i,1  ,1),'=ss(i,1  ,1) ss(i,1  ,nmo)=',ss(i,1  ,nmo)
     .          ,ss(i,nmo,1),'=ss(i,nmo,1) ss(i,nmo,nmo)=',ss(i,nmo,nmo)
      enddo !! i = 1,iatt

      return
      end
