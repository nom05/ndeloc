!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine ndelocma(cloc,deloc,giamb,nindex,iperm,ndeloc,nato,
     .    iamat ,bonddist,xx ,yy ,zz ,allring,nmo,matindx,lloc,itmo,moc,
     .         nproc,icodepar,icolapse,luhf,locc,lgiamb,s ,mxring,debug)
C
C I don't use icolapse. Check mulldeloc for good parallelization scheme.
C Parallelization for  2-atom indices
C Parallelization for  3-atom rings
C Parallelization for  4-atom rings
C Parallelization for  5-atom rings
C Parallelization for  6-atom rings
C Parallelization for  7-atom rings
C Parallelization for  8-atom rings
C Parallelization for  9-atom rings
C Parallelization for 10-atom rings
C Parallelization for 11-atom rings
C Parallelization for 12-atom rings
C Parallelization for 13-atom rings
C Parallelization for 14-atom rings
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical        debug,allring,lloc,IFAULT,lgiamb,luhf,locc
      character*7    charint
      character*25   formato
      integer, dimension (:,:), allocatable :: matper
C
C=======================================================================
C
      dimension         iamat(nato)         ,moc(itmo)
      dimension       initmat(nato)
      dimension       matindx(ndeloc,nindex)
      dimension      matnperm(nindex)
      dimension         deloc(ndeloc)       ,cloc(nato)
      dimension         giamb(ndeloc)
      dimension            xx(nato)         ,yy(nato)  ,zz(nato)
      dimension             s(nato,nmo,nmo)
      dimension       matring(mxring,nindex)
C
C=======================================================================
C
      if (debug) print *,'In SUBROUTINE ndelocma:','cloc,deloc,giamb',ni
     .ndex,iperm,ndeloc,nato,iamat ,bonddist,'xx ,yy zz ',allring,nmo,'m
     .atindx',lloc,itmo,moc,luhf,locc,lgiamb,'s' ,mxring,debug
      if (debug) print *,'                          cloc,deloc,giamb ,ni
     .ndex,iperm,ndeloc,nato,iamat ,bonddist, xx ,yy zz  ,allring,nmo, m
     .atindx ,lloc,itmo,moc,luhf,locc,lgiamb, s  ,mxring,debug'
C
      giamb = 0.0
      deloc = 0.0
      cloc  = 0.0
C
      if (debug) print *,'ORBS=',(moc(i),i=1,itmo)
C
C ++++++ DELOC LOOPS
      do jc = 1,nato
         initmat(jc) = jc
      enddo !! jc = 1,nato
      if (.NOT.allring) then
         call       ALLNR(nato,nindex,initmat,ndeloc,matindx,IFAULT)
         if (IFAULT) stop ' ** Problem with the atom combinations **'
      else
C >>> # 2-comp combs     <<<
         ncomb2 = nbinom(nato,2     ,nato,debug)
C >>> # n-comp combs     <<<
         ncomb  = nbinom(nato,nindex,nato,debug)
         call       ringdtct(ncomb,ncomb2,nato,nindex,xx,yy,zz,mxring,
     .                               iamat,bonddist,nring,matring,debug)
         write(charint,'(I7)') nring
         write(*,'("  >> # Detected rings ",5("."),X,A)')
     .                              trim(charint(verify(charint,' '):7))
         matindx = 0
         ndeloc  = nring
C >>> Copy detected rings
         do i = 1,nring
            do jc = 1,nindex
               matindx(i,jc) = matring(i,jc)
            enddo !! jc = 1,nindex
         enddo !! i = 1,nring
      endif !! (.NOT.allring) then
C
      if (.NOT.lgiamb) then
         write(charint,'(I7)') iperm
         write(*,'(A,A)') "  >> # Permutations ....... ",
     .                              trim(charint(verify(charint,' '):7))
      endif !! (.NOT.lgiamb) then
      allocate(matper(iperm,nindex))    !! Whims of the ifort compiler
      write(*,'(/,"  >> Computing requested delocalization indices:")')
      call          showprog(0   ,ndeloc*iperm)
      do idel = 1,ndeloc                                 !!COMBINATIONS
         icodeparb = 1
         if (debug) print *,'>>Deloc index',idel
         matper = matindx(idel,1)
         do jc = 2,nindex
            matper(1,jc) = matindx(idel,jc)
         enddo !! jc = 2,nindex
C >>> Giambiagi part  START
         idi = 1 
         do jc = 1,nindex
            matnperm(jc) = matper(idi,jc)
         enddo !! jc = 1,nindex
         call       looptree(idi,idel,nato,nmo,ndeloc,iperm,nindex,itmo,
     .                       nproc,icodeparb,matnperm,moc,s,giamb,debug)
         if (debug) print *,'giamb=',giamb(idel)
C >>> Giambiagi part  END
C >>> Ponec     part  START
         if (.NOT.lgiamb) then
            icodeparb = icodepar
            if (nproc.GT.1) nprfrag = nproc
            deloc(idel) = giamb(idel)
            call permut(iperm,nindex,matper)
            icount = 2
C$OMP PARALLEL default(none) num_threads(nproc)          
C$OMP&              if(icodeparb.EQ.2)                   
C$OMP&         private(ifrag,idi,jc,matnperm)
C$OMP&          shared(iperm,nprfrag,nindex,idel,nato,nmo,ndeloc,itmo,
C$OMP&                 nproc,icodeparb,matper,moc,s,debug,icount)
C$OMP&       reduction(+:deloc)
C
C$OMP DO
            do idi = 2,iperm   !!  !!PERMUTATIONS
               call showprog(
     .                          (idel-1)*iperm+icount-1
     ,,                         ndeloc*iperm
     .                      )
               do jc = 1,nindex
                  matnperm(jc) = matper(idi,jc)
               enddo !! jc = 1,nindex
               call looptree(idi,idel,nato,nmo,ndeloc,iperm,nindex,itmo,
     .                       nproc,icodeparb,matnperm,moc,s,deloc,debug)
               icount = icount+1
            enddo !! idi = 2,iperm   !!  !!PERMUTATIONS
C$OMP END DO
C$OMP END PARALLEL
         endif !! (.NOT.lgiamb) then
C >>> Ponec     part  END
         call       showprog(
     .                          idel
     ,,                         ndeloc
     .   )
      enddo !! idel = 1,ndeloc                           !!COMBINATIONS
C ++++++ DELOC LOOPS END
C ++++++        PART OF LOC INDICES
      if (lloc) then !! Paralelización?
         idi   = 1
         write(*,'(/,"  >> Computing requested localization indices: ")'
     .        )
         do iaa = 1,nato
            matnperm = iaa
            call    looptree(idi,iaa ,nato,nmo,nato  ,iperm,nindex,itmo,
     .                       nproc,icodeparb,matnperm,moc,s,cloc ,debug)
            call showprog(
     .                          iaa
     ,,                         nato
     .                   )
         enddo !! iaa = 1,nato
      endif !! (lloc) then
      write(*,'(/)')
C ++++++        PART OF LOC INDICES END
C
      deloc = deloc*nindex
      if (.NOT.luhf) then
C$OMP PARALLEL default(none) num_threads(nproc)          
C$OMP&          shared(deloc,cloc,giamb)
C$OMP WORKSHARE
         giamb = giamb*2
         deloc = deloc*2
          cloc =  cloc*2
C$OMP END WORKSHARE
C$OMP END PARALLEL
      endif !! (.NOT.luhf..AND..NOT.locc) then
C
C ++++++  >>> DEBUG
      if (debug) then
         do idel = 1,ndeloc      !! DELOC
C
      print *,'del(',
     .                (matindx(idel,jc),jc=1,nindex),
     .                                                  ')=',deloc(idel)
C
         enddo !! idel = 1,ndeloc
C
         if (lloc) then          !!   LOC
            do iaa = 1,nato                                       !! AT 1
C
      print *,'loc(',
     .    iaa,',',iaa,',',iaa,',',iaa,',',iaa,',',iaa,
     .                                                  ')=', cloc(iaa )
C
            enddo !! iaa = 1,nato                                 !! AT 1
         endif !! (lloc) then
      endif !! (debug) then
C ++++++  >>> DEBUG END
C
      if (.NOT.allring) then
         call       ALLNR(nato,nindex,iamat,ndeloc,matindx,IFAULT)
         if (IFAULT) stop ' ** Problem with the atom combinations **'
      else
         do i = 1,nring
            do jc = 1,nindex
               matindx(i,jc) = iamat(matring(i,jc))
            enddo !! jc = 1,nindex
         enddo !! i = 1,nring
         if (debug) print *,('matindx(',i,')',(matindx(i,j),j=1,nindex),
     .i=1,nring)
      endif !! (.NOT.allring)
      deallocate(matper)    !! Whims of the ifort compiler
C
      if (debug) print *,'***** END *** ndelocma ***'
      return
      end

