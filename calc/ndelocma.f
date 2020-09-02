!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine ndelocma(cloc,deloc,giamb,nindex,iperm,ndeloc,nato,
     .    iamat ,bonddist,xx ,yy ,zz ,allring,nmo,matindx,lloc,itmo,moc,
     .              nproc,icodepar,icolapse,luhf,lgiamb,s ,mxring,debug)
C
C I don't use icolapse. Check mulldeloc for good parallelization scheme.
C Parallelization for 5-atom rings
C Parallelization for 6-atom rings
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical        debug,allring,lloc,IFAULT,lgiamb,luhf
      character*7    charint
      character*25   formato
      character*100  text
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
     .atindx',lloc,itmo,moc,luhf,lgiamb,'s' ,mxring,debug
      if (debug) print *,'                          cloc,deloc,giamb ,ni
     .ndex,iperm,ndeloc,nato,iamat ,bonddist, xx ,yy zz  ,allring,nmo, m
     .atindx ,lloc,itmo,moc,luhf,lgiamb, s  ,mxring,debug'
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
      text = ' ** Percent complete for delocalization indices ... '
      allocate(matper(iperm,nindex))    !! Whims of the ifort compiler
      do idel = 1,ndeloc                                 !!COMBINATIONS
         icodeparb = 1
         if (debug) print *,'>>Deloc index',idel
         matper = matindx(idel,1)
         do jc = 2,nindex
            matper(1,jc) = matindx(idel,jc)
         enddo !! jc = 2,nindex
C >>> Giambiagi part  START
         call       npercent(0   ,ndeloc,text,len(trim(text)))
         idi = 1 
         do jc = 1,nindex
            matnperm(jc) = matper(idi,jc)
         enddo !! jc = 1,nindex
         call       looptree(idi,idel,nato,nmo,ndeloc,iperm,nindex,itmo,
     .                       nproc,icodeparb,matnperm,moc,s,giamb,debug)
         call          npercent((idel-1)*iperm+1         ,
     .                                                     ndeloc*iperm,
     .                                             text,len(trim(text)))
         if (debug) print *,'giamb=',giamb(idel)
C >>> Giambiagi part  END
C >>> Ponec     part  START
         if (.NOT.lgiamb) then
            nprfrag   = 10
            icodeparb = icodepar
            if (nproc.GT.1) nprfrag = nproc
            deloc(idel) = giamb(idel)
            call permut(iperm,nindex,matper)
C OMP PARALLEL default(none) num_threads(nproc)          
C OMP&              if(icodeparb.EQ.2)                   
C OMP&         private(ifrag,idi,jc,matnperm)
C OMP&          shared(iperm,nprfrag,nindex,idel,nato,nmo,ndeloc,itmo,
C OMP&                 nproc,icodeparb,matper,moc,s,debug)
C OMP&       reduction(+:deloc)
C
C OMP DO
            do ifrag = 1,(iperm-1)/nprfrag
               do idi = 2+(ifrag-1)*nprfrag,ifrag*nprfrag+1
C                                                        !!PERMUTATIONS
                  do jc = 1,nindex
                     matnperm(jc) = matper(idi,jc)
                  enddo !! jc = 1,nindex
               call looptree(idi,idel,nato,nmo,ndeloc,iperm,nindex,itmo,
     .                       nproc,icodeparb,matnperm,moc,s,deloc,debug)
               enddo !! idi = 2+(ifrag-1)*nprfrag,ifrag*nprfrag+1
               call    npercent((idel-1)*iperm+ifrag*nprfrag+1,
     .                                                     ndeloc*iperm,
     .                                             text,len(trim(text)))
            enddo !! ifrag = 1,(iperm-1)/nprfrag
C OMP END DO
C
C OMP DO
            do idi = ((iperm-1)/nprfrag)*nprfrag+2,
     .                ((iperm-1)/nprfrag)*nprfrag+mod(iperm-1,nprfrag)+1
               do jc = 1,nindex
                  matnperm(jc) = matper(idi,jc)
               enddo !! jc = 1,nindex
               call looptree(idi,idel,nato,nmo,ndeloc,iperm,nindex,itmo,
     .                       nproc,icodeparb,matnperm,moc,s,deloc,debug)
            enddo !! idi = ifrag*10+1,ifrag*10+mod(iperm,10)
C OMP END DO
C OMP END PARALLEL
         endif !! (.NOT.lgiamb) then
C >>> Ponec     part  END
         call          npercent( idel                    ,
     .                                                     ndeloc      ,
     .                                             text,len(trim(text)))
      enddo !! idel = 1,ndeloc                           !!COMBINATIONS
C ++++++ DELOC LOOPS END
C ++++++        PART OF LOC INDICES
      if (lloc) then
         text = ' ** Percent complete for localization indices ... '
         idi   = 1
         do iaa = 1,nato
            matnperm = iaa
            call    looptree(idi,iaa ,nato,nmo,nato  ,iperm,nindex,itmo,
     .                       nproc,icodeparb,matnperm,moc,s,cloc ,debug)
            call    npercent(iaa ,nato  ,text,len(trim(text)))
         enddo !! iaa = 1,nato
      endif !! (lloc) then
      write(*,'(/)')
C ++++++        PART OF LOC INDICES END
C
      deloc = deloc*nindex
      if (.NOT.luhf) then
         deloc = deloc*2
          cloc =  cloc*2
      endif !! (.NOT.luhf) then
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

