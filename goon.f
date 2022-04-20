C-----------------------------------------------------------------------
C
      subroutine goon(iwfn,iout,iinp,isom,nmo,nprim,nat,allmo,iato,iint,
     .     allatoms,allheavy,outersh,lpi,npi,lloc,nindex,ndeloc,inpline,
     .version,nheavy,itype,bonddist,mxring,allring,genint,intfuk,lgiamb,
     .  runparal,nproc,lsigma,loutsig,imos,ifrmo,ifrag,ixyz,lxyz,nzeros,
     .           locc,nbasin,laom,lbom,lmwfnrdname,lmwfn,linear,laimall,
     .                                    nfpi,lpostHF,moldentype,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical        debug,filex,lloc,luhf,lxyz,lpostHF,runparal
      logical        genint,intfuk,laimall,lmwfn,lmwfnrdname,lfixbug
      logical        allmo,lpi,outersh,lsigma,loutsig,linear,locc
      logical        allatoms,allheavy,allring,lgiamb,laom,lbom
      integer        itype !! =0 wfn, =1 fchk, =2 molden
      character*3    atemp
      character*7    version,charint
      character*15   charreal,atomlabel
      character*100  filwfn,filinp,filout,filw,filsom,command,filouw
      character*1000 nome
      integer,dimension (:)    , allocatable :: mocred
      real*8, dimension (:)    , allocatable :: pel
      real*8, dimension (:,:)  , allocatable :: xyzgc
      real*8, dimension (:,:,:), allocatable :: ss  !! Intel compiler quirks??
C
C----------------------------------------------------------------------
      common /names/  filwfn,filinp,filout,filw,filsom,filouw
C----------------------------------------------------------------------
      include 'cnstants.h' !! Constants
      include 'elements.h'
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      integer,dimension(:),allocatable :: iq,iheavy,iatomat,moc
      integer,dimension(:,:),allocatable :: matindx
      real*8,dimension(:),allocatable :: pop,cloc,deloc,giamb,xx,yy,zz
     .                                 , xxr,yyr,zzr
      character(len=100),dimension(:),allocatable :: filint
      character(len=7),dimension(:),allocatable :: atoms
C=======================================================================
C
      external ndeloc1
      external ndeloc2
C***********************************************************************
c     lloc = .FALSE.
C***********************************************************************
      allocate(iq(nat),iheavy(nat),iatomat(iato),matindx(ndeloc,nindex))
      allocate(pop(iato),cloc(iato),deloc(ndeloc),giamb(ndeloc))
      allocate(xx(nat),yy(nat),zz(nat),xxr(nat),yyr(nat),zzr(nat))
      allocate(filint(iato),atoms(iato))
      allocate(moc(nmo))          !! Array with the MOs used for the
                                  !! calculations.
      if (debug) print *,'In SUBROUTINE goon :',iwfn,iout,iinp,isom,nmo,
     .nprim,nat,allmo,iato,iint,allatoms,allheavy,outersh,lpi,npi,locc,l
     .loc,nindex,ndeloc,inpline,nheavy,itype,bonddist,mxring,allring,gen
     .int,intfuk,runparal,nproc,lsigma,loutsig,imos,ifrmo,ifrag,ixyz,lxy
     .z,nzeros,laimall,nbasin,laom,lbom,lmwfnrdname,lmwfn,linear,nfpi,lp
     .ostHF,moldentype,debug
      if (debug) print *,'                      iwfn,iout,iinp,isom,nmo,
     .nprim,nat,allmo,iato,iint,allatoms,allheavy,outersh,lpi,npi,locc,l
     .loc,nindex,ndeloc,inpline,nheavy,itype,bonddist,mxring,allring,gen
     .int,intfuk,runparal,nproc,lsigma,loutsig,imos,ifrmo,ifrag,ixyz,lxy
     .z,nzeros,laimall,nbasin,laom,lbom,lmwfnrdname,lmwfn,linear,nfpi,lp
     .ostHF,moldentype,debug'
C
C >>> Read wavefunction file   <<<
      select case(itype)
        case(0)
          allocate(pel(nmo))
          call    readwfn(iwfn,nmo,nprim,nat,debug,xx,yy,zz,nheavy,iq,
     .                                          pel,iheavy,luhf,lpostHF)
        case(1)
          call    readfch(iwfn,nmo,nprim,nat,debug,xx,yy,zz,nheavy,iq,
     .                                                      luhf,iheavy)
        case(2)
          allocate(pel(nmo))
          call   readmold(iwfn,moldentype,nmo,nprim,nat,debug,xx,yy,zz,
     .                                nheavy,iq,pel,iheavy,luhf,lpostHF)
      end select !! case(itype)

C >>> Read  atom/basin matrix  <<<
      rewind(iinp)
      if (.NOT.allatoms.AND..NOT.allheavy) then
         if (debug) print *,' -> from input file'
         do i = 1,inpline-2
            read (iinp,*) atemp
         enddo !! i = 1,inpline
         read (iinp,'(A)') nome
         if (debug) print *,'text line= "',trim(nome),'"'
         if (len(trim(nome)).EQ.1000) stop ' ** Line too long !!! **'
         j = 1
         if (allring) then !! Disabled for basins
            k = index(nome(j:len(trim(nome))),' ')
         else
            k = len(trim(nome))+1
         endif !! (allring) then
         if (debug) print *,'# characters=',k,len(trim(nome))
         call    procnumb(nome,k ,iato,ifrag,iatomat,n  ,debug)
C >>> Check atom matrix  <<<
         if (minval(iatomat(:iato)).LT.1) stop 'Basin or atom specificat
     .ion cannot be neither zero nor negative.'
         if (laom.AND..NOT.lbom) then
            do i = 1,iato
               if (iatomat(i).GT.nat) 
     .          stop ' ** PROBLEM while the atom specification was read'
            enddo !! i = 1,iato
         else if (lbom.AND..NOT.laom) then
            if (nbasin.LT.maxval(iatomat(:iato))) stop '# basins < the m
     .ax basin label'
            if (minval(iatomat(:iato)).LT.1) stop 'Basins cannot be
     .neither zero nor negative.'
         else
            stop 'AOM/BOM detectation failed'
         endif !! (laom.AND..NOT.lbom) then
      else
         do i = 1,inpline-1
            read (iinp,*) atemp
         enddo !! i = 1,inpline
         if (allatoms) then
            if (debug) print *,' -> all atoms or basins'
            do i = 1,iato
               iatomat(i) = i
            enddo !! i = 1,nat
         elseif (allheavy) then
            if (debug) print *,' -> all heavy atoms'
            do i = 1,iato
               iatomat(i) = iheavy(i)
            enddo !! i = 1,nat
         else
            stop ' ** Unexpected error **'
         endif !! (allatoms) then
      endif !! (.NOT.allatoms.AND..NOT.allheavy) then
      if (debug) print *,'iatomat=',iatomat
      write(*,'("  >> POPULATIONS ..........",$)') 
      read (iinp,*,iostat=iii) atemp
      if (iii.NE.0) stop ' ** Unexpected End-Of-File error **'
C >>> Atom labels        <<<
      do i = 1,iato
         atoms(i) = atomlabel(iatomat(i),nat ,iq(iatomat(i)),nzeros
     .                                                           ,debug)
      enddo !! i = 1,iato
C >>> Read over. matrix  <<<
      allocate(ss(iato,nmo,nmo))
      if (.NOT.genint.AND..NOT.intfuk) then        !! >>>>>>>>>>>>> READ
         do i = 1,iato
            read (iinp,'(a)',iostat=iii) nome
            if (iii.ne.0) stop 'File with atomic overlap matrix not foun
     .d'
            if (debug) print *,trim(nome)
            filint(i) = trim(nome)
            call       openfile(filint(i),iint,debug)
            call      atomovma(iint,nmo,nprim,nat,i  ,iato,pop,ss,3    ,
     .                                                            debug)
            close(iint)
            write(charreal,'(F6.2)') pop(i)
            write(*,'(1X,a,$)') trim(charreal(verify(charreal,' '):15))
            if (debug) print *,'** input file finished'
         enddo !! i = 1,iato
      elseif (.NOT.genint.AND.intfuk.AND..NOT.lmwfn) then !! >>>>>> ELOC
               filint(1) = trim(filw)//'.eloc'
               call       openfile(filint(1),iint,debug)
               call       eloc(iint ,nmo,nat,iato,iatomat,pop,ss,debug)
               close(iint)
               do i = 1,iato
                  write(charreal,'(F6.2)') pop(i)
                  write(*,'(1X,A,$)') trim(charreal(verify(charreal,' ')
     .:15))
               enddo !! i = 1,iato
      elseif (.NOT.genint.AND.intfuk.AND.     lmwfn) then !! >>>>>> MWFN
               if (.NOT.lmwfnrdname) then
                  if (laom) filint(1) = 'AOM.txt'
                  if (lbom) filint(1) = 'BOM.txt'
               else
                  read (iinp,'(A)',iostat=iii) nome
                  if (iii.ne.0) 
     .                  stop 'File with atomic overlap matrix not found'
                  if (debug) print *,trim(nome)
                  filint(1) = trim(nome)
               endif !! (.NOT.lmwfnrdname) then
               call       openfile(filint(1),iint,debug)
               call       mwfn(iint ,nmo,nat,laom,lbom,iato,iatomat,pop
     ,                                                        ,ss,debug)
               close(iint)
c              do i = 1,iato
c                 write(charreal,'(F6.2)') pop(i)
c                 write(*,'(1X,A,$)') trim(charreal(verify(charreal,' ')
c    .:15))
c              enddo !! i = 1,iato
      elseif (genint.AND.intfuk) then               !! >>>>>>>>>>>>> INT
            do i = 1,iato
               if (laimall) then
                 charreal(:) = ''
                 charint(:)  = ''
                 charreal    = trim(elem(iq(iatomat(i))))
                 do j = 1,len_trim(charreal) !! LOWERCASE
                    jj            = ichar(charreal(j:j))
                    if (jj.GE.65.AND.jj.LE.91) jj = jj + 32
                    charreal(j:j) = char(jj)
                 enddo !! j = 1,len_trim(charreal)
                 write(charint,'(I7)') iatomat(i)
                 filint(i) = trim(charreal)
     .                    // trim(charint(verify(charint,' '):7))
     .                    //'.int'
                 call     openfile(filint(i),iint,debug)
                 if (debug) print *,' ;;;>>> AIMAll FORMAT ***'
                 call atovmall(iint,nmo,nprim,nat,i  ,iato,pop,ss,
     ,                                                    lfixbug,debug)
               else
                 filint(i) = trim(filw)//'_'//trim(atoms(i))//'.int'
                 call     openfile(filint(i),iint,debug)
                 if (debug) print *,' ;;;>>> AIMPAC FORMAT ***'
                 call atomovma(iint,nmo,nprim,nat,i  ,iato,pop,ss,2    ,
     .                                                            debug)
               endif !! (laimall) then
               close(iint)
              write(charreal,'(F6.2)') pop(i)
               write(*,'(1X,a,$)') trim(charreal(verify(charreal,' '):15
     .))
            enddo !! i = 1,iato
      elseif (genint.AND..NOT.intfuk) then          !! >>>>>>>>>>>>> FUK
            do i = 1,iato
                filint(i) = trim(filw)//'_'//trim(filw)//'_'//trim(atoms
     .(i))//'.fuk'
               call       openfile(filint(i),iint,debug)
               call   atomovma(iint,nmo,nprim,nat,i  ,iato,pop,ss,3    ,
     .                                                            debug)
               close(iint)
               write(charreal,'(F6.2)') pop(i)
               write(*,'(1X,a,$)') trim(charreal(verify(charreal,' '):15
     .))
            enddo !! i = 1,iato
      endif !! (.NOT.genint.AND..NOT.intfuk) then
      write(*,'(/)')
      if (laimall.AND.lfixbug) write(*,'("  >> Format change due to BUG 
     .in AIMAll detected and fixed <<")') 
C >>> Scale overlap matrices if required<<<
      if (locc.AND.allocated(pel)) 
     +                           call scaleovermat(luhf,nmo,iato,pel,ss)
C >>> Choose MOs for calc<<<
      if (allmo)       then
         if (debug) print *,'** ALL   MOL ORBS **'
         do i = 1,nmo
            moc(i) = i
         enddo !! i = 1,nmo
         imo = nmo
      elseif (lpi)     then
         if (debug) print *,'** PI    MOL ORBS ** nfpi=',nfpi
         imo = npi
         select case(nfpi)
           case(1)
             rewind(isom)
             read(isom,*,iostat=i) j,j
             if (j.NE.imo) stop 'ERROR: .sg or # pi MO modified!'
             read (isom,*,iostat=j) moc(:imo)
             if (i.NE.0.OR.j.NE.0) stop 'ERROR: Reading .sg file'
           case(2)
             call   readsom(isom,lpi,imo,moc,linear,debug)
           case default
             stop 'ERROR: PI enabled without file type'
         end select !! case(nfpi)
      elseif (lsigma)  then
         if (debug) print *,'** SIGMA MOL ORBS ** nfpi=',nfpi
         imo = npi
         select case(nfpi)
           case(1)
             rewind(isom)
             read(isom,*,iostat=i) j,j,j
             if (j.NE.imo) stop 'ERROR: .sg or # pi MO modified!'
             read (isom,'(A)')
             read (isom,*,iostat=j) moc(:imo)
             if (i.NE.0.OR.j.NE.0) stop 'ERROR: Reading .sg file'
           case(2)
             call   readsom(isom,lpi,imo,moc,linear,debug)
           case default
             stop 'ERROR: SIGMA enabled without file type'
         end select !! case(nfpi)
      elseif (outersh) then
         if (debug) print *,'** OUTER MOL ORBS **'
         imo = nmo-nheavy
         do i = 1,imo
            moc(i) = nheavy+i
         enddo !! i = nheavy+1,nmo
         if (lpostHF) imo = imo-nheavy
      elseif (loutsig) then
         if (debug) print *,'** OUTER SIGMA MOL ORBS ** nfpi=',nfpi
         imo = npi
         select case(nfpi)
           case(1)
             rewind(isom)
             read(isom,*,iostat=i) j,j,j
             if (j.NE.imo) stop 'ERROR: .sg or # pi MO modified!'
             read (isom,'(A)')
             read (isom,*,iostat=j) moc(:imo)
             if (i.NE.0.OR.j.NE.0) stop 'ERROR: Reading .sg file'
           case(2)
             call   readsom(isom,lpi,imo,moc,linear,debug)
           case default
             stop 'ERROR: OUTER SIGMA enabled without file type'
         end select !! case(nfpi)
         if (npi.LE.nheavy) stop '# Outer SIGMA MOs >= # MOs'
         imo = npi-nheavy
         moc(:imo) = moc(nheavy+1:nheavy+imo)
         if (debug) print *,'@@ REAL outer sigma mol orbs:',moc(:imo)
      else
         rewind(iinp)
         do i = 1,inpline-4
            read (iinp,*) atemp
         enddo !! i = 1,inpline
         read (iinp,'(A)') nome
         if (len(trim(nome)).EQ.1000) stop ' ** Line too long !!! **'
         k = len(trim(nome))+1
         if (debug) print *,'# characters=',k,len(trim(nome))
         call    procnumb(nome,k ,imos,ifrmo,moc    ,imo,debug)
      endif !! (allmo) then
      if (debug) print '("Specified MOs=",25I4)',(moc(i),i=1,imo)
      if (debug) print '("Total #   MOs=",  I4)',imo

C >> IF lpostHF, reduce set of MOs if null occ. numbers
      if (lpostHF) then
         allocate(mocred(imo))
         j = 0
         do i = 1,imo
            if (pel(moc(i)).NE.0.) then
               j = j+1
               mocred(j) = moc(i)
            endif !! (pel(moc(i)).NE.0.) then
         enddo !! i = 1,imo
         moc(:j) = mocred(:j)
         imo = j
         deallocate(mocred)
         if (debug) then
            PRINT '("New truncated MO set=",25I4)',(moc(i),i=1,imo)
            PRINT '("Total new # MOs=",I4)',imo
         endif !! (debug) then
      endif !! (lpostHF) then
C >>> Parallelization strategy<<<
      call       strategy(nindex ,nproc,runparal,imo,icodepar,icolapse,
     .                                                      iperm,debug)
C >>> Index   calculation<<<
      igoto  = 2
      if (nindex.LT.3) then
         goto (101,102) nindex
C
 101  continue
      call       ndeloc1(nindex,igoto,iout,pop,iato) !! << IF NOT POPS,
C                                             IT PRINTS ">> NOT IMPLEMENTED <<"
      goto 100
C
 102  continue
      call       ndeloc2(cloc,deloc,ndeloc,iato,iatomat,nmo,imo ,moc,ss,
     .                                            nproc,luhf,locc,debug)
      goto 100
C
      else
         do i = 1,iato
            xxr(i) = xx(iatomat(i))
            yyr(i) = yy(iatomat(i))
            zzr(i) = zz(iatomat(i))
         enddo !! i = 1,nato
         ntotdel = ndeloc
         if (debug) print *,'iperm=',iperm
         call    ndelocma(cloc,deloc,giamb,nindex,iperm,ndeloc,iato,
     .   iatomat,bonddist,xxr,yyr,zzr,allring,nmo,matindx,lloc,imo ,moc,
     .         nproc,icodepar,icolapse,luhf,locc,lgiamb,ss,mxring,debug)
      endif !! (nindex.LT.3) then
C
 100  continue
C
      allocate(xyzgc(3,ndeloc))
      if (lxyz) then             !! >> GEOMETRICAL CENTERS
         xyzgc = 0.0
         do i = 1,ndeloc
            do j = 1,nindex
               xyzgc(1,i) = xyzgc(1,i)+xx(matindx(i,j))
               xyzgc(2,i) = xyzgc(2,i)+yy(matindx(i,j))
               xyzgc(3,i) = xyzgc(3,i)+zz(matindx(i,j))
            enddo !! j = 1,nindex
         enddo !! i = 1,ndeloc
         xyzgc = xyzgc*bohr2a/nindex
         if (debug) then
            do i = 1,ndeloc
               print '(A,I4,A,3(F10.4))','Geom c',i,'=',
     .                                                (xyzgc(j,i),j=1,3)
            enddo !! i = 1,ndeloc
         endif !! (debug) then
      endif !! (lxyz) then       !! >> GEOMETRICAL CENTERS
C
C >>> OUTPUT  <<<
      write(*,'("  >> Writing OUTPUT .......",$)') 
cc    do i = 1,iato              !! >> ATOM LABELS
cc       atoms(i) = atomlabel(iatomat(i),nat,iq(iatomat(i)),nzeros
cc   .                                                           ,debug)
cc    enddo !! i = 1,iato        !! >> ATOM LABELS
C
      call       results(iout,nmo,nprim,nheavy,cloc,deloc,nindex,ndeloc,
     . iato,iatomat,nat ,iq,matindx,atoms,filint,allmo,outersh,lpi,lloc,
     .    giamb,lgiamb,version,imo ,itype,bonddist,allring,ntotdel,luhf,
     . loutsig,lsigma,genint,intfuk,moc,ixyz,lxyz,xx,yy,zz,xyzgc,nzeros,
     .                       locc,lpostHF,nbasin,laom,lbom,linear,debug)
      if (allocated(xyzgc)) deallocate(xyzgc)
      if (allocated(ss)   ) deallocate(ss)
      if (allocated(pel)  ) deallocate(pel)
      write(*,'(" Done")') 
C
      if (debug) print *,'***** END *** goon ***'
      return
      end
C=======================================================================
C

