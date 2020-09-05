      program ndeloca
C
C Program that calculates the N-DELOC indices.
C
C Nicolás Otero Martínez, January 12 2017
C
C =====================
C === Version 1.2.12 ==
C =====================
C
C#######################################################################
C COMPILE   :
C
C   fortran_compiler ndeloc.f ndelocdir/ndeloc1.f ndelocdir/ndeloc2.f \
C         ndelocdir/looporb*.f ndelocdir/choose.f -o ndeloc.x
C
C                                OR
C
C   fortran_compiler ndeloc.f ndelocdir/ndeloc[12].f \
C         ndelocdir/looporb*.f ndelocdir/choose.f -o ndeloc.x
C
C ** fortran_compiler :  A fortran compiler. I have used ifort 
C                        and gfortran to write this program
C#######################################################################
C EXAMPLES  :
C                      Example 1 : 2B1.ndinp   file
C********************
C*2B1 fchk          * -> FCHK reading algorith will be used.
C*2B1               * -> Output file name without extension.
C*pi                * -> PI orbitals from a .som file (2B1.som).
C*6 all             * -> 6-DELOC indices. DELOC+LOC.
C*heavy ring 1.7    * -> 6-member ring detection with heavy atoms only.
C*fuk               * -> fuk format will be employed.
C********************
C                      Example 2 : C6H6.ndinp  file
C********************
C*C6H6              * -> WFN  reading algorith will be used.
C*example2          * -> Output file name without extension.
C*outersh           * -> Inner s orbitals will be skipped.
C*6 deloc           * -> 6-DELOC indices. DELOC only.
C*all               * -> All atom combinations.
C*int               * -> int format will be employed.
C********************
C                      Example 3 : phosp.ndinp file
C********************
C*phosp             * -> WFN  reading algorith will be used.
C*phosphole xyz     * -> Output file name without extension and a xyz with
C*all               * -> All orbitals will be used.   | the N-DELOC indices.
C*5                 * -> 5-DELOC indices. LOC+DELOC.
C*all ring 1.8      * -> 5-member ring detection with all atoms
C*read              *    (1.8 is the distance cut-off for bond criterion).
C*file1.ext         * -> Files will be read.
C*file2.ext         * ->
C*file3.ext         * ->
C* ...              * ->
C********************
C                      Example 4 : C6H6.ndinp  file
C********************
C*C6H6              * -> WFN  reading algorith will be used.
C*example4          * -> Output file name without extension.
C*outersh           * -> Inner s orbitals will be skipped.
C*6 deloc giamb     * -> 6-DELOC indices. DELOC and Giambiagi indices only.
C*all               * -> All atom combinations.
C*int               * -> int format will be employed.
C********************
C                      Example 5 : C6H6.ndinp  file
C********************
C*C6H6              * -> WFN  reading algorith will be used.
C*example5          * -> Output file name without extension.
C*7-15,20-22        * -> MO specification: 7,8,9,10,11,12,13,14,15,20,21,22
C*6 deloc giamb     * -> 6-DELOC indices. DELOC and Giambiagi indices only.
C*1-3,6,7,10 ring   * -> Atoms 1,2,3,6,7,10 will be used with a ring sort.
C*int 5             * -> int format will be employed with atom label
C********************    length 5, i.e., X00001.
C
C                      Example 6 : C6H6.ndinp  file
C********************
C*C6H6              * -> WFN  reading algorith will be used.
C*example5          * -> Output file name without extension.
C*7-15,20-22        * -> MO specification: 7,8,9,10,11,12,13,14,15,20,21,22
C*6 deloc giamb     * -> 6-DELOC indices. DELOC and Giambiagi indices only.
C*1-3,6,7,10 ring   * -> Atoms 1,2,3,6,7,10 will be used with a ring sort.
C*mwfn              * -> Multiwfn format will be employed. The program
C********************    will search for AOM.txt by default
C
C ** If distance cut-off is omitted, 1.6 will be used by default **
C      In general, it is a good default value for C-based rings
C
C#######################################################################
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      logical        debug,filex,genint,lloc,intfuk,lfchk,lgiamb,laimall
     ,              ,lmwfn,lmwfnrdname
      logical        runparal
      logical        lxyz
      logical        allmo,lpi,outersh,lsigma,loutsig
      logical        allatoms,allheavy,allring,linear
      character*4    extxyz
      character*6    extndinp,extndout
      character*7    charint,version
      character*15   charreal
      character*20   text,tmpname
      character*100  filwfn,filinp,filout,filw,filsom,command,filouw
      character*1000 nome
C
C----------------------------------------------------------------------
      common /names/  filwfn,filinp,filout,filw,filsom,filouw
      common /ttime/  start,qtime1,qtime2,rtime1,rtime2
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension mati(6),matf(6,5)
C
C***********************************************************************
      include 'ndeloc.param.h'  !! MAX VALUES, SOME OPTIONS, DEBUG FLAG
C***********************************************************************
C
      write(*,'(/,14X,a,a,/)') ' N D E L O C  v.',trim(version)
      if (debug) print *,'DEBUG flag is activated'
      call getarg (2,nome)
      charint = trim(nome(1:index(nome,' ')))
      read (charint,*,iostat=iii) nindex
      if (iii.NE.0) then
C >>> Open the input  file <<<
      call getarg (1,nome)
      filinp=nome(1:index(nome,' ')-1)//extndinp
      call       openfile(filinp  ,iinp,debug)
      rewind(iinp)
C
      inpline = 0  !! Number of line where program is reading
C >>> Open the wfn/fchk file <<<   line 1
      read(iinp,'(A)',iostat=iii) nome
      if (iii.NE.0) stop ' ** PROBLEM while the input file was read'
      inpline  = inpline + 1
      filex    = .TRUE.
      filw     = nome(1:index(nome,' ')-1)
      lfchk    = index(trim(filw),'fchk').GT.0
      nproc    = 1
      runparal = .FALSE.
      if (.NOT.lfchk.AND.(index(trim(filw),'wfn').LE.0)) stop 'NEITHER F
     .CHK NOR WFN FILE WAS SPECIFIED'
      if (debug) print *,'filw=+',trim(filw),'+',lfchk,
     .                                      index(trim(filw),'wfn').GT.0
      filw(index(filw,'.',.TRUE.):index(filw,' ',.TRUE.)) = ' '
      iword = 1
      j = len(trim(nome))
      do i = 1,j
         if (nome(i:i).EQ.' ') then
            if (filex) iword = iword+1
            ipos  = i
            filex = .FALSE. 
         else
            filex = .TRUE.
         endif !! (nome(i:i).NE.' ') then
      enddo !! i = 1,len(trim(nome))
      if (debug) print *,'iword=',iword
      if (iword.EQ.2) then
         read (nome(ipos+1:j),'(I5)') nproc
         if (debug) print *,'nproc=',nproc
      else
         stop ' ** Too many arguments in the LINE 1 were read'
      endif !! (iword.EQ.1) then
      if (nproc.GT.1) then
         runparal = .TRUE.
         write(charint,'(I7)') nproc
         write(*,'("  >> Parallelization enabled <<")')
         call    init_par(runparal,nproc)
         write(*,'("  >> # threads ............ ",A)')
     .                              trim(charint(verify(charint,' '):7))
      endif !! (nproc.GT.1) then
      if (lfchk) then
         filwfn = trim(filw)//'.fchk'
         write(*,'("  >> FCHK File ............ ",A)') trim(filwfn)
      else
         filwfn = trim(filw)//'.wfn'
         write(*,'("  >> WFN  File ............ ",A)') trim(filwfn)
      endif !! (lfchk) then
      call       openfile(filwfn  ,iwfn,debug)
C >>> Open the output file <<<   line 2
      read(iinp,'(a)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read ** O
     .UTPUT file is NOT specified **'
      inpline = inpline + 1
      filouw=nome(1:index(nome,' ')-1)
      filout=filouw(1:len(trim(filouw)))//extndout
      inquire(file=filout,exist=filex)
      if (filex) then
         command = 'mv '//trim(filout)//' '//trim(filout)//'.$(date +%g%
     .m%d%H%M%S)'
         write(*,'(2X,"OUTPUT file exists: ",a,/,2X,"A back-up will be p
     .erformed")') trim(filout)
         i = system(command)
         write(*,'(2x,a,I3,/," ** Restart the calculation")') command,
     .i
         stop
      endif !! (filex) then
      filex = .TRUE.
      iword = 1
      j = len(trim(nome))
      do i = 1,j
         if (nome(i:i).EQ.' ') then
            if (filex) iword = iword+1
            ipos = i
            filex = .FALSE. 
         else
            filex = .TRUE.
         endif !! (nome(i:i).NE.' ') then
      enddo !! i = 1,len(trim(nome))
      lxyz = .FALSE.
      if (debug) print *,'iword=',iword
      if (iword.LE.2) then
         if (debug) print *,'2nd_word=',trim(nome(ipos+1:j))
         lxyz = index(trim(nome(ipos+1:j)),'xyz').GT.0
      else
         stop ' ** Too many arguments in the OUTPUT config line were rea
     .d'
      endif !! (iword.LE.2) then
      write(*,'("  >> OUTPUT File .......... ",a)') trim(filout)
      if (lxyz) write(*,'("  >> XYZ file option detected <<")')
C >>> Select MOs           <<<   line 3
      read(iinp,'(A)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read ** M
     .Os?'
      inpline = inpline + 1
      allmo   = .FALSE.
      lpi     = .FALSE.
      lsigma  = .FALSE.
      outersh = .FALSE.
      loutsig = .FALSE.
      linear  = .FALSE.
      write(*,'("  >> Specified orbitals ... ",$)')
      if (index(trim(nome),'all').GT.0) then
         allmo   = .TRUE.
         write(*,'(a)') 'ALL'
      elseif (index(trim(nome),'pi').GT.0) then
         write(*,'(a,$)') 'PI ('
         filsom = filw(1:index(filw,' ')-1)//'.som'
         call       openfile(filsom  ,isom,debug)
C.............
         text   = ' respecto al eje: '
         call sudgfdim(18  ,text,isom,icde,1  ,jjj,debug)
         linear = jjj.EQ.0
         rewind(isom)
         if (linear) then
            text   = 'PI   respecto al eje'
         else
            text   = 'PI   respecto al pla'
         endif !! (linear) then
C.............
         npi    = 0
         jjj    = 0
         irw    = 1
         do while (jjj.EQ.0) 
            call sudgfchk(text,isom,icde,irw,jjj,debug)
            if (jjj.EQ.0) npi = npi+1
         enddo !! while (jjj.EQ.0) 
         write(charint,'(I7)') npi
         write(*,'(a," MOs)")') trim(charint(verify(charint,' '):7))
         lpi     = npi.GT.0
         allmo   = npi.LE.0
      elseif (index(trim(nome),'sigma').GT.0) then
         write(*,'(a,$)') 'SIGMA ('
         filsom = filw(1:index(filw,' ')-1)//'.som'
         call       openfile(filsom  ,isom,debug)
C.............
         text   = ' respecto al eje: '
         call sudgfdim(18  ,text,isom,icde,1  ,jjj,debug)
         linear = jjj.EQ.0
         rewind(isom)
         if (linear) then
            text   = 'SIGMA respecto al ej'
         else
            text   = 'SIGMA respecto al pl'
         endif !! (linear) then
C.............
         npi = 0
         jjj = 0
         irw = 1
         do while (jjj.EQ.0) 
            call sudgfchk(text,isom,icde,irw,jjj,debug)
            if (jjj.EQ.0) npi = npi+1
         enddo !! while (jjj.EQ.0) 
         write(charint,'(I7)') npi
         write(*,'(a," MOs)")') trim(charint(verify(charint,' '):7))
         lsigma  = npi.GT.0
         allmo   = npi.LE.0
      elseif (index(trim(nome),'outsig').GT.0) then
         write(*,'(a,$)') 'OUTER SIGMA ORBS. # SIGMA = '
         filsom = filw(1:index(filw,' ')-1)//'.som'
         call       openfile(filsom  ,isom,debug)
C.............
         text   = ' respecto al eje: '
         call sudgfdim(18  ,text,isom,icde,1  ,jjj,debug)
         linear = jjj.EQ.0
         rewind(isom)
         if (linear) then
            text   = 'SIGMA respecto al ej'
         else
            text   = 'SIGMA respecto al pl'
         endif !! (linear) then
C.............
         npi = 0
         jjj = 0
         irw = 1
         do while (jjj.EQ.0) 
            call sudgfchk(text,isom,icde,irw,jjj,debug)
            if (jjj.EQ.0) npi = npi+1
         enddo !! while (jjj.EQ.0) 
         write(charint,'(I7)') npi
         write(*,'(a," MOs")') trim(charint(verify(charint,' '):7))
         loutsig = npi.GT.0
         allmo   = npi.LE.0
      elseif (index(trim(nome),'outersh').gt.0) then
         outersh = .TRUE.
         write(*,'(A)') 'Inner s shells will be omitted'
      else
         call    hmnumb(nome,imos,ifrmo,debug)
         if (debug) print *,'processed=> ifrmo,imos= ',ifrmo,imos
         write(charint,'(I7)') imos
         write(*,'(A,A)') trim(charint(verify(charint,' '):7)),' MOs'
      endif !! (index(trim(nome),'all').gt.0) then
C >>> Select N-(DE)LOC index<<<  line 4
      read(iinp,'(a)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read'
      inpline = inpline + 1
      lloc    = index(trim(nome),'deloc').LE.0
      if (lloc)    write(*,'("  >> LOC indices will be considered <<")')
      lgiamb  = index(trim(nome),'giamb').GT.0
      if (lgiamb)
     .  write(*,'("  >> Only Giambiagi indices will be considered <<")')
      charint = trim(nome(1:index(nome,' ')))
      read (charint,*) nindex
      write(*,'("  >> ",a,"-(DE)LOC will be calculated <<")')trim(charin
     .t(verify(charint,' '):7))
C >>> Initial data       <<<
      if (lfchk) then
         call fchkinfo(iwfn,nmo,nprim,nat,nheavy,debug)
         write(*,'("  >> NMOs,NBFunc,NAtoms ... ",$)') 
      else
         call  wfninfo(iwfn,nmo,nprim,nat,nheavy,debug)
         write(*,'("  >> NMOs,NPrims,NAtoms ... ",$)') 
      endif !! (lfchk) then
      write(charint,'(I7)') nmo
      write(*,'(a,", ",$)') trim(charint(verify(charint,' '):7))
      write(charint,'(I7)') nprim
      write(*,'(a,", ",$)') trim(charint(verify(charint,' '):7))
      write(charint,'(I7)') nat
      write(*,'(a       )') trim(charint(verify(charint,' '):7))
      write(*,'("  >> NHeavy Atoms ......... ",$)') 
      write(charint,'(I7)') nheavy
      write(*,'(a       )') trim(charint(verify(charint,' '):7))
C >>> Select Atoms          <<<  line 5
      read(iinp,'(a)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read'
      inpline = inpline + 1
      if (debug) print *,'nome +',trim(nome),'+'
      allatoms = .FALSE.
      allheavy = .FALSE.
      if (index(trim(nome),'all').GT.0) then
         allatoms = .TRUE.
         iato     = nat
      elseif (index(trim(nome),'heavy').GT.0) then
         allheavy = .TRUE.
         iato     = nheavy
      else
         call    hmnumb(nome,iato,ifrag,debug)
      endif !! (index(trim(nome),'all').gt.0) then
      if (debug) print *,'iato,ifrag=',iato,ifrag
      allring = index(trim(nome),'ring').GT.0
      if (allring) then
         if (nindex.LT.3) stop ' ** 2-atom rings?!?!'
      elseif (lgiamb) then
         write(*,'("  >> giamb specified without ring detection <<",2(/)
     .,10X,"** CHECK THE ATOM ORDER !!! ***",/)')
      endif !! (allring) then
      write(charint,'(I7)') iato
      write(*,'("  >> # Specified atoms .... ",a)') trim(charint(verify(
     .charint,' '):7))
      if (iato.LT.nindex) stop ' ** PROBLEM :  The number of atoms is no
     .t enough to perform the calculation'
      if (allring) then 
         write(*,'("  >> Ring detection will be used <<")')
         filex = .TRUE.
         iword = 1
         j = len(trim(nome))
         do i = 1,j
            if (nome(i:i).EQ.' ') then
               if (filex) iword = iword+1
               ipos = i
               filex = .FALSE. 
            else
               filex = .TRUE.
            endif !! (nome(i:i).NE.' ') then
         enddo !! i = 1,len(trim(nome))
         if (iword.GT.2) then
            charreal = trim(nome(ipos+1:j))
            read (charreal,*,iostat=iii) bonddist
            if (iii.NE.0) stop ' ** PROBLEM while the bond distance was 
     .read'
            if (debug) print *,bonddist,iii
         else
            bonddist = defbdist
         endif !! (iword.GT.2) then
      else
         if (lxyz) then
            write(*,'("  >> XYZ files will not be created <<",/,5X,
     .                              "(ring option is not included)")')
            lxyz = .FALSE.
         endif !! (lxyz) then
      endif !! (allring) then 
C >>> Number of DELOC inds <<<
C NINDEX     1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10- 11- 12- 13- 14
      goto (101,102,103,103,103,103,103,103,103,103,103,103,103,103)
     .                                                            nindex
C
 101  continue
      stop ' ** This index is not implemented **'
      goto 100
C
 102  continue
      ndeloc = iato*(iato-1)/2    !! Triangular number (n*(n+1)/2) - diagonal elements (n)
      goto 100
C
 103  continue
      ndeloc =  int(nbinom(iato,nindex,iato,debug))  !! Binomial coeff
      goto 100
C
 100  continue
      if (.NOT.allring) then
         i = ndeloc
         if (lloc) i = i+iato
         write(charint,'(I7)') i
         write(*,'("  >> # (de)loc indices .... ",a)') trim(charint(veri
     .fy(charint,' '):7))
      else
         write(*,'("  >> # (de)loc indices will be calculated with ring 
     .detection <<")')
      endif !! (.NOT.allring) then
C >>> Read file names (optional) lines 6 ...
      nome(:)    = ''
      read(iinp,'(a)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read'
      lmwfn      = .FALSE.
      laimall    = .FALSE.
      tmpname(:) = ''
      tmpname    = trim(nome(:len(tmpname)))
      if (index(tmpname(1:4),'read').GT.0) then
         genint = .FALSE.
         intfuk = .FALSE.
         if (debug) print *,'** atomic overlap matrix files: '
     ,                     ,genint,intfuk
         write(*,'("  >> Files will be read from the input file << ")')
         inpline = inpline + 1
      elseif (index(trim(nome),'int').GT.0) then
         inpline = inpline + 1
         genint  = .TRUE.
         intfuk  = .TRUE.
         laimall = .FALSE.
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     .tfuk
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'int'
      elseif (index(trim(nome),'aimall').GT.0) then
         inpline = inpline + 1
         laimall = .TRUE.
         genint  = .TRUE.
         intfuk  = .TRUE.
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     .tfuk,' for AIMAll'
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'AIMAll int'
      elseif (index(trim(nome),'fuk').GT.0) then
         genint = .TRUE.
         intfuk = .FALSE.
         inpline = inpline + 1
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     .tfuk
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'fuk'
      elseif (index(trim(nome),'eloc').GT.0) then
         genint = .FALSE.
         intfuk = .TRUE.
         inpline = inpline + 1
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     .tfuk
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'eloc'
      elseif (index(trim(nome),'mwfn').GT.0) then
         genint = .FALSE.
         intfuk = .TRUE.
         lmwfn  = .TRUE.
         lmwfnrdname = .FALSE.
         if (index(trim(nome),'=read').GT.0) lmwfnrdname = .TRUE.
         inpline = inpline + 1
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     .tfuk
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'mwfn'
      else
         stop ' ** read,int,eloc,mwfn or fuk were not specified'
      endif !! (index(trim(nome),'int').gt.0) then
      if (genint) then 
         print *,' ** Checking number of zeros in the file names'
         filex = .TRUE.
         iword = 1
         j = len(trim(nome))
         do i = 1,j
            if (nome(i:i).EQ.' ') then
               if (filex) iword = iword+1
               ipos = i
               filex = .FALSE. 
            else
               filex = .TRUE.
            endif !! (nome(i:i).NE.' ') then
         enddo !! i = 1,len(trim(nome))
         if (iword.GE.2) then
            charint = trim(nome(ipos+1:j))
            read (charint,*,iostat=iii) nzeros
            if (iii.NE.0) stop ' ** PROBLEM while the number of zeros in
     . the file names was read'
            if (debug) print *,'nzeros was read= ',nzeros,iii
         else
            nzeros = nzerodef
         endif !! (iword.GT.2) then
      else
         nzeros = nzerodef
      endif !! (genint) then 
      if (debug) print *,'nzeros= ',nzeros
C +++ ELSE Check if a 2nd argument exists to calculate direct indices +++
      else
         print *,'NOT IMPLEMENTED'
         stop
      endif
C >>> Check   data       <<<
      if (nindex.GT.nat   ) stop ' ** N-(DE)LOC index > NAtoms **'
      if (nmo   .GT.maxmo ) stop ' ** Increase MAXMO, please **'
      if (nat   .GT.maxato) stop ' ** Increase MAXATO, please **'
C
      call cpu_time(start)
      call       goon(iwfn,iout,iinp,isom,nmo,nprim,nat,allmo,iato,iint,
     .     allatoms,allheavy,outersh,lpi,npi,lloc,nindex,ndeloc,inpline,
     .version,nheavy,lfchk,bonddist,mxring,allring,genint,intfuk,lgiamb,
     .  runparal,nproc,lsigma,loutsig,imos,ifrmo,ifrag,ixyz,lxyz,nzeros,
     .                           lmwfnrdname,lmwfn,linear,laimall,debug)
      call cpu_time(finish)
      write(iout,'(/)')
      if (allring) then
         write(charreal,'(F15.2)') qtime2-qtime1
         write(iout,'("   >> qcksort spent ",11("."),X,a," seconds")')
     .                           trim(charreal(verify(charreal,' '):15))
         write(charreal,'(F15.2)') rtime2-rtime1
         write(iout,'("   >> Connect analysis spent ",2("."),X,a," sec
     .onds")')
     .                           trim(charreal(verify(charreal,' '):15))
      endif !! (allring) then
      write(charreal,'(F15.2)') finish-start
      write(*   ,'(   "  >> Elapsed time ", 9("."),X,a," seconds")') 
     .                           trim(charreal(verify(charreal,' '):15))
      write(iout,'(   "   >> Elapsed time ",12("."),X,a," seconds")') 
     .                           trim(charreal(verify(charreal,' '):15))
C
      close(iwfn)
      close(isom)
      close(iinp)
      close(iout)
C
      stop
      end
C=======================================================================
C
