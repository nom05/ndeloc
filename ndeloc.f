      program ndeloca
C
C Program that calculates the N-DELOC indices.
C
C * Authors:
C   - Nicolás Otero Martínez
C   - Marcos Mandado Alonso
C * Publications:
C   - First implementation:
C     o Mandado, M.; González-Moa, M. J.; Mosquera, R. A. "QTAIM n-center
C       delocalization indices as descriptors of aromaticity in mono and
C       polyheterocycles", J. Comput. Chem. 28, 127-136 (2007).
C     o Mandado, M.; González-Moa, M. J.; Mosquera, R. A. "Chemical graph theory
C       and n-center electron delocalization indices: A study on polycyclic
C       aromatic hydrocarbons", J. Comput. Chem. 28, 1625-1633 (2007).
C   - Initial version of the program developed for:
C     o Karamanis, P.; Otero, N.; Pouchan, C. "Unleashing the Quadratic
C       Nonlinear Optical Responses of Graphene by Confining White-Graphene
C       (h-BN) Sections in Its Framework", J. Am. Chem. Soc. 136,
C       7464-7473 (2014).
C * Quicksort module:
C   - JAMS Fortran library
C     https://github.com/mcuntz/jams_fortran
C     Distributed under the MIT License
C     Copyright (c) 2011-2020 Matthias Cuntz and Juliane Mai  
C----------------------------------------------------------------------------
C This file is part of the NDELOC: ``a program to compute multicenter electron
C delocalization indices'', distributed under the MIT License.
C Copyright (c) 2022 Nicolás Otero Martínez (nom05 at uvigo.es) and Marcos Mandado Alonso
C (mandado at uvigo.es)
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C----------------------------------------------------------------------------
C * Last edited: April 21 2022
C
C =====================
C === Version 1.2.17 ==
C =====================
C
C#######################################################################
C COMPILE   :
C Use CMake:
C   $ Exit this file
C   $ mkdir build; cd build
C   $ cmake ..
C   $ make
C * To force the use of a specific compiler (gfortran,ifort,...)
C   $ cmake -DCMAKE_Fortran_COMPILER=compiler ..
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
C*example5 xyz      * -> Output file name without extension.
C*7-15,20-22        * -> MO specification: 7,8,9,10,11,12,13,14,15,20,21,22
C*6 deloc giamb     * -> 6-DELOC indices. DELOC and Giambiagi indices only.
C*1-3,6,7,10 ring   * -> Atoms 1,2,3,6,7,10 will be used with a ring sort.
C*int 5             * -> int format will be employed with atom label
C********************    length 5, i.e., X00001.
C
C                      Example 6 : C6H6.ndinp  file
C********************
C*C6H6              * -> WFN  reading algorith will be used.
C*example6 xyz      * -> Output file name without extension.
C*7-15,20-22        * -> MO specification: 7,8,9,10,11,12,13,14,15,20,21,22
C*6 deloc giamb aom * -> 6-DELOC indices. DELOC and Giambiagi indices only. AOMs will be read.
C*1-3,6,7,10 ring   * -> Atoms 1,2,3,6,7,10 will be used with a ring sort.
C*mwfn              * -> Multiwfn format will be employed. The program
C********************    will search for AOM.txt by default
C
C                      Example 7 : C6H6.ndinp  file
C********************
C*C6H6              * -> WFN  reading algorith will be used.
C*example7          * -> Output file name without extension.
C*7-15,20-22        * -> MO specification: 7,8,9,10,11,12,13,14,15,20,21,22
C*6 deloc giamb bom * -> 6-DELOC indices. DELOC and Giambiagi indices only. BOMs will be read.
C*1-3,6,7,10        * -> Basins 1,2,3,6,7,10 from Multiwfn file will be used.
C*mwfn              * -> Multiwfn format will be employed. The program
C********************    will search for AOM.txt by default
C NOTE: The use of basins disables ring detection.
C
C                      Example 8 : pentane.ndinp  file
C********************
C*pentane debug     * -> WFN  reading algorith will be used. Debug mode is set.
C*example7          * -> Output file name without extension.
C*all occ           * -> All MO orbitals will be used. Calculation using orbital occupations(*)
C*6 deloc           * -> 6-DELOC indices. DELOC indices.
C*1-3,6,7,10        * -> Basins 1,2,3,6,7,10 from Multiwfn file will be used.
C*int               * -> AIMPAC int format will be employed.
C********************
C (*) This is an approach for multideterminant wavefunctions.
C
C
C ** If distance cut-off is omitted, 1.6 will be used by default **
C      In general, it is a good default value for C-based rings
C
C#######################################################################
C
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)

      include 'omp_lib.h'

      logical        debug,filex,genint,lloc,intfuk,lgiamb,laimall
     +              ,lmwfn,lmwfnrdname
      logical        laom,lbom
      logical        runparal
      logical        lxyz
      logical        allmo,lpi,outersh,lsigma,loutsig,locc
      logical        allatoms,allheavy,allring,linear,lpostHF
      integer        itype !! Wave function file format. =0 wfn, =1 fchk, =2 molden
      character*4    extxyz
      character*6    extndinp,extndout,extension
      character*7    charint,version
      character*15   charreal
      character*20   text,tmpname
      character*100  filwfn,filinp,filout,filw,filsom,command,filouw
     +              ,filabom
      character*1000 nome
C
C----------------------------------------------------------------------
      common /names/  filwfn,filinp,filout,filw,filsom,filouw
      common /ttime/  start,qtime1,qtime2,rtime1,rtime2
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
!=======================================================================
C
      dimension mati(6),matf(6,5),ipos(2)
C
C***********************************************************************
      include 'ndeloc.param.h'  !! MAX VALUES, SOME OPTIONS, DEBUG FLAG
C***********************************************************************
C
      nfpi = 0  ! =1 .sg; =2 .som
      lpostHF = .FALSE.
      laom = .TRUE.
      lbom = .FALSE.
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
      inpline   = inpline + 1
      filex     = .TRUE.
      filw      = nome(1:index(nome,' ')-1) !! Full name including extension
      extension = filw(index(filw,'.',.TRUE.)+1:len_trim(filw)) !!  extract extension
      filw(index(filw,'.',.TRUE.):len(filw)) = '' !! remove extension from file name
      select case(trim(extension)) !! Wave function file format. =0 wfn, =1 fchk, =2 molden
        case('wfn')
          itype = 0
        case('WFN')
          itype = 0
        case('fch')
          itype = 1
        case('fchk')
          itype = 1
        case('FCH')
          itype = 1
        case('FCHK')
          itype = 1
        case('molden')
          itype = 2
        case('MOLDEN')
          itype = 2
        case default
          stop 'Wrong file or wave function file type not implemented'
      end select
      if (debug) PRINT *,'itype=',itype
      nproc    = 1
      runparal = .FALSE.
      if (debug) print *,'filw=+',trim(filw),'+',trim(extension)
     +                                                 ,'+ itype=',itype
      iword = 1
      j = len(trim(nome))
      k = 0
      do i = 1,j
         if (nome(i:i).EQ.' ') then
            if (filex) iword = iword+1
            k       = k+1
            if (k.GT.2) stop ' ** Too many arguments in the LINE 1'
            ipos(k) = i
            filex   = .FALSE. 
         else
            filex = .TRUE.
         endif !! (nome(i:i).NE.' ') then
      enddo !! i = 1,len(trim(nome))
      if (debug) print *,'iword=',iword
      text(:) = ''
      select case (iword)
         case(1)
            if (debug) print *,'DBG: wf file is opened - ',trim(filinp)
         case(2)
            read (nome(ipos(1)+1:j),'(A)') text
            if (debug) print *,'DEBUG:',trim(text),'+'
            if (index(trim(text),'debug').GT.0) then
               debug = .TRUE.
               if (debug) print *,'DEBUG mode is set'
            else
               read (nome(ipos(1)+1:j),'(I5)') nproc
               if (debug) print *,'nproc=',nproc
            endif !! (index(trim(nome),'debug').GT.0) then
         case(3)
            if (debug) PRINT *,'DEBUG:',nome(ipos(1)+1:ipos(2)-1),'+'
            read (nome(ipos(1)+1:ipos(2)-1),'(A)') text
            if (index(trim(text),'debug').GT.0) then
               debug = .TRUE.
               print *,'DEBUG mode is set'
               print *,'DEBUG:',nome(ipos(2)+1:j),'+'
               read (nome(ipos(2)+1:j),'(I5)') nproc
               print *,'nproc=',nproc
            else
               read (nome(ipos(1)+1:ipos(2)-1),'(I5)') nproc
               if (debug) print *,'nproc=',nproc
               if (index(nome(ipos(2)+1:j),'debug').GT.0) then
                  debug = .TRUE.
                  print *,'DEBUG mode is set'
                  print *,'DEBUG:',nome(ipos(2)+1:j),'+'
               endif !! (index(nome(ipos(2)+1:j),'debug').GT.0) then
            endif !! (index(trim(nome),'debug').GT.0) then
         case default
            stop ' ** Too many arguments in the LINE 1 were read'
      end select
      if (nproc.GT.1) then
         runparal = .TRUE.
         write(charint,'(I7)') nproc
         write(*,'("  >> Parallelization enabled <<")')
         call    init_par(runparal,nproc)
         write(*,'("  >> # threads ............ ",A)')
     +                              trim(charint(verify(charint,' '):7))
      endif !! (nproc.GT.1) then
      select case(itype)
        case(0)
          filwfn = trim(filw)//'.wfn'
          write(*,'("  >> WFN    File .......... ",A)') trim(filwfn)
        case(1)
          filwfn = trim(filw)//'.fchk'
          write(*,'("  >> FCHK   File .......... ",A)') trim(filwfn)
        case(2)
          filwfn = trim(filw)//'.molden'
          write(*,'("  >> MOLDEN File .......... ",A)') trim(filwfn)
      end select
      call       openfile(filwfn  ,iwfn,debug)
C >>> Open the output file <<<   line 2
      read(iinp,'(a)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read ** O
     +UTPUT file is NOT specified **'
      inpline = inpline + 1
      filouw=nome(1:index(nome,' ')-1)
      filout=filouw(1:len(trim(filouw)))//extndout
      inquire(file=filout,exist=filex)
      if (filex) then
         command = 'mv '//trim(filout)//' '//trim(filout)//'.$(date +%g%
     +m%d%H%M%S)'
         write(*,'(2X,"OUTPUT file exists: ",a,/,2X,"A back-up will be p
     +erformed")') trim(filout)
         i = system(command)
         write(*,'(2x,a,I3,/," ** Restart the calculation")') command
     +,i
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
         if (debug) print *,'2nd_word=',trim(nome(ipos(1)+1:j))
         lxyz = index(trim(nome(ipos(1)+1:j)),'xyz').GT.0
      else
         stop ' ** Too many arguments in the OUTPUT config line were rea
     +d'
      endif !! (iword.LE.2) then
      write(*,'("  >> OUTPUT File .......... ",a)') trim(filout)
      if (lxyz) write(*,'("  >> XYZ file option detected <<")')
C >>> Select MOs           <<<   line 3
      read(iinp,'(A)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read ** M
     +Os?'
      inpline = inpline + 1
      allmo   = .FALSE.
      lpi     = .FALSE.
      lsigma  = .FALSE.
      outersh = .FALSE.
      loutsig = .FALSE.
      linear  = .FALSE.
      locc    = .FALSE.
      write(*,'("  >> Specified orbitals ... ",$)')
      if (index(trim(nome),'all').GT.0) then
         allmo   = .TRUE.
         write(*,'(a)') 'ALL'
      elseif (index(trim(nome),'pi').GT.0) then
         allmo  = .FALSE.
         npi    = 0
         write(*,'(a,$)') 'PI ('
         filsom = filw(1:index(filw,' ')-1)//'.sg'
         inquire(file=filsom,exist=filex)
         if (filex) then
            nfpi = 1
         else
            filsom = filw(1:index(filw,' ')-1)//'.som'
            inquire(file=filsom,exist=filex)
            if (filex) nfpi = 2
         endif !! (filex) then
         call openfile(filsom  ,isom,debug)
         select case(nfpi)
           case(0)
             write(*,'(a,a)') filw(1:index(filw,' ')-1),
     ,            '.{sg,som} not found! -> ALL'
             allmo   = .TRUE.
           case(1)
             read (isom,*,iostat=i) itmpmo,npi
             if (i.NE.0) stop 'ERROR reading .sg file'
           case(2)
             text   = ' respecto al eje: '
             call sudgfdim(18  ,text,isom,icde,1  ,jjj,debug)
             linear = jjj.EQ.0
             rewind(isom)
             if (linear) then
                text   = 'PI   respecto al eje'
             else
                text   = 'PI   respecto al pla'
             endif !! (linear) then
             npi    = 0
             jjj    = 0
             irw    = 1
             do while (jjj.EQ.0) 
                call sudgfchk(text,20,isom,icde,irw,jjj,debug)
                if (jjj.EQ.0) npi = npi+1
             enddo !! while (jjj.EQ.0) 
           case default
             stop 'SIGMA/PI identification code not implemented'
         end select !! case(nfpi)
         lpi     = npi.GT.0
         allmo   = npi.LE.0
         write(charint,'(I7)') npi
         if (allmo) then
            write(*,'("0 MOs) -> ALL")')
         else
            write(*,'(a," MOs)")') 
     .                          trim(charint(verify(charint,' '):7))
         endif !! (allmo) then
      elseif (index(trim(nome),'sigma').GT.0) then
         allmo  = .FALSE.
         npi    = 0
         write(*,'(a,$)') 'SIGMA ('
         filsom = filw(1:index(filw,' ')-1)//'.sg'
         inquire(file=filsom,exist=filex)
         if (filex) then
            nfpi = 1
         else
            filsom = filw(1:index(filw,' ')-1)//'.som'
            inquire(file=filsom,exist=filex)
            if (filex) nfpi = 2
         endif !! (filex) then
         call openfile(filsom  ,isom,debug)
         select case(nfpi)
           case(0)
             write(*,'(a,a)') filw(1:index(filw,' ')-1),
     ,            '.{sg,som} not found! -> ALL'
             allmo   = .TRUE.
           case(1)
             read (isom,*,iostat=i) itmpmo,itmp,npi
             if (i.NE.0) stop 'ERROR reading .sg file'
           case(2)
             text   = ' respecto al eje: '
             call sudgfdim(18  ,text,isom,icde,1  ,jjj,debug)
             linear = jjj.EQ.0
             rewind(isom)
             if (linear) then
                text   = 'SIGMA respecto al ej'
             else
                text   = 'SIGMA respecto al pl'
             endif !! (linear) then
             npi    = 0
             jjj    = 0
             irw    = 1
             do while (jjj.EQ.0) 
                call sudgfchk(text,20,isom,icde,irw,jjj,debug)
                if (jjj.EQ.0) npi = npi+1
             enddo !! while (jjj.EQ.0) 
           case default
             stop 'SIGMA/PI identification code not implemented'
         end select !! case(nfpi)
         lsigma  = npi.GT.0
         allmo   = npi.LE.0
         write(charint,'(I7)') npi
         if (allmo) then
            write(*,'("0 MOs) -> ALL")')
         else
            write(*,'(a," MOs)")') 
     .                          trim(charint(verify(charint,' '):7))
         endif !! (allmo) then
      elseif (index(trim(nome),'outsig').GT.0) then
         write(*,'(a,$)') 'OUTER SIGMA ORBS. # SIGMA = '
         allmo  = .FALSE.
         npi    = 0
         write(*,'(a,$)') 'SIGMA ('
         filsom = filw(1:index(filw,' ')-1)//'.sg'
         inquire(file=filsom,exist=filex)
         if (filex) then
            nfpi = 1
         else
            filsom = filw(1:index(filw,' ')-1)//'.som'
            inquire(file=filsom,exist=filex)
            if (filex) nfpi = 2
         endif !! (filex) then
         call openfile(filsom  ,isom,debug)
         select case(nfpi)
           case(0)
             write(*,'(a,a)') filw(1:index(filw,' ')-1),
     ,            '.{sg,som} not found! -> ALL'
             allmo   = .TRUE.
           case(1)
             read (isom,*,iostat=i) itmpmo,itmp,npi
             if (i.NE.0) stop 'ERROR reading .sg file'
           case(2)
             text   = ' respecto al eje: '
             call sudgfdim(18  ,text,isom,icde,1  ,jjj,debug)
             linear = jjj.EQ.0
             rewind(isom)
             if (linear) then
                text   = 'SIGMA respecto al ej'
             else
                text   = 'SIGMA respecto al pl'
             endif !! (linear) then
             npi    = 0
             jjj    = 0
             irw    = 1
             do while (jjj.EQ.0) 
                call sudgfchk(text,20,isom,icde,irw,jjj,debug)
                if (jjj.EQ.0) npi = npi+1
             enddo !! while (jjj.EQ.0) 
           case default
             stop 'SIGMA/PI identification code not implemented'
         end select !! case(nfpi)
         lsigma  = npi.GT.0
         allmo   = npi.LE.0
         write(charint,'(I7)') npi
         if (allmo) then
            write(*,'("0 MOs) -> ALL")')
         else
            write(*,'(a," MOs)")') 
     .                          trim(charint(verify(charint,' '):7))
         endif !! (allmo) then
      elseif (index(trim(nome),'outersh').gt.0) then
         outersh = .TRUE.
         write(*,'(A)') 'Inner s shells will be omitted'
      else
         call    hmnumb(nome,imos,ifrmo,debug)
         if (debug) print *,'processed=> ifrmo,imos= ',ifrmo,imos
         write(charint,'(I7)') imos
         write(*,'(A,A)') trim(charint(verify(charint,' '):7)),' MOs'
      endif !! (index(trim(nome),'all').gt.0) then
      locc = index(trim(nome),'occ').GT.0
      if (locc.AND.itype.EQ.1) stop 'fchk files do not contain occupatio
     +n numbers explicitly!!'
      if (locc) write(*,'("  >> Occupation numbers will be used to scale
     + overlap matrices <<")')
C >>> Select N-(DE)LOC index<<<  line 4
      read(iinp,'(a)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read'
      inpline = inpline + 1
      lloc    = index(trim(nome),'deloc').LE.0
      if (lloc)    write(*,'("  >> LOC indices will be considered <<")')
      lgiamb  = index(trim(nome),'giamb').GT.0
      if (lgiamb)
     +  write(*,'("  >> Only Giambiagi indices will be considered <<")')
      charint = trim(nome(1:index(nome,' ')))
      read (charint,*) nindex
      write(*,'("  >> ",a,"-(DE)LOC will be calculated <<")')trim(charin
     +t(verify(charint,' '):7))
      laom    = index(trim(nome),'aom').GT.0
      lbom    = index(trim(nome),'bom').GT.0
      if (lbom) then
         write(*,'("  >> Basin overlap matrices will be read <<")')
         write(*,'("  >> WARNING: Ring detection disabled with BOMs !! <
     +<")')
      else
         write(*,'("  >> Atomic overlap matrices will be read <<")')
         laom = .TRUE.
      endif !! (lbom) then
      if (lbom.AND.laom) stop 'AOMs and BOMs cannot be combined'
C >>> Initial data       <<<
      select case(itype)
        case(0)
          call    wfninfo(iwfn,nmo,nprim,nat,nheavy,debug)
          write(*,'("  >> NMOs,NPrims,NAtoms ... ",$)') 
        case(1)
          call   fchkinfo(iwfn,nmo,nprim,nat,nheavy,debug)
          write(*,'("  >> NMOs,NBFunc,NAtoms ... ",$)') 
        case(2)
          call moldeninfo(iwfn,moldentype,nmo,nprim,nat,nheavy,lpostHF,
     .                                                            debug)
          write(*,'("  >> NMOs,NPrims,NAtoms ... ",$)') 
      end select
      write(charint,'(I7)') nmo
      write(*,'(a,", ",$)') trim(charint(verify(charint,' '):7))
      write(charint,'(I7)') nprim
      write(*,'(a,", ",$)') trim(charint(verify(charint,' '):7))
      write(charint,'(I7)') nat
      write(*,'(a       )') trim(charint(verify(charint,' '):7))
      write(*,'("  >> NHeavy Atoms ......... ",$)') 
      write(charint,'(I7)') nheavy
      write(*,'(a       )') trim(charint(verify(charint,' '):7))
      if (lpi.AND.nfpi.EQ.1.AND.itmpmo.NE.nmo) 
     .      stop 'Incompatible # MO between .sg and wave function files'
C >>> Select Atoms or basins<<<  line 5
      read(iinp,'(a)',iostat=iii) nome
      if (iii.ne.0) stop ' ** PROBLEM while the input file was read'
      inpline = inpline + 1
      if (debug) print *,'nome +',trim(nome),'+'
      allatoms = .FALSE.
      allheavy = .FALSE.
      if (index(trim(nome),'all').GT.0) then
         allatoms = .TRUE.
         if (laom) then
            iato     = nat
         else if (lbom) then
            if (debug) PRINT *,'# of BOMs will be determined later ...'
         else
            stop 'AOM or BOM not detected.'
         endif !! (laom) then
      elseif (index(trim(nome),'heavy').GT.0) then
         if (lbom) stop 'This option is only for HEAVY ATOMS in AOMs!!'
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
     +,10X,"** CHECK THE ATOM ORDER !!! ***",/)')
      endif !! (allring) then
      write(charint,'(I7)') iato
      if (lbom) then
         write(*,'("  >> # Specified basins ... ",a)') trim(charint(veri
     +fy(charint,' '):7))
      else if (laom) then
         write(*,'("  >> # Specified atoms .... ",a)') trim(charint(veri
     +fy(charint,' '):7))
         if (iato.LT.nindex) stop ' ** PROBLEM :  The number of atoms is
     + not enough to perform the calculation'
      else
         stop 'AOM/BOM not detected in input file.'
      endif !! (lbom) then
      if (allring.AND..NOT.lbom.AND.laom) then 
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
            charreal = trim(nome(ipos(1)+1:j))
            read (charreal,*,iostat=iii) bonddist
            if (iii.NE.0) stop ' ** PROBLEM while the bond distance was 
     +read'
            if (debug) print *,bonddist,iii
         else
            bonddist = defbdist
         endif !! (iword.GT.2) then
      else
         if (lxyz) then
            write(*,'("  >> XYZ files will not be created <<",/,5X,
     +                    "(ring option is not included or disabled)")')
            lxyz = .FALSE.
         endif !! (lxyz) then
         if (allring) allring = .FALSE.
      endif !! (allring) then 
C >>> Number of DELOC inds <<<
C NINDEX     1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10- 11- 12- 13- 14
      goto (101,102,103,103,103,103,103,103,103,103,103,103,103,103)
     +                                                            nindex
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
     +fy(charint,' '):7))
      else
         write(*,'("  >> # (de)loc indices will be calculated with ring 
     +detection <<")')
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
         if (lbom) stop 'Option only compatible with AOM. Use mwfn.'
         genint = .FALSE.
         intfuk = .FALSE.
         if (debug) print *,'** atomic overlap matrix files: '
     +                     ,genint,intfuk
         write(*,'("  >> Files will be read from the input file << ")')
         inpline = inpline + 1
      elseif (index(trim(nome),'int').GT.0) then
         if (lbom) stop 'Option only compatible with AOM. Use mwfn.'
         inpline = inpline + 1
         genint  = .TRUE.
         intfuk  = .TRUE.
         laimall = .FALSE.
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     +tfuk
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'int'
      elseif (index(trim(nome),'aimall').GT.0) then
         if (lbom) stop 'Option only compatible with AOM. Use mwfn.'
         inpline = inpline + 1
         laimall = .TRUE.
         genint  = .TRUE.
         intfuk  = .TRUE.
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     +tfuk,' for AIMAll'
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'AIMAll int'
      elseif (index(trim(nome),'fuk').GT.0) then
         if (lbom) stop 'Option only compatible with AOM. Use mwfn.'
         genint = .TRUE.
         intfuk = .FALSE.
         inpline = inpline + 1
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     +tfuk
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'fuk'
      elseif (index(trim(nome),'eloc').GT.0) then
         if (lbom) stop 'Option only compatible with AOM. Use mwfn.'
         genint = .FALSE.
         intfuk = .TRUE.
         inpline = inpline + 1
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     +tfuk
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM File type ........ ",A)') 'eloc'
      elseif (index(trim(nome),'mwfn').GT.0) then
         genint      = .FALSE.
         intfuk      = .TRUE.
         lmwfn       = .TRUE.
         lmwfnrdname = .FALSE.
         filabom(:)  = ''
         if (index(trim(nome),'=read').GT.0) then
            lmwfnrdname = .TRUE.
            nome(:)     = ''
            read (iinp,'(A)') nome
            if(iii.ne.0)stop ' ** PROBLEM while the input file was read'
            filabom = trim(nome)
         else if (laom.AND..NOT.lbom) then
            filabom = 'AOM.txt'
         else if (.NOT.laom.AND.lbom) then
            filabom = 'BOM.txt'
         else
            stop 'AOM or BOM calculation type not detected'
         endif !! (index(trim(nome),'=read').GT.0) then
         inpline = inpline + 1
         if (debug) print *,'** atomic overlap matrix files: ',genint,in
     +tfuk
         if (debug) print *,'** input file finished'
         write(*,'("  >> AOM/BOM file type .... ",A)') 'mwfn'
C          (determine # basins if lbom enabled):
         if (lbom) then
            call openfile(filabom ,iom ,debug)
            text   = 'rlap matrix of basin'
            nbasin = 0
            jjj    = 0
            irw    = 1
            do while (jjj.EQ.0) 
               call sudgfchk(text,20,iom ,icde,irw,jjj,debug)
               if (jjj.EQ.0) nbasin = nbasin+1
            enddo !! while (jjj.EQ.0) 
            close(iom)
            if (allatoms) iato = nbasin
         endif !! (lbom) then
      else
         stop ' ** read,int,eloc,mwfn or fuk were not specified'
      endif !! (index(trim(nome),'int').gt.0) then
      if (genint) then 
         print *,' >> Checking number of zeros in the file names'
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
            charint = trim(nome(ipos(1)+1:j))
            read (charint,*,iostat=iii) nzeros
            if (iii.NE.0) stop ' ** PROBLEM while the number of zeros in
     + the file names was read'
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
         stop 'NOT IMPLEMENTED'
      endif
C >>> Check   data       <<<
      if (laom.AND.nindex.GT.nat   ) 
     +                           stop ' ** N-(DE)LOC index > NAtoms  **'
      if (lbom.AND.nindex.GT.nbasin)
     +                           stop ' ** N-(DE)LOC index > NBasins **'
      if (nmo.GT.maxmo ) stop ' ** Increase MAXMO, please **'
      if (nat.GT.maxato) stop ' ** Increase MAXATO, please **'
C
      start  = omp_get_wtime()

      call       goon(iwfn,iout,iinp,isom,nmo,nprim,nat,allmo,iato,iint,
     +     allatoms,allheavy,outersh,lpi,npi,lloc,nindex,ndeloc,inpline,
     .version,nheavy,itype,bonddist,mxring,allring,genint,intfuk,lgiamb,
     +  runparal,nproc,lsigma,loutsig,imos,ifrmo,ifrag,ixyz,lxyz,nzeros,
     .           locc,nbasin,laom,lbom,lmwfnrdname,lmwfn,linear,laimall,
     .                                    nfpi,lpostHF,moldentype,debug)
      finish = omp_get_wtime()
      write(iout,'(/)')
      if (allring) then
         write(charreal,'(F15.2)') qtime2-qtime1
         write(iout,'("   >> qcksort spent ",11("."),X,a," seconds")')
     +                           trim(charreal(verify(charreal,' '):15))
         write(charreal,'(F15.2)') rtime2-rtime1
         write(iout,'("   >> Connect analysis spent ",2("."),X,a," secon
     +ds")')
     +                           trim(charreal(verify(charreal,' '):15))
      endif !! (allring) then
      write(charreal,'(F15.2)') finish-start
      write(*   ,'(   "  >> Elapsed time ", 9("."),X,a," seconds")') 
     +                           trim(charreal(verify(charreal,' '):15))
      write(iout,'(   "   >> Elapsed time ",12("."),X,a," seconds")') 
     +                           trim(charreal(verify(charreal,' '):15))
C
      close(iwfn)
      close(isom)
      close(iinp)
      close(iout)
C
      stop
      end
!=======================================================================
C
