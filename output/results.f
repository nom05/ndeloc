C-----------------------------------------------------------------------
      subroutine results(iout,nmo,nprim,nheavy,cloc,deloc,nindex,ndeloc,
     . iato,iatomat,nato,iq,matindx,atoms,filint,allmo,outersh,lpi,lloc,
     .    giamb,lgiamb,version,itmo,itype,bonddist,allring,ntotdel,luhf,
     . loutsig,lsigma,genint,intfuk,moc,ixyz,lxyz,xx,yy,zz,xyzgc,nzeros,
     .                       locc,lpostHF,nbasin,laom,lbom,linear,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical        debug,lloc,allmo,lpi,outersh,allring,lgiamb,
     .               lsigma,loutsig,linear,laom,lbom,locc,lpostHF
      logical        luhf,lxyz,genint,intfuk
      integer        itype !! =0 wfn, =1 fchk, =2 molden
      character*3    aombom
      character*7    charint,charintd,atoms,version
      character*15   charreal,atomlabel
      character*54   formato
      character*100  filwfn,filinp,filout,filw,filsom,filint,filxyz,
     .               filouw
      character*1000 nome
C
C----------------------------------------------------------------------
      include '../elements.h' !! Includes char definitions,dimension,data
      include '../cnstants.h' !! Parameters
C----------------------------------------------------------------------
      common /names/ filwfn,filinp,filout,filw,filsom,filouw
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension   deloc(ndeloc),giamb(ndeloc)
      dimension iatomat(*)
      dimension      iq(nato)
      dimension     moc(*)
      dimension      xx(*)   , yy(*), zz(*)
      dimension   xyzgc(3,ndeloc)
      dimension matindx(ntotdel,nindex)
      dimension  filint(iato),atoms(iato),cloc(iato),deloctot(iato)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE results:',iout,nmo,nprim,nheavy,
     .cloc,deloc,nindex,ndeloc,iato,'iatomat',nato,iq,'matindx',trim(ato
     .ms(1)),trim(filint(1)),allmo,outersh,lpi,lloc,giamb,lgiamb,version
     .,itmo,itype,bonddist,allring,ntotdel,luhf,lpostHF,loutsig,lsigma,g
     .enint,intfuk,'moc',ixyz,lxyz,'xx,yy,zz,xyzgc',nzeros,nbasin,laom,l
     .bom,linear,debug

      if (debug) print *,'                        iout,nmo,nprim,nheavy,
     .cloc,deloc,nindex,ndeloc,iato, iatomat ,nato,iq, matindx ,     ato
     .ms(1) ,     filint(1) ,allmo,outersh,lpi,lloc,giamb,lgiamb,version
     .,itmo,itype,bonddist,allring,ntotdel,luhf,lpostHF,loutsig,lsigma,g
     .enint,intfuk, moc ,ixyz,lxyz, xx,yy,zz,xyzgc ,nzeros,nbasin,laom,l
     .bom,linear,debug'
C >>> Open output file
      open(unit=iout,file=filout,status='unknown',form='formatted')
C >>> HEADER <<<
      write(charint ,'(I7)') nindex
      i = len(trim(charint(verify(charint,' '):7)))+len(trim(filw))+28
      write(charintd,'(I7)') i
      formato='(9X,'//trim(charintd(verify(charintd,' '):7))//'("+"))'
      write(iout,'(/)')
      write(iout,formato)
      write(iout,'(14X,a,a)') ' N D E L O C  v.',trim(version)
      write(iout,formato)
      write(iout,'(9X,"+ ",a,"-(DE)LOC calculation :  ",a," +")') trim(c
     .harint(verify(charint,' '):7)),trim(filw)
      write(iout,formato)
C >>> SUMMARY <<<
      write(iout,'(2(/),3X,">> SUMMARY :",/        )')
      write(iout,'(     5X,"* INPUT file ............ ",a)')trim(filinp)
      select case(itype)
        case(0)
          write(iout,'(     5X,"* WFN   file ............ ",a)')
     .                                                      trim(filwfn)
          write(iout,'(     5X,"* Wave function type .... ",$)')
          if (lpostHF) then
             write(iout,'(A)') 'post-HF or 2hybrid-KS'
          else
             write(iout,'(A)') 'HF/KS'
          endif !! (lpostHF) then
          write(iout,'(     5X,"* NMO,NPRIMS,NAtoms ..... ",$)')
        case(1)
          write(iout,'(     5X,"* FCHK   file ........... ",a)')
     .                                                      trim(filwfn)
          write(iout,'(     5X,"* NMO,NBFUNC,NAtoms ..... ",$)')
        case(2)
          write(iout,'(     5X,"* MOLDEN file ........... ",a)')
     .                                                      trim(filwfn)
          write(iout,'(     5X,"* Wave function type .... ",$)')
          if (lpostHF) then
             write(iout,'(A)') 'post-HF or 2hybrid-KS'
          else
             write(iout,'(A)') 'HF/KS'
          endif !! (lpostHF) then
          write(iout,'(     5X,"* NMO,NPRIMS,NAtoms ..... ",$)')
      end select !! case(itype)
      write(charintd,'(I7)') nmo
      write(iout,'(a,", ",$)') trim(charintd(verify(charintd,' '):7))
      write(charintd,'(I7)') nprim
      write(iout,'(a,", ",$)') trim(charintd(verify(charintd,' '):7))
      write(charintd,'(I7)') nato
      write(iout,'(a," " ,$)') trim(charintd(verify(charintd,' '):7))
      if (luhf) then
         write(iout,'("(",a,")")') 'Unrestricted'
      else
         write(iout,'("(",a,")")') 'Restricted'
      endif !! (luhf) then
      write(charintd,'(I7)') nheavy
      write(iout,'(     5X,"* # Heavy atoms ......... ",a)') 
     .                            trim(charintd(verify(charintd,' '):7))
!!!!!!!!!!!!!!!! O R B I T A L S !!!!!!!!!!!!!!!
      write(iout,'(     5X,"* Specified orbitals .... ",$)') 
      call       prepnumb(itmo,moc    ,nome,debug)
      if (allmo)       then
         write(iout,'(A,$)') trim(nome)
         write(charintd,'(I7)') itmo
         write(iout,'(X,A,A,A)')
     .                 'ALL(',trim(charintd(verify(charintd,' '):7)),')'
      elseif (lpi)     then
         write(iout,'(A,$)') trim(nome)
         write(charintd,'(I7)') itmo
         write(iout,'(X,A,A,A)')
     .                  'PI(',trim(charintd(verify(charintd,' '):7)),')'
      elseif (lsigma)  then
         write(iout,'(A,$)') trim(nome)
         write(charintd,'(I7)') itmo
         write(iout,'(X,A,A,A)')
     .               'SIGMA(',trim(charintd(verify(charintd,' '):7)),')'
      elseif (outersh) then
         write(iout,'(A,$)') trim(nome)
         write(charintd,'(I7)') itmo
         write(iout,'(X,"(",A,")",A)')
     .                           trim(charintd(verify(charintd,' '):7)),
     .                                  ' (inner s shells were omitted)'
      elseif (loutsig) then
         write(iout,'(A,$)') trim(nome)
         write(charintd,'(I7)') itmo
         write(iout,'(X,A,A,A)')
     .         'OUTER SIGMA(',trim(charintd(verify(charintd,' '):7)),')'
      else
         write(iout,'(A,$)') trim(nome)
         write(charint,'(I7)' ) itmo
         write(iout,'(X,"(",A,")")')trim(charint(verify(charint,' '):7))
      endif !! (allmo) then
      if (lpostHF) write(iout,'(    5X,"* PostHF automatic orbital trunc
     .. by null occ. numbers *")')
      if (linear) write(iout,'(     5X,"* Linear molecule *")')
      if (locc)   write(iout,'(     5X,"* Occupation numbers used to sca
     .le overlap matrices *")')
      write(iout,'(     5X,"* (DE)LOC index order ... ",a)')
     .                              trim(charint(verify(charint,' '):7))
      if (lloc) then
         write(iout,'(     5X,"* LOC indices included .. YES")') 
      else
         write(iout,'(     5X,"* LOC indices included .. NO (Available i
     .f N>2)" )') 
      endif !! (lloc) then
      i = ndeloc
      if (lloc) i = i+iato
      write(charint,'(I7)') i
      write(iout,'(     5X,"* # (DE)LOC indices ..... ",a,
     .             " (LOC+DELOC)")')trim(charint(verify(charint,' '):7))
      if (laom) then
         write(iout,'(     5X,"* Atomic overlap matrices (AOM) employed 
     .*")')
         write(iout,'(     5X,"* Considered atoms ...... ",$)')
         aombom(1:3) = 'AOM'
      else if (lbom) then
         write(iout,'(     5X,"* Basin  overlap matrices (BOM) employed 
     .*")')
         write(charint,'(I7)' ) nbasin
         write(iout,'(     5X,"* Total # basins ........ ",A)')
     .                              trim(charint(verify(charint,' '):7))
         write(iout,'(     5X,"* Considered basins...... ",$)')
         aombom(1:3) = 'BOM'
      else
         stop 'Structural problem in the program, Calculation type not d
     .etected'
      endif
      call       prepnumb(iato,iatomat,nome,debug)
      write(iout,'(A,$)') trim(nome)
      write(charint,'(I7)' ) iato
      write(iout,'(X,"(",A,")")') trim(charint(verify(charint,' '):7))
      if (allring) then
         write(charint ,'(I7)'   ) ndeloc
         write(iout,'(     5X,"* # detected rings ...... ",a)')
     .                              trim(charint(verify(charint,' '):7))
         write(charreal,'(F15.3)') bonddist
         write(iout,'(     5X,"* Bond dist cut-off ..... ",a)')
     .                           trim(charreal(verify(charreal,' '):15))
         if (lxyz) write(iout,'(     5X,"* XYZ Files with DELOC .. YES")
     .')
      endif !! (allring) then
      write(iout,'(/)')
C >>> AOM/BOM FILES   <<<
      write(iout,'(     3X,">> FILES   :",/        )')
      if (.NOT.genint.AND.intfuk) then         !! >>>>>>>>>>>>> ELOC
         write(iout,'(     5X,"* ",A," File ",I3," .......... ",a)') 
     .                                    trim(aombom),1,trim(filint(1))
      else
         do i = 1,iato
            write(iout,'(  5X,"* ",A," File ",I3," .......... ",a)') 
     .                                    trim(aombom),i,trim(filint(i))
         enddo !! i = 1,iato
      endif !! (.NOT.genint.AND.intfuk) then   !! >>>>>>>>>>>>> ELOC
C >>> XYZ FILES   <<<
      if (lxyz) then
         write(charint ,'(I7)') nindex
         filxyz = trim(filouw)//'-ndel'//trim(charint(verify(charint,' '
     .):7))//'-gia'//'.xyz'
         if (debug) print *,' * Writing xyz file ','"',trim(filxyz),'"'
         write(iout,'(/,7X,"Multiplication factors (gia,pon): 10000,1000
     .")')
         write(iout,'(  5X,"* XYZ File ",14("."),X,A  )') trim(filxyz)
         open(unit=ixyz,file=filxyz,status='unknown',form='formatted')
         do j = 1,nato
            write(ixyz,'( A2 ,3(3X,F10.6))') trim(elem(iq(j))),
     .                         xx (j)*bohr2a,yy (j)*bohr2a,zz (j)*bohr2a
         enddo !! i = 1,nato
         do j = 1,ndeloc                      !! Conv. au -> A alr. done 
            write(charreal,'(F15.3)') 10000*giamb(j)
            write(ixyz,'("X ",3(3X,F10.6),6X,A,A,A)')   
     .(xyzgc(i,j),i=1,3),'"',trim(charreal(verify(charreal,' '):15)),'"'
         enddo !! i = 1,nato
         close(ixyz)
         if (.NOT.lgiamb) then
            write(charint ,'(I7)') nindex
            filxyz = trim(filouw)//'-ndel'//trim(charint(verify(charint,
     .' '):7))//'-pon'//'.xyz'
            if (debug) print *,' * Writing xyz file ','"',trim(filxyz),'
     ."'
            write(iout,'(  5X,"* XYZ File ",14("."),X,A,/)')trim(filxyz)
           open(unit=ixyz,file=filxyz,status='unknown',form='formatted')
            do j = 1,nato
               write(ixyz,'( A2 ,3(3X,F10.6))') trim(elem(iq(j))),
     .                         xx (j)*bohr2a,yy (j)*bohr2a,zz (j)*bohr2a
            enddo !! i = 1,nato
            do j = 1,ndeloc                   !! Conv. au -> A alr. done 
               write(charreal,'(F15.3)') 1000*deloc(j)
               write(ixyz,'("X ",3(3X,F10.6),6X,A,A,A)')   
     .(xyzgc(i,j),i=1,3),'"',trim(charreal(verify(charreal,' '):15)),'"'
            enddo !! i = 1,nato
            close(ixyz)
         else
            write(iout,'(/)')
         endif !! (.NOT.lgiamb) then
      endif !! (lxyz) then
C >>> RESULTS <<<
      write(iout,'((/),3X,">> RESULTS :",/        )')
      if (nindex.EQ.2) then                   !! =2-DELOC indices
         write(charint,'(I5)') iato
         formato = '(26x,'//trim(charint(verify(charint,' '):5))//'(3X,A
     .5,3X))'
         if (debug) print *,'Formats :  ',trim(formato)
         write(iout,formato)(trim(atoms(i)),i=1,iato)
         formato = '(a20,1x,a5,1x,'//trim(charint(verify(charint,' '):5)
     .)//'F11.5)'
         if (debug) print *,'Formats :  ',trim(formato)
         deloctot = 0.0
         do i = 1,iato
            if (debug) print *,(1+(i-1)*(i-2)/2),(i*(i-1)/2)
            write(iout,formato) trim(filw),trim(atoms(i)),(deloc(j),j=(1
     .+(i-1)*(i-2)/2),i*(i-1)/2),cloc(i)
            do j = 1+(i-1)*(i-2)/2,(i*(i-1)/2)
               deloctot(i) = deloctot(i)+deloc(j)
            enddo !! j = 1+(i-1)*(i-2)/2,(i*(i-1)/2)
            do j = i,iato-1
               deloctot(i) = deloctot(i)+deloc(i+j*(j-1)/2)
            enddo !! j = i,iato-1
         enddo !! i = 1,iato
         write (iout,'(/)') 
         deloctot = deloctot/2
         write(iout,formato) 
     .                  trim(filw),' TOT',(cloc(i)+deloctot(i),i=1,iato)
         write (iout,'(/,"LOC :")') 
         do i = 1,iato
            write (iout,'(a,F11.5)') trim(atoms(i)),cloc(i)
         enddo !! i = 1,nato
         write (iout,'(/,"DELOC :")') 
         do i = 1,iato
            do j= i+1,iato
               write(iout,'(I5,1X,I5,F11.5)') iatomat(i),iatomat(j),
     .                                            deloc(i+(j-1)*(j-2)/2)
            enddo !! ib=ia+1,nato
         enddo !! ia=1,nato
      elseif (nindex.GT.2) then                   !! >2-DELOC indices
         if (lloc) then
            write(charint,'(I5)') iato
            formato = '(29x,'//trim(charint(verify(charint,' '):5))//'(3
     .x,a5,3x))'
            if (debug) print *,'Formats :  ',trim(formato)
            write(iout,formato)(trim(atoms(i)),i=1,iato)
            formato = '(A20,X,a,"-LOC",X,3("."),'//trim(charint(verify(c
     .harint,' '):5))//'F11.5)'
            if (debug) print *,'Formats :  ',trim(formato)
            write(charint,'(I5)') nindex
            write(iout,formato) trim(filw),trim(charint(verify(charint,'
     . '):5)),(cloc(j),j=1,iato)
            write(iout,'(/)') 
         endif !! (lloc) then
C >>> GIAMBIAGI INDEX ONLY
         if (lgiamb) then
            write(charint ,'(I5)') nindex
            write(charintd,'(I5)') nindex*4+4+len(trim(charint))
            formato = '(A20,X,A,"-DELOC :",'//trim(charintd(verify(chari
     .ntd,' '):5))//'X,A11)'
            write(iout,formato)
     .                  trim(filw),trim(charint(verify(charint,' '):5)),
     .                             adjustr("Giambiagi")
            write(charint,'(I5)') nindex
            formato = '(5X,"* Index ",I3,X,13("."),X,'//trim(charint(ver
     .ify(charint,' '):5))//'(A,X),8X,1X,1PE10.3)'
            if (debug) then
               print *,'Formats :  ',trim(formato)
               print *,'matindx=',(matindx(1,j),j=1,nindex)
            endif !! (debug) then
            if (laom) then
               do i = 1,ndeloc
                  write(iout,formato) i,(trim(atomlabel(matindx(i,j)
     ,        ,nato,iq(matindx(i,j)),nzeros,debug)),j=1,nindex),giamb(i)
               enddo !! i = 1,ndeloc
            else if (lbom) then
               do i = 1,ndeloc
                  write(iout,formato) i,(trim(atomlabel(matindx(i,j)
     ,        ,nato,0               ,nzeros,debug)),j=1,nindex),giamb(i)
               enddo !! i = 1,ndeloc
            endif !! (laom) then
         else
C >>> BOTH      INDICES (Giambiagi,Ponec)
            write(charint ,'(I5)') nindex
            write(charintd,'(I5)') nindex*4+4+len(trim(charint))
            formato = '(A20,X,A,"-DELOC :",'//trim(charintd(verify(chari
     .ntd,' '):5))//'X,A11,A11)'
            write(iout,formato)
     .                  trim(filw),trim(charint(verify(charint,' '):5)),
     .                             adjustr("Giambiagi"),adjustr("Ponec")
            write(charint,'(I5)') nindex
            formato = '(5X,"* Index ",I3,X,13("."),X,'//trim(charint(ver
     .ify(charint,' '):5))//'(A,X),8X,2(1X,1PE10.3))'
            if (debug) then
               print *,'Formats :  ',trim(formato)
               print *,'matindx=',(matindx(1,j),j=1,nindex)
            endif !! (debug) then
            if (laom) then
               do i = 1,ndeloc
                  write(iout,formato) i,(trim(atomlabel(matindx(i,j)
     ,        ,nato,iq(matindx(i,j)),nzeros,debug)),j=1,nindex),giamb(i)
     ,                                                         ,deloc(i)
               enddo !! i = 1,ndeloc
            else if (lbom) then
               do i = 1,ndeloc
                  write(iout,formato) i,(trim(atomlabel(matindx(i,j)
     ,        ,nato,0               ,nzeros,debug)),j=1,nindex),giamb(i)
     ,                                                         ,deloc(i)
               enddo !! i = 1,ndeloc
            endif !! (laom) then
         endif !! (lgiamb) then
      endif !! (ndeloc.EQ.2) then
C
      if (debug) print *,'***** END *** results ***'
      return
      end
