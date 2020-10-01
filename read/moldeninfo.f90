subroutine     moldeninfo(iwfn,moldentype,nmo,nprim,nat,nheavy,lpostHF,debug)
  implicit none
  integer             :: iok,jjj,iz,iat,nat2,nprimat,nshell,i,nprimd=0,nprimf=0,nprimg=0 &
                       , dfunc=5,ffunc=7,gfunc=9,ierror,moldentype,nocmo=0
  integer,intent(in)  :: iwfn
  integer,intent(out) :: nmo,nprim,nat,nheavy
  real     (kind=8 )  :: tmp
  logical,intent(in)  :: debug
  logical             :: lpostHF
  character( len=2 )  :: ch2
  character( len=20)  :: text,text2
  character( len=80)  :: line

  lpostHF     = .FALSE.
  nheavy      = 0
  nat         = 0
  nprim       = 0
  nmo         = 0
  nprimd      = 0
  nprimf      = 0
  nprimg      = 0
  nshell      = 0
  moldentype  = 0

  !moldentype = 0 << ORCA
  !moldentype = 1 << MOLCAS

  rewind(iwfn)
  read(iwfn,'(A)') line
  if (index(lowercase(trim(line)),'[molden format]').LE.0) stop 'Unknown Molden format'
  read(iwfn,'(A)') line
  if (index(lowercase(line),'[title]').GT.0) then
     read(iwfn,'(A)') line
     if (index(lowercase(trim(line)),'orca').EQ.0) then
        write(*,'("  >> WARNING: Molden files by ORCA and MOLCAS were implemented only. Check results <<")')
     else
        write(*,'("  >> ORCA molden format assumed <<")')
        moldentype = 0
     endif !! (index(lowercase(trim(line)),'orca').EQ.0) then
  else if (index(lowercase(line),'[n_atoms]').GT.0) then
     write(*,'("  >> MOLCAS molden format assumed <<")')
     moldentype = 1
     read(iwfn,*) nat2
  else
    stop 'FATAL ERROR: Molden format is unknown'
  endif
  text(:) = ''
  text    = '[Atoms]'
  if (moldentype.EQ.1) text = uppercase(trim(text))
  call sudgfchk(text,7,iwfn,iok,0,jjj,debug)
  if (iok.eq.1) then
     do
        read (iwfn,'(A)') line
        if (line(1:1).EQ." ".OR.index(line(1:1),"[").NE.0) exit
        read (line,*) text,text2,iz
        if (iz.GT.1) nheavy = nheavy+1
        nat = nat+1
     enddo
     backspace(iwfn)
  else
     stop 'FATAL ERROR: atomic coordinates not FOUND'
  endif
  if (debug) PRINT *,nat,'=nat,nheavy=',nheavy
  if (moldentype.EQ.1) then
     if (nat.NE.nat2) stop 'Wrong number of atoms determined in .molden file'
     do !! Read d,f and g types if MOLCAS
        read (iwfn,'(A)') text
        if (debug) PRINT *,trim(text)
        select case(trim(text))
          case('[5D]')
            dfunc=5
          case('[6D]')
            dfunc=6
          case('[7F]')
            ffunc=7
          case('[10F]')
            ffunc=10
          case('[9G]')
            gfunc=9
          case('[15G]')
            gfunc=15
          case default
            exit
        end select !! case(trim(text)
        if (debug) PRINT *,'type of d,f,g function: ',text
     enddo
  endif !! (moldentype.EQ.1) then
  nat2        = 0
  text(:) = ''
  text    = '[GTO]'
  call sudgfchk(text,5,iwfn,iok,0,jjj,debug)
  if (iok.eq.1) then
     do
        read (iwfn,'(A)') text
        if (index(text(1:1),"[").NE.0) exit
        if (debug) PRINT *,'basis set atom: ',trim(text)
        read (text,*) iat
        nat2 = nat2+1
        if (iat.GT.nat.OR.iat.NE.nat2) stop 'Error reading basis set info'
        do
           read (iwfn,'(A)') text
           if (debug) PRINT *,'exponets and type: ',text
           nshell = nshell+1
           read (text,*) ch2,nprimat
           select case(ch2)
             case('s')
               nprim = nprim+nprimat*1
             case('p')
               nprim = nprim+nprimat*3
             case('sp')
               nprim = nprim+nprimat*4
             case('d')
               nprimd = nprimd+nprimat
             case('f')
               nprimf = nprimf+nprimat
             case('g')
               nprimg = nprimg+nprimat
             case default
               stop 'This basis function is not implemented'
           end select !! case(ch2)
           do i = 1,nprimat
              read (iwfn,*)
           enddo !! i = 1,nprimat
           read (iwfn,'(A)') text
           if (text(1:7).EQ."       ".OR.index(text(1:1),"[").NE.0) exit
           backspace(iwfn)
        enddo
     enddo
     backspace(iwfn)
     if (moldentype.EQ.0) then !! Read d,f and g types if ORCA
        do
           read (iwfn,'(A)') text
           select case(trim(text))
             case('[5D]')
               dfunc=5
             case('[6D]')
               dfunc=6
             case('[7F]')
               ffunc=7
             case('[10F]')
               ffunc=10
             case('[9G]')
               gfunc=9
             case('[15G]')
               gfunc=15
             case default
               exit
           end select !! case(trim(text)
           if (debug) PRINT *,'type of d,f,g function: ',text
        enddo
     endif !! (moldentype.EQ.0) then
  else
    stop 'FATAL ERROR: basis set info not FOUND or Slater basis set'
  endif
  nprim = nprim + nprimd*dfunc + nprimf*ffunc + nprimg*gfunc
  if (debug) then
     PRINT *,nshell,'=nshell,nprim=',nprim
     PRINT *,nprimd,'=nprimd,nprimf=',nprimf
     PRINT *,nprimg,'=nprimg,d size=',dfunc
     PRINT *,ffunc,'=f size,g size=',gfunc
  endif !! (debug) then
  text(:) = ''
  text    = '[MO]'
  call sudgfchk(text,4,iwfn,iok,0,jjj,debug)
  do
    read(iwfn,'(A)',iostat=ierror) text
    if (ierror.NE.0.OR.text(1:1).EQ.'[') exit
    if (index(text(:20),'Occup=').GT.0) then
       read (text(index(text,'=')+1:len_trim(text)),*) tmp
       nmo = nmo+1
       if (tmp.NE.0.) nocmo = nocmo+1
       if (ceiling(tmp).GT.floor(tmp)) lpostHF = .TRUE.
       if (debug) PRINT *,'occupation number',nmo,'=',tmp,tmp.NE.0.,'=occup?'
    endif !! (index(lowercase(text(:10)),'occup=').GT.0) then
  enddo
  if (debug) then
     PRINT *,'postHF?=',lpostHF
     PRINT *,nocmo,'=occuped nmo,nmo=',nmo
  endif !! (debug) then
  if (.NOT.lpostHF) then
     nmo = nocmo
     if (debug) PRINT *,'nmo set to # occup orbs:',nmo
  endif !! (lpostHF) then

  contains

    function lowercase(str)    !! From libSUFR
      implicit none
      character,intent(in) :: str*(*)
      character            :: lowercase*(len(str))
      integer              :: i,ch

      lowercase = str
      do i=1,len_trim(lowercase)
         if (lowercase(i:i).NE.'[') then
            ch = ichar(lowercase(i:i))
            if (ch.GE.65.AND.ch.LE.91) ch = ch + 32
            lowercase(i:i) = char(ch)
         endif !! (lowercase(i:i).NE.'[') then
      end do
    end function lowercase

    function uppercase(str)    !! From libSUFR
      implicit none
      character,intent(in) :: str*(*)
      character            :: uppercase*(len(str))
      integer              :: i,ch

      uppercase = str
      do i=1,len_trim(uppercase)
         if (uppercase(i:i).NE.'[') then
            ch = ichar(uppercase(i:i))
            if (ch.GE.97.AND.ch.LE.123) ch = ch - 32
            uppercase(i:i) = char(ch)
         endif !! (uppercase(i:i).NE.'[') then
      end do
    end function uppercase

end subroutine moldeninfo
