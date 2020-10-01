subroutine     readmold(iwfn,moldentype,nmo,nprim,nat,debug,xx,yy,zz,nheavy,iq,pel,iheavy,luhf,lpostHF)

  implicit none

  integer                       ,intent(in) :: iwfn,nmo,nprim,nat,nheavy,moldentype
  integer                                   :: iok,jjj,i,j,nheavy2,ierror,nmo2,nocmo
  integer          ,dimension(:)            :: iq(nat),iheavy(nat)
  real     (kind=8),dimension(:)            :: xx(nat),yy(nat),zz(nat) &
                                             , pel(nmo)
  real     (kind=8)                         :: tmp
  logical                       ,intent(in) :: debug,lpostHF
  logical                                   :: luhf,lpostHF2
  character(len=20)                         :: text,text2

  text(:) = ''
  text    = '[Atoms]'
  if (moldentype.EQ.1) text = uppercase(text)
  call sudgfchk(text,7,iwfn,iok,0,jjj,debug)
  do i = 1,nat
     read (iwfn,*) text,text2,iq(i),xx(i),yy(i),zz(i)
  enddo !! i = 1,nat
  if (debug) PRINT '(X,A,/,3(F9.3),I4)','coords,charge=',(xx(i),yy(i),zz(i),iq(i),i=1,nat)
! >>> Detect HEAVY Atoms <<<
  nheavy2 = 0
  do i = 1,nat
     if (iq(i).GT.1) then
        nheavy2         = nheavy2+1
        iheavy(nheavy2) = i
     endif !! (iq(i).GT.1) then
  enddo !! i = 1,nat
  if (nheavy.NE.nheavy2) stop 'Detection of heavy atoms failed!!'
  if (debug) PRINT *,'heavy atoms=',(iheavy(i),i=1,nheavy)
! >>> Molecular Orbitals <<<
  text(:) = ''
  text    = '[MO]'
  call sudgfchk(text,4,iwfn,iok,0,jjj,debug)
  nocmo = 0
  nmo2  = 0
  do
    read(iwfn,'(A)',iostat=ierror) text
    if (ierror.NE.0.OR.text(1:1).EQ.'[') exit
    if (index(text(:20),'Occup=').GT.0) then
       read (text(index(text,'=')+1:len_trim(text)),*) tmp
       nmo2 = nmo2+1
       if (tmp.NE.0..AND..NOT.lpostHF) then
          nocmo      = nocmo+1
          if (nocmo.GT.nmo) stop 'Wrong # MO detected (monodeterminant)'
          pel(nocmo) = tmp
       else if (lpostHF) then
          if (nmo2.GT.nmo) stop 'Wrong # MO detected (multideterminant)'
          pel(nmo2)  = tmp
       endif !! (tmp.NE.0..AND..NOT.lpostHF) then
       if (debug) PRINT *,'occupation number',nmo2,'=',tmp,tmp.NE.0.,'=occup?'
    endif !! (index(text(:10),'occup=').GT.0) then
  enddo
  if (debug) then
     PRINT *,'final pel=',pel
  endif !! (debug) then
! >>> Detect UHF
  luhf = .FALSE.
  if (nint(pel(1)).NE.2) luhf=.TRUE.
  if (debug) PRINT *,'luhf=',luhf
! >>> Detect Multideterminant wf
  lpostHF2 = any(ceiling(pel).GT.floor(pel))
  if (lpostHF.NEQV.lpostHF2) stop 'Wrong wf detection'
  if (debug) PRINT *,'lpostHF(any(ceiling(pel).GT.floor(pel)))=',lpostHF2

  contains

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

end subroutine readmold
