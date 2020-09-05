subroutine p1fromgauss(inp,ncf,S)

!     use commonmod ,only : ndimtriang
      
      implicit none
      integer,intent(in) :: inp
      integer,intent(in) :: ncf
      integer            :: istart,irow,l,ir,ierr
      integer,parameter  :: numcol=5
      real(kind=8)       :: S(ncf*(ncf+1)/2)
!       
      do istart = 1,ncf,numcol
        read (inp,*)
        do irow = istart,ncf
           l  = min(irow,istart+numcol-1)
           ir = irow*(irow-1)/2
           read (inp,'(6X,5(F14.8))',iostat=ierr) S(ir+istart:ir+l)
           if (ierr.NE.0) stop 'Error 1 in p1fromgauss'
        enddo !! irow = istart,ncf
      enddo !! istart = 1,ncf

end subroutine p1fromgauss
