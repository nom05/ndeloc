      subroutine connect(nbond,ncomb2,ncomb,nindex,nat ,matcomb2,indx2,
     .                              xx,yy,zz,mxring,nring,matring,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical       debug,IFAULT,skip
c     integer, dimension (:,:,:), allocatable :: ibndpart
C
C----------------------------------------------------------------------
c     common /names/  filwfn,filinp,filout,filw,filsom
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension     xx(nat)  ,yy(nat),zz(nat)
      dimension     indx2(ncomb2)
      dimension     matcomb2(ncomb2,2)
      dimension     matring(mxring,nindex)

      integer,allocatable,dimension(:) :: irow,matats,ibndpsum,newtemp
      integer,allocatable,dimension(:,:) :: ibondmat,matcomb
C=======================================================================
C
c     external ndelocma
C***********************************************************************
c     nindex = 6
C***********************************************************************
      if (debug) print *,'In SUBROUTINE connect:',nbond,ncomb2,ncomb,nin
     .dex,nat ,'matcomb2 ,indx2,xx,yy,zz',mxring,nring,'matring',debug
      if (debug) print *,'                        nbond,ncomb2,ncomb,nin
     .dex,nat , matcomb2 ,indx2,xx,yy,zz ,mxring,nring, matring ,debug'

      allocate(irow(nat),matats(nat),ibndpsum(ncomb),newtemp(nindex))
      allocate(ibondmat(nat,nat),matcomb(ncomb,nindex))
C
C >>> Bond matrix        <<<
      ibondmat = 0
      do i = 1,nbond
         ibondmat(matcomb2(indx2(i),1),matcomb2(indx2(i),2)) = 1
         ibondmat(matcomb2(indx2(i),2),matcomb2(indx2(i),1)) = 1
      enddo !! i = 1,nbond
      if (debug) then
         print *,' ++ BOND MATRIX ++ bond=1'
         print '(200I3)',0,(i,i=1,nat)
         do i = 1,nat
            print '(200I3)',i,(ibondmat(i,j),j=1,nat)
         enddo !! i = 1,nat
      endif !! (debug) then
C >>> Remove 1-bond atoms<<<
      irow = 0
      do i = 1,nat
         do j = 1,nat
            irow(i) = irow(i)+ibondmat(i,j)
         enddo !! j = 1,nat
         if (irow(i).EQ.1) then
            do j = 1,nat
               ibondmat(i,j) = 0
               ibondmat(j,i) = 0
            enddo !! j = 1,nat
         endif !! (irow(i).EQ.1) then
      enddo !! i = 1,nat
      if (debug) then
         print *,' ++ BOND MATRIX without 1-bond atoms ++ bond=1'
         print '(200I3)',0,(i,i=1,nat)
         do i = 1,nat
            print '(200I3)',i,(ibondmat(i,j),j=1,nat)
         enddo !! i = 1,nat
      endif !! (debug) then
C >>> Generate n-comp combs <<<
      do i = 1,nat
         matats(i) = i
      enddo !! i = 1,nat
      call       ALLNR(nat,nindex,matats,ncomb,matcomb,IFAULT)
      if (IFAULT) stop ' ** Problem while the combinations were created 
     .**'
C >>> Create n-comp matrices<<< Sum matrix elements <<<<
c     allocate(ibndpart(ncomb,nindex,nindex))  !! Whims of the ifort compiler
      ibndpsum = 0
      if (debug) print *,'ncomb=',ncomb
      do i = 1,ncomb
         do j = 1,nindex
            do k = 1,nindex
               ibndpsum(i) = ibndpsum(i)+
     .                               ibondmat(matcomb(i,j),matcomb(i,k))
            enddo !! k = 1,nindex
         enddo !! j = 1,nindex
      enddo !! i = 1,ncomb
c     do n = 1,ncomb
c        print '(7I3)',0,(matcomb(n,i),i=1,nindex)
c        do i = 1,nindex
c           print '(7I3)',matcomb(n,i),(ibndpart(n,i,k),k=1,nindex)
c        enddo !! i = 1,nat
c        print *,'--- sum = ',ibndpsum(n)
c     enddo !! n = 1,ncomb
C >>> Check if Sum matrix elements = 2*nindex <<<
      nring = 0
      if (debug) print *,' ==== R I N G S ===='
      do n = 1,ncomb
         if (ibndpsum(n).EQ.2*nindex) then
C >>> Check if Rows sum > 1 <<<
            do i = 1,nindex
               itemp = 0
               do j = 1,nindex
                  itemp = itemp+ibondmat(matcomb(n,i),matcomb(n,j))
               enddo !! j = 1,nindex
               skip = itemp.LT.2
               if (skip) exit
            enddo !! i = 1,nindex
            if (skip) then
               if (debug) print *,' <**> ',(matcomb(n,i),i=1,nindex),' w
     .ill be skipped'
            else
               nring = nring+1
               if (nring.GT.mxring) stop ' ** Increase MXRING, please **
     .'
               do i = 1,nindex
                  matring(nring,i) = matcomb(n,i)
               enddo !! i = 1,nindex
C >>> Follow bonds to obtain the direct connected atoms in the ring <<<
               last = 0
               iat  = 1
               if (debug) then
                  print *,nring,'->',(matring(nring,i),i=1,nindex)
                  print '(33I3)',0,  (matring(nring,i),i=1,nindex)
               endif !! (debug) then
               do i = 1,nindex
                  if (debug) print '(33I3)',matring(nring,i),(ibondmat(m
     .atring(nring,i),matring(nring,j)),j=1,nindex)  !! n-comp connectivity matrix
                  do j = 1,nindex
                     if (ibondmat(matring(nring,iat),matring(nring,j)).G
     .T.0.AND.j.NE.last) then
                        last = iat
                        iat = j
                        newtemp(i) = matring(nring,iat)
                        goto 55
                     endif !! (ibondmat(matring(nring,i) ...
                  enddo !! j = 1,nindex
 55               continue
               enddo !! i = 1,nindex
               matring(nring,1) = newtemp(nindex)
               do i = 2,nindex
                  matring(nring,i) = newtemp(i-1)
               enddo !! i = 1,nindex
               if (debug) print *,'according to connectivity=',
     .                                     (matring(nring,i),i=1,nindex)
            endif !! (skip) then
         endif !! (ibndpsum(n).EQ.2*nindex) then
      enddo !! n = 1,ncomb
C
      if (debug) print *,'***** END *** connect'
      return
      end

