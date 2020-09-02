      function nbinom(iup,idown,nmax,debug)
      implicit real*8 (a-h,o-z)
      logical   debug
      dimension c(0:nmax,0:nmax)
      if (debug) print *,'function nbinom=',iup,idown,nmax,debug
      c = 0
      c(0,0) = 1
      c(1,0) = 1
      c(1,1) = 1
      do n = 2,nmax
         c(n,0) = 1
         c(n,n) = 1
         do m = 1,n-1
            c(n,m) = c(n-1,m-1) + c(n-1,m)
            if (debug) print *,n,m,c(n,m)
         enddo !! m = 1,n-1
      enddo !! n = 2,nmax
      nbinom = int(c(iup,idown))
      if (debug) print *,'binomial coeff=',c(iup,idown)
      if (debug) print *,' ** END function nbinom **'
      return
      end
C=======================================================================
C

