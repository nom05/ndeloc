      function nextp(n,a)
C
      integer n,a,i,j,k,t
      logical nextp
      dimension a(n)
C
      i     = n-1
   10 if (a(i).lt.a(i+1)) goto 20
      i     = i-1
      if (i.eq.0) goto 20
      goto 10
   20 j     = i+1
      k     = n
   30 t     = a(j)
      a(j)  = a(k)
      a(k)  = t
      j     = j+1
      k     = k-1
      if (j.lt.k) goto 30
      j     = i
      if (j.ne.0) goto 40
      nextp = .FALSE.
      return
   40 j     = j+1
      if (a(j).lt.a(i)) goto 40
      t     = a(i)
      a(i)  = a(j)
      a(j)  = t
      nextp = .true.
C
      return
      end
C=======================================================================
C

