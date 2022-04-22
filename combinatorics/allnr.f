      subroutine allnr(n,itmp,mati,idi,matf,ifault)
C
C ALGORITHM AS 88 APPL.  STATIST.  (1975) VOL.24, NO.3
C
      integer itmp, iarray(itmp), mati(n),matf(idi,itmp)
      logical ifault

      ifault = .TRUE.
      if (itmp.LT.1.OR.itmp.GT.n) return
      ifault = .FALSE.
      icount = 0
      nn     = n-itmp
      i      = 1
      iarray(1)   = 1
 1    if (i.EQ.itmp) goto 2
      ip1    = i+1
      do l   = ip1,itmp
         iarray(l) = iarray(l-1) + 1
      enddo
 2    icount = icount+1
      matf(icount,:itmp) = mati(iarray(:itmp))
      i      = itmp
 3    if (iarray(i).LT.nn+i) goto 4
      i      = i-1
      if (i.LE.0) return
      goto 3
 4    iarray(i)   = iarray(i)+1
      goto 1
      end

