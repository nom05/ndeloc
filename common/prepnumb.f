      subroutine prepnumb(imos,moc    ,nome,debug)
C
C-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      logical        debug
      character*7    charint,charintd
      character*1000 nome
C
C----------------------------------------------------------------------
C     parameter (a=b)
C----------------------------------------------------------------------
C
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C=======================================================================
C
      dimension      moc(imos)
C***********************************************************************
C     iout       = 4
C***********************************************************************
      if (debug) print *,'In SUBROUTINE prepnumb:',imos,'moc,nome',debug
      if (debug) print *,'                         imos, moc,nome ,debug
     .'
C
      do i = 1,1000
         nome(i:i) = ' '
      enddo !! i = 1,1000
      j = 0
      n = 0
      do i = 1,imos
         if (moc(i)-j.NE.n) then
            if (debug) print *,moc(i),moc(i)-j,n
            n = moc(i)
            write(charint ,'(I7)') n
            if (i.NE.1) then
               if (j.EQ.1) then
                  nome = nome(1:len(trim(nome)))//
     .                                                             ','//
     .                          trim(charint (verify(charint ,' '):7))
               else
                  write(charintd,'(I7)') moc(i-1)
                  nome = nome(1:len(trim(nome)))//
     .                                                             '-'//
     .                          trim(charintd(verify(charintd,' '):7))//
     .                                                             ','//
     .                          trim(charint (verify(charint ,' '):7))
               endif !! (j.EQ.1) then
            else
                  nome = nome(1:len(trim(nome)))//
     .                              trim(charint(verify(charint,' '):7))
            endif !! (i.NE.1) then
            j = 1
            if (debug) print *,trim(nome)
         else
            j = j+1
            if (i.EQ.imos) then
                  write(charint ,'(I7)') moc(i)
                  nome = nome(1:len(trim(nome)))//
     .                                                             '-'//
     .                          trim(charint (verify(charint ,' '):7))  
            endif !! (i.EQ.imos) then
         endif !! (moc(i)-j.NE.n) then
      enddo !! i = 1,imos
      if (debug) print *,'numbers= "',trim(nome),'"'
C
      return
      end
C=======================================================================
C

