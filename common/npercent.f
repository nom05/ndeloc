      subroutine npercent(ia,it,text,iformat)
      implicit real*8 (a-h,o-z)
      character*7   charint
      character*50  formato
      character*100 text
C
      write(charint,'(I7)') iformat+12
      formato = '(A1,10X,A,t'//trim(charint(verify(charint,' '):7))//',F
     .6.2,"%",$)'
      if (ia.LT.it) then
          write(*,formato)  achar(13),trim(text),real(ia)/real(it)*100.0
      elseif (ia.GT.it) then
          stop ' ** WRONG PERCENTAGE INPUT **'
      else
          write(*,formato)  achar(13),trim(text),                  100.0
      endif !! (ia.LT.it) then
      return
      end

