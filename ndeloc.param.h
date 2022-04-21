C----------------------------------------------------------------------
C---- DEFAULT PARAMETERS ----------------------------------------------
C----------------------------------------------------------------------
      parameter (maxmo   = 570         )
      parameter (maxato  = 300         )
      parameter (mxring  =  25         )
      parameter (defbdist= 1.6         ) !! Default bond distance
      parameter (nzerodef=   3         ) !! If nzerodef=2 => X01
      parameter (iinp    =  10         )
      parameter (iwfn    =  11         )
      parameter (iout    =  12         )
      parameter (iint    =  13         )
      parameter (isom    =  14         )
      parameter (ixyz    =  15         )
      parameter (extndinp='.ndinp'     ) !! Modify the size(character*6)
      parameter (extndout='.ndout'     ) !! Modify the size(character*6)
      parameter (extxyz  ='.xyz'       ) !! Modify the size(character*4)
C----------------------------------------------------------------------
      version            = '1.2.17'      !! 220421
      debug              = .FALSE.       !! This option can be set from 
c     debug              = .TRUE.        !! the input file.
C----------------------------------------------------------------------
