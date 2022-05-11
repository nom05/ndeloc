! ** ./ndeloc.param.h >>
!
!  Copyright (c) 2022  Nicolás Otero Martínez - Marcos Mandado Alonso

!  This file is part of the NDELOC program available in:
!      https://github.com/nom05/ndeloc
!
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.


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
