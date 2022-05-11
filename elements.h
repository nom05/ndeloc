! ** ./elements.h >>
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


C
C  ** KEEP THIS FORMAT, PLEASE **
C
      character*3   elem
      dimension  elem(0:80)
      data elem/'b  ',
     .          'H  ','He ','Li ','Be ','B  ','C  ','N  ','O  ','F  ',
     .          'Ne ','Na ','Mg ','Al ','Si ','P  ','S  ','Cl ','Ar ',
     .          'K  ','Ca ','Sc ','Ti ','V  ','Cr ','Mn ','Fe ','Co ',
     .          'Ni ','Cu ','Zn ','Ga ','Ge ','As ','Se ','Br ','Kr ',
     .          'Rb ','Sr ','Y  ','Zr ','Nb ','Mo ','Tc ','Ru ','Rh ',
     .          'Pd ','Ag ','Cd ','In ','Sn ','Sb ','Te ','I  ','Xe ',
     .          'Cs ','Ba ','La ','Ce ','Pr ','Nd ','Pm ','Sm ','Eu ',
     .          'Gd ','Tb ','Dy ','Ho ','Er ','Tm ','Yb ','Lu ','Hf ',
     .          'Ta ','W  ','Re ','Os ','Ir ','Pt ','Au ','Hg '      /
