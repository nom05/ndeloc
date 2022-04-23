# _NDELOC_

## About _NDELOC_

    Name of the Program: NDELOC
    Program Version : 1.2.17
    Program Version Date: Apr 22, 2022
    Manual  Version Date: Apr 23, 2022

_NDELOC_ performs (de)localization indices calculations by means of the use of real space partitionings (Hirshfeld, QTAIM, ELF, ...)

## How to cite

The following publications must be cited in any work presenting results obtained with _NDELOC_:

   - Mandado, M.; González-Moa, M. J.; Mosquera, R. A. "QTAIM n-center delocalization indices as descriptors of aromaticity in mono and polyheterocycles", *J. Comput. Chem.*, 28, 127-136 (**2007**).
   - Mandado, M.; González-Moa, M. J.; Mosquera, R. A. "Chemical graph theory and n-center electron delocalization indices: A study on polycyclic aromatic hydrocarbons", *J. Comput. Chem.*, 28, 1625-1633 (**2007**).
   - Karamanis, P.; Otero, N.; Pouchan, C. "Unleashing the Quadratic Nonlinear Optical Responses of Graphene by Confining White-Graphene (h-BN) Sections in Its Framework", *J. Am. Chem. Soc.*, 136, 7464-7473 (**2014**).

## Licensing and Distribution 

_NDELOC version 1.2.17_

MIT LICENSE

Copyright (c) 2022, Nicolás Otero Martínez (nom05 (at) uvigo.es) and Marcos Mandado Alonso (mandado (at) uvigo.es)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software
is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.


## Description of files

Contents of the folders distributed in this version:
To be finished ...        

## Installation

*NOTE1:* The preferred platform or, more specifically, the platform the developers use is GNU/Linux. MS Windows is an untested alternative. Through WSL or WSL2 the compilation would be equivalent to the following instructions. It is beyond the scope of this manual to explain how to activate these options in MS Windows.

Follow the instructions:
   - Clone firstly the repository: git clone https://github.com/nom05/ndeloc
	 - Enter in the directory: cd ndeloc
   - Verify your current CMake version is equal or greater than 2.8.12 (probably we will change the requirements because this version is obsolete): cmake --version
   - Create a new directory called ``build'' and enter inside, for example: mkdir build; cd build
   - Next, create the compilation environment: cmake ..
      - CMake will detect the compiler searching from several targets. Usually, the first one it finds is GNU Fortran (gfortran). To specify another alternative compiler, use the option ``-DCMAKE\_Fortran\_COMPILER'': cmake -DCMAKE\_Fortran\_COMPILER=ifort ..
        *WARNING:* We recommend to use _gfortran_ as default compiler according to our tests. Intel Fortran compiler (_ifort_) does not represent any advantage over the former.
   - And finally compile the code: make
     TIP: To compile faster use several processor threads (increase the number of threads, \#threads) through the option ``-j\#threads''. For example, ``make -j3'' will set three (3) threads.
   - You will find the executable in the current compilation directory: ndeloc.x

## Execution

You can run _NDELOC_: ./ndeloc.x

By default, when the program is executed without arguments, it prints a short help.

## Tests set

To be finished ...
