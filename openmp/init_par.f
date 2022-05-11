! ** openmp/init_par.f >>
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


      subroutine init_par(runparal,nproc)
      implicit none
      integer  nproc,nprocmax,wtime,n_threads,id
      logical  runparal
      include 'omp_lib.h'        !! Enabling OpenMP functions !!
C
      wtime = omp_get_wtime()
      write(*,'(/,X,
     .    "**************** THREAD INFORMATION ******************"
     .                                                               )')
      write(*,'(5X,">> Job running using OpenMP.")')
      nprocmax = omp_get_num_procs()
      write(*,'(5X,">> The number of processors is",X,5("."),I4)')
     .                                                          nprocmax
      if (nprocmax.LT.nproc) then
         write(*,'(/,15X,
     .        "** WARNING **",I4,"=spec.proc >",I4,"=av.proc.")')
     .                                                    nproc,nprocmax
         write(*,'(25X,"av.proc. will be set",/)')
         nproc = nprocmax
      else if (nproc.EQ.1) then
         nproc = nprocmax
      endif !! (nprocmax.LT.nproc) then
      call    OMP_SET_NUM_THREADS(nproc)
      n_threads = omp_get_max_threads()
      write(*,'(5X,">> The number of threads is"   ,X,8("."),I4)')
     .                                                         n_threads
C
C  INSIDE THE PARALLEL REGION, have each thread say hello.
C
C$omp parallel
C$omp& private ( id )
C
C  Have each thread say hello.
C
      id = omp_get_thread_num()
      write(*,'(25X,"Hello from process",I8)') id
C$omp end parallel
C
C  Finish up by measuring the elapsed time.
C
      wtime = omp_get_wtime()-wtime
      write( *,'(5X,">> Elapsed wall clock time"   ,X,8("."),G11.4)' )
     .                                                             wtime
      write(*,'(/,X,
     .    "******************************************************",
     .                                                              /)')
C
      return
      end
