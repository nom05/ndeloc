# CMake project file for NDELOC

# States that CMake required version must be greater than 2.6
cmake_minimum_required(VERSION 2.6)

## >> DEFAULT COMPILER << ## To force compiler, use:
# cmake -DCMAKE_Fortran_COMPILER=gfortran src_directory

message("CMAKE_Fortran_COMPILER is ${CMAKE_Fortran_COMPILER}")
message("CMAKE_GENERATOR_FC is ${CMAKE_GENERATOR_FC}")

## >> SETTINGS         << ##
project(ndeloc)
enable_language (Fortran)

set(CMAKE_BUILD_TYPE Release)
message("CMAKE_BUILD_TYPE is ${CMAKE_BUILD_TYPE}")
# Of course, change “Release” (O3) with “Debug” (no optimizations) for debug builds.

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
# set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -static") # This is for code based on Multiwfn
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3") # This is for code based on Multiwfn
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g") # This is for code based on Multiwfn
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort
# set (CMAKE_Fortran_FLAGS_RELEASE "-fast -static")
  set (CMAKE_Fortran_FLAGS_RELEASE "-fast")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -static")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

message("CMAKE_Fortran_COMPILER_VERSION: " "${CMAKE_Fortran_COMPILER_VERSION}")
find_package(OpenMP)
if (OPENMP_FOUND)
   message("OPENMP FOUND")
#  if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
#   # gfortran
#    set (OpenMP_C_FLAGS              "-fopenmp")
#  elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
#    # ifort
#    if(CMAKE_Fortran_COMPILER_VERSION LESS "14.*")
#       set (OpenMP_C_FLAGS            "-openmp")
#    elseif(CMAKE_Fortran_COMPILER_VERSION GREATER_EQUAL "14.*")
#       set (OpenMP_C_FLAGS            "-qopenmp")
#    endif(CMAKE_Fortran_COMPILER_VERSION LESS "14.*")
#  endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")
   message("OPENMP FLAGS: " ${OpenMP_C_FLAGS})
   set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${OpenMP_C_FLAGS}")
   set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_RELEASE} ${OpenMP_C_FLAGS}")
   if (CMAKE_BUILD_TYPE MATCHES "RELEASE")
       message("${CMAKE_Fortran_FLAGS_RELEASE}")
   elseif (CMAKE_BUILD_TYPE MATCHES "DEBUG")
       message("${CMAKE_Fortran_FLAGS_DEBUG}")
   endif (CMAKE_BUILD_TYPE MATCHES "RELEASE")
   set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
endif ()
###
##################################################################################
### >> CALC              subrout. << ##
add_subdirectory(calc)
set(clc calc)
set (CALC
    ${clc}/ndelocma.f
    ${clc}/scaleovermat.f90
)
### >> COMBINATORICS     subrout. << ##
add_subdirectory(combinatorics)
set(cmb combinatorics)
set (COMBINATORICS
    ${cmb}/allnr.f
    ${cmb}/ft.f
    ${cmb}/nbinom.f
    ${cmb}/nextp.f
    ${cmb}/permut.f
)
### >> COMMON            subrout. << ##
add_subdirectory(common)
set(cmm common)
set (COMMON
    ${cmm}/atomlabel.f
    ${cmm}/calcdist.f
    ${cmm}/hmnumb.f
    ${cmm}/ncolumn.f
    ${cmm}/normaliz.f90
    ${cmm}/openfile.f
    ${cmm}/prepnumb.f
    ${cmm}/procnumb.f
    ${cmm}/showprog.f90
)
### >> LOOPDIR           subrout. << ##
add_subdirectory(loopdir)
set(lpd loopdir)
set (LOOPDIR
    ${lpd}/choose.f
    ${lpd}/looporb3.f
    ${lpd}/looporb4.f
    ${lpd}/looporb5.f
    ${lpd}/looporb6.f
    ${lpd}/looporb7.f
    ${lpd}/looporb8.f
    ${lpd}/looporb9.f
    ${lpd}/looporb10.f
    ${lpd}/looporb11.f
    ${lpd}/looporb12.f
    ${lpd}/looporb13.f
    ${lpd}/looporb14.f
    ${lpd}/looptree.f
    ${lpd}/ndeloc1.f
    ${lpd}/ndeloc2.f
)
### >> MODULES           subrout. << ##
add_subdirectory(modules)
set(mod modules)
set (MODULES
    ${mod}/mo_kind.f90
    ${mod}/mo_quicksort.f90
    ${mod}/mo_utils.f90
)
### >> OPENMP            subrout. << ##
add_subdirectory(openmp)
set(omp openmp)
set (OPENMP
    ${omp}/init_par.f
    ${omp}/strategy.f
)
### >> OUTPUT            subrout. << ##
add_subdirectory(output)
set(out output)
set (OUTPUT
    ${out}/results.f
    ${out}/ringhost.f
)
### >> READ              subrout. << ##
add_subdirectory(read)
set(rd read)
set (READ
    ${rd}/atomovma.f
    ${rd}/atovmall.f
    ${rd}/eloc.f
    ${rd}/fchkinfo.f
    ${rd}/moldeninfo.f90
    ${rd}/mwfn.f
    ${rd}/p1fromgauss.f90
    ${rd}/readfch.f
    ${rd}/readmold.f90
    ${rd}/readsom.f
    ${rd}/readwfn.f
    ${rd}/sudgfchk.f
    ${rd}/sudgfdim.f
    ${rd}/wfninfo.f
)
### >> RING              subrout. << ##
add_subdirectory(ring)
set(rg ring)
set (RING
    ${rg}/connect.f
    ${rg}/ringdtct.f
)
### >> SORT              subrout. << ##
add_subdirectory(sort)
set(srt sort)
set (SORT
    ${srt}/indexxabs.f90
)
### >> MAIN COMPILATION << ##
   add_executable(ndeloc.x 
	${CALC}
	${COMBINATORICS}
	${COMMON}
	${LOOPDIR}
	${OPENMP}
	${MODULES}
	${OUTPUT}
	${READ}
	${RING}
	${SORT}
	goon.f
	ndeloc.f
                 )
