          write(*,'(
     ."  INPUT file HELP/TIPS:",/
     ."      ( * = possible options in the corresponding line )"      ,/
     ."     o Line 1 :  WFN/FCHK file with extension + # threads"     ,/
     ."              *  file   -> File with extension:"               ,/
     ."                         > wfn  reading algorithm to be used." ,/
     ."                         > fchk reading algorithm to be used." ,/
     ."              *  # threads to be used through OpenMP."         ,/
     ."     o Line 2 :  OUTPUT          specification:"               ,/
     ."              *  text   -> file without extension."            ,/
     ."              *  xyz    -> print xyz with the N-DELOC indices" ,/
     ."                           (if ring is specified)"             ,/
     ."                           (only for Chemcraft)"               ,/
     ."     o Line 3 :  MO              specification:"               ,/
     ."              *  all    -> All mol orbitals included."         ,/
     ."              *  pi     -> All PI mol orbitals included."      ,/
     ."              *  outersh-> Inner s shell not included."        ,/
     ."              *  1,3-5  -> Numbering specif. will be"          ,/
     ."                           considered separated by comma or"   ,/
     ."                           dash (this example: 1,3,4,5)."      ,/
     ."     o Line 4 :  N-(DE)LOC index specification:"               ,/
     ."              *  integer-> N-(DE)LOC index order."             ,/
     ."              *  deloc  -> Deloc index only (for n>2)."        ,/
     ."              *  giamb  -> Only Giambiagi index will be"       ,/
     ."                           considered."                        ,/
     ."                           Ring option in line is recommended.",/
     ."     o Line 5 :  Atom specification:"                          ,/
     ."              *  all    -> All atoms will be included."        ,/
     ."              *  heavy  -> All heavy atoms will be included."  ,/
     ."              *  1,3-5  -> Numbering specif. will be"          ,/
     ."                           considered separated by comma or"   ,/
     ."                           dash (this example: 1,3,4,5)."      ,/
     ."              *  ring   -> Ring detection."                    ,/
     ."                           It is used together with all prev." ,/
     ."                           options."                           ,/
     ."     o Line 6 :  Atomic Overlap Matrices specification:"       ,/
     ."              *  read   -> Files will be read below."          ,/
     ."              *  fuk    -> fuk format to be used: mol_mol_X01.",/
     ."              *  int    -> int format to be used: mol_X01."    ,/
     ."              *  aimall -> int format to be used: mol_X01."    ,/
     ."                           (same file name fmt,diff. data str.",/
     ."              *  eloc   -> eloc format to be used: mol."       ,/
     ."              *  mwfn   -> Multiwfn format to be used."        ,/
     ."                           =read is optional and it will read ",/
     ."                           the next line to obtain the file   ",/
     ."                           name.                              ",/
     ."              *  Integer-> length for atom label"              ,/
     ."                           (1=X1,2=X01,3=X001, ...)"           ,/
     ."     o Lines 7-... :  Files (if applicable)."                  ,/
     .)')   !!   HELP END
