c*** LOADDM
c
      subroutine loaddm(filflg,numcol,numrow,numlay,nddm,grsens,
     &                  icl,jcl,kcl,nsen,nspc,ngas,nrad,nhet,sens,
     &                  convfac)
      use tracer
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c   Description:
c     This routine loads the sensitivies which are stored in a
c     4-D array from/into a 2-D array in conclusion/preperation 
c     of the DDM chemistry routine.  The 2-D array contains the 
c     family of sensitivities for one cell and all modeled species.
c     The flag "filflg" determines wether the values in the 4-D
c     array are loaded into the 2-D array or vice-versa.  Sensitivities
c     are converted from umol/m3 to ppm for chemistry and back.
c
c     Copyright 1996 - 2021
c     Ramboll
c
c   Argument descriptions:
c       filflg  L  flag for determining which direction to fill
c                  .TRUE.  = put 2-D values into 4-D gridded array
c                  .FALSE. = put 4-D values into 2-D array
c       numcol  I  number of cells in X direction
c       numrow  I  number of cells in Y direction
c       numlay  I  number of layers 
c       nddm    I  number of total DDM species
c       grsens  R  4-D array of DDM sensitivities
c       icl     I  the X grid location of current cell
c       jcl     I  the Y grid location of current cell
c       kcl     I  the vertical grid location of current layer
c       nsen    I  number of DDM families
c       nspc    I  number of modeled species
c       ngas    I  number of gaseous species
c       nrad    I  number of radical species
c       nhet    I  number of heterogeneous rxns
c       sens    R  2-D array for this cell and species
c       convfac R  conversion from ppm to umol/m3
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     07/16/07   --bkoo--      Added radical array mapping
c     09/21/13   --bkoo--      Added PM array mapping
c     01/03/14   --bkoo--      Added organic nitrate hydrolysis rate sensitivities
c     02/29/16   --bkoo--      Revised SENS array for hetero N2O5 + HCL rxn
c     07/20/16   --bkoo--      Revised SENS array for hetero INTR hydrolysis
c     09/02/16   --bkoo--      Set SENS array size with # of het rxns
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      logical filflg
      integer numcol
      integer numrow
      integer numlay
      integer nddm
      real    grsens(numcol,numrow,numlay,nddm)
      integer icl
      integer jcl
      integer kcl
      integer nsen
      integer nspc
      integer nrad
      real    sens(nsen,nspc+nhet)
      real    convfac
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   isen, ispc, iddm
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- task 1: load 4-D array
c
      if( filflg ) then
c
c  --- loop over the modeled species ---
c
        do ispc=nrad+1,ngas
c
c  --- loop over the number of DDM families ---
c
          do isen=1,nsen
c
c  --- calculate the index into the DDM species list ---
c
            iddm = iptddm(ispc)+isen-1
            grsens(icl,jcl,kcl,iddm) = sens(isen,ispc)*convfac
c
c  --- next family ---
c
          enddo
c
c  --- next species ---
c
        enddo
c
c  --- radical species ---
c
        do ispc=1,nrad
          do isen=1,nsen
            iddm = iptddm(ispc)+isen-1
            grsens(icl,jcl,kcl,iddm) = sens(isen,ispc)
          enddo
        enddo
c
c  --- particulate species & k_hetero -> no need to load these sensitivities back
c
c
c  --- task 2: load 2-D array
c
      else
c
c  --- loop over the modeled species ---
c
        do ispc=nrad+1,ngas
c
c  --- loop over the number of DDM families ---
c
          do isen=1,nsen
c
c  --- calculate the index into the DDM species list ---
c
            iddm = iptddm(ispc)+isen-1
            sens(isen,ispc) = grsens(icl,jcl,kcl,iddm)/convfac
c
c  --- next family ---
c
          enddo
c
c  --- next species ---
c
        enddo
c
c  --- radical species ---
c
        do ispc=1,nrad
          do isen=1,nsen
            iddm = iptddm(ispc)+isen-1
            sens(isen,ispc) = grsens(icl,jcl,kcl,iddm)
          enddo
        enddo
c
c  --- particulate species ---
c
        do ispc=ngas+1,nspc
          do isen=1,nsen
            iddm = iptddm(ispc)+isen-1
            sens(isen,ispc) = grsens(icl,jcl,kcl,iddm)
          enddo
        enddo
c
c  --- k_hetero ---
c
        do ispc=nspc+1,nspc+nhet
          do isen=1,nsen
            sens(isen,ispc) = 0.0 ! reset to zero
          enddo
        enddo
c
c  --- done ---
c
      endif
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end
