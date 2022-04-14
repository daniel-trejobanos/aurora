c*** CAMX_INCLUDES
c
      Module camx_includes
      
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c        Put all includes in a module so this can be "used" in F90 code.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2021
c     Ramboll
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c  --- core model include files --
c
      include 'camx.prm'
      include 'chmdat.inc'
      include 'flags.inc'
      include 'soap.inc'
      include 'deposit.inc'
c
c======================== DDM Begin =======================
c
      include 'ddmchm.inc'
c
c========================= DDM End ========================
c
c   --- namelist must be last, it depends on probing tools parameters ---
c
      include 'namelist.inc'
c
c
c-----------------------------------------------------------------------
c    End point:
c-----------------------------------------------------------------------
c
      end Module
