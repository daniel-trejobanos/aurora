      Module filunit
      include 'filunit.inc'
c
c----CAMx v6.50 180430
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the FILUNIT.COM
c        include file.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2018
c     Ramboll
c
c-----------------------------------------------------------------------
c    LOG:
c        01/04/11  Revised for new met input format
c        04/30/13  Added surface model
c        09/02/14  Added subgrid convective model
c-----------------------------------------------------------------------
c
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_FILUNIT
c-----------------------------------------------------------------------
c
         subroutine alloc_filunit(numgrds)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numgrds    I  number of grids
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c         
         allocate( iavg  (numgrds) )
         allocate( idep  (numgrds) )
         allocate( ismin (numgrds) )
         allocate( ismout(numgrds) )
         allocate( iarem (numgrds) )
         allocate( isurf (numgrds) )
         allocate( i3dmet(numgrds) )
         allocate( i2dmet(numgrds) )
         allocate( ikv   (numgrds) )
         allocate( icld  (numgrds) )
         allocate( lcig  (numgrds) )
c
         allocate( n3dmet(numgrds) )
         allocate( n2dmet(numgrds) )
         allocate( nkvmet(numgrds) )
         allocate( ncldmet(numgrds) )
         allocate( nsrfvar(numgrds) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_FILUNIT
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_FILUNIT_SAMPLE
c-----------------------------------------------------------------------
c
         subroutine alloc_filunit_sample(numsamples)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numsamples   I  number of sampling grids
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numsamples
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c         
         allocate( isample(numsamples) )
c
         return
         end subroutine
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_FILUNIT_SAMPLE
c-----------------------------------------------------------------------
c
      end Module
