      Module ptemiss
      include 'ptemiss.inc'
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the PIGSTY.COM
c        include file.
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
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PTEMISS
c-----------------------------------------------------------------------
c
         subroutine alloc_ptemiss(numspcs,numgrids,numpoints)
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
c        numspcs    I  number of species
c        numgrids   I  number of grids
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numspcs
         integer :: numgrids
         integer :: numpoints
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( xloc   (numpoints) )
         allocate( yloc   (numpoints) )
         allocate( hstk   (numpoints) )
         allocate( dstk   (numpoints) )
         allocate( tstk   (numpoints) )
         allocate( vstk   (numpoints) )
         allocate( flowrat(numpoints) )
         allocate( effph  (numpoints) )
         allocate( lpiglet(numpoints) )
c
         allocate( xstk   (numpoints,numgrids) )
         allocate( ystk   (numpoints,numgrids) )
         allocate( ptemis (numpoints,numspcs)  )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PTEMISS
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PTEMISS_NULL
c-----------------------------------------------------------------------
c
         subroutine alloc_ptemiss_null(numspcs)
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
c        numspcs    I  number of species
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numspcs
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( ptemis (1,numspcs)  )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PTEMISS_NULL
c-----------------------------------------------------------------------
c
      end Module


