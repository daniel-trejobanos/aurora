c**** CVTICDDM.F
c
      subroutine cvticddm(igrd,nox,noy,noz,nspsa,lsagas,
     &                                             saconc,tpgrd,prgrd)
      use bndary
      use camxcom
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine converts the intial conditions in the gridded 
c   array from PPM to ug/m.
c
c     Copyright 1996 - 2021
c     Ramboll
c
c      Argument description:
c       Outputs:
c           saconc   R  tracer concentrations
c       Inputs:
c           igrd     I  grid number
c           nox      I  number of X cells in the grid
c           noy      I  number of Y cells in the grid
c           noz      I  number of layers in the grid
c           nspsa    I  number of tracer species
c           lsagas   L  true if species is gas
c           tpgrd    I  3-D temperature field
c           prgrd    I  3-D pressure field
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     03/31/99   --gwilson--    Original development
c     08/23/13   --bkoo--       Set convfac to 1 for PM species
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer*4 igrd
      integer   nox
      integer   noy
      integer   noz
      integer   nspsa
      logical   lsagas(nspsa)
      real      saconc(nox,noy,noz,nspsa)
      real      tpgrd(nox,noy,noz)
      real      prgrd(nox,noy,noz)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer      ispc, i, j, ilay
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- loop over the species ---
c
      do ispc=1,nspsa
c
c   --- loop over layers ---
c
         do ilay=1,noz
c
c  --- loop over columns, skip the boundary cells ---
c
            do 10 j=2,noy-1
c
c  --- loop over rows in this column ----
c
               do i=2,nox-1
c
c  --- get the conversion factor to umol/m3 ---
c
                    convfac = 1.0
                    if ( lsagas(ispc) ) convfac = densfac*
     &                                       273./tpgrd(i,j,ilay)*
     &                                       prgrd(i,j,ilay)/1013.
                    saconc(i,j,ilay,ispc) = 
     &                              saconc(i,j,ilay,ispc) * convfac
c
c  --- next cell ---
c
              enddo
 10         continue
c
c  --- if this is a nest, zero out the concentrations on the
c      boundary ----
c
            if( igrd .GT. 1 ) then
                do j=1,noy
                   saconc(1,j,ilay,ispc) =  0.
                   saconc(nox,j,ilay,ispc) =  0.
                enddo
                do i=1,nox
                   saconc(i,1,ilay,ispc) =  0.
                   saconc(i,noy,ilay,ispc) =  0.
                enddo
            endif
c
c  --- next layer ---
c
         enddo
c
c  --- next species ---
c
      enddo
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
