      subroutine nodemass(igrd)
      use filunit
      use chmstry
      use camxfld
c
c----CAMx v7.10 210105
c
c     NODEMASS re-initializes the mass summary arrays for the slices
c
c     Copyright 1996 - 2021
c     Ramboll
c          
c     Modifications:
c
c     Input arguments:
c        igrd                grid index
c
c     Output arguments:
c        none
c
c     Subroutines called:
c        none
c
c     Called by:
c        CAMx
c
      include "camx.prm"
c
c-----Entry point
c
c-----Compute residual
c
c-----Move current mass array to old mass array
c
      do l = 1,nspec
        xmsold(l,igrd) = xmass(l,igrd)
      enddo
c
c-----Zeros the mass and fluxes
c
      do l = 1,nspec
        armass(l,igrd) = 0.
        ptmass(l,igrd) = 0.
        do i=1,11
          fluxes(l+(i-1)*nspec,igrd) = 0.
        enddo
        xmschem(l,igrd) = 0.
        xmsfin(l,igrd) = 0.
        pigdump(l,igrd) = 0.
        pgmserr(l,igrd) = 0.
      enddo
c
      return
      end
