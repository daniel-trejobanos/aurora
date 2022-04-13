      subroutine average(m1,m2,m3,losat,igrd,dt,ncol,nrow,nlay,
     &                   nlayav,nspav,nspc,lmap,lgassp,
     &                   tempk,press,conc,avcnc)
      use chmstry
      use bndary
      use camxcom
      use procan
      use rtracchm
      use tracer
      use node_mod
c
c----CAMx v6.50 180430
c
c     AVERAGE computes time-averaged concentrations
c
c     Copyright 1996 - 2018
c     Ramboll
c
c     Modifications: 
c        01/30/02    --gwilson--  Added code for RTRAC probing tool
c        12/15/08    --gwilson--  Added code to handle averaging of
c                                 radicals
c        09/27/10    --gwilson--  Removed the code to store process
c                                 analysis conversion factors. Now
c                                 handled by separate routine.
c
c     Input arguments:
c        losat              .TRUE. if concentrations are tracer species
c        igrd               grid index
c        dt                 time step for present grid concentration (s)
c        ncol               number of columns
c        nrow               number of rows
c        nlay               number of layers in instantaneous array
c        nlayav             number of layers in average array
c        nspav              number of average species
c        nspc               number of species in conc array
c        lmap               mapping array for average species
c        lgassp             true if species is gas
c        tempk              temperature field (K)
c        press              pressure field (mb)
c        conc               instant species concentration (umol/m3)
c        avcnc              average species concentration (gas=ppm,
c                                                          other=ug/m3)
c     Output arguments:
c        avcnc              average species concentration (gas=ppm,
c                                                          other=ug/m3)
c
c     Routines Called:
c        none
c
c     Called by:
c        CAMx
c        FGAVRG
c
      include "camx.prm"
 
      integer m1,m2,m3
c
      logical lgassp(nspc)
      logical losat
      real    tempk(m1,m2,m3),press(m1,m2,m3)
      real    avcnc(m1,m2,nlayav,nspav)
      real    conc(m1,m2,m3,nspc)
      integer lmap(*)
c
c-----Entry point
c
c-----Increment running average
c
      dtfact = dt/(dtout*60.)
      do 40 l = 1,nspav
        lsp = lmap(l) 
        convfac = 1.
        do 30 j = 2,m2-1
          do i = 2,m1-1
c
            do k=1,nlayav
                if( lgassp(lsp) ) then
                    tmp = 273./tempk(i,j,k)*press(i,j,k)/1013.
                    convfac = 1./(densfac*tmp)
                endif
                avcnc(i,j,k,l) = convfac*conc(i,j,k,lsp)*dtfact +
     &                                                  avcnc(i,j,k,l)
            enddo
          enddo
  30    continue
  40  continue
c
      dtfact = dt/(dtout*60.)
      return
      end
