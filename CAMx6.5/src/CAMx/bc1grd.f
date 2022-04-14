      subroutine bc1grd(nspec,ncol,nrow,nlay,ncolf,nrowf,nlayf,i1,j1,
     &                  nmesh,conc,concf)
c
c----CAMx v6.50 180430
c
c     BC1GRD sets up boundary conditions for one fine grid using 
c     concentrations of its parent grid
c
c     Copyright 1996 - 2018
c     Ramboll
c          
c     Modifications:
c        8/23/06           Fixed indexing bug for CG cells
c       11/05/12           Removed vertical nesting
c
c     Input arguments:
c        nspec             Number of species
c        ncol              Number of columns on the parent grid
c        nrow              Number of rows on the parent grid
c        nlay              Number of layers on the parent grid
c        ncolf             Number of columns on the fine grid
c        nrowf             Number of rows on the fine grid
c        nlayf             Number of layers on the fine grid
c        i1                Starting i-index of fine grid
c        j1                Starting j-index of fine grid
c        nmesh             meshing factor relative to parent grid
c        conc              concentration on the coarse grid (umol/m3)
c
c     Output arguments:
c        concf             concentration on the fine grid (umol/m3)
c
c     Routines called:
c        none
c
c     Called by:
c        SETBC 
c
      dimension conc(ncol,nrow,nlay,nspec),
     &          concf(ncolf,nrowf,nlayf,nspec)
c
c-----Entry point
c
      do 30 ispc = 1,nspec
        do 20 kp=1,nlay
            kg = kp
c
c-----Southern boundary
c
            j = 1
            jj = j1 - 1
            do i = 2,ncolf-1
              ii = i1 + (i-2)/nmesh
              concf(i,j,kg,ispc) = conc(ii,jj,kp,ispc)
            enddo
c
c-----Northern boundary
c
            j = nrowf
            jj = j1 + (nrowf-2)/nmesh
            do i = 2,ncolf-1
              ii = i1 + (i-2)/nmesh
              concf(i,j,kg,ispc) = conc(ii,jj,kp,ispc)
            enddo
c
c-----Western boundary
c
            i =1
            ii = i1 - 1
            do j = 2,nrowf-1
              jj = j1 + (j-2)/nmesh
              concf(i,j,kg,ispc) = conc(ii,jj,kp,ispc)
            enddo
c
c-----Eastern boundary
c
            i = ncolf
            ii = i1 + (ncolf-2)/nmesh
            do j = 2,nrowf-1
              jj = j1 + (j-2)/nmesh
              concf(i,j,kg,ispc) = conc(ii,jj,kp,ispc)
            enddo
c
  20    continue
c
  30  continue
c
      return
      end
