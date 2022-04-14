      subroutine intrpcnc(nspec,ncol,nrow,nlay,io,jo,nmesh,
     &                    ncolf,nrowf,nlayf,conc,concf)
c
c----CAMx v7.10 210105
c
c     INTRPCNC interpolates coarse grid concentration fields to a fine grid
c
c     Copyright 1996 - 2021
c     Ramboll
c          
c     Modifications:
c       11/05/12   Removed vertical nesting
c
c     Input arguments:
c        nspec             number of species
c        ncol              number of columns
c        nrow              number of rows
c        nlay              number of layers
c        io                starting i index for the fine grid
c        jo                starting j index for the fine grid
c        nmesh             mesh number
c        ncolf             number of columns in fine grid
c        nrowf             number of rows in fine grid
c        nlayf             number of layers in fine grid
c        conc              species concentration on coarse grid
c
c     Output arguments:
c        concf             species concentration on fine grid
c
c     Subroutine called:
c        INTERP2D
c
c     Called by:
c        READCNC
c
      real    conc(ncol*nrow*nlay,nspec)
      real    concf(ncolf*nrowf*nlayf,nspec)
c
c-----Entry point
c
      do ispc = 1,nspec
        call interp2d(ncol,nrow,nlay,io,jo,nmesh,ncolf,nrowf,
     &                conc(1,ispc),concf(1,ispc))
      enddo
c
      return
      end
