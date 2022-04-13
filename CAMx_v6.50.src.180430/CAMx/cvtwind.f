      subroutine cvtwind(ncol,nrow,nlay,windu,windv)
c
c----CAMx v6.50 180430
c
c     CVTWIND converts the wind field from cell-centered to staggered on cell
c     interfaces (Arakawa C grid):
c
c     Copyright 1996 - 2018
c     Ramboll
c
c     Modifications:
c        none
c
c     Input arguments:
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        windu               cell centered windspeed in x-direction (m/s)
c        windv               cell centered windspeed in y-direction (m/s)
c
c     Output arguments:
c        windu               staggered windspeed in x-direction (m/s)
c        windv               staggered windspeed in y-direction (m/s)
c
c     Routines Called:
c        none
c
c     Called by:
c        METINIT
c        READINP
c
      real windu(ncol,nrow,nlay)
      real windv(ncol,nrow,nlay)
c
      do 10 k=1,nlay
c
c-----x-direction
c
        do j=1,nrow
          do i=1,ncol-1
            windu(i,j,k) = (windu(i,j,k) + windu(i+1,j,k))/2.
          enddo
        enddo
c
c-----y-direction
c
        do j=1,nrow-1
          do i=1,ncol
            windv(i,j,k) = (windv(i,j,k) + windv(i,j+1,k))/2.
          enddo
        enddo
c
  10  continue
c
      return
      end
