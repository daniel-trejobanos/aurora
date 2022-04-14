      subroutine finwind(ncol,nrow,nlay,io,jo,nmesh,ncolf,
     &                   nrowf,nlayf,windu,windv,winduf,windvf)
c
c----CAMx v7.10 210105
c
c     FINWIND estimates wind speed for a fine grid from its parent
c
c     Copyright 1996 - 2021
c     Ramboll
c
c     Modifications:
c        11/06/01  Input dates are now Julian
c        11/04/09  Removed setting nest grid boundary winds from parent
c        11/05/12  Removed vertical nesting
c
c     Input arguments:
c        ncol              number of columns on parent grid
c        nrow              number of rows on parent grid
c        nlay              number of layers on parent grid
c        io,jo             parent starting grid cell indices
c        nmesh             fine grid meshing factor
c        ncolf             number of columns on fine grid
c        nrowf             number of rows on fine grid
c        nlayf             number of layers on fine grid
c        windu             u-component wind speed on parent grid (m/s)
c        windv             v-component wind speed on parent grid (m/s)
c
c     Output arguments:
c        winduf            u-component wind speed on fine grid (m/s)
c        windvf            v-component wind speed on fine grid (m/s)
c
c     Subroutine called:
C        none
c
c     Called by:
c        STARTUP
c        INTRPDAT
c
      implicit none
      integer ncol,nrow,nlay,io,jo,nmesh,ncolf,nrowf,nlayf
      real    windu(ncol,nrow,nlay)
      real    windv(ncol,nrow,nlay)
      real    winduf(ncolf,nrowf,nlayf)
      real    windvf(ncolf,nrowf,nlayf)
c
      integer kp,kg,i,ii,j,jj,i1,j1
      real du,dv
c
c-----Entry point
c
c-----Assign wind speed to the fine grid: mass is conserved
c
      do 50 kp = 1,nlay
          kg = kp
c
c-----Use the wind where it available from the parent grid
c
          do j = 2,nrowf-1
            do i = 1,ncolf-1,nmesh
              ii = io - 1 + (i-1)/nmesh
              jj = jo + (j-2)/nmesh
              winduf(i,j,kg) = windu(ii,jj,kp)
            enddo
          enddo
c
          do j = 1,nrowf-1,nmesh
            do i = 2,ncolf-1
              ii = io + (i-2)/nmesh
              jj = jo - 1 + (j-1)/nmesh
              windvf(i,j,kg) = windv(ii,jj,kp)
            enddo
          enddo
c
c-----Interpolate in between
c
          do j = 2,nrowf-1
            do i = 1,ncolf-nmesh,nmesh
              du = (winduf(i+nmesh,j,kg)-winduf(i,j,kg))/nmesh
              do i1 = i+1,i+nmesh-1
                winduf(i1,j,kg) = winduf(i1-1,j,kg) + du
              enddo
            enddo
          enddo
c
          do j = 1,nrowf-nmesh,nmesh
            do i = 2,ncolf-1
              dv = (windvf(i,j+nmesh,kg)-windvf(i,j,kg))/nmesh
              do j1 = j+1,j+nmesh-1
                windvf(i,j1,kg) = windvf(i,j1-1,kg) + dv
              enddo
            enddo
          enddo
c
  50  continue
c
      return
      end
