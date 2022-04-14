      subroutine timestep()
      use grid
      use bndary
      use camxfld
      use camxcom
      use pigsty
c
c----CAMx v7.10 210105
c
c     TIMESTEP computes the time step size for each grid 
c
c     Copyright 1996 - 2021
c     Ramboll
c
c     Modifications:
c        11/30/99  Removed timestep calculation for horizontal diffusion
c        1/25/02   Revised I/O frequencies and max time step to minutes,
c                  added code to check winds at next update time to ensure
c                  CFL stability throughout period
c        4/10/03   Now calculating max timestep by layer, AND
c                  DTMAX moved from control file to parameter defined here
c        10/12/04  Added determination of finest grid timestep for PiG
c                  initialization and transport
c
c     Input arguments:
c        none
c
c     Output Arguments:
c        none
c
c     Routines called:
c        GETDELT
c
c     Called by:
c        TSTEP_INIT
c
      implicit none
      include "camx.prm"
c
      integer igrd,kmax,k,nsteps,ip,ic,n
      real dtmx,dtio,dtmin
      real dtmx1(MXLAYER)
      real dtmx2(MXLAYER)
      real dtlay(MXLAYER)
c
c-----Entry point
c
c-----Coarse grid: maximum time step size according to stability criterion
c
      igrd = 1
      call getdelt(ncol(1),nrow(1),nlay(1),deltax(1,1),deltay(1),
     &             windu(1),windv(1),height(1),mapscl(1),dtmx1,kmax)
      call getdelt(ncol(1),nrow(1),nlay(1),deltax(1,1),deltay(1),
     &             unxt(1),vnxt(1),hnxt(1),mapscl(1),dtmx2,kmax)
c
c-----Make sure an integer number of coarse grid time steps are to be
c     completed each hour and between I/O times
c
      dtio = 60.*amin1(60.,dtinp,dtems,dtout)
      dtmx = dtio
     
      do k = 1,nlay(1)
        dtlay(k) = amin1(dtmx1(k),dtmx2(k),dtio,60.*dtmax)
        if (k.le.kmax) dtmx = amin1(dtmx,dtlay(k))
      enddo
      nsteps = INT( 0.999*dtio/dtmx ) + 1
      deltat(1) = dtio/FLOAT( nsteps )
c
c-----Number of advection steps per layer in coarse grid
c
      do k = 1,nlay(1)
        nadv(k,1) = INT( 0.999*deltat(1)/dtlay(k) ) + 1
      enddo
c
c-----Fine grids: make sure an integer number of fine grid time steps 
c     are to be completed between coarser grid steps
c
      ntimcrs(1) = 1
      do ip = 1,ngrid
        do ic = 1,nchdrn(ip)
          igrd = idchdrn(ic,ip)
          call getdelt(ncol(igrd),nrow(igrd),nlay(igrd),
     &                 deltax(1,igrd),deltay(igrd),windu(iptr3d(igrd)),
     &                 windv(iptr3d(igrd)),height(iptr3d(igrd)),
     &                 mapscl(iptr2d(igrd)),dtmx1,kmax)
          call getdelt(ncol(igrd),nrow(igrd),nlay(igrd),
     &                 deltax(1,igrd),deltay(igrd),unxt(iptr3d(igrd)),
     &                 vnxt(iptr3d(igrd)),hnxt(iptr3d(igrd)),
     &                 mapscl(iptr2d(igrd)),dtmx2,kmax)
          dtmx = deltat(ip)
          do k = 1,nlay(igrd)
            dtlay(k) = amin1(dtmx1(k),dtmx2(k),deltat(ip))
            if (k.le.kmax) dtmx = amin1(dtmx,dtlay(k))
          enddo
          ntim(igrd) = INT( 0.999*deltat(ip)/dtmx ) + 1
          ntimcrs(igrd) = ntim(igrd)*ntimcrs(ip)
          deltat(igrd) = deltat(ip)/FLOAT( ntim(igrd) )
c
c-----Number of advection steps per layer in this fine grid
c
          do k = 1,nlay(igrd)
            nadv(k,igrd) = INT( 0.999*deltat(igrd)/dtlay(k) ) + 1
          enddo
c
        enddo
      enddo
c
c-----Determine which grid has the smallest timestep for PiG
 
c
      dtmin = 3600.
      do n = 1,ngrid
        if (deltat(n).lt.dtmin) then
          dtmin = deltat(n)
          ipigint = n
        endif
      enddo
c
      return
      end
