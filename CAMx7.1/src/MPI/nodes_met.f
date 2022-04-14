      subroutine nodes_met(numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use chmstry
      use filunit
      use o3colmap
      use bndary
      use camxfld
      use camxcom
      use pigsty
      use ptemiss
      use procan
      use rtracchm
      use tracer
c
      implicit none
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c        This routine passes all of the data that is time-step
c        dependent to the compute nodes when in MPI mode.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numprocs            the number of processes
c        iproc_id            processor ID for this process
c     Output:  
c
c    Called by:
c       EMISTRNS
c       TSTEP_INIT
c    Subroutines called:
c       NODES_PASS
c       MASTER_SEND_GRIDDED_DATA
c       NODE_RECV_GRIDDED_DATA
c       MASTER_SEND_1SPECIES_DATA
c       NODE_RECV_1SPECIES_DATA
c
c     Copyright 1996 - 2021
c     Ramboll
c
c-----------------------------------------------------------------------
c    LOG:
c     11/4/09  -cemery- Removed input top concentrations
c     08/08/14 -cemery- Added new snow cover input fields
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'deposit.inc'
      include 'flags.inc'
      include 'camx_aero.inc'
      include 'soap.inc'
      include 'lsbox.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: numprocs
      integer :: iproc_id
c     
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: nlaya
      integer :: mvec2d
      integer :: mvec3d
      integer :: ispc
      integer :: i
      integer :: mvec4d
      integer :: mvecem
      integer :: mveclu
      integer :: mvecdp
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.NOT. lmpi) return
c
      nlaya = maxval( nlay(1:ngrid) )
c
c  --- send the variables in the camx include file ---
c
      call nodes_pass(date,          1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(time,          1,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(datec,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(timec,     ngrid,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(deltat,    ngrid,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(ntim,      ngrid,   MPI_REAL,itag,numprocs,iproc_id) 
      call nodes_pass(ntimcrs,   ngrid,   MPI_REAL,itag,numprocs,iproc_id) 
      call nodes_pass(nadv,nlaya*ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
c
c  --- calculate the number of bytes to send ---
c
      mvec2d = 0
      mvec3d = 0
      do i=1,ngrid
         mvec2d = mvec2d + nrow(i) * ncol(i)
         mvec3d = mvec3d + nrow(i) * ncol(i) * nlay(i)
      enddo
      mvecem = mvec2d * nspec
      mvec4d = mvec3d * nspec
      mveclu = mvec2d * nlu
      mvecdp = mvec2d * (nspec*3 + 2)
c
c  --- pass the fields that are dimensioned by mvec2d ---
c
      do i=1,ngrid
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(tsurf(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(tsurf(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(topo(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(topo(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pspt(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(pspt(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(snow(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(snow(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(snowage(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(snowage(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(snowrat(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(snowrat(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(snowfrc(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(snowfrc(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(snowalb(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(snowalb(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(albedo(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(albedo(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
c
c  --- pass the fields that are dimensioned by mvec3d ---
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(depth(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (depth(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(height(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (height(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(phpt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (phpt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1

         if (iproc_id .eq. 0) then
            call master_send_gridded_data(windu(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (windu(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(windv(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (windv(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pupt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pupt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pvpt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pvpt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(tempk(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (tempk(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(ptpt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (ptpt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(press(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (press(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pppt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pppt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(rkv(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (rkv(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pkpt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pkpt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(water(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (water(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pwpt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pwpt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
cae         if (iproc_id .eq. 0) then
cae            call master_send_gridded_data(fcloud(iptr3d(i)),i,nlay(i),1,itag)
cae         else
cae            call node_recv_gridded_data  (fcloud(iptr3d(i)),i,nlay(i),1,itag)
cae         endif
cae         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cwc(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (cwc(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pwr(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pwr(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pws(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pws(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pwg(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pwg(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cod(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (cod(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cph(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (cph(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cigfrc(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data  (cigfrc(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cigtim(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data  (cigtim(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cigwtr(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (cigwtr(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cigph(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (cigph(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cigpcp(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (cigpcp(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cigent(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (cigent(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(cigdet(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (cigdet(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(hnxt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (hnxt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(pnxt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (pnxt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(unxt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (unxt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(vnxt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (vnxt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(tnxt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (tnxt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(tsnxt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (tsnxt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(knxt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (knxt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(wnxt(iptr3d(i)),i,nlay(i),1,itag)
         else
            call node_recv_gridded_data  (wnxt(iptr3d(i)),i,nlay(i),1,itag)
         endif
         itag = itag+1
c
c  --- pass the deposition arrays ---
c
         do ispc=1,ndepspc*3+2
            if (iproc_id .eq. 0) then
               call master_send_1species_data(depfld(iptrdp(i)),i,1,1,
     &                                            ndepspc*3+2,ispc,itag)
            else
               call node_recv_1species_data(depfld(iptrdp(i)),i,1,1,
     &                                            ndepspc*3+2,ispc,itag)
            endif
            itag = itag+1
         enddo
      enddo
c
c  --- pass the point source emissions arrays ---
c
      call nodes_pass(ptemis,nptsrc*nspec,MPI_REAL,itag,numprocs,iproc_id)
c
c  --- pass the variables in the pigsty include file ---
c
      if (ipigflg .EQ. IRONPIG .OR. ipigflg .EQ. GRESPIG) then
         call nodes_pass(ipigint,1,MPI_INTEGER,itag,numprocs,iproc_id)
      endif
c
c  --- pass the variables in the lsbox include file ---
c
      call nodes_pass(idsolv,1,         MPI_INTEGER,itag,numprocs,iproc_id)   !add by cbwmpi
      call nodes_pass(dH2O,  1,MPI_DOUBLE_PRECISION,itag,numprocs,iproc_id)
      call nodes_pass(dM,    1,MPI_DOUBLE_PRECISION,itag,numprocs,iproc_id)
      call nodes_pass(dO2,   1,MPI_DOUBLE_PRECISION,itag,numprocs,iproc_id)
      call nodes_pass(dCH4,  1,MPI_DOUBLE_PRECISION,itag,numprocs,iproc_id)
      call nodes_pass(dH2,   1,MPI_DOUBLE_PRECISION,itag,numprocs,iproc_id)
      if (idsolv .EQ. IDLSOD) then
         call nodes_pass(dbrk,MXREACT,
     &                   MPI_DOUBLE_PRECISION,itag,numprocs,iproc_id)
         call nodes_pass(rrk,MXREACT, MPI_REAL,itag,numprocs,iproc_id)
      endif
c
      end
