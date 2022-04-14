c
c-----------------------------------------------------------------------
c    BEGIN subroutine tstep_init:
c-----------------------------------------------------------------------
c
      subroutine tstep_init(inptim,inpdate,endtim,enddate,nsteps,
     &                      numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use filunit
      use grid
      use chmstry
      use o3colmap
      use bndary
      use camxfld
      use camxcom
      use ptemiss
      use procan
      use rtracchm
      use rtcmcchm
      use tracer
      use master_mod
      use node_mod
c
      implicit none
c  
c----CAMx v6.50 180430
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    This routine updates all of the met fields at the update time-step.
c    It is called when any of the input fields need to be updated
c    (usually every hour), and reads ICs if this is the start of the run.
c    It calls the routines responsible for reading the appropriate files
c    and then passes the information to the slices for MPI.
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       CAMX
c    Subroutines called:
c       READINP
c       INTRPDAT
c       DRVTUV
c       FLUSH
c       READCNC
c       WRTMASS
c       MASSUM
c       RDICRT
c       INTRPCNC
c       CVTICRT
c       FILAQSA
c       RDICDDM
c       CVTICDDM
c       INITIPR
c       NODE_RECV_1SPECIES_DATA
c       MASTER_SEND_1SPECIES_DATA
c       TIMESTEP
c       MPI_BARRIER
c       NODES_MET
c       NEWGRID
c       KHORZ
c
c     Copyright 1996 - 2018
c     Ramboll
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c       10/29/09 -cemery- Added RTRAC surface model
c        7/14/10       Added code for in-line TUV cloud adjustment
c       03/29/11       Support in-line TUV with aerosol optical depth
c       04/02/12       Removed RADM cloud adjustment option, cloud/aerosol
c                      adjustments now always done with in-line TUV
c       05/14/13       Added surface model
c       08/11/14       Albedo now includes effects of snow
c       09/02/14       Added subgrid convective model
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
      include 'rtracsrf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: inptim,endtim
c
      integer :: inpdate,enddate
      integer :: nsteps
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: igrd
      integer :: l
      integer :: ip
      integer :: ic
      integer :: ig
      integer :: ierr
      integer :: ilay
      integer :: ispc
c
      real    :: whr
      real    :: wmn
c
      character*8  :: chtime
      character*8  :: chdate
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (iproc_id .EQ. 0) then 
         write(*,'(a20,$)') 'readinp ......'
         do igrd = 1,ngrid
            call readinp(igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                   height(iptr3d(igrd)),phpt(iptr3d(igrd)),
     &                   hnxt(iptr3d(igrd)),
     &                   press(iptr3d(igrd)),pppt(iptr3d(igrd)),
     &                   pnxt(iptr3d(igrd)),
     &                   windu(iptr3d(igrd)),pupt(iptr3d(igrd)),
     &                   unxt(iptr3d(igrd)),
     &                   windv(iptr3d(igrd)),pvpt(iptr3d(igrd)),
     &                   vnxt(iptr3d(igrd)),
     &                   tsurf(iptr2d(igrd)),pspt(iptr2d(igrd)),
     &                   tsnxt(iptr2d(igrd)),
     &                   tempk(iptr3d(igrd)),ptpt(iptr3d(igrd)),
     &                   tnxt(iptr3d(igrd)),
     &                   water(iptr3d(igrd)),pwpt(iptr3d(igrd)),
     &                   wnxt(iptr3d(igrd)),
     &                   rkv(iptr3d(igrd)),pkpt(iptr3d(igrd)),
     &                   knxt(iptr3d(igrd)),
     &                   cwc(iptr3d(igrd)),
     &                   pwr(iptr3d(igrd)),pws(iptr3d(igrd)),
     &                   pwg(iptr3d(igrd)),cod(iptr3d(igrd)),
     &                   cph(iptr3d(igrd)),snow(iptr2d(igrd)),
     &                   snowage(iptr2d(igrd)),snowrat(iptr2d(igrd)),
     &                   cigfrc(iptr2d(igrd)),cigtim(iptr2d(igrd)),
     &                   cigwtr(iptr3d(igrd)),cigpcp(iptr3d(igrd)),
     &                   cigent(iptr3d(igrd)),cigdet(iptr3d(igrd)))
         enddo
c
c  --- Estimate those fields that were not read ---
c
         call intrpdat()
c
c-----Calculate surface albedo fields
c
         do igrd = 1,ngrid
           call getalbedo(igrd,ncol(igrd),nrow(igrd),snow(iptr2d(igrd)),
     &                    snowage(iptr2d(igrd)),tsurf(iptr2d(igrd)),
     &                    fsurf(iptrlu(igrd)),albedo(iptr2d(igrd)),
     &                    snowfrc(iptr2d(igrd)),snowalb(iptr2d(igrd)))
         enddo
c
         write(*,'(a)') '   Done'
         call flush(6)
c
c  --- Increment next met read time/date
c
         whr = aint(inptim/100.)
         wmn = amod(inptim,100.)
         inptim = 100.*(whr + aint((wmn + dtinp)/60.)) + amod((wmn+dtinp),60.)
         if (inptim .GE. 2400.) then
            inptim = inptim - 2400.
            inpdate = inpdate + 1
            if (MOD(inpdate,1000) .GT. 365) then
               if (MOD(INT(inpdate/1000),4) .EQ. 0) then
                  if (MOD(inpdate,1000) .EQ. 367)
     &               inpdate = (INT(inpdate/1000)+1)*1000 + 1
               else
                  inpdate = (INT(inpdate/1000)+1)*1000 + 1
               endif
            endif
         endif
c
      endif
c
c  --- Initialize concentrations from an AIRQUALITY file or RESTART files ---
c
      if (nsteps .EQ. 1) then
         if (iproc_id .EQ. 0) then
            call readcnc()
            do igrd = 1,ngrid
               call wrtmass(igrd,chdate,chtime,0)
               call massum(ncol(igrd),nrow(igrd),nlay(igrd),0,0,
     &                     2,ncol(igrd)-1,2,nrow(igrd)-1,15,
     &                     igrd,nspec,ncol(igrd),nrow(igrd),
     &                     nlay(igrd),deltax(1,igrd),deltay(igrd),
     &                     depth(iptr3d(igrd)),mapscl(iptr2d(igrd)),
     &                     conc(iptr4d(igrd)),xmass0(1,igrd)        )
               do l = 1,nspec
                  xmsold(l,igrd) = xmass0(l,igrd)
               enddo
            enddo
c
c --- Initialize surface model mass fields
c
            if (lsrfmod) then
              do igrd = 1,ngrid
                call rdsrfmod(igrd,ncol(igrd),nrow(igrd),nsmspc,
     &                        endtim,enddate,solmas(iptrsm(igrd)),
     &                        vegmas(iptrsm(igrd)))
              enddo 
              if (lrstrt) then
                do ip = 1,ngrid
                  do ic = 1,nchdrn(ip)
                    ig = idchdrn(ic,ip)
                    if( ismin(ig) .EQ. 0 ) then
                      write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                    'Assigning srf mass from parent grid at ',
     &                    begtim,begdate,' grid',ig
                      call rassgn4d(ncol(ip),nrow(ip),1,nsmspc,i1(ig),
     &                      j1(ig),nmesh(ig),ncol(ig),nrow(ig),1,
     &                      solmas(iptrsm(ip)),solmas(iptrsm(ig)) )
                      call rassgn4d(ncol(ip),nrow(ip),1,nsmspc,i1(ig),
     &                      j1(ig),nmesh(ig),ncol(ig),nrow(ig),1,
     &                      vegmas(iptrsm(ip)),vegmas(iptrsm(ig)) )
                    endif
                  enddo
                enddo
              endif
            endif
c
c======================== Source Apportion Begin =======================
c
c  --- if this is not a restart, call routine to fill the initial
c      conditions arrays for the tracers ---
c
            if (ltrace .AND. .NOT. lrstrt) then
               if (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) then
                  call rdicrt(ncol(1),nrow(1),nlay(1),ntotsp,ptconc(1))
c
c  --- interpolate initial conditions to nests ---
c
                  if (ngrid.gt.1) then
                     do ip = 1,ngrid
                        do ic = 1,nchdrn(ip)
                           ig = idchdrn(ic,ip)
                           call intrpcnc(ntotsp,ncol(ip),nrow(ip),
     &                                   nlay(ip),i1(ig),j1(ig),nmesh(ig),
     &                                   ncol(ig),nrow(ig),nlay(ig),
     &                                   ptconc(ipsa3d(ip)),
     &                                   ptconc(ipsa3d(ig)) )
                        enddo
                     enddo
                  endif
c
c --- call routine to convert IC to ug/m ---
c
                  do igrd=1,ngrid
                     call cvticrt(igrd,ncol(igrd),nrow(igrd),
     &                            nlay(igrd),ntotsp,ptconc(ipsa3d(igrd)),
     &                            tempk(iptr3d(igrd)),press(iptr3d(igrd)))
                  enddo
               else
                  do igrd = 1,ngrid
                     call filaqsa(igrd,ncol(igrd),nrow(igrd),
     &                            nlay(igrd),nspec,ntotsp,
     &                            conc(iptr4d(igrd)),ptconc(ipsa3d(igrd)) )
                 enddo
              endif
            endif
            if (ltrace .AND. tectyp .EQ. RTRAC .AND. lsrfmodrt) then
               do igrd = 1,ngrid
                  call rdsrfrt(igrd,ncol(igrd),nrow(igrd),ntotsp,
     &                         rtsolmas(ipsa2d(igrd)),
     &                         rtvegmas(ipsa2d(igrd)))
               enddo
            endif
c
c========================= Source Apportion End ========================
c
c============================= DDM Begin ===============================
c
c  --- call routine to read the IC file for DDM ---
c
            if((lddm.OR.lhddm) .AND. .NOT.lrstrt .AND. nicddm.GT.0) then
               call rdicddm(ncol(1),nrow(1),nlay(1),ntotsp,ptconc(1))
c
c  --- interpolate initial conditions to nests ---
c 
               if (ngrid.gt.1) then
                  do ip = 1,ngrid
                     do ic = 1,nchdrn(ip)
                        ig = idchdrn(ic,ip)
                        call intrpcnc(ntotsp,ncol(ip),nrow(ip),
     &                                nlay(ip),i1(ig),j1(ig),nmesh(ig),
     &                                ncol(ig),nrow(ig),nlay(ig),
     &                                ptconc(ipsa3d(ip)),
     &                                ptconc(ipsa3d(ig))               )
                     enddo
                  enddo
               endif
c
c --- call routine to convert IC to ug/m ----
c
               do igrd=1,ngrid
                  call cvticddm(igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                          ntotsp,lsagas,ptconc(ipsa3d(igrd)),
     &                          tempk(iptr3d(igrd)),press(iptr3d(igrd)) )
               enddo
            endif
c
c============================= DDM End =================================
c
c========================= Process Analysis Begin ======================
c
            if (lipr .OR. lirr) then
               if (.NOT. lcpacum) call pazero()
               if (lipr) then
                  do igrd = 1,ngrid
                     call initipr(.TRUE.,iproc_id,igrd,nspec,ncol(igrd),
     &                                     nrow(igrd),nlay(igrd),
     &                                              conc(iptr4d(igrd)))
                  enddo
               endif
            endif
c
c========================= Process Analysis End ========================
c
         endif
c
c  --- send initial field to compute nodes ---
c
         if (lmpi) then
            do igrd=1,ngrid
               do ilay=1,nlay(igrd)
                  do ispc=1,nspec
                     if (iproc_id .gt. 0) then
                        call node_recv_1species_data(conc(iptr4d(igrd)),
     &                       igrd,nlay(igrd),ilay,nspec,ispc,itag       )
                     else
                        call master_send_1species_data(conc(iptr4d(igrd)),
     &                                                 igrd,nlay(igrd),ilay,
     &                                                 nspec,ispc,itag      )
                     endif
                     itag = itag+1
                  enddo
               enddo
            enddo
c
c========================= Probing Tools Begin ======================
c
c
c   --- pass the initial concentrations for probing tools ---
c
             if( ltrace .OR. lddm .OR. lhddm .OR. lirr ) then
                do igrd=1,ngrid
                   do ilay=1,nlay(igrd)
                      do ispc=1,ntotsp
                         if( iproc_id .GT. 0 ) then
                           call node_recv_1species_data  (ptconc(ipsa3d(igrd)),
     &                           igrd, nlay(igrd), ilay, ntotsp, ispc, itag)
                         else
                           call master_send_1species_data (ptconc(ipsa3d(igrd)),
     &                           igrd, nlay(igrd), ilay, ntotsp, ispc, itag)
                         endif
                         itag=itag+1
                      enddo
                   enddo
                enddo
             endif
         endif
c
c========================= Probing Tools End ========================
c
      endif
c
c  --- Calculate timestep ---
c
      if (iproc_id .EQ. 0) then
         write(*,'(a20,$)') 'timestep ......'
         call timestep()
         write(*,'(a)') '   Done'
         call flush(6)
      endif
c
c========================= Process Analysis Begin ========================
c
c  --- update the process analysis data ---
c
      if(  iproc_id .GT. 0 .AND. (lipr .OR. lirr) ) then
          if( .NOT. lcpacum ) call pazero()
          if( lipr ) then
              do igrd = 1,ngrid
                 call initipr(.TRUE.,iproc_id,igrd,nspec,mmxp(igrd),
     &                         mmyp(igrd),nlay(igrd),conc(iptr4d(igrd)))
              enddo
           endif
      endif
c
c========================= Process Analysis End ========================
c
c --- force the nodes to wait for the master ---
c
      if (lmpi) then
         call MPI_Barrier(MPI_COMM_WORLD, ierr)
         call nodes_met(numprocs,iproc_id)
      endif
c
      if (.NOT. lmpi .OR. iproc_id .GT. 0) then
c
c  --- calculate the initial mass on the slices ---
c
         if (nsteps .EQ. 1) then
            do igrd=1,ngrid
               if( iproc_id .GT. 0 ) then
                  call massum(mmxp(igrd),mmyp(igrd),mmzp(igrd),mi0(igrd),
     &                     mj0(igrd),mia(igrd),miz(igrd),mja(igrd),
     &                     mjz(igrd),mibcon(igrd),igrd,nspec,ncol(igrd),
     &                     nrow(igrd),nlay(igrd),deltax(1,igrd),
     &                     deltay(igrd),depth(iptr3d(igrd)),
     &                     mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),
     &                     xmass0(1,igrd))
                  do l = 1,nspec
                     xmsold(l,igrd) = xmass0(l,igrd)
                     xmass(l,igrd) = xmass0(l,igrd)
                  enddo
               endif
            enddo
         endif
c
c-----Calculate horizontal diffusion coefficients
c
         if (iproc_id .LE. 1) write(*,'(a20,$)') 'khorz ......'
         do igrd = 1,ngrid
            call newgrid(igrd)
            call khorz(igrd,mmxp(igrd),mmyp(igrd),
     &                 nlay(igrd),nrow(igrd),j0,
     &                 deltax(1,igrd),deltay(igrd),deltat(igrd),
     &                 windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                 idfin(iptr2d(igrd)),rkx(iptr3d(igrd)),
     &                 rky(iptr3d(igrd))                        )
         enddo
         if (iproc_id .LE. 1) write(*,'(a)') '   Done'
         call flush(6)
c
c-----Calculate subgrid convective transport matrices
c
         if (iproc_id .LE. 1) write(*,'(a20,$)') 'convtrans ......'
         do igrd = 1,ngrid
            if (lcig(igrd)) then
             call convtrans(igrd,dtinp,mmxp(igrd),mmyp(igrd),nlay(igrd),
     &                       deltat(igrd),idfin(iptr2d(igrd)),
     &                       depth(iptr3d(igrd)),press(iptr3d(igrd)),
     &                       tempk(iptr3d(igrd)),cigfrc(iptr2d(igrd)),
     &                       cigtim(iptr2d(igrd)),cigent(iptr3d(igrd)),
     &                       cigdet(iptr3d(igrd)),cigmas(iptrcig(igrd)))
            endif
         enddo
         if (iproc_id .LE. 1) write(*,'(a)') '   Done'
         call flush(6)
      endif
c
      end
c
c-----------------------------------------------------------------------
c    END subroutine tstep_init:
c-----------------------------------------------------------------------
c
