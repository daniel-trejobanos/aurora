      subroutine emistrns(igrd,chtime,chdate,
     &                               numprocs,iproc_id,lupdtdep)
      use filunit
      use grid
      use grid_nodes
      use chmstry
      use bndary
      use o3colmap
      use camxfld
      use camxcom
      use pigsty
      use ptemiss
      use procan
      use rtracchm
      use rtcmcchm
      use tracer
c
      use master_mod
      use node_mod 

      implicit none
c
c----CAMx v7.10 210105
c
c     EMISTRNS performs the following tasks for one grid:
c        1.  Injects new PiG puffs and moves all puffs
c        2.  Determines mass-conserving vertical velocity parameters
c        3.  Updates concentrations due to emissions
c        4.  Performs 3-D transport
c        5.  Performs wet scavenging 
c        6.  Updates met fields to current time step
c        7.  Performs 3-D diffusion
c
c     Copyright 1996 - 2021
c     Ramboll 
c            
c     Modifications:  
c        01/30/02   Added code for RTRAC probing tool
c        10/12/04   Water vapor and vertical diffusivity fields are now
c                   time-interpolated
c        10/12/04   PIGINIT and PIGWALK moved into this routine, and
c                   IRONDRIV+GRESDRIV moved out to NESTING and CAMx
c        05/07/07   Added calls to subroutines to update Probing Tools
c                   boundary conditions after met is updated
c       03/15/09    Added code for deposition output for tracers
c       07/16/07 -bkoo-     Added check for HDDM
c       07/16/08 -bkoo-     Added DDM turn-off flag
c       10/29/09 -cemery-   Added code for RTRAC surface mass
c       11/04/09    Revised vertical advection solver technique, now
c                   employs zero-gradient top boundary condition
c       04/02/12    Removed drought stress and snow flag; AHO
c                   file is now just ozone column
c       04/30/13    Added surface model emissions, deposition
c       04/07/14    Added top con file
c       08/11/14    Albedo now includes effects of snow
c       10/09/14    Added subgrid convective model
c       12/18/15    Added oceanic I2/HOI emissions from O3 dep
c       07/23/18    Added emiss flux of Bi-Di NH3 drydep (eflxnh3)
c       11/17/20    RTCMC dry/wet deposition is now consistent with RTRAC
c
c     Input arguments:
c        igrd                grid index
c        chtime              simulation time ('HH:MM:SS')
c        chdate              simulation date ('YY/MM/DD')
c
c     Output arguments:
c        none
c
c     Routines called:
c        PIGINIT
c        PIGWALK
c        ZRATES
c        EMISS
c        XYADVEC
c        ZADVEC
c        WETDEP
c        UPDTMET
c        DIFFUS
c
c     Called by:
c        CAMx
c        NESTING
c
      include 'camx.prm'
      include 'flags.inc'
      include 'deposit.inc'
      include 'mpif.h'
      include 'rtracsrf.inc'
c
      character*8 chtime, chdate
      integer     igrd, i
      integer     numprocs, iproc_id
      logical lupdtdep
      logical lvupsolv(MXCELLS*MXCELLS*MXLAYER)
      real rhoscl(MXCELLS*MXCELLS)
      integer mxp_rt, myp_rt, nsp_rt
c
      common /com1emistrns/ rhoscl,lvupsolv
c
c======================== Source Apportion Begin =======================
c
      integer iptr
      real*8 ardum(MXTRSP)
      real*8 ptdum(MXTRSP)
      real*8 fluxdum(MXTRSP*11)
      real*8 depdum(1)
      real cigdum1(1,1,1,1)
      real cigdum2(1,1,1)
      real adjwest(MXCELLS,MXLAYER)
      real adjeast(MXCELLS,MXLAYER)
      real adjsouth(MXCELLS,MXLAYER)
      real adjnorth(MXCELLS,MXLAYER)
c
      common /com2emistrns/ ptdum, ardum, fluxdum
c
c========================= Source Apportion End ========================
c
c-----Entry point
c
      if( .NOT. lmpi .OR. (iproc_id .eq. 1) ) then
         write(*,'(/,a,i3)')           ' Processing grid: ',igrd
         write(*,'(a,f7.1)')           '    Timestep (s): ',deltat(igrd)
         write(*,'(a,2x,a8,1X,a8,/)')  '  Grid Date/Time: ',
     &                                                     chdate,chtime
         call flush(6)
      endif
      if( .not. LMPI .or. (iproc_id .gt. 0) ) then  !cbwmpi
         write(iout,'(/,a,i3)')          ' Processing grid: ',igrd
         write(iout,'(a,f7.1)')          '        Timestep: ',
     &                                                     deltat(igrd)
         write(iout,'(a,2x,a8,1x,a8,/)') '  Grid Date/Time: ',
     &                                                     chdate,chtime
      endif
c
c-----Initialize new pig puffs
c
      if (ipigflg .NE. 0 .AND. ipigint .EQ. igrd) then
        if( iproc_id .eq. 0 ) then
           write(*,'(a20,$)') 'piginit ......'
           write(iout,'(a20,$)') 'piginit ......'
           call piginit(numprocs,deltat(igrd),nptsrc,nrad,nspec,nemspc,
     &                           ptemis,height,windu,windv,tempk,press)
           write(*,'(a)') '   Done'
           call flush(6)
           write(iout,'(a)') '   Done'
           call flush(iout)
c
c-----Update PiG locations 
c
           write(*,'(a20,$)') 'pigwalk ......'
           write(iout,'(a20,$)') 'pigwalk ......'
           call pigwalk(deltat(igrd))
           write(*,'(a)') '   Done'
           call flush(6)
           write(iout,'(a)') '   Done'
           call flush(iout)
        endif
c
        if( lmpi ) call nodes_met_pig(numprocs,iproc_id)
c       
      endif
c
c-----Inject emissions
c
      if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
        write(*,'(a20,$)') 'emiss ......'
      endif
      if( .NOT. lmpi .OR. (iproc_id .GT. 0) ) then
        write(iout,'(a20,$)') 'emiss ......'
        call emiss(igrd,mxp,myp,mzp,i0,j0,ia,iz,ja,jz,
     &           nspec,nemspc,nlu,3*ndepspc+2,densfac,
     &           lemmap,.FALSE.,
     &           nosrc(igrd),idsrc(1,igrd),isrc(1,igrd),
     &           jsrc(1,igrd),ncol(igrd),nrow(igrd),
     &           nlay(igrd),nlayers_ems,
     &           deltat(igrd),deltax(1,igrd),deltay(igrd),
     &           mapscl(iptr2d(igrd)),
     &           height(iptr3d(igrd)),depth(iptr3d(igrd)),
     &           windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &           tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &           icdocn(iptr2d(igrd)),fsurf(iptrlu(igrd)),
     &           tsurf(iptr2d(igrd)),eflxnh3(iptr2d(igrd)),
     &           aremis(iptrem(igrd)),ptemis,MAX(nptsrc,1),
     &           armass(1,igrd),ptmass(1,igrd),
     &           reemis(iptrsm(igrd)),
     &           conc(iptr4d(igrd)),
     &           depfld(iptrdp(igrd)),ipacl_3d(iptr3d_full(igrd)))
c
c======================== Source Apportion Begin =======================
c
c  --- call routine with tracer species arrays, send dummy
c      arguemnts for the total mass arrays ---
c
        if( ltrace .OR. ((lddm .OR. lhddm) .AND. lddmcalc(igrd)) ) then
           call emiss(igrd,mxp,myp,mzp,i0,j0,ia,iz,ja,jz,
     &              ntotsp,ntotsp,nlu,1,densfac,lsamap,.TRUE.,
     &              nosrc(igrd),idsrc(1,igrd),isrc(1,igrd),jsrc(1,igrd),
     &              ncol(igrd),nrow(igrd),nlay(igrd),nlayers_ems,deltat(igrd),
     &              deltax(1,igrd),deltay(igrd),mapscl(iptr2d(igrd)),
     &              height(iptr3d(igrd)),
     &              depth(iptr3d(igrd)),windu(iptr3d(igrd)),
     &              windv(iptr3d(igrd)),
     &              tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &              icdocn(iptr2d(igrd)),fsurf(iptrlu(igrd)),
     &              tsurf(iptr2d(igrd)),eflxnh3(iptr2d(igrd)),
     &              saemis(ipsa3d_ems(igrd)),sapnts,MAX(nptsrc,1),
     &              ardum,ptdum,ardum,ptconc(ipsa3d(igrd)),
     &              depdum,ipacl_3d(iptr3d_full(igrd)) )
           if( tectyp .EQ. RTRAC .AND. lsrfmodrt ) then
              call reemisrt(mxp,myp,mzp,i0,j0,ncol(igrd),nrow(igrd),
     &                      nlay(igrd),nrtrac,deltat(igrd),
     &                      tsurf(iptr2d(igrd)),cellat(iptr2d(igrd)),
     &                      cellon(iptr2d(igrd)),fsurf(iptrlu(igrd)),
     &                      height(iptr3d_full(igrd)),
     &                      press(iptr3d(igrd)),windu(iptr3d(igrd)),
     &                      windv(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &                      snow(iptr2d(igrd)),rtsolmas(ipsa2d(igrd)),
     &                      rtvegmas(ipsa2d(igrd)),ptconc(ipsa3d(igrd)))
           endif
        endif
c
c========================= Source Apportion End ========================
c
        if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
          write(*,'(a)') '   Done'
          call flush(6)
        endif
        write(iout,'(a)') '   Done'
        call flush(iout)
c
c-----Determine mass-conserving vertical velocity parameters
c
        if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
          write(*,'(a20,$)') 'zrates ......'
        endif
        write(iout,'(a20,$)') 'zrates ......'
      endif
      if( iproc_id .gt. 0 ) then
          call node_send_lbc(iproc_id, nspec,igrd)
          if( ltrace .OR. lddm .OR. lhddm )
     &        call node_send_lbc_pt(iproc_id, ntotsp, igrd)
      endif
      if( iproc_id .GT. 0 ) then
          call node_get_lbc(iproc_id, nspec,igrd)
          if( ltrace .OR. lddm .OR. lhddm )
     &         call node_get_lbc_pt(iproc_id, ntotsp, igrd)
      endif
      if( .NOT. lmpi .OR. (iproc_id .GT. 0) ) then
        call zrates(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,igrd,
     &            ncol(igrd),nrow(igrd),nlay(igrd),
     &            nadv(1,igrd),deltat(igrd),deltax(1,igrd),
     &            deltay(igrd),depth(iptr3d(igrd)),
     &            phpt(iptr3d(igrd)),pppt(iptr3d(igrd)),
     &            ptpt(iptr3d(igrd)),windu(iptr3d(igrd)),
     &            windv(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &            press(iptr3d(igrd)),mapscl(iptr2d(igrd)),
     &            dilut(iptr3d(igrd)),entrn(iptr3d(igrd)),
     &            rhoscl,lvupsolv)
      endif
      if( .NOT. lmpi .OR. (iproc_id .GT. 0) ) then
          if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
            write(*,'(a)') '   Done'
            call flush(6)
          endif
          write(iout,'(a)') '   Done'
          call flush(iout)
c
c======================= Source Apportion Begin ========================
c
c
c  --- if this is the master grid, update the boundary
c      conditions for the tracer species ---
c
          if( ltrace .AND. .NOT. lsa_iorbc .AND. igrd .EQ. 1 .AND. 
     &                  tectyp .NE. RTRAC .AND. tectyp .NE. RTCMC ) then
             call clrbdysa(1,mmxp(1),mmyp(1),mmzp(1),ntotsp,ptconc(1))
             call filbdysa(1,mmxp(1),mmyp(1),mmzp(1),nspec,ntotsp,
     &                                              conc(1),ptconc(1))
          endif
c
c======================== DDM Begin =======================
c
c
c-----Perform 2-D transport
c
          call xyadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &             igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &             nrad,nspec,MAX(1,ntotsp),nadv(1,igrd),
     &             deltat(igrd),deltax(1,igrd),deltay(igrd),
     &             windu(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &             mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),
     &             ctop(iptr1lay(igrd)),pttop,fluxes(1,igrd),ipsa3d(igrd),
     &             ipacl_3d(iptr3d_full(igrd)),iproc_id )
          call flush(6)
          call flush(iout)
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to load the top concentration for tracers ---
c
          if( ltrace .AND. 
     &              tectyp .NE. RTRAC .AND. tectyp .NE. RTCMC ) then
              if( .NOT. lsa_iortc ) call filptopsa(igrd,mxp,myp,
     &                     nspec,ntotsp,ctop(iptr1lay(igrd)),pttop)
          endif
c
c======================== Source Apportion End =======================
c
c
c-----Perform vertical transport
c
          call zadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &             .FALSE.,ltopcon,igrd,ncol(igrd),nrow(igrd),
     &             nlay(igrd),nrad,nspec,MAX(1,ntotsp),deltat(igrd),
     &             deltax(1,igrd),deltay(igrd),
     &             idfin(iptr2d(igrd)),depth(iptr3d(igrd)),
     &             entrn(iptr3d(igrd)),dilut(iptr3d(igrd)),
     &             tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &             spname,conc(iptr4d(igrd)),rhoscl,ctop(iptr1lay(igrd)),
     &             lvupsolv,pttop,fluxes(1,igrd),fluxtmp,ipsa3d(igrd),
     &             ipacl_2d(iptr2d_full(igrd)),
     &             ipacl_3d(iptr3d_full(igrd)),iproc_id )
          call flush(6)
          call flush(iout)
c
c======================== Source Apportion Begin =======================
c
          if( ltrace ) then
c
             if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
                 call xyadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,igrd,
     &                ncol(igrd),nrow(igrd),nlay(igrd),
     &                0,ntotsp,ntotsp,nadv(1,igrd),deltat(igrd),
     &                deltax(1,igrd),deltay(igrd),windu(iptr3d(igrd)),
     &                windv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &                mapscl(iptr2d(igrd)),ptconc(ipsa3d(igrd)),pttop,
     &                pttop,fluxdum,ipsa3d(igrd),
     &                ipacl_3d(iptr3d_full(igrd)),iproc_id )
c
c  --- call routine with tracer species arrays, send dummy
c      arguments for the total mass arrays ---
c
                 call zadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                .TRUE.,ltopcon,igrd,ncol(igrd),nrow(igrd),
     &                nlay(igrd),0,ntotsp,1,deltat(igrd),deltax(1,igrd),
     &                deltay(igrd),idfin(iptr2d(igrd)),
     &                depth(iptr3d(igrd)),entrn(iptr3d(igrd)),
     &                dilut(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &                press(iptr3d(igrd)),ptname,ptconc(ipsa3d(igrd)),
     &                rhoscl,pttop,lvupsolv,pttop,fluxdum,fluxtmp,
     &                ipsa3d(igrd),ipacl_2d(iptr2d_full(igrd)),
     &                ipacl_3d(iptr3d_full(igrd)),iproc_id )
             else
                 call zadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                .TRUE.,ltopcon,igrd,ncol(igrd),nrow(igrd),
     &                nlay(igrd),0,ntotsp,1,deltat(igrd),
     &                deltax(1,igrd),deltay(igrd),
     &                idfin(iptr2d(igrd)),depth(iptr3d(igrd)),
     &                entrn(iptr3d(igrd)),dilut(iptr3d(igrd)),
     &                tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                ptname,ptconc(ipsa3d(igrd)),rhoscl,pttop,
     &                lvupsolv,pttop,fluxdum,fluxtmp,
     &                ipsa3d(igrd),ipacl_2d(iptr2d_full(igrd)),
     &                ipacl_3d(iptr3d_full(igrd)),iproc_id )
c
c   --- call routine to recalibrate the tracers ----
c
                 call caliball(mxp,myp,mzp,nspec,ntotsp,
     &                conc(iptr4d(igrd)),ptconc(ipsa3d(igrd)))
c
             endif

c
c----- call routine to advect the timing tracers horizontally ---
c
             if( npttim .GT. 0 ) call timadv(iproc_id,mxp,myp,mzp,j0,
     &             igrd,nrow(igrd),ntotsp,
     &             deltat(igrd),deltax(1,igrd),deltay(igrd),
     &             windu(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &             mapscl(iptr2d(igrd)),ptconc(ipsa3d(igrd)))
          endif
      endif
c
c========================= Source Apportion End ========================
c
c-----Update vertical grid and environmental fields for this timestep
c
      if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
         write(*,'(a20,$)') 'updtmet ......'
      endif
      write(iout,'(a20,$)') 'updtmet ......'
      call updtmet(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &             igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &             nspec,ngas,ntotsp,densfac,deltat(igrd),
     &             phpt(iptr3d(igrd)),height(iptr3d(igrd)),
     &             depth(iptr3d(igrd)),pppt(iptr3d(igrd)),
     &             press(iptr3d(igrd)),pupt(iptr3d(igrd)),
     &             windu(iptr3d(igrd)),pvpt(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),pspt(iptr2d(igrd)),
     &             tsurf(iptr2d(igrd)),
     &             ptpt(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &             pwpt(iptr3d(igrd)),water(iptr3d(igrd)),
     &             pkpt(iptr3d(igrd)),rkv(iptr3d(igrd)),
     &             conc(iptr4d(igrd)),ptconc(ipsa3d(igrd)),
     &             adjwest,adjeast,adjsouth,adjnorth)
      if( ldry .OR. lacm2 .OR. ipigflg .NE. 0 ) then
        call srfruf(mxp,myp,mzp,ncol(igrd),nrow(igrd),nlay(igrd),
     &            datec(igrd),cellat(iptr2d(igrd)),cellon(iptr2d(igrd)),
     &            windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &            fsurf(iptrlu(igrd)),snow(iptr2d(igrd)),
     &            lrdlai(igrd),lai(iptr2d(igrd)),sfcz0(iptr2d(igrd)))
      endif
      if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
         write(*,'(a)') '   Done'
      endif
      write(iout,'(a)') '   Done'
      call flush(6)
      call flush(iout)
c
c-----Calculate dry deposition rates
c
      if( .NOT. lmpi .OR. (iproc_id .GT. 0) ) then
          if (ldry) then
           if( lupdtdep ) then
            lupdtdep = .FALSE.
            if( .NOT. lmpi .OR. iproc_id .EQ. 1 ) then
               write(*,'(a20,$)') 'drydep ......'
            endif
            write(iout,'(a20,$)') 'drydep ......'
            call drydep(mxp,myp,mzp,
     &              itzon,tsurf(iptr2d(igrd)),cellat(iptr2d(igrd)),
     &              cellon(iptr2d(igrd)),lrdlai(igrd),lai(iptr2d(igrd)),
     &              pwr(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &              height(iptr3d(igrd)),press(iptr3d(igrd)),
     &              windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &              cod(iptr3d(igrd)),
     &              water(iptr3d(igrd)),fsurf(iptrlu(igrd)),
     &              tempk(iptr3d(igrd)),snow(iptr2d(igrd)),
     &              icdocn(iptr2d(igrd)),conc(iptr4d(igrd)),
     &              vdep(iptr1lay(igrd)),eflxnh3(iptr2d(igrd)))
            call depsmry(igrd,mxp,myp,nspec,vdep(iptr1lay(igrd)) )
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. (tectyp.EQ.RTRAC .OR. tectyp.EQ.RTCMC)) then
               call drydeprt(mxp,myp,mzp,nrtrac,itzon,
     &                  tsurf(iptr2d(igrd)),cellat(iptr2d(igrd)),
     &                  cellon(iptr2d(igrd)),lrdlai(igrd),
     &                  lai(iptr2d(igrd)),
     &                  pwr(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &                  height(iptr3d(igrd)),press(iptr3d(igrd)),
     &                  windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                  cod(iptr3d(igrd)),
     &                  water(iptr3d(igrd)),fsurf(iptrlu(igrd)),
     &                  tempk(iptr3d(igrd)),
     &                  snow(iptr2d(igrd)),icdocn(iptr2d(igrd)),
     &                  vdeprt(ipsa2d(igrd)))
            endif
c
c========================= Source Apportion End ========================
c
            if( .NOT. lmpi .OR. iproc_id .EQ. 1 ) then
              write(*,'(a)') '   Done'
            endif
            write(iout,'(a)') '   Done'
            call flush(6)
            call flush(iout)
           endif
          else
            call zeros(vdep(iptr1lay(igrd)),mxp*myp*nspec)
          endif
c
c======================= Source Apportion Begin ========================
c
c
c  --- if this is the master grid, update the boundary
c      conditions for the tracer species ---
c
          if( ltrace .AND. .NOT. lsa_iorbc .AND. igrd .EQ. 1 .AND. 
     &                 tectyp .NE. RTRAC .AND. tectyp .NE. RTCMC ) then
             call clrbdysa(1,mmxp(1),mmyp(1),mmzp(1),ntotsp,ptconc(1))
             call filbdysa(1,mmxp(1),mmyp(1),mmzp(1),nspec,ntotsp,
     &                                                  conc(1),ptconc(1))
          endif
c
c======================== DDM Begin =======================
c
          if( (lddm.OR.lhddm) .and. nbcddm .GT. 0 
     &                              .and. igrd .EQ. 1 ) then
               call adjbcddm(adjwest,adjeast,adjsouth,adjnorth,mmxp(1),
     &                              mmyp(1),mmzp(1),ntotsp,ptconc(1))
          endif
c
c======================== DDM End =======================
c
c
c========================= Source Apportion End ========================
c
c   --- call routine to calculate the depostion velocities
c       for the tracer species ---
c
          if( ltrace ) then
             mxp_rt = 1
             myp_rt = 1
             nsp_rt = 1
             if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
                mxp_rt = mxp
                myp_rt = myp
                nsp_rt = ntotsp
             endif
             if( ldry ) call filvdsa(mxp,myp,mzp,nspec,ntotsp,
     &                     mxp_rt,myp_rt,nsp_rt,conc(iptr4d(igrd)),
     &                                     vdep(iptr1lay(igrd)),ptvdep,
     &                                             vdeprt(ipsa2d(igrd)))
          endif
c
c-----Perform 3-D Diffusion
c
          call diffus(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,.FALSE.,
     &          .TRUE.,igrd,ncol(igrd),nrow(igrd),nlay(igrd),nrad,nspec,
     &          ndepspc*3,nsmspc,nsmspc,deltat(igrd),deltax(1,igrd),
     &          deltay(igrd),
     &          idfin(iptr2d(igrd)),vdep(iptr1lay(igrd)),
     &          rkx(iptr3d(igrd)),rky(iptr3d(igrd)),
     &          rkv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &          tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &          windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &          water(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &          tsurf(iptr2d(igrd)),mapscl(iptr2d(igrd)),
     &          sfcz0(iptr2d(igrd)),snow(iptr2d(igrd)),
     &          snowfrc(iptr2d(igrd)),conc(iptr4d(igrd)),
     &          fluxes(1,igrd),fluxtmp,depfld(iptrdp(igrd)),
     &          solmas(iptrsm(igrd)),vegmas(iptrsm(igrd)),
     &          fsurf(iptrlu(igrd)),ipsa3d(igrd),
     &          iproc_id,ipacl_2d(iptr2d_full(igrd)),
     &          ipacl_3d(iptr3d_full(igrd)),
     &          '  z diffusion ......','     x/y diff ......')
          call flush(6)
          call flush(iout)
c
c======================== Source Apportion Begin =======================
c
          if( ltrace ) then
c
c  --- call routine with tracer species arrays, send dummy
c      arguments for the total mass arrays ---
c
               if( lptdepout ) then
                   call diffus(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                  .TRUE.,.TRUE.,igrd,ncol(igrd),nrow(igrd),
     &                  nlay(igrd),0,ntotsp,notimespc,1,1,deltat(igrd),
     &                  deltax(1,igrd),
     &                  deltay(igrd),idfin(iptr2d(igrd)),ptvdep,
     &                  rkx(iptr3d(igrd)),rky(iptr3d(igrd)),
     &                  rkv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &                  tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                  windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                  water(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &                  tsurf(iptr2d(igrd)),mapscl(iptr2d(igrd)),
     &                  sfcz0(iptr2d(igrd)),snow(iptr2d(igrd)),
     &                  snowfrc(iptr2d(igrd)),ptconc(ipsa3d(igrd)),
     &                  fluxdum,fluxtmp,ptdryfld(ipsadep(igrd)),depdum,
     &                  depdum,fsurf(iptrlu(igrd)),ipsa3d(igrd),
     &                  iproc_id,ipacl_2d(iptr2d_full(igrd)),
     &                  ipacl_3d(iptr3d_full(igrd)),
     &                  '  SA z diffus ......','  SA x/y diff ......')
               elseif( tectyp .EQ. RTRAC .AND. lsrfmodrt ) then
                   call diffus(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                  .TRUE.,.TRUE.,igrd,ncol(igrd),nrow(igrd),
     &                  nlay(igrd),0,ntotsp,ntotsp,ntotsp,1,
     &                  deltat(igrd),
     &                  deltax(1,igrd),deltay(igrd),idfin(iptr2d(igrd)),
     &                  ptvdep,rkx(iptr3d(igrd)),rky(iptr3d(igrd)),
     &                  rkv(iptr3d(igrd)),depth(iptr3d_full(igrd)),
     &                  tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                  windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                  water(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &                  tsurf(iptr2d(igrd)),mapscl(iptr2d(igrd)),
     &                  sfcz0(iptr2d(igrd)),snow(iptr2d(igrd)),
     &                  snowfrc(iptr2d(igrd)),ptconc(ipsa3d(igrd)),
     &                  fluxdum,fluxtmp,rtsolmas(ipsa2d(igrd)),
     &                  rtvegmas(ipsa2d(igrd)),depdum,
     &                  fsurf(iptrlu(igrd)),ipsa3d(igrd),
     &                  iproc_id,ipacl_2d(iptr2d_full(igrd)),
     &                  ipacl_3d(iptr3d_full(igrd)),
     &                  '  SA z diffus ......','  SA x/y diff ......')
               else
                   call diffus(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                  .TRUE.,.FALSE.,igrd,ncol(igrd),nrow(igrd),
     &                  nlay(igrd),0,ntotsp,notimespc,1,1,deltat(igrd),
     &                  deltax(1,igrd),
     &                  deltay(igrd),idfin(iptr2d(igrd)),ptvdep,
     &                  rkx(iptr3d(igrd)),rky(iptr3d(igrd)),
     &                  rkv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &                  tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                  windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                  water(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &                  tsurf(iptr2d(igrd)),mapscl(iptr2d(igrd)),
     &                  sfcz0(iptr2d(igrd)),snow(iptr2d(igrd)),
     &                  snowfrc(iptr2d(igrd)),ptconc(ipsa3d(igrd)),
     &                  fluxdum,fluxtmp,depdum,depdum,depdum,
     &                  fsurf(iptrlu(igrd)),ipsa3d(igrd),
     &                  iproc_id,ipacl_2d(iptr2d_full(igrd)),
     &                  ipacl_3d(iptr3d_full(igrd)),
     &                  '  SA z diffus ......','  SA x/y diff ......')
               endif
          endif
c
c-----Subgrid Cloud-in-Grid transport, wet scavenging, aqueous chemistry
c

          if (lwet .or. lcig(igrd)) then
            if( .NOT. lmpi .OR. iproc_id .EQ. 1 ) then
              write(*,'(a20,$)') ' cigdrive ......'
            endif
            write(iout,'(a20,$)') ' cigdrive ......'
            if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) then
              iptr = 1
              if( lsrfmodrt ) iptr = ipsa2d(igrd)
              call cigdrive(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,igrd,
     &                    ncol(igrd),nrow(igrd),nlay(igrd),nspec,
     &                    ndepspc*3,idfin(iptr2d(igrd)),deltat(igrd),
     &                    densfac,dtout,deltax(1,igrd),deltay(igrd),
     &                    mapscl(iptr2d(igrd)),ldark(iptr2d(igrd)),
     &                    depth(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &                    press(iptr3d(igrd)),water(iptr3d(igrd)),
     &                    cigfrc(iptr2d(igrd)),cigwtr(iptr3d(igrd)),
     &                    cigph(iptr3d(igrd)),cigpcp(iptr3d(igrd)),
     &                    cwc(iptr3d(igrd)),cph(iptr3d(igrd)),
     &                    pwr(iptr3d(igrd)),pws(iptr3d(igrd)),
     &                    pwg(iptr3d(igrd)),conc(iptr4d(igrd)),
     &                    cigmas(iptrcig(igrd)),depfld(iptrdp(igrd)),
     &                    fluxes(1,igrd),xmschem(1,igrd),ipsa3d(igrd),
     &                    ipacl_3d(iptr3d_full(igrd)),
     &                    ptwetfld(MAX(ipsadep(igrd),1)),
     &                    fsurf(iptrlu(igrd)),ptconc(ipsa3d(igrd)),
     &                    rtsolmas(iptr),rtvegmas(iptr),mxp,myp,mzp,
     &                    nrtrac)
            else
              call cigdrive(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,igrd,
     &                 ncol(igrd),nrow(igrd),nlay(igrd),nspec,ndepspc*3,
     &                 idfin(iptr2d(igrd)),deltat(igrd),densfac,
     &                 dtout,deltax(1,igrd),deltay(igrd),
     &                 mapscl(iptr2d(igrd)),ldark(iptr2d(igrd)),
     &                 depth(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &                 press(iptr3d(igrd)),water(iptr3d(igrd)),
     &                 cigfrc(iptr2d(igrd)),cigwtr(iptr3d(igrd)),
     &                 cigph(iptr3d(igrd)),cigpcp(iptr3d(igrd)),
     &                 cwc(iptr3d(igrd)),cph(iptr3d(igrd)),
     &                 pwr(iptr3d(igrd)),pws(iptr3d(igrd)),
     &                 pwg(iptr3d(igrd)),conc(iptr4d(igrd)),
     &                 cigmas(iptrcig(igrd)),depfld(iptrdp(igrd)),
     &                 fluxes(1,igrd),xmschem(1,igrd),ipsa3d(igrd),
     &                 ipacl_3d(iptr3d_full(igrd)),
     &                 ptwetfld(MAX(ipsadep(igrd),1)),
     &                 fsurf(iptrlu(igrd)),cigdum1,cigdum2,cigdum2,1,1,1,1)
            endif
            if( .NOT. lmpi .OR. iproc_id .EQ. 1 ) then
               write(*,'(a)') '   Done'
            endif
            write(iout,'(a)') '   Done'
            call flush(6)
            call flush(iout)
          endif      !lwet,lcig
c
c  --- call routine to do haze adjustment ---
c
          if( lchem ) then
            if( iproc_id .LE. 1 ) then
               write(*,'(a20,$)') 'TUV ......'
            endif
            write(iout,'(a20,$)') 'TUV ......'
            call flush(6)
            call drvtuv(igrd,mxp,myp,mzp,
     &                  nspec,time,date,itzon,
     &                  idfin(iptr2d(igrd)),
     &                  cellat(iptr2d(igrd)),cellon(iptr2d(igrd)),
     &                  height(iptr3d(igrd)),press(iptr3d(igrd)), 
     &                  tsurf(iptr2d(igrd)),tempk(iptr3d(igrd)),
     &                  water(iptr3d(igrd)),conc(iptr4d(igrd)),
     &                  cod(iptr3d(igrd)),albedo(iptr2d(igrd)),
     &                  cldtrns(iptr3d(igrd)))
            if( iproc_id .LE. 1 ) then
               write(*,'(a)') '   Done'
            endif
            write(iout,'(a)') '   Done'
            call flush(6)
            call flush(iout)
          endif
c
      endif   !end of if( .not. LMPI .or. (iproc_id .gt. 0) ) on line332
c
      return
      end
