      subroutine diffus(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,losat,ldepfld,
     &                  igrd,ncol,nrow,nlay,nrads,nspc,ndpspc1,ndpspc2,
     &                  ndpspc3,deltat,dx,dy,idfin,vdep,rkx,rky,rkv,
     &                  depth,tempk,press,uwind,vwind,qvap,cwc,temps,
     &                  mapscl,sfcz0,snow,snowfrc,conc,fluxes,fluxtmp,
     &                  depfld1,depfld2,depfld3,fsurf,isaptr,iproc_id,
     &                  ipa_xy,ipa_lay,strz,strxy)
      use filunit
      use chmstry
      use bndary
      use procan
      use tracer
      use rtracchm
c
c----CAMx v7.10 210105
c
c     DIFFUS drives 3-D diffusion of concentrations.  This version also
c     performs diffusion of sensitivities if DDM is enabled.
c
c     Copyright 1996 - 2021
c     Ramboll
c          
c     Modifications:
c        4/17/00   Revised diffusion equations to weight fluxes by density
c       12/07/01   added instructions for OMP
c        1/13/03   added deposited mass array
c       10/12/04   Multiple substeps as f(Kv) applied for vertical diffusion 
c       10/09/08   Added ACM2 option (DOES NOT WORK WITH DDM OR IPR)
c       10/10/08   Revised to move species loop into VDIFF routines for
c                  better efficiency
c       07/16/07 -bkoo-     Added check for HDDM
c       07/16/08 -bkoo-     Added DDM turn-off flag
c       04/29/13   Deposit to surface model fields
c       08/25/14   Extended Zhang LU to surface model, added snow effects
c       05/31/16   Revised ACM2 to parallel speed improvements in CMAQ v5.1,
c                  extended to H/DDM
c
c     Input arguments:
c        losat             flag that determines if this for tracers
c        ldepfld           flag that determines if dep fields are saved
c        igrd              grid index
c        ncol              number of columns
c        nrow              number of rows
c        nlay              number of layers
c        nrads             number of radicals
c        nspc              number of species
c                          or 1, whichever is larger
c        ndpspc1           number of species in first depostion array
c        ndpspc2           number of species in second depostion array
c        ndpspc3           number of species in third depostion array
c        deltat            time step (s)
c        dx                cell size in x-direction (m)
c        dy                cell size in y-direction (m)
c        idfin             map of nested grids in this grid
c        vdep              deposition velocity (m/s)
c        rkx/y             horizontal diffusion coefficient (m2/s)
c        rkv               vertical diffusion coefficient (m2/s)
c        depth             layer depth (m)
c        tempk             temperature (K)
c        press             pressure (mb)
c        uwind             u-component wind (m/s)
c        vwind             v-component wind (m/s)
c        qvap              water vapor (ppm)
c        cwc               cloud water content (g/m3)
c        temps             surface temperature (K)
c        mapscl            map-scale factor at cell centroid
c        sfcz0             surface roughness (m)
c        snow              snow cover equivalent water (m)
c        snowfrc           snow cover fraction
c        conc              species concentrations (umol/m3)
c        fsurf             fractional landuse coverage
c        fluxtmp           temporary array for fluxes
c        strz              string for labeling the z diffusion process
c        strxy             string for labeling the x/y diffusion process
c        ipa_xy            2-D gridded array to identify if cell is
c                          in a IPRM sub-domain
c        ipa_lay           3-D gridded array to identify which IPRM sub-domain
c                          each layer is in
c        iproc_id          ID for this processor in MPI rank
c
c     Output arguments:
c        conc              species concentrations (umol/m3)
c        fluxes            fluxes across the boundaries (umol)
c        depfld1           2-D array of dry deposited mass (mol/ha, g/ha)
c        depfld2           2-D array of dry deposited mass (mol/ha, g/ha)
c        depfld3           2-D array of dry deposited mass (mol/ha, g/ha)
c
c     Routines Called:
c        VDIFFIMP
c        VDIFFACM2
c        CALDATE
c
c     Called by:
c        EMISTRNS
c
      implicit none
      include "camx.prm"
      include "flags.inc"
      include "deposit.inc"
      include "rtracsrf.inc"
c
c=============================DDM Begin================================
c
      integer   lk,isen,ioff
      integer*8 idx
      integer*8 isaptr
c
      real depmas
c
c=============================DDM End==================================
c
c======================== Process Analysis Begin ====================================
c
      integer ipa_idx
      integer ipa_xy(ncol,nrow)
      integer ipa_lay(ncol,nrow,nlay)
      logical ldoipts
c
      real fcup(MXLAYER,MXSPEC)
      real fcdn(MXLAYER,MXSPEC)
c
c========================= Process Analysis End =====================================
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon
c
      character*20 strz
      character*20 strxy
      logical      losat
      logical      ldepfld
      integer      igrd,ncol,nrow,nlay,nrads,nspc,ndpspc1,ndpspc2,
     &                                                    ndpspc3
      real         deltat
      real         dy
      real         fluxtmp(m1,m2,m3,nspc)
      real         conc(m1,m2,m3,nspc)
      real         vdep(m1,m2,nspc)
      real         depfld1(m1,m2,ndpspc1)
      real         depfld2(m1,m2,ndpspc2)
      real         depfld3(m1,m2,ndpspc3)
      real         fsurf(m1,m2,NLU)
      real, dimension(m1,m2,m3) :: rkx,rky,rkv,tempk,press
      real, dimension(m1,m2,m3) :: uwind,vwind,qvap,cwc
      real, dimension(m1,m2,m3) :: depth
      real, dimension(m1,m2)    :: temps,mapscl,sfcz0,snow,snowfrc
      real         dx(nrow)
      integer      idfin(m1,m2)
      real*8       fluxes(nspc,11)
      integer      iproc_id
c
      real    dtv,dtmp,rokp1,rhoavg,scl,dfxp,dfxm,dfyp,dfym,fxp,
     &        fxm,fyp,fym
      real    dzmid,press0,dpdz,z0,zz
      real    sfrac
      real    fconv
      integer n,i,j,k,l,ispc,ll,numspcs,lsm,nstepv,kpbl
c
      real    vdry(MXSPEC+MXTRSP)
      real    d1d(MXLAYER)
      real    rk1d(MXLAYER)
      real    rkro(MXLAYER)
      real    ro1d(MXLAYER)
      real    t1d(MXLAYER)
      real    p1d(MXLAYER)
      real    w1d(MXLAYER)
      real    qv1d(MXLAYER)
      real    cnc(MXCELLS,MXCELLS)
      real    rho(MXCELLS,MXCELLS)
      real    fluxbot(MXSPEC+MXTRSP)

      real, allocatable, dimension (:,:) :: c1d
c
c-----Entry point
c
c-----Vertical diffusion
c
      numspcs = nspc
      if( losat ) numspcs = nsaspc
      if( iproc_id .LE. 1 ) then
        write(*,'(a20,$)') strz
      endif
      write(iout,'(a20,$)') strz
c
c  ---- Initialize the temp flux array to zero ---
c
      if( .NOT. losat ) call zeros(fluxtmp,m1*m2*m3*nspc)
c
c$omp parallel default(shared)
c$omp&  private(c1d,l,lk,i,j,k,lsm,idx,
c$omp&          ll,ro1d,d1d,rk1d,rkro,dzmid,dpdz,press0,t1d,p1d,w1d,
c$omp&          qv1d,vdry,z0,ioff,fluxbot,isen,rokp1,kpbl,fconv,sfrac,
c$omp&          ldoipts,fcup,fcdn,ipa_idx,n,dtv,dtmp,nstepv,zz,depmas)
c
c$omp do schedule(guided)
c
      do 60 j = 2,m2-1
        do 50 i = 2,m1-1
c
c  --- allocate space for temporary array ---
c
           if( allocated(c1d) ) deallocate( c1d )     
           if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              allocate( c1d(nlay+nlay*nddmsp,numspcs) )
           else
              allocate( c1d(nlay,numspcs) )
           endif
c
c-----Skip cells occupied by child grids; load 1-D arrays
c
          if (idfin(i,j).gt.igrd) goto 50
c
c-----Determine max vertical diffusion time step
c
          dtv = deltat
          if (deltat.ge.300.) then
            do k = 1,nlay-1
              dtmp = 0.75*depth(i,j,k)*depth(i,j,k+1)/rkv(i,j,k)
              dtv = min(dtv,dtmp)
            enddo
            dtv = max(dtv,300.)
          endif
          nstepv = INT( 0.999*deltat/dtv ) + 1
          dtv = deltat/FLOAT( nstepv )
c
c-----Load 1-D variables
c
          z0 = sfcz0(i,j)
          dzmid = (depth(i,j,2) + depth(i,j,1))/2.
          dpdz  = (press(i,j,2) - press(i,j,1))/dzmid
          press0 = press(i,j,1) - dpdz*depth(i,j,1)/2.
          do k = 1,nlay
            ro1d(k) = press(i,j,k)/tempk(i,j,k)
            d1d(k) = depth(i,j,k)
            t1d(k) = tempk(i,j,k)
            p1d(k) = press(i,j,k)
            w1d(k) = sqrt(uwind(i,j,k)**2. + vwind(i,j,k)**2.)
            qv1d(k) = (qvap(i,j,k)/1.e6)*18./28.8
          enddo
          zz = 0.
          do k = 1,nlay-1
            zz = zz + depth(i,j,k)
            rk1d(k) = amin1(rkv(i,j,k),1000.,0.4*zz)
            rkro(k) = rkv(i,j,k)*(ro1d(k) + ro1d(k+1))/2.
          enddo
          rk1d(nlay) = 0.
          rkro(nlay) = 0.
          do l = nrads+1,numspcs
            vdry(l-nrads) = vdep(i,j,l)
          enddo

          do n = 1,nstepv
            do l = nrads+1,numspcs
              do k = 1,nlay
                c1d(k,l-nrads) = conc(i,j,k,l)
              enddo
            enddo

c
c=============================DDM Begin================================
c
c------Load sensitivities
c
            if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              do l = nrads+1,numspcs
                lk = nlay
                do isen = 1,nddmsp
                  ioff = iptddm(l)+isen-1
                  do k = 1,nlay
                    lk = lk + 1
                    idx =  DBLE(isaptr-1) + DBLE(i) + 
     &                 DBLE(m1)*DBLE(j-1) + DBLE(m1)*DBLE(m2)*DBLE(k-1) + 
     &                             DBLE(m1)*DBLE(m2)*DBLE(m3)*DBLE(ioff-1)
                    c1d(lk,l-nrads) = ptconc(idx)
                  enddo
                enddo
              enddo
            endif
c
c=============================DDM End==================================
c
c
c======================== Process Analysis Begin ====================================
c
            ldoipts = .FALSE.
            if( .NOT. ltrace .AND. lipr ) then
                if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja
     &                                         .AND. j .LE. jz ) then
                   if(ipa_xy(i+i0,j+j0) .GT. 0) ldoipts = .TRUE.
                endif
            endif
c
c========================= Process Analysis End =====================================
c
            if (lacm2) then
              fconv = 0.
              if ( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
                call vdiffacmx(nspc-nrads,nddmsp,nlay,dtv,temps(i,j),
     &                         press0,z0,d1d,rk1d,qv1d,t1d,p1d,w1d,kpbl,
     &                         fconv,c1d)
              else
                call vdiffacmx(nspc-nrads,0,nlay,dtv,temps(i,j),
     &                         press0,z0,d1d,rk1d,qv1d,t1d,p1d,w1d,kpbl,
     &                         fconv,c1d)
              endif
              if (n.eq.1) then
                do k = 1,kpbl-1
                  rkro(k) = rkro(k)*(1. - fconv)
                enddo
              endif
            endif
            if ( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              call vdiffk(nspc-nrads,nlay,dtv,vdry,d1d,ro1d,rkro,c1d,
     &                    nddmsp,fluxbot,fcup,fcdn,ldoipts)
            else
              call vdiffk(nspc-nrads,nlay,dtv,vdry,d1d,ro1d,rkro,c1d,
     &                    0,fluxbot,fcup,fcdn,ldoipts)
            endif
c
c======================== Process Analysis Begin ====================================
c
            if( ldoipts ) then
              do l = nrads+1,numspcs
                do k = 1,nlay
                  if( ipa_lay(i+i0,j+j0,k) .GT. 0) then
                     ipa_idx  = ipa_lay(i+i0,j+j0,k)
                     if( k .GT. 1 ) then
                        cipr(IPR_BDIF, ipa_idx, l) =
     &                     cipr(IPR_BDIF, ipa_idx, l) + fcdn(k,l-nrads)
                     else
                        cipr(IPR_DDEP, ipa_idx, l) =
     &                     cipr(IPR_DDEP, ipa_idx, l) + fcdn(k,l-nrads)
                     endif
                     cipr(IPR_TDIF, ipa_idx, l) =
     &                     cipr(IPR_TDIF, ipa_idx, l) + fcup(k,l-nrads)
                  endif
                enddo
              enddo
            endif
c
c========================= Process Analysis End =====================================
c
            do l = nrads+1,numspcs
              do k = 1,nlay
                conc(i,j,k,l) = c1d(k,l-nrads)
              enddo

              if (lsrfmod .and. .NOT.losat) then
                lsm = 0
                do ll = 1,nsmspc
                  if (l .EQ. idsmsp(ll)) lsm = ll
                enddo
              endif

              if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja
     &                                      .AND. j .LE. jz ) then
                 fluxbot(l-nrads) = fluxbot(l-nrads)*depth(i,j,1)
                 if( .NOT. losat ) then
                   fluxtmp(i,j,1,l) = fluxtmp(i,j,1,l) +
     &                   fluxbot(l-nrads)*dx(j+j0)*dy/mapscl(i,j)**2.
                   do ll = 1,navspc
                     if( ldepfld .and. l .eq. lavmap(ll) ) then
                       depfld1(i,j,ll) = depfld1(i,j,ll) - 
     &                                   1.e-2*fluxbot(l-nrads)
                       goto 100
                     endif
                   enddo
 100               continue
                   if( lsrfmod .AND. lsm.NE.0) then
                     do ll = 1,NLU
                       if (NLU .eq. NLUW89 .and. ll.ne.7) then
                         sfrac = fsoil(ll)*fsurf(i,j,ll)
                       elseif (NLU .eq. NLUZ03 .and. ll.ne.1 .and.
     &                         ll.ne.3) then
                         sfrac = fsoilz(ll)*fsurf(i,j,ll)
                       endif
                     enddo
                     if (snow(i,j).ge.0.01)       ! Snow > 10 cm
     &                 sfrac = max(sfrac,snowfrc(i,j))
                     depmas = -1.e-2*fluxbot(l-nrads)
                     depfld2(i,j,lsm) = depfld2(i,j,lsm) + sfrac*depmas
                     depfld3(i,j,lsm) = depfld3(i,j,lsm) +
     &                                              (1. - sfrac)*depmas
                   endif
                 endif
              endif
c
c=============================Source Apportion Begin================================
c
              if( ldepfld .and. losat .AND. lptdepout 
     &                                    .AND. l .LE. ndpspc1 ) then
                depfld1(i,j,l) = depfld1(i,j,l) - 1.e-2*fluxbot(l-nrads)
              endif
              if( losat .AND. lsrfmodrt ) then
                 depmas = -1.e-2*fluxbot(l)
                 do ll = 1,NLUW89
                   if (ll.ne.7) then
                     depfld1(i,j,l) = depfld1(i,j,l) +
     &                                    fsoil(ll)*fsurf(i,j,ll)*depmas
                     depfld2(i,j,l) = depfld2(i,j,l) +
     &                             (1. - fsoil(ll))*fsurf(i,j,ll)*depmas
                   endif
                 enddo
              endif
            enddo
c
c=============================Source Apportion End ================================
c
c
c=============================DDM Begin================================
c
            if ( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              do l = nrads+1,numspcs
                lk = nlay
                do isen = 1,nddmsp
                  ioff = iptddm(l)+isen-1
                  do k = 1,nlay
                    lk = lk + 1
                    idx =  DBLE(isaptr-1) + DBLE(i) + 
     &                   DBLE(m1)*DBLE(j-1) + DBLE(m1)*DBLE(m2)*DBLE(k-1) + 
     &                               DBLE(m1)*DBLE(m2)*DBLE(m3)*DBLE(ioff-1)
                    ptconc(idx) = c1d(lk,l-nrads)
                  enddo
                enddo
              enddo
            endif
c
c=============================DDM End==================================
c
          enddo
  50    continue
  60  continue
c
c$omp end parallel
c
c-----Put fluxes in global array
c
      if( .NOT. losat ) then
         do j = 2,m2-1
           do i = 2,m1-1
             do l = 1,nspc
               fluxes(l,11) = fluxes(l,11) + DBLE(fluxtmp(i,j,1,l))
             enddo
           enddo
         enddo
      endif
c
c$omp master
c
      if( iproc_id .LE. 1 ) then
        write(*,'(a)') '   Done'
      endif
      write(iout,'(a)') '   Done'
      call flush(6)
      call flush(iout)
c
c-----Perform explicit horizontal diffusion
c
      if( iproc_id .LE. 1 ) then
        write(*,'(a20,$)') strxy
      endif
      write(iout,'(a20,$)') strxy
c
c$omp end master
c
c$omp parallel default(shared)
c$omp&  private(ispc,i,j,k,ioff,rho,cnc,rhoavg,scl,
c$omp&          dfxp,dfxm,dfyp,dfym,fxp,fxm,fyp,fym,isen,idx,ipa_idx)
c
c$omp do schedule(guided)
c
      do 91 ispc = nrads+1,numspcs
        do 90 k = 1,nlay
          do 80 j = 1,m2   !nrow
            do 70 i = 1,m1    !ncol
              rho(i,j) = press(i,j,k)/tempk(i,j,k)
              cnc(i,j) = conc(i,j,k,ispc)/rho(i,j)
  70        continue
  80      continue
c
c=============================DDM Begin================================
c
         if ( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
           do isen = 1,nddmsp
             ioff = iptddm(ispc)+isen-1
             do j=1,m2   !nrow
               do i = 1,m1   !ncol
                 idx =  DBLE(isaptr-1) + DBLE(i) + 
     &                DBLE(m1)*DBLE(j-1) + DBLE(m1)*DBLE(m2)*DBLE(k-1) + 
     &                            DBLE(m1)*DBLE(m2)*DBLE(m3)*DBLE(ioff-1)
                 sns(i,j,isen,ispc) = ptconc(idx)/rho(i,j)
                enddo
              enddo
           enddo
         endif
c
c=============================DDM End ================================
c
          do 85 j = 2,m2-1   !2,nrow-1
            do 75 i = 2,m1-1   !2,ncol-1
              if (idfin(i,j).gt.igrd) goto 75
c
              rhoavg = (rho(i,j) + rho(i+1,j))/2.
              scl = (mapscl(i,j) + mapscl(i+1,j))/2.
              dfxp = scl*rhoavg*rkx(i,j,k)*deltat/dx(j+j0)/dx(j+j0)
              rhoavg = (rho(i,j) + rho(i-1,j))/2.
              scl = (mapscl(i,j) + mapscl(i-1,j))/2.
              dfxm = scl*rhoavg*rkx(i-1,j,k)*deltat/dx(j+j0)/dx(j+j0)
c
              rhoavg = (rho(i,j) + rho(i,j+1))/2.
              scl = (mapscl(i,j) + mapscl(i,j+1))/2.
              dfyp = scl*rhoavg*rky(i,j,k)*deltat/dy/dy
              rhoavg = (rho(i,j) + rho(i,j-1))/2.
              scl = (mapscl(i,j) + mapscl(i,j-1))/2.
              dfym = scl*rhoavg*rky(i,j-1,k)*deltat/dy/dy
c
              fxp  = (cnc(i+1,j) - cnc(i,j))*dfxp
              fxm  = (cnc(i,j) - cnc(i-1,j))*dfxm
              fyp  = (cnc(i,j+1) - cnc(i,j))*dfyp
              fym  = (cnc(i,j) - cnc(i,j-1))*dfym
              conc(i,j,k,ispc) = conc(i,j,k,ispc) + mapscl(i,j)*
     &                           ((fxp - fxm) + (fyp - fym))
c
c======================== Process Analysis Begin ====================================
c
              if( .NOT. ltrace .AND. lipr ) then
                if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja
     &                                         .AND. j .LE. jz ) then
                    if ( ipa_lay(i+i0,j+j0,k) .GT. 0 ) then
                     ipa_idx = ipa_lay(i+i0,j+j0,k)
                     cipr(IPR_WDIF, ipa_idx, ispc) =
     &                       cipr( IPR_WDIF, ipa_idx, ispc) -
     &                                       mapscl(i,j)*fxm

                     cipr(IPR_EDIF, ipa_idx, ispc) =
     &                       cipr(IPR_EDIF, ipa_idx, ispc) +
     &                                       mapscl(i,j)*fxp

                     cipr(IPR_SDIF, ipa_idx, ispc) =
     &                       cipr(IPR_SDIF, ipa_idx, ispc) -
     &                                       mapscl(i,j)*fym

                     cipr(IPR_NDIF, ipa_idx, ispc) =
     &                       cipr(IPR_NDIF, ipa_idx, ispc) +
     &                                       mapscl(i,j)*fyp
                   endif
                 endif
              endif
c
c========================= Process Analysis End =====================================
c
c
c=============================DDM Begin================================
c
              if ( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
               do isen =1,nddmsp
                 fxp  = (sns(i+1,j,isen,ispc) - sns(i,j,isen,ispc))*dfxp
                 fxm  = (sns(i,j,isen,ispc) - sns(i-1,j,isen,ispc))*dfxm
                 fyp  = (sns(i,j+1,isen,ispc) - sns(i,j,isen,ispc))*dfyp
                 fym  = (sns(i,j,isen,ispc) - sns(i,j-1,isen,ispc))*dfym
                 ioff = iptddm(ispc)+isen-1
                 idx =  DBLE(isaptr-1) + DBLE(i) + 
     &               DBLE(m1)*DBLE(j-1) + DBLE(m1)*DBLE(m2)*DBLE(k-1) + 
     &                            DBLE(m1)*DBLE(m2)*DBLE(m3)*DBLE(ioff-1)
                 ptconc(idx) = ptconc(idx) + mapscl(i,j)*
     &                                   ((fxp - fxm) + (fyp - fym))
               enddo
             endif
c
c=============================DDM End==================================
c
  75        continue
  85      continue
          call flush(6)
          call flush(iout)
  90    continue
  91  continue
      if( allocated( c1d ) ) deallocate( c1d )
c
c$omp end parallel
c$omp master
c
  92  continue
      if( iproc_id .LE. 1 ) then
        write(*,'(a)') '   Done'
      endif
      write(iout,'(a)') '   Done'
c
c$omp end master
c
      call flush(6)
      call flush(iout)
c
      return
      end
