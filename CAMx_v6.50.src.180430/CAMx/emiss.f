      subroutine emiss(m1,m2,m3,i0,j0,ia,iz,ja,jz,
     &                 nspc,nlu,ndpspc,densfac,lamap,lpmap,lprobe,nsrc,
     &                 idsrc,isrc,jsrc,ncol,nrow,nlay,deltat,dx,dy,
     &                 mapscl,height,depth,windu,windv,tempk,press,
     &                 icdocn,fsurf,tsurf,
     &                 aremis,pttrace,numpts,armass,ptmass,reemis,conc,
     &                 depfld,ipa_cel)
      use bndary
      use chmstry
      use ptemiss
      use procan
      use tracer
c
c----CAMx v6.50 180430
c
c     EMISS updates species concentrations due to emissions
c
c     Copyright 1996 - 2018
c     Ramboll
c          
c     Modifications: 
c        10/29/01   Map scale factor added to emissions increment
c        04/21/04   Added optional plume rise override
c        04/08/10   Updates to plume rise algorithm (multi-layer injection)
c        01/15/13   Allow user-set multi-layer injection using EFFPH and FLOWRAT
c        04/29/13   Inject surface model re-emissions
c        12/18/15   Add oceanic I2/HOI emissions from O3 dep
c
c     Input arguments:
c        nspc                number of species
c        nlu                 number of land use categories
c        ndpspc              number of deposition species inc. I2,HOI emissions
c        densfac             gas concentration conversion factor
c        lamap               area source species map
c        lpmap               point source species map
c        lprobe              flag for a Probing Tool call
c        nsrc                number of point sources
c        idsrc               point source ID map
c        isrc                grid column index for point sources
c        jsrc                grid row index for point sources
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        deltat              time step size (s)
c        dx                  cell size in x-direction (m)
c        dy                  cell size in y-direction (m)
c        mapscl              map scale factor
c        height              layer height (m)
c        depth               layer depth (m)
c        windu               wind speed in x-direction (m/s)
c        windv               wind speed in y-direction (m/s)
c        tempk               air temprature (K)
c        press               air pressure (mb)
c        icdocn              ocean map
c        fsurf               landuse fraction
c        tsurf               surface temperature (K)
c        aremis              area source strength (mol/s)
c        pttrace             point source strength (mol/s)
c        numpts              number of point sources
c        armass              mass from area source emission (umol)
c        ptmass              mass from point source emission (umol)
c        reemis              emissions from surface model (mol/s)
c        conc                species concentrations (umol/m3)
c        depfld              deposited mass carrying I2,HOI emissions (mol)
c        ipa_cel             gridded array to identify if cell is
c                            in a IPRM sub-domain
c
c     Output arguments:
c        conc                species concentrations (umol/m3)
c        armass              mass from area source emission (umol)
c        ptmass              mass from point source emission (umol)
c
c     Routines called:
c        PLUMERIS
c        IXEMIS
c
c     Called by:
c        EMISTRNS
c
      implicit none
      include  "camx.prm"
      include  "flags.inc"
c
c-----Input arguments
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz
      integer :: nspc,nsrc,ncol,nrow,nlay,numpts
      integer :: nlu
      integer :: ndpspc
      real    :: deltat,dy
      real    :: densfac
      logical :: lprobe
      integer icdocn(m1,m2)
      real*8  armass(*)
      real*8  ptmass(*)
      integer lamap(*)
      integer lpmap(*)
      integer idsrc(*)
      integer isrc(*)
      integer jsrc(*)
      real    pttrace(numpts,nspc)
      real    conc(m1,m2,m3,nspc)
      real    aremis(m1,m2,nspc)
      real    reemis(m1,m2,nsmspc)
      real    dx(nrow)
      real    mapscl(m1,m2)
      real, dimension(m1,m2,m3) :: windu,windv,tempk,press
      real, dimension(m1,m2,m3) :: height, depth
      real, dimension(m1,m2)    :: tsurf
      real, dimension(m1,m2,nlu) :: fsurf
      real, dimension(m1,m2,ndpspc) :: depfld
c
c======================== Process Analysis Begin ====================================
c
      integer ipa_cel(ncol,nrow,nlay)
      integer ipa_idx
c
c========================= Process Analysis End =====================================
c
c-----Local variables
c
      integer l,i,j,k,kstk,kt,kb,lsrc,n
      integer ll,lsm
      real hght1d(0:MXLAYER)
      real wind1d(MXLAYER)
      real tempk1d(MXLAYER)
      real dtdz1d(MXLAYER)
      real pfrac(MXLAYER)
      real gamma,p0,grav
      real i2flx,hoiflx
      real vol,dconc,w2,dz,dtheta,zstk,zbot,ztop,dstkabs,wp,tp,zrise,
     &     trise,pwidth,rip,fp,qp2,cq1,cq2,rkp,bot,top,fwtr,co3,convfac
      real*8  dmass
c
      data gamma,p0,grav /0.286,1000.,9.8/
      data cq1,cq2 /0.4,3.0/
c
c-----Entry point
c
c-----Update concentration due to area source
c
      if (larsrc) then
c
c$omp parallel default(shared)
c$omp&  private(l,i,j,ll,lsm,vol,dmass,dconc,ipa_idx,
c$omp&          i2flx,hoiflx,fwtr,co3,convfac,w2)
c
c$omp do schedule(static)
c
        do 10 l = 1,nspc
          if (lsrfmod) then
            lsm = 0
            do ll = 1,nsmspc
              if (l .EQ. idsmsp(ll)) lsm = ll
            enddo
          endif
          do 20 i = 2, m1-1
            do j = 2, m2-1
              vol = dx(j+j0)*dy*depth(i,j,1)/(mapscl(i,j)**2.)
              dmass = 0.
c
c------Standard gridded emissions
c
              if( lamap(l) .GT. 0 ) dmass = DBLE(aremis(i,j,l)*deltat*1.e6)
c
c------Re-emissions from surface model
c
              if (lsrfmod .and. lsm .NE. 0)
     &                    dmass = dmass + DBLE(reemis(i,j,lsm)*deltat*1.d6)
c
c------Oceanic halogen emissions related to ozone deposition
c
              if (lixemis .AND. icdocn(i,j).GT.0 .AND. .NOT.lprobe .AND.
     &            (l.EQ.ki2 .OR. l.EQ.khoi)) then
                w2 = sqrt(windu(i,j,1)**2. + windv(i,j,1)**2.)
                w2 = amax1(w2,0.1)
                if (nlu.EQ.NLUW89) then
                  fwtr = fsurf(i,j,7)
                else
                  fwtr = fsurf(i,j,1)
                endif
                convfac = densfac*(273./tempk(i,j,1))*
     &                            (press(i,j,1)/1013.)
                co3 = 1000.*conc(i,j,1,ko3)/convfac
                call ixemis(dx(j+j0),dy,mapscl(i,j),icdocn(i,j),fwtr,
     &                      co3,tsurf(i,j),w2,i2flx,hoiflx)
                if (l.eq.ki2)
     &            dmass = dmass + DBLE(i2flx*deltat*1.d6)
                  depfld(i,j,ndpspc-1) = depfld(i,j,ndpspc-1) + 
     &                                   i2flx*deltat/
     &                                   (dx(j+j0)*dy)*(mapscl(i,j)**2)
                if (l.eq.khoi)
     &            dmass = dmass + DBLE(hoiflx*deltat*1.d6)
                  depfld(i,j,ndpspc) = depfld(i,j,ndpspc) + 
     &                                 hoiflx*deltat/
     &                                 (dx(j+j0)*dy)*(mapscl(i,j)**2)
              endif
 
              dconc = REAL(dmass)/vol
              if( i .GE. ia .and. i .LE. iz .AND. 
     &                    j .GE. ja .AND. j .LE. jz ) armass(l) = 
     &                                                armass(l) + dmass
              conc(i,j,1,l) = conc(i,j,1,l) + dconc
c
c======================== Process Analysis Begin ====================================
c
c----- if surface layer in this column is in a sub-domain
c      then track the area emissions in this grid cell ---
c
              if( (.NOT. ltrace) .AND. lipr ) then 
                   if( i .GE. ia .and. i .LE. iz .AND. 
     &                           j .GE. ja .AND. j .LE. jz ) then
                      if( ipa_cel(i+i0,j+j0,1) .GT. 0 ) then
                        ipa_idx = ipa_cel(i+i0,j+j0,1)
                        cipr(IPR_AEMIS, ipa_idx, l) =
     &                               cipr(IPR_AEMIS, ipa_idx, l) + dconc
                      endif
                  endif
              endif
c
c========================= Process Analysis End =====================================
c
            enddo
 20       continue
 10     continue
c
c  --- end of parallelized loop ---
c
c$omp end parallel
c
      endif
c
c-----Update concentration due to point sources
c
      if (lptsrc) then
        do 50 lsrc = 1,nsrc
          n = idsrc(lsrc)
          i = isrc(lsrc)-i0
          j = jsrc(lsrc)-j0
          if (i < 2 .or. i >= m1 .or. j < 2 .or. j >= m2) goto 50
c
          hght1d(0) = 0.
          do k = 1,nlay
            hght1d(k) = height(i,j,k)
            tempk1d(k) = tempk(i,j,k)
            w2 = windu(i,j,k)*windu(i,j,k) + windv(i,j,k)*windv(i,j,k)
            wind1d(k) = amax1(sqrt(w2),0.1)
            if (k.lt.nlay) then
              dz = height(i,j,k+1)/2. 
              if (k.gt.1) dz = (height(i,j,k+1) - 
     &                          height(i,j,k-1))/2. 
              dtheta = (tempk(i,j,k+1)*(p0/press(i,j,k+1))**gamma - 
     &                  tempk(i,j,k)*(p0/press(i,j,k))**gamma) 
              dtdz1d(k) = dtheta/dz 
            else
              dtdz1d(k) = dtdz1d(k-1)
            endif 
          enddo
c
c-----Apply plume rise OVERRIDE if effph is negative
c
          if (effph(n) .lt. 0. .and. flowrat(n) .ge. 0.) then
            zstk = abs(effph(n))
            do kstk = 1,nlay
              if (hght1d(kstk).gt.zstk) goto 14
            enddo
            kstk = nlay
            zstk = hght1d(kstk-1) + 1.
  14        zbot = max(0.,zstk - 1.)
            ztop = min(hght1d(nlay),zstk + 1.)
c
c-----Handle unique situation for EPS3 point source files
c
          else if(effph(n) .LT. 0. .AND. flowrat(n) .LT. 0. .AND.
     &            effph(n) .EQ. flowrat(n) ) then
            zstk = abs(effph(n))
            do kstk = 1,nlay
              if (hght1d(kstk).gt.zstk) goto 13
            enddo
            kstk = nlay
            zstk = hght1d(kstk-1) + 1.
  13        zbot = max(0.,zstk - 1.)
            ztop = min(hght1d(nlay),zstk + 1.)
c
c-----Apply plume layer distribution if stack height is also negative ---
c
          else if(effph(n) .LT. 0. .AND. flowrat(n) .LT. 0. .AND.
     &            effph(n) .NE. flowrat(n) ) then
            zstk = 0.5*( ABS( effph(n) ) + ABS( flowrat(n) ) )
            do kstk = 1,nlay
              if (hght1d(kstk) .GT. zstk) goto 18
            enddo
            kstk = nlay
  18        zbot = MAX(0., MIN( ABS( flowrat(n) ), ABS( effph(n) ) ) )
            ztop = MIN( hght1d(nlay),
     &                     MAX( ABS( effph(n) ), ABS( flowrat(n) ) ) )
          else
c
c-----Calculate plume rise
c
            dstkabs = abs(dstk(n))
            call plumeris(nlay,hght1d,tempk1d,dtdz1d,wind1d,hstk(n),
     &                    dstkabs,tstk(n),vstk(n),zstk)
            do kstk = 1,nlay
              if (hght1d(kstk).gt.zstk) goto 15
            enddo
            kstk = nlay
            zstk = hght1d(kstk-1) + 1.
c
c-----Determine plume depth
c
  15        wp = amax1(1.,vstk(n)/2.)
            tp = (tempk1d(kstk) + tstk(n))/2.
            zrise = zstk - hstk(n)
            trise = zrise/wp
            pwidth = sqrt(2.)*dstkabs
            rip = grav*pwidth*abs(tp - tempk1d(kstk))/
     &            (tempk1d(kstk)*wp*wp)
            fp = 1. + 4.*rip
            qp2 = fp*wp*wp*(cq1 + cq2*wind1d(kstk)*wind1d(kstk)/
     &            (wind1d(kstk)*wind1d(kstk) + wp*wp))
            rkp = 0.15*pwidth*sqrt(qp2)
            pwidth = 3.*sqrt(pwidth*pwidth + 2.*rkp*trise)
            pwidth = max(1.,min(pwidth,zrise))
            zbot = max(hght1d(0),zstk - pwidth/2.)
            ztop = min(hght1d(nlay),zstk + pwidth/2.)
          endif
c
c-----Calculate layers to receive emission injection
c
          pwidth = ztop - zbot
          do k = 1,nlay
            pfrac(k) = 0.
          enddo
          do kt = kstk,nlay
            if (hght1d(kt).ge.ztop) goto 16
          enddo
          kt = nlay 
  16      do kb = 1,kstk
            if (hght1d(kb).gt.zbot) goto 17
          enddo
  17      do k = kb,kt
            bot = max(hght1d(k-1),zbot)
            top = min(hght1d(k),ztop)
            pfrac(k) = (top - bot)/pwidth
          enddo

          do 40 l = 1,nspc
            if( lpmap(l) .LE. 0 ) goto 40
            if (lpiglet(n) .AND. ipigflg .NE. 0) goto 40
            do k = kb,kt
              vol = dx(j+j0)*dy*depth(i,j,k)/(mapscl(i,j)**2.)
              dmass = DBLE(pttrace(n,l)*deltat*1e6*pfrac(k))
              dconc = REAL(dmass)/vol
              if( i .GE. ia .and. i .LE. iz .AND. 
     &            j .GE. ja .AND. j .LE. jz ) ptmass(l) =  
     &                                               ptmass(l) + dmass
              conc(i,j,k,l) = conc(i,j,k,l) + dconc
c
c======================== Process Analysis Begin ====================================
c
c   --- if layer containing plume for this cell is in a sub-domain
c       then track the point source emissions for this cell ----
c
              if( (.NOT. ltrace) .AND. lipr ) then 
                 if( i .GE. ia .and. i .LE. iz .AND. 
     &                           j .GE. ja .AND. j .LE. jz ) then
                    if( ipa_cel(i+i0,j+j0,k) .GT. 0 ) then
                        ipa_idx = ipa_cel(i+i0,j+j0,k)
                        cipr(IPR_PTEMIS, ipa_idx, l) =
     &                              cipr(IPR_PTEMIS, ipa_idx, l) + dconc
                     endif
                  endif
              endif
c
c========================= Process Analysis End =====================================
c
            enddo
 40       continue
 50     continue
      endif
c
      return
      end
