      subroutine piginit(numprocs,dt,npts,nrads,nspc,nspcems,pttrace,height,windu,
     &                                         windv,tempk,press)
      use filunit
      use grid
      use chmstry
      use pigsty
      use ptemiss
      use rtracchm
      use tracer
      use node_mod
      implicit none
c
c----CAMx v7.10 210105
c
c     PIGINIT initializes PiG puffs from flagged sources
c
c     Copyright 1996 - 2021
c     Ramboll
c          
c     Modifications: 
c        07/05/02    Added code for IRON-PiG
c        06/24/03    Removed thetapig
c        09/04/03    Removed adiabatic factor; improved plume rise
c                    entrainment; revamped puff initiation for "chained" puffs
c        11/12/03    Added RTRAC species injection
c        08/25/05    Added new PiG-OSAT pointers to allow different input
c                    point source lists across model restarts
c        02/02/06    Removed GREASD-PiG specific conditional code
c        12/20/06    Added plume height overrided
c        06/04/09    Added sigmax and multiple puff releases according to
c                    PIGMXDT
c        04/08/10    Minor mods for updated plume rise algorithm
c        11/06/12    Added check to make sure PiGged source does not
c                    trigger plume distribution override
c        04/16/13    Added update to Lslice when puff array space
c                    is recycled.
c
c     Input arguments:
c        numprocs            number of MPI slices
c        dt                  time step size (s)
c        npts                number of point sources in the array
c        nrads               number of radical species
c        nspc                number of model species
c        nspcems             number of emissions species
c        pttrace             point source emissions (mol/s)
c        height              layer height (m)
c        windu               wind speed in x-direction (m/s)
c        windv               wind speed in y-direction (m/s)
c        tempk               air temprature (deg.K)
c        press               air pressure (mb)
c
c     Output arguments:
c        none
c
c     Subroutines Called:
c        PLUMERIS
c
c     Called By:
c        EMISTRNS
c
      include  "camx.prm"
      include  "flags.inc"
c
      integer numprocs
      real    dt
      integer npts
      integer nrads
      integer nspc
      integer nspcems
      real    pttrace(npts,nspcems)
      real    height(*)
      real    windu(*)
      real    windv(*)
      real    tempk(*)
      real    press(*)
c
      real hght1d(0:MXLAYER),wind1d(MXLAYER),tempk1d(MXLAYER),
     &     dtdz1d(MXLAYER),press1d(MXLAYER)
      real frctr(MXRECTR)
      real gamma,p0,t1,t2,frsum,xtmp,ytmp,w2,dz,
     &     dtheta,dstktmp,zstk,dtpuff,dtime,grav,wp,tp,zrise,
     &     trise,sigma,pwidth,rip,fp,qp2,cq1,cq2,rkp,xlmax,
     &     hstktmp, tstktmp, vstktmp
      integer nr,l,m1,lsrc,n,i,j,k,igrd0,ip,ic,igrd,ig,
     &        ii,jj,kk,n3d,n3d_full,npuff,ipuff,m,m0,is,iempty
      integer iprc, igr, iems
      real xerf
c
      data gamma /0.286/
      data p0 /1000./
      data grav /9.8/
      data cq1,cq2 /0.4,3.0/
c
c-----Entry point
c
c-----Divide initial mass among PiG reactors: (1) partition according to 
c     ERF function; (2) correct for truncation of xerf to conserve mass
c
      frsum = 0.
      do nr = 1,nreactr
        t1 = nr*sqrt(2.)/nreactr
        t2 = (nr-1)*sqrt(2.)/nreactr
        frctr(nr) = xerf(t1) - xerf(t2)
        frsum = frsum + frctr(nr)
      enddo
      do nr = 1,nreactr
        frctr(nr) = frctr(nr)/frsum
      enddo
c
c-----Loop over all elevated point sources in domain (master grid)
c
      m1 = 1
      do 50 lsrc = 1,nosrc(1)
        n = idsrc(lsrc,1)
        if (.not.lpiglet(n)) goto 50
        iempty = 0
        do l = 1,nspc
          if( lemmap(l) .GT. 0 ) then
             if( pttrace(n,lemmap(l)) .GT. 0. ) iempty = 1
          endif
        enddo
c
c======================== Source Apportion Begin =======================
c
        if( ltrace .AND. (tectyp .EQ. RTRAC .OR.
     &                                      tectyp .EQ. RTCMC) ) then
           do is = 1,nrtrac
             if (sapnts(n,is).gt.0.) iempty = 1
           enddo
        endif
c
c========================= Source Apportion End ========================
c
        if (iempty.eq.0) goto 50
c
c-----Found a PiG point source with non-zero emissions; load local 1-D
c     vectors from finest grid
c
        i = isrc(lsrc,1)
        j = jsrc(lsrc,1)
        igrd0 = 1
        do ip = 1,ngrid
          do ic = 1,nchdrn(ip)
            igrd = idchdrn(ic,ip)
            ig = mapgrd(igrd)
            if (i.ge.inst1(ig) .and. i.le.inst2(ig) .and.             
     &          j.ge.jnst1(ig) .and. j.le.jnst2(ig)) 
     &      igrd0 = igrd
          enddo
        enddo

        if (igrd0.eq.1) then
          ii = i
          jj = j
        else
          xtmp = xstk(n,1) - (inst1(igrd0) - 1)*delx
          ytmp = ystk(n,1) - (jnst1(igrd0) - 1)*dely
          ii = 2 + FLOOR(xtmp/delx*meshold(igrd0))
          jj = 2 + FLOOR(ytmp/dely*meshold(igrd0))
        endif
c
c--- adjust cell index and skip if PiG not in this slice ---
c
        ii = ii-mi0(igrd0)
        jj = jj-mj0(igrd0)
        if( ii .LT. 0 .OR. ii .GT. mmxp(igrd0) ) goto 50
        if( jj .LT. 0 .OR. jj .GT. mmyp(igrd0) ) goto 50
c
        hght1d(0) = 0.
        do k = 1,nlay(igrd0)
          n3d = ii + mmxp(igrd0)*(jj - 1) +
     &                         mmxp(igrd0)*mmyp(igrd0)*(k - 1)
          n3d_full = ii + ncol(igrd0)*(jj - 1) +
     &                         ncol(igrd0)*nrow(igrd0)*(k - 1)
          hght1d(k) = height(iptr3d(igrd0)-1+n3d_full)
          tempk1d(k) = tempk(iptr3d(igrd0)-1+n3d)
          press1d(k) = press(iptr3d(igrd0)-1+n3d)
          w2 = windu(iptr3d(igrd0)-1+n3d)*windu(iptr3d(igrd0)-1+n3d) +
     &         windv(iptr3d(igrd0)-1+n3d)*windv(iptr3d(igrd0)-1+n3d)
          wind1d(k) = amax1(sqrt(w2),0.1) 
        enddo
        do k = 1,nlay(igrd0)
          if (k.lt.nlay(igrd0)) then 
            dz = (hght1d(k+1) - hght1d(k-1))/2.  
            dtheta = (tempk1d(k+1)*(p0/press1d(k+1))**gamma -  
     &                tempk1d(k)*(p0/press1d(k))**gamma)  
            dtdz1d(k) = dtheta/dz  
          else
            dtdz1d(k) = dtdz1d(k-1)  
          endif  
        enddo
c
c-----Disable the plume distribution override for PiG
c
        if( flowrat(n) .LT. 0. ) then
            write(iout,'(//,a)') 'ERROR in PIGINIT:'
            write(iout,*) 'Found negative flow rate for a PiGged source.'
            write(iout,*) 'The plume distribution override is not ',
     &                    'allowed for PiGged sources.'
            write(iout,*) 'Either reset the flow rate ',
     &                                     'on all PiGged sources'
            write(iout,*) 'Or turn off PiG treatment.'
            call camxerr()
        endif
c
c-----Calculate plume rise
c
        dstktmp = abs(dstk(n))
        hstktmp = hstk(n)
        tstktmp = tstk(n)
        vstktmp = vstk(n)
        if (effph(n) .lt. 0.) then
          zstk = abs(effph(n))
        else
          call plumeris(nlay(igrd0),hght1d,tempk1d,dtdz1d,wind1d,
     &                  hstktmp,dstktmp,tstktmp,vstktmp,zstk) 
        endif
        do kk = 1,nlay(igrd0)
          if (hght1d(kk).gt.zstk) goto 15
        enddo
        kk = nlay(igrd0)
        if (zstk.gt.hght1d(kk)) zstk = hght1d(kk)
  15    continue
c
c-----Divide the emissions into several puffs if the length exceeds half
c     the finest grid cell size; divide into several puffs if the current
c     timestep exceeds a specific max timestep
c
        xlmax = 1.e6
        do ip = 1,ngrid
          xlmax = amin1(xlmax,deltay(ip)/2.)
        enddo
        npuff = int(wind1d(kk)*dt/xlmax - 0.1) + 1
        dtpuff = amin1(dt,pigmxdt)
        npuff = max(npuff,int(dt/dtpuff-0.01)+1)
        dtpuff = dt/float(npuff)
        dtime = dt
        do 40 ipuff = 1,npuff
          dtime = dtime - dtpuff
c
c-----Calculate initial cross-puff sigma accounting for turbulent 
c     entrainment during release
c
          wp = amax1(1.,vstktmp/2.)
          tp = (tempk1d(kk) + tstktmp)/2.
          zrise = zstk - hstktmp
          trise = zrise/wp
          pwidth = sqrt(2.)*dstktmp
          sigma =  pwidth
          rip = grav*pwidth*abs(tp - tempk1d(kk))/(tempk1d(kk)*wp*wp)
          fp = 1. + 4.*rip
          qp2 = fp*wp*wp*(cq1 + cq2*wind1d(kk)*wind1d(kk)/
     &          (wind1d(kk)*wind1d(kk) + wp*wp))
          rkp = 0.15*pwidth*sqrt(qp2)
          sigma = sqrt(sigma*sigma + 2.*rkp*trise)
c
c-----Looking for a spot for the pig; add to the first empty location
c
          do m = m1,npig
            if (ingrd(m).eq.0) then
              m0 = m
              m1 = m0 + 1
              goto 20
            endif
          enddo
          npig = npig + 1
          m0 = npig
          m1 = npig + 1
  20      continue
          if (m0.gt.MXPIG) then
            write(iout,'(//,a)') 'ERROR in PIGINIT:'
            write(iout,*) 'PiG number exceeds maximum of ',MXPIG
            write(iout,*) 'Increase MXPIG and recompile'
            call camxerr()
          endif
c
c-----Load puff variables
c
          lnewt(m0) = .true.
          lnewg(m0) = .true.
          idpig(m0) = n
          ingrd(m0) = igrd0
          xpigf(m0) = xstk(n,1)
          xpigb(m0) = xstk(n,1)
          ypigf(m0) = ystk(n,1)
          ypigb(m0) = ystk(n,1)
          zpig(m0)  = zstk
          sigz(m0)  = amin1(sigma,amax1(zrise,1.))
          sigx(m0)  = sigma
          sigy(m0)  = sigma
          agepigf(m0) = dtime + dtpuff
          agepigb(m0) = dtime
          htfms(m0) = 0.
          htfmb(m0) = 0.
          vtfms(m0) = 0.
          vtfmb(m0) = 0.
          if( lmpi ) then
              do iprc=1,numprocs
                 do igr=1,ngrid
                    Lslice(igr,iprc,m0) = 0
                  enddo
              enddo
          endif
c
c======================== Source Apportion Begin =======================
c
          if( ltrace .AND. (tectyp .NE. RTRAC .OR.
     &                                      tectyp .NE. RTCMC) ) then
             ipufmap(m0) = ipigmap(n)
             ipufgrp(m0) = ipiggrp(n)
          endif
c
c========================= Source Apportion End ========================
c
c
c-----Calculate 3-D puff dimensions and fill with concentrations
c
          axisz(m0) = 3.*sigz(m0)
          axisy(m0) = 3.*sigy(m0)
          fmspig(m0) = 1.
          pufftop(m0) = zstk + 1.5*sigz(m0)
          puffbot(m0) = zstk - 1.5*sigz(m0)
          if (puffbot(m0).lt.0.) then
            axisz(m0) = axisz(m0) + puffbot(m0)
            puffbot(m0) = 0.
          endif
          if (pufftop(m0).gt.hght1d(nlay(igrd0))) then
            axisz(m0) = axisz(m0) - 
     &                  (pufftop(m0) - hght1d(nlay(igrd0)))
            pufftop(m0) = hght1d(nlay(igrd0))
          endif
c
c-----Initialize all PiG concentrations to zero, then fill emissions
c     and initialize radicals
c
          do nr = 1,nreactr
            do l = nrads+1,nspc
              puffmass(l,nr,m0) = 0.
              is = lemmap(l)
              if( is .GT. 0 )
     &           puffmass(l,nr,m0) = pttrace(n,is)*
     &                                      dtpuff*1.e6*frctr(nr)
            enddo
            do l = 1,nrads
              puffmass(l,nr,m0) = 1.e-9 ! puffmass(radical) in ppm
            enddo
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. (tectyp .EQ. RTRAC .OR.
     &                        tectyp .EQ. RTCMC) ) then
               do is = 1,nrtrac
                 puffrt(is,nr,m0) = sapnts(n,is)*
     &                              dtpuff*1.e6*frctr(nr)
               enddo
            endif
c
c========================= Source Apportion End ========================
c
          enddo
  40    continue
  50  continue
c
      return
      end
