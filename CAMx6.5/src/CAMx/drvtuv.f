      subroutine drvtuv(igrd,ncol,nrow,nlay,nspc,time,date,itzon,
     &                  idfin,cellat,cellon,height,press,tsurf,
     &                  tempk,water,conc,cod,alb,cldtrns)
      use o3colmap
      use chmstry
c
c----CAMx v6.50 180430
c
c     DRVTUV is the driver for the cloud/aerosol adjustment to
c     clear-sky photolysis rates.  A streamlined version of the TUV
c     radiative transfer model (based on v4.8) is called to calculate
c     actinic fluxes through the grid column for 2 cases: clear with
c     default haze, and cloudy with actual haze.  The ratio
c     of the two actinic flux profiles is used to apply to
c     the clear-sky lookup photolysis rates in KPHOTO.F.
c
c     Copyright 1996 - 2018
c     Ramboll
c
c     Modifications:
c        03/29/11    Support in-line TUV with aerosol optical depth
c        04/02/12    Removed RADM cloud adjustment option, cloud/aerosol
c                    adjustments now always done with in-line TUV
c        04/20/13    Moved call to EMISTRNS, removed averaging across
c                    input times
c        08/11/14    Albedo now includes effects of snow
c
c     Input arguments:
c        igrd                grid index
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        nspc                number of species
c        time                current time (HHMM)
c        date                current date (YYJJJ)
c        itzon               time zone
c        idfin               map of nested grids in this grid
c        cellat              cell centroid latitude (deg)
c        cellon              cell centroid longitude (deg)
c        height              current layer interface height field (m)
c        press               current layer pressure field (mb)
c        tsurf               current surface temperature field (K)
c        tempk               current layer temperature field (K)
c        water               current layer water vapor field (mb)
c        conc                concentration field (umol/m3, ug/m3)
c        cod                 layer cloud optical depth field (unitless)
c        alb                 surface UV albedo
c
c     Output arguments:
c        cldtrns             Cloud adjustment factor field (unitless) 
c
c     Routines called:
c        GETZNTH
c        TUV
c
c     Called by:
c        EMISTRNS
c
      implicit none
      include 'camx.prm'
      include 'camx_aero.inc'
      include 'flags.inc'
c
c-----Arguments
c
      integer igrd, ncol, nrow, nlay, nspc, date, itzon
      integer idfin(ncol,nrow)
      real time
      real cellat(ncol,nrow), cellon(ncol,nrow), alb(ncol,nrow)
      real height(ncol,nrow,nlay)
      real press(ncol,nrow,nlay)
      real tsurf(ncol,nrow)
      real tempk(ncol,nrow,nlay)
      real water(ncol,nrow,nlay)
      real conc(ncol,nrow,nlay,nspc)
      real cod(ncol,nrow,nlay)
      real cldtrns(ncol,nrow,nlay)
c
c-----Local variables
c
      integer NELT
      parameter (NELT = 51)
      integer i, j, k, l
      integer iz, izz, nz, nn
      integer rh1d(MXLAYER)
      real tuvalb
      real prslev,tmplev
      real zen1, zen2, zen, coszen
      real dtdz, tavg
      real wvapor,qwatr,ev,es,rhfac
      real totext,totssa,totcon,sumcod
      real factr,odint
      real temp1d(MXLAYER),pres1d(MXLAYER)
      real rafcld(MXLAYER+1)
      real z(MXLAYER+1),midht1d(MXLAYER)
      real airlev(MXLAYER+1)
      real odcld(MXLAYER+1), omcld(MXLAYER+1), gcld(MXLAYER+1)
      real odaer1(MXLAYER+1), omaer1(MXLAYER+1), gaer(MXLAYER+1)
      real odaer2(MXLAYER+1), omaer2(MXLAYER+1)
      real frh(100)
      real eltod(NELT)
      logical ldark

      real RGAS
      parameter(RGAS = 1.3806503d-19) !cm3 mbar K-1 molec-1
c
c-----Aerosol optical depth profile from Elterman (1968).
c     These are vertical optical depths per km, in 1 km
c     intervals from 0 km to 50 km, at 340 nm.
c     Total integrated OD is 0.38 (~0.23 at 550 nm).
c
      data eltod/
     &     2.40E-01,1.06E-01,4.56E-02,1.91E-02,1.01E-02,7.63E-03,
     &     5.38E-03,5.00E-03,5.15E-03,4.94E-03,4.82E-03,4.51E-03,
     &     4.74E-03,4.37E-03,4.28E-03,4.03E-03,3.83E-03,3.78E-03,
     &     3.88E-03,3.08E-03,2.26E-03,1.64E-03,1.23E-03,9.45E-04,
     &     7.49E-04,6.30E-04,5.50E-04,4.21E-04,3.22E-04,2.48E-04,
     &     1.90E-04,1.45E-04,1.11E-04,8.51E-05,6.52E-05,5.00E-05,
     &     3.83E-05,2.93E-05,2.25E-05,1.72E-05,1.32E-05,1.01E-05,
     &     7.72E-06,5.91E-06,4.53E-06,3.46E-06,2.66E-06,2.04E-06,
     &     1.56E-06,1.19E-06,9.14E-07/
c
c-----Aerosol humidity adjustment parameter as f(RH); FLAG (2000)
c
      data frh/1.,1.,1.,1.,1.,
     &         1.,1.,1.,1.,1.,
     &         1.,1.,1.,1.0001,1.0001,
     &         1.0004,1.0006,1.0024,1.0056,1.0089,
     &         1.0097,1.0105,1.0111,1.0115,1.0118,
     &         1.0122,1.0126,1.0130,1.0135,1.0139,
     &         1.0173,1.0206,1.0254,1.0315,1.0377,
     &         1.0486,1.0596,1.0751,1.0951,1.1151,
     &         1.1247,1.1343,1.1436,1.1525,1.1615,
     &         1.1724,1.1833,1.1955,1.2090,1.2224,
     &         1.2368,1.2512,1.2671,1.2844,1.3018,
     &         1.3234,1.3450,1.3695,1.3969,1.4243,
     &         1.4628,1.5014,1.5468,1.5992,1.6516,
     &         1.6991,1.7466,1.7985,1.8549,1.9113,
     &         1.9596,2.0080,2.0596,2.1146,2.1695,
     &         2.2630,2.3565,2.4692,2.6011,2.7330,
     &         2.8461,2.9592,3.0853,3.2245,3.3637,
     &         3.5743,3.7849,4.0466,4.3594,4.6721,
     &         5.3067,5.9412,6.9627,8.3710,9.7793,
     &         12.4288,15.0773,18.0590,21.3709,22. /
c
c-----Entry point
c
      nz = nlay + 1
c
c-----Loop over all vertical grid columns; skip columns containing nested grids
c
c$omp parallel default(shared)
c$omp&  private(i,j,k,rafcld,zen,ldark,coszen,
c$omp&          sumcod,tuvalb,z,temp1d,pres1d,wvapor,
c$omp&          qwatr,ev,es,rh1d,midht1d,airlev,prslev,
c$omp&          tmplev,dtdz,tavg,iz,odcld,omcld,gcld,
c$omp&          izz,factr,odint,odaer1,omaer1,gaer,
c$omp&          odaer2,omaer2,totext,totssa,totcon,l,rhfac)
c$omp do schedule(static) collapse(2)
      do j = 2,nrow-1
        do i = 2,ncol-1
          do k = 1,nz
            rafcld(k) = 1.
          enddo
          if (idfin(i,j).gt.igrd) goto 100
c
c-----Get solar zenith angle
c
          call getznth(cellat(i,j),cellon(i,j),time,date,itzon,zen,
     &                 ldark)
          if (ldark) goto 100
          zen = min(zen,75.)
          coszen = COS(zen*3.1415/180.)
c
c-----Check if this is a cloud-free column; adjustmnet ratio = 1
c
          sumcod = 0.
          do k = 1,nlay
            sumcod = sumcod + cod(i,j,k)
          enddo
          if (sumcod.eq.0. .AND. naero.eq.0) goto 100
c
c-----Set albedo
c
          tuvalb = alb(i,j)
c
c-----Prepare 1-D met variables for TUV
c
          z(1) = 0.
          do k = 1,nlay
            z(k+1) = height(i,j,k)*1.e-3
            temp1d(k) = tempk(i,j,k)
            pres1d(k) = press(i,j,k)
            wvapor    = water(i,j,k)
            qwatr     = 1.e-6*wvapor*18./28.8
            ev        = qwatr*pres1d(k)/(qwatr + eps)
            es        = e0*exp((lv/rv)*(1./273. - 1./temp1d(k)))
            rh1d(k)   = nint(100.*max(0.01,min(0.95,ev/es)))
          enddo
          do k = 1,nlay
            midht1d(k) = (z(k) + z(k+1))*0.5
          enddo
c
c-----Interpolate temperature and pressure to layer interfaces, then
c     calculate air density
c
          prslev = pres1d(1) - (midht1d(1)-z(1))*
     &             (pres1d(2)-pres1d(1))/(midht1d(2)-midht1d(1))
          airlev(1) = prslev/(Rgas*tsurf(i,j))

          do iz = 2,nz-1
            prslev = pres1d(iz-1) + (z(iz)-midht1d(iz-1))*
     &      (pres1d(iz)-pres1d(iz-1))/(midht1d(iz)-midht1d(iz-1))
            tmplev = temp1d(iz-1) + (z(iz)-midht1d(iz-1))*
     &      (temp1d(iz)-temp1d(iz-1))/(midht1d(iz)-midht1d(iz-1))
            airlev(iz) = prslev/(Rgas*tmplev)
          enddo

          dtdz = (temp1d(nz-1) - temp1d(nz-2))/
     &           (midht1d(nz-1) - midht1d(nz-2))
          tmplev = temp1d(nz-1) + dtdz*(z(nz) - midht1d(nz-1))
          tavg = (tmplev + temp1d(nz-1))/2.
          prslev = pres1d(nz-1)*
     &             exp(-9.8*(z(nz) - midht1d(nz-1))/(2.*287.*tavg))
          airlev(nz) = prslev/(Rgas*tmplev)
c
c-----Set cloud params
c
          do iz = 1,nz-1
            odcld(iz) = cod(i,j,iz)
            omcld(iz) = 0.9999
            gcld(iz) = 0.85
          enddo
c
c-----Set aerosol params: all are assumed to be applicable at 340 nm.
c     For the default case, use the Elterman profile.
c     If aerosols are run, use PM concentration fields and optical
c       params provided by the chemparam file.
c
          izz = 1
          do 30 k = 1,NELT-1
            do iz = izz,nz-1
              if (midht1d(iz) .lt. float(k)) then
                factr = midht1d(iz) - float(k-1)
                odint = factr*eltod(k+1) + (1. - factr)*eltod(k)
                odaer1(iz) = odint*(z(iz+1) - z(iz))
                omaer1(iz) = 0.90
                gaer(iz) = 0.61
              else
                izz = iz
                goto 30
              endif
            enddo
            goto 31
 30       continue
c
 31       do iz = 1,nz-1
            if (naero.eq.0) then
              odaer2(iz) = odaer1(iz)
              omaer2(iz) = omaer1(iz)
            else
              totext = 0.
              totssa = 0.
              totcon = 0.
              do l = ngas+1,nspc
                rhfac = 1.
                if (rhadj(l).eq.1) rhfac = frh(rh1d(iz))
                totext = totext + bext(l)*rhfac*conc(i,j,iz,l)
                totssa = totssa + ssa(l)*conc(i,j,iz,l)
                totcon = totcon + conc(i,j,iz,l)
              enddo
              odaer2(iz) = totext*(z(iz+1)-z(iz))*1000.
              omaer2(iz) = max(0.20,min(0.99,totssa/totcon))
            endif
          enddo
c
c-----Call the tuv-cloud routine
c
          call tuv(nz,z,airlev,tuvalb,coszen,odcld,omcld,gcld,
     &             odaer1,odaer2,omaer1,omaer2,gaer,rafcld)
c
c-----Load 3-D CLDTRNS array
c
 100      do k = 1,nlay
            cldtrns(i,j,k) = 0.5 * (rafcld(k) + rafcld(k+1))
          enddo
        enddo
      enddo
c$omp end parallel
c
c-----Set boundary to be clear sky
c
      do j = 1,nrow
        do k = 1,nlay
          cldtrns(1,j,k) = 1.
          cldtrns(ncol,j,k) = 1.
        enddo
      enddo 
      do i = 1,ncol
        do k = 1,nlay
          cldtrns(i,1,k) = 1.
          cldtrns(i,nrow,k) = 1.
        enddo
      enddo 
       
      return
      end