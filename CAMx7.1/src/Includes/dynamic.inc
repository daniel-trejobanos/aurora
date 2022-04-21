c     This file contains the necessary parameteres for the new generation
c     multicomponent aerosol dynamics model (MADM)
c
       implicit double precision (a-h,o-z)
c
c *************************************************************************
c     WRITTEN BY DR. CHRISTODOULOS PILINIS
c              December, 1996
c
c     Modified for CAMx and secondary organic aerosols by Fred Lurmann May 2000
c
c     03/09/03: bkoo
c               - commented out parameter nsoap (nsoap is now assigned in soap.com)
c     12/29/2006: bkoo
c               - modified for the updated SOA scheme
c     03/18/2014: bkoo
c               - modified for benzene SOA
c     08/25/2016: bkoo
c               - Updated for new SOAP
c *************************************************************************
c
      include 'section.inc'
c  ** changes in nsec necesitate changes in formats for subroutine SPCPRINT **
cbk      parameter(neqsec=4)             ! Number of equilibrium sections
      parameter(neqsec=2)
      parameter(nsecd=nsec-neqsec)    ! Number of dynamic sections
      parameter(nexti=6)              ! Number of external inorganic aerosol species
c
      parameter(imovsec=1)         ! 0 is fixed sections 1 is moving sections
c      parameter(imovsec=0)         ! 0 is fixed sections 1 is moving sections
c
c  external inorganic species are those to be used by the 3D-gas-aerosol model
c  while internal inorganic species are the detailed species, handled by MADM

      parameter( KH2O=1 )       ! pointer for external aerosol water
      parameter(  KNa=2 )       ! pointer for external aerosol sodium
      parameter( KSO4=3 )       ! pointer for external aerosol sulfate
      parameter( KNO3=4 )       ! pointer for external aerosol nitrate
      parameter( KNH4=5 )       ! pointer for external aerosol ammonium
      parameter(  KCL=6 )       ! pointer for external aerosol chlorides
c
      parameter (KSOA1 = 7)!pointer for external Secondary Organic Aerosol #1
      parameter (KSOA2 = 8)!pointer for external Secondary Organic Aerosol #2
      parameter (KSOA3 = 9)!pointer for external Secondary Organic Aerosol #3
      parameter (KSOA4 =10)!pointer for external Secondary Organic Aerosol #4
      parameter (KSOPA =11)!pointer for Polymerized Anthro Organic Aerosol
      parameter (KSOPB =12)!pointer for Polymerized Biogenic Organic Aerosol
      parameter (KPOM  =13)!pointer for external Primary Organic Aerosol
      parameter (KEC   =14)!pointer for external Elemental Carbon Aerosol
      parameter (KCRUS =15)!pointer for external Crustal Aerosol 
c
c
c      parameter(ninti=18)       ! Number of internal inorganic aerosol species
      parameter(ninti=19)       ! bkoo (02/14/02)

c      1 = H2O
c      2 = H+
c      3 = Na+
c      4 = NH4+
c      5 = Cl-
c      6 = SO4--
c      7 = HSO4-
c      8 = NO3-
c      9 = NaCl
c     10 = Na2SO4
c     11 = NaNO3
c     12 = (NH4)2SO4
c     13 = NH4NO3
c     14 = NH4Cl
c     15 = H2SO4 (aq)
c     16 = NH4HSO4
c     17 = NaHSO4
c     18 = (NH4)3H(SO4)2
c     19 = OH-                  ! bkoo (02/14/02)
c
       parameter(ninert=2)            ! Number of inert aerosol species (EC & Crustal)
       parameter(npolyo=2)            ! Number of polymerized organic aerosol species
       parameter(norg=1+4)            ! Number of organic aerosol species
                                      ! (POA + volatile SOA)
cbk       parameter(nsoap=4)             ! Number or secondary organic aerosol species
cbk       parameter(ngas=4+nsoap)        ! Number of gases of interest, 4 inorganic + nsoap
       parameter(ngas=4+4)            ! Number of gases (4 inorganic + nsoap)
       parameter(INH3  =1)            ! Pointer of NH3 in the gas phase
       parameter(IHNO3 =2)            ! Pointer of HNO3 in the gas phase
       parameter(IH2SO4=3)            ! Pointer of H2SO4 in the gas phase
       parameter(IHCL  =4)            ! Pointer of HCl in the gas phase
       parameter(ICG1  =5)            ! Pointer of CG1 condensable gas #1 in the gas phase arrary
       parameter(ICG2  =6)            ! Pointer of CG2 condensable gas #2 in the gas phase arrary
       parameter(ICG3  =7)            ! Pointer of CG3 condensable gas #3 in the gas phase arrary
       parameter(ICG4  =8)            ! Pointer of CG4 condensable gas #4 in the gas phase arrary
c
       parameter(nsp=nexti+norg+npolyo+ninert)
       parameter(ntotal=nsec*nsp+ngas)
       parameter(ntotald=(nsec-neqsec)*nsp+ngas)
       parameter(ntotale=neqsec*nsp+ngas)
       parameter(nsxgas=nsec*ngas)
       parameter(nsxint=nsec*ninti)
       parameter(naer=ntotal-ngas)
c
c      SWITCHES FOR TURNING PROCESSES ON OR OFF
c
       parameter (inucl = 1)           ! 1 is on, 0 is off
       parameter (icoag = 1)           ! 1 is on, 0 is off
c
c      ADDITIONAL RESOLUTION FOR COAGULATION CALCULATIONS
c
       parameter (nres = 3)
c
       logical dry(nsec)              ! true, for dry aerosol       

       dimension diffus(nsp)          ! diffusion coefficients of condensible gases (SI units)
       dimension delta(nsp)           ! accomodation coefficients of condensible gases 
       dimension vel(nsp)             ! Mean speed of condensible gas molecules
       double precision lamda(nsp)    ! Mean free path of condensible gases
       dimension dsec(nsecp1)          ! Sectional diameters (1-->lower limit, nsec+1-->upper limit) 
c                                                        units=umeters
       dimension qt(nsec)              ! Total aerosol concentration at each section
       dimension qtt(nsp)              ! Total concentrations of aerosol species (ug/m3)
       dimension qn(nsec)              ! 0th moment (number) of total mass for seciton (qt/dsec**3)
       dimension ps(nsec,nsp)         ! Gas phase partial pressures of 4 inorganic and norg organic
c                                        species at the particle surface (Pa)
c
       dimension qsource(naer)              ! Aerosol module master array for primary aer. emissions of
                                   ! aerosol species in ugr/m3/sec.
        dimension gsource(ngas)         ! Gas phase source in ug/m3/sec.
c
       dimension c(nsec, ninti)       ! Concentrations of internal inorganic aerosol species (ugr/m3) 
       dimension emw(nsp)             ! Molecular weights of external inorganics + organics
       dimension gmw(ngas)            ! Molecular weights of condensible gases ( inorganics + organics )
       double precision intmw(ninti)  ! Molecular weights of internal inorganics
       dimension hi(nsec,ngas)        ! Hi coefficients of condensible gases ( inorganics + organics )
c                                       from Equation 8 of Pilinis et al. (1987)
       double precision qtot0(nexti)  ! initial aerosol plus gas concentrations of inorganic ions
c
c      parameter(lrw=22 + ntotal * ( 7 + ntotal + 9))  ! added 7 so that the min is 16 as required by lsoda
       parameter(lrw=22 +  9*ntotal + ntotal**2 )      ! for lsode
       parameter(liw=20+ntotal)
       dimension rwork(lrw)                ! work array for lsoda
       dimension iwork(liw)                ! integer work array for lsoda
       dimension dsecf(nsecp1)             ! sectional diameters of the fixed grid
       character*4 aerm             ! aerosol module
                                    !  EQUI for equilibrium model
                                    !  MADM for dynamic model (MADM)
                                    !  HYBR for hybrid model
       character*20 ver             ! version info.

       common /module/ pi, dsec, ps, qsource, c, emw, intmw, qtt, rh,
     & temp, dry, rgas, diffus, vel, delta, lamda,hi,qt,gmw,qn,tcom,
     & gsource, dsecf
c
      common / tinys / tinys  ! minimum non-zero concentration (ug/m3)
      common / massbal / qtot0,ifgprt,ifgbal
      common / aerm / aerm
      common / ver / ver
      common / dt / dt,pres
c
c     added by bkoo (09/19/00)
      common / arraysize / ntotalx, nsecx, ntotalx2, nsecx2
c
c     added by bkoo (02/26/02)
      parameter(atoli=1.0d-6) ! absolute tolerance for aerosols
      parameter(rtoli=1.0d-3) ! relative tolerance for all species
      parameter(govra=1.0d-2) ! ratio of PPM to UG/M3 for most species

c     added by bkoo (03/05/02)
      logical isfirst         ! first call to ISRPIA of the dynamic step?
      logical dold(nsec)      ! previous state of the aerosol (dry?)
      integer ims(nsec)       ! 0 - deliquescent, 1 - metastable
      integer icdry(nsec)     ! counter for WET/DRY oscillation
      common / ifms / isfirst, dold, ims, icdry