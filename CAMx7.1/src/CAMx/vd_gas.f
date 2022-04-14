      subroutine vd_gas(ilu,istress,iwet,iso2,io3,z0,deltaz,psih,ustar,
     &                 diffrat,henry,henso2,f0,rscale,ts,dstress,
     &                 solflux,rj,rlu,rac,rlcs,rlco,rgss,rgso,icdocn,vd)
c
c----CAMx v7.10 210105
c
c     VD_GAS calculates a deposition velocity for a specific gas species,
c     grid cell, and land use category.  The parallel resistance approach of
c     Wesely and Hicks (1977) is used with the improvements of Wesely (1989).
c     Surface resistance (rs) to water (landuse 7) is determined following
c     Sehmel (1980), as implemented in UAM-AERO (STI, 1996).
c
c     Copyright 1996 - 2021
c     Ramboll
c 
c     Modifications: 
c        8/18/03       Relate drought stress to PDI index
c        3/21/03       Modified Ra calculation to use layer 1 midpoint height
c                      rather than default 10 m; changed drought stress effects
c                      to stomatal resistance to be consistent with effects in
c                      GLOBEIS
c        3/26/03       Added scaling factor to surface resistance (provided
c                      on chemparam file)
c
c     Input arguments: 
c        ilu                 land use index
c        istress             vegetation drought stress index
c        iwet                surface wetness index
c                            0 = dry
c                            1 = dew wetted
c                            2 = rain wetted
c        iso2                SO2 species flag (1=SO2,0=other)
c        io3                 O3 species flag (1=O3,0=other)
c        z0                  surface roughness length (m)
c        deltaz              Layer 1 midpoint height (m)
c        psih                similarity stability correction term 
c        ustar               friction velocity (m/s)
c        diffrat             ratio of molecular diffusivity of water to species
c        henry               Henry's Law constant (M/atm)
c        henso2              Henry's Law constant for SO2 (M/atm)
c        f0                  normalized reactivity parameter
c        rscale              user-defined surface resistance scaling factor
c        ts                  surface temperature (C)
c        dstress             adjustment factors for drought stress
c        solflux             Solar radiation flux (W/m2)
c        rj                  baseline minimum stomatal resistance (s/m)
c        rlu                 baseline upper canopy (cuticle) resistance (s/m)
c        rac                 baseline canopy height/density resistance (s/m)
c        rlcs                baseline SO2 lower canopy resistance (s/m)
c        rlco                baseline O3 lower canopy resistance (s/m)
c        rgss                baseline SO2 ground surface resistance (s/m)
c        rgso                baseline O3 ground surface resistance (s/m)
c        icdocn              flag indicating an over-ocean cell
c      
c     Output arguments: 
c        vd                  deposition velocity (m/s)
c      
c     Routines called: 
c        none 
c      
c     Called by: 
c        DRYDEP
c 
      real dstress(0:5)
c
      real*8 solflux
c
      data vk/0.4/, rmin/1.0/, rmax/1.e5/, d1/2./, d2/0.667/
      data vair/1.5e-5/, diffh2o/2.3e-5/
c
c-----Entry point
c
c-----Compute atmospheric resistance, RA
c
      ra = (alog(deltaz/z0) - psih)/(vk*ustar)
      ra = amax1(ra,rmin)
c
c-----Compute the deposition layer resistance, RD
c
      schmidt = vair*diffrat/diffh2o
      rd = d1*schmidt**d2/(vk*ustar)
      rd = amax1(rd,rmin)
c
c-----Compute the surface layer resistance over water, RS
c
      if (ilu.eq.7) then
        if (io3.eq.1 .and. icdocn.eq.1) then
          rs = 1./(1.e-4 + 5.0e-6*henry*ustar*ts**3.)
          rs = amax1(rs,1500.)
        else
          rs = 1./(3.9e-5*henry*ustar*(ts + 273.15))
          rs = amax1(rs,rmin)
        endif
        goto 100
      endif
c
c-----Compute stomatal resistance, RST
c     Adjust for vegetation drought stress
c
      rst = rmax
      if (ts.gt.0. .and. ts.lt.40.) then
        rst = diffrat*rj*(1. + (200./(solflux + 0.1))**2)*
     &        (400./(ts*(40.-ts)))
        rst = rst * dstress(istress)
      endif
      if (iwet.gt.0) rst = 3.*rst
      rst = amin1(rmax,rst)
c
c-----Compute mesophyll resistance, RM
c
      rm = 1./(henry/3000. + 100.*f0)
      rm = amax1(rmin,rm)
      rm = amin1(rmax,rm)
c
c-----Compute upper canopy resistance, RUC
c     Adjust for surface wetness
c
      if (iwet.eq.0) then
        ruc = rlu/(henry/henso2 + f0)
        ruc = amin1(rmax,ruc)
      else
        if (iwet.eq.1) then
          rlus = 100.
          if (ilu.eq.1) rlus = 50.
c 
c  --- original equations from Wesely 89 ---
c         rluo = 1./(1./3000. + 1./(3.*rlu))
c
          rluo = 1000. + rlu
        else
c 
c  --- original equations from Wesely 89 ---
c         rlus = 1./(1./5000. + 1./(3.*rlu))
c
          rlus = 2000. + rlu
          if (ilu.eq.1) rlus = 50.
          rluo = 1./(1./1000. + 1./(3.*rlu))
        endif
        if (iso2.eq.1) then
          ruc = rlus
        elseif (io3.eq.1) then
          ruc = rluo
        else
c 
c  --- original equations from Wesely 89 ---
c         ruc = 1./(1./(3.*rlu) + 1.e-7*henry + f0/rluo)
c
          ruc = 1./(henry/(henso2*rlus) + f0/rluo)
          ruc = amin1(rmax,ruc)
        endif
      endif
c
c-----Compute resistance to buoyant convection, RDC
c     (assume effect of local terrain slope is non-resolvable; this
c     factor is set to 1)
c
      rdc = 100.*(1. + 1000./(solflux + 10.))
      rdc = amin1(rmax,rdc)
c
c-----Compute resistance of exposed surfaces in lower canopy, RCL
c
      rlc = 1./(henry/(henso2*rlcs) + f0/rlco)
      rlc = amin1(rmax,rlc)
c
c-----Compute resistance of ground surface, RGS
c
      rgs = 1./(henry/(henso2*rgss) + f0/rgso)
      rgs = amin1(rmax,rgs)
c
c-----Determine total surface resistance over land, RS
c
      rs = 1./(rst + rm) + 1./ruc + 1./(rdc + rlc) + 1./(rac + rgs)
      rs = amax1(rmin,1./rs)
c
c-----Scale surface resistance for acids according to user-definition
c
 100  continue
      rs = rs*rscale
c
c-----Final deposition velocity for this cell, land use, and species
c
      vd = 1./(ra + rd + rs)
c
      return
      end
