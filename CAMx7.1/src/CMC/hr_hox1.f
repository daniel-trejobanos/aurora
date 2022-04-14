      subroutine hr_hox1(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v7.10 210105
c
c     HR_HOX1 solves the HOx family using Hertel's equations
c
c     Copyright 1996 - 2021
c     Ramboll
c     Created by the CMC version 5.2.6 on 2020 02 20
c
c --- Subroutines Called:
c        none
c
c --- Called by:
c        EBISOLV
c
c --- Argument definitions:
c        y0  - initial Conc for this step (ppm)
c        yh  - current Conc iteration (ppm)
c        y1  - next Conc iteration (ppm)
c        H2O - water vapor Conc (ppm)
c        M   - total gas Conc (ppm)
c        O2  - oxygen Conc (ppm)
c        CH4 - methane Conc (ppm)
c        H2  - hydrogen Conc (ppm)
c        ny  - dimension of y0, y1 and yh
c        rk  - rate constants (ppm-n hr-1)
c        r   - reaction rates (hr-1)
c        nr  - dimension of rk and r
c        dt  - time step (hr)
c
c --- Includes:
      include "camx.prm"
      include "chmdat.inc"
      include "ddmchm.inc"
c
c --- Arguments:
      integer ny, nr
      real y0(ny+1), y1(ny+1), yh(ny+1)
      real rk(nr), r(nr)
      real H2O, M, O2, CH4, H2, dt
c
c --- Local variables:
      real N2
      real t1, t2, t3, A, B, C, Q
      real newOH, newHO2, newHONO, newPNA
      real lsOH, lsHO2, lsHONO, lsPNA, lsO1D, yldOH, self
      real rOH_HO2, rHO2_OH, rOH_HONO, rHONO_OH, rHO2_PNA, rPNA_HO2
c
c --- Entry Point
c
      N2 = M - O2
c
c --- O1D loss
c
      lsO1D = 
     &       + rk( 10)*M
     &       + rk( 11)*H2O
c
c --- Yield of OH from O1D
c
      yldOH  = ( 
     &         ( 2.000)*rk( 11)*H2O ) / lsO1D
c
c --- Conversion of HONO to OH
c
      rHONO_OH  = dt*(
     &       +          rk( 43) )
c
c --- Conversion of HO2 to OH
c
      rHO2_OH  = dt*(
     &       +          rk( 13)*yh(lO3)
     &       +          rk( 15)*yh(lO)
     &       +          rk( 25)*yh(lNO)
     &       +          rk( 33)*yh(lNO3)
     &       + ( 0.500)*rk( 57)*yh(lC2O3)
     &       + ( 0.500)*rk( 65)*yh(lCXO3)
     &       + ( 0.200)*rk(103)*yh(lHCO3)
     &       + ( 0.120)*rk(145)*yh(lISO2)
     &       + ( 1.125)*rk(159)*yh(lEPX2)
     &       + ( 0.500)*rk(202)*yh(lOPO3) )
c
c --- Other OH production terms
c
      newOH = y0(lOH) + dt*(
     &       + ( 2.000)*r( 21)
     &       +          r( 23)
     &       +          r( 47)
     &       + ( 0.410)*r( 50)
     &       +          r( 88)
     &       +          r( 90)
     &       + ( 0.190)*r(111)
     &       + ( 0.170)*r(135)
     &       + ( 0.334)*r(138)
     &       + ( 0.500)*r(141)
     &       + ( 0.266)*r(149)
     &       + ( 0.461)*r(152)
     &       +          r(156)
     &       + ( 0.125)*r(160)
     &       + ( 0.100)*r(161)
     &       + ( 0.125)*r(162)
     &       + ( 0.570)*r(165)
     &       + ( 0.500)*r(191)
     &       + ( 0.500)*r(195)
     &       +          r(210)
     &       +          r(  9)*yldOH )
c
c --- Conversion of PNA to HO2
c
      rPNA_HO2  = dt*(
     &       +          rk( 49)
     &       + ( 0.590)*rk( 50) )
c
c --- Conversion of OH to HO2
c
      rOH_HO2  = dt*(
     &       +          rk( 12)*yh(lO3)
     &       +          rk( 14)*yh(lO)
     &       +          rk( 22)*yh(lH2O2)
     &       +          rk( 32)*yh(lNO3)
     &       +          rk( 52)*yh(lSO2)
     &       +          rk( 93)*yh(lFACD)
     &       +          rk( 96)*yh(lFORM)
     &       + ( 0.200)*rk(110)*yh(lGLYD)
     &       +          rk(113)*yh(lGLY)
     &       +          rk(119)*H2
     &       +          rk(120)*yh(lCO)
     &       +          rk(123)*yh(lMEOH)
     &       + ( 0.900)*rk(124)*yh(lETOH)
     &       + ( 0.300)*rk(133)*yh(lETHY)
     &       + ( 0.137)*rk(151)*yh(lISPD)
     &       + ( 0.530)*rk(167)*yh(lBENZ)
     &       + ( 0.180)*rk(172)*yh(lTOL)
     &       + ( 0.155)*rk(177)*yh(lXYL)
     &       +          rk(182)*yh(lCRES)
     &       + ( 0.200)*rk(197)*yh(lCAT1) )
c
c --- Other HO2 production terms
c
      newHO2 = y0(lHO2) + dt*(
     &       +          r( 23)
     &       +          r( 71)
     &       + ( 0.900)*r( 73)
     &       + ( 0.370)*r( 74)
     &       +          r( 75)
     &       + ( 0.800)*r( 77)
     &       + ( 0.600)*r( 78)
     &       + ( 0.800)*r( 85)
     &       +          r( 90)
     &       + ( 2.000)*r( 97)
     &       +          r( 99)
     &       +          r(101)
     &       +          r(102)
     &       +          r(106)
     &       +          r(109)
     &       + ( 1.400)*r(111)
     &       + ( 2.000)*r(114)
     &       +          r(115)
     &       +          r(116)
     &       +          r(131)
     &       + ( 0.270)*r(135)
     &       + ( 0.080)*r(138)
     &       + ( 0.818)*r(144)
     &       + ( 0.728)*r(146)
     &       + ( 0.728)*r(147)
     &       +          r(148)
     &       + ( 0.066)*r(149)
     &       + ( 0.398)*r(152)
     &       + ( 0.760)*r(154)
     &       + ( 0.825)*r(160)
     &       + ( 0.660)*r(161)
     &       + ( 0.825)*r(162)
     &       + ( 0.918)*r(168)
     &       +          r(169)
     &       +          r(171)
     &       + ( 0.860)*r(173)
     &       +          r(174)
     &       +          r(176)
     &       + ( 0.860)*r(178)
     &       +          r(180)
     &       +          r(181)
     &       +          r(188)
     &       + ( 0.700)*r(189)
     &       +          r(193)
     &       + ( 0.560)*r(195)
     &       + ( 0.800)*r(199) )
c
c
c --- Conversion of OH to HONO
c
      rOH_HONO  = dt*(
     &       +          rk( 40)*yh(lNO) )
c
c --- Other HONO production terms
c
      newHONO = y0(lHONO) + dt*(
     &       + ( 2.000)*r( 41)
     &       +          r(188) ) 
c
c --- Conversion of HO2 to PNA
c
      rHO2_PNA  = dt*(
     &       +          rk( 48)*yh(lNO2) )
c
c --- Other PNA production terms
c
      newPNA = y0(lPNA) 
c
c --- Net loss of OH
c
      lsOH = 1.0 + dt*(
     &       +          rk( 12)*yh(lO3)
     &       +          rk( 14)*yh(lO)
     &       +          rk( 16)*yh(lOH)
     &       +          rk( 17)*yh(lOH)
     &       +          rk( 18)*yh(lHO2)
     &       +          rk( 22)*yh(lH2O2)
     &       +          rk( 32)*yh(lNO3)
     &       +          rk( 40)*yh(lNO)
     &       +          rk( 44)*yh(lHONO)
     &       +          rk( 45)*yh(lNO2)
     &       +          rk( 46)*yh(lHNO3)
     &       +          rk( 51)*yh(lPNA)
     &       +          rk( 52)*yh(lSO2)
     &       + ( 0.600)*rk( 87)*yh(lMEPX)
     &       + ( 0.600)*rk( 89)*yh(lROOH)
     &       +          rk( 91)*yh(lNTR1)
     &       +          rk( 93)*yh(lFACD)
     &       +          rk( 94)*yh(lAACD)
     &       +          rk( 95)*yh(lPACD)
     &       +          rk( 96)*yh(lFORM)
     &       +          rk(104)*yh(lALD2)
     &       +          rk(107)*yh(lALDX)
     &       +          rk(110)*yh(lGLYD)
     &       +          rk(113)*yh(lGLY)
     &       +          rk(118)*yh(lMGLY)
     &       +          rk(119)*H2
     &       +          rk(120)*yh(lCO)
     &       +          rk(121)*CH4
     &       +          rk(122)*yh(lETHA)
     &       +          rk(123)*yh(lMEOH)
     &       +          rk(124)*yh(lETOH)
     &       +          rk(127)*yh(lACET)
     &       +          rk(128)*yh(lPRPA)
     &       +          rk(129)*yh(lPAR)
     &       + ( 0.300)*rk(133)*yh(lETHY)
     &       +          rk(134)*yh(lETH)
     &       +          rk(137)*yh(lOLE)
     &       +          rk(140)*yh(lIOLE)
     &       +          rk(143)*yh(lISOP)
     &       +          rk(151)*yh(lISPD)
     &       + ( 0.067)*rk(155)*yh(lISPX)
     &       +          rk(158)*yh(lEPOX)
     &       +          rk(163)*yh(lINTR)
     &       +          rk(164)*yh(lTERP)
     &       + ( 0.882)*rk(167)*yh(lBENZ)
     &       + ( 0.900)*rk(172)*yh(lTOL)
     &       + ( 0.756)*rk(177)*yh(lXYL)
     &       +          rk(182)*yh(lCRES)
     &       +          rk(186)*yh(lCRON)
     &       +          rk(190)*yh(lXOPN)
     &       +          rk(194)*yh(lOPEN)
     &       +          rk(197)*yh(lCAT1)
     &       +          rk(205)*yh(lOPAN)
     &       +          rk(206)*yh(lPANX)
     &       +          rk(208)*yh(lECH4)
     &       +          rk(218)*yh(lOIO)
     &       +          rk(231)*yh(lDMS)
     &       +          rk(232)*yh(lDMS)*O2
     &       +          rk(234)*yh(lNO2)*H2O )
c
c --- Loss of HO2, excluding self-reaction
c     (net either HO2 or OH produced)
c
      lsHO2 = 1.0 + rHO2_OH + rHO2_PNA + dt*(
     &       +          rk( 18)*yh(lOH)
     &       + ( 0.500)*rk( 57)*yh(lC2O3)
     &       + ( 0.500)*rk( 65)*yh(lCXO3)
     &       +          rk( 72)*yh(lMEO2)
     &       +          rk( 76)*yh(lXO2H)
     &       +          rk( 80)*yh(lXO2)
     &       +          rk( 84)*yh(lXO2N)
     &       +          rk(100)*yh(lFORM)
     &       + ( 0.600)*rk(103)*yh(lHCO3)
     &       + ( 0.760)*rk(145)*yh(lISO2)
     &       +          rk(170)*yh(lBZO2)
     &       +          rk(175)*yh(lTO2)
     &       +          rk(179)*yh(lXLO2)
     &       +          rk(185)*yh(lCRO)
     &       + ( 0.500)*rk(202)*yh(lOPO3)
     &       +          rk(214)*yh(lIO) )
c
c --- HO2 self-reaction
c
      self = dt*2.0*( 
     &       +          rk( 19)
     &       +          rk( 20)*H2O )
c
c --- Loss of HONO
c
      lsHONO = 1.0 + dt*(
     &       + ( 2.000)*rk( 42)*yh(lHONO)
     &       +          rk( 43)
     &       +          rk( 44)*yh(lOH) )
c
c --- Loss of PNA
c
      lsPNA = 1.0 + dt*(
     &       +          rk( 49)
     &       +          rk( 50)
     &       +          rk( 51)*yh(lOH) )
c
c --- Collect common terms
c
      t1 = 1.0 / ( lsOH*lsHONO - rHONO_OH*rOH_HONO )
      t2 = rOH_HO2*t1
      t3 = rPNA_HO2 / lsPNA
c
c --- Solve for HO2
c
      A = self
      B = lsHO2 - t3*rHO2_PNA - t2*rHO2_OH*lsHONO
      C = newHO2 + t3 * newPNA + t2*( newOH*lsHONO + newHONO*rHONO_OH )
      Q = -0.5 * (B + SIGN(1.0,B)*SQRT(B*B + 4.0*A*C))
c
c --- Update Concentrations
c
      y1(lHO2)  = MAX(1.0E-18, MAX(Q/A ,-C/Q) )
      y1(lOH)   = MAX(1.0E-18, ( ( newOH + rHO2_OH*y1(lHO2) )*lsHONO + 
     &                                        rHONO_OH*newHONO ) * t1 )
      y1(lPNA)  = MAX(1.0E-15, ( newPNA + rHO2_PNA*y1(lHO2) ) / lsPNA )
      y1(lHONO) = MAX(1.0E-15, ( newHONO + rOH_HONO*y1(lOH) ) / lsHONO )
c
      return
      end

