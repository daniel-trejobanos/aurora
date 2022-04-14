      subroutine ebirate4(ny,nr,r,gain,loss)
      implicit none
c
c----CAMx v7.10 210105
c
c     EBIRATE4 computes species production and loss
c     for the EBI solver
c
c     Copyright 1996 - 2021
c     Ramboll
c     Created by the CMC version 5.2.6
c
c --- Subroutines Called:
c        none
c
c --- Called by:
c        EBISOLV
c
c --- Argument definitions:
c        ny   - dimension of gain and loss
c        nr   - dimension of r
c        r    - reaction rates (hr-1)
c        gain - species production (ppm/hr)
c        loss - species destruction (ppm/hr)
c
c --- Includes:
      include "camx.prm"
      include "chmdat.inc"
      include "ddmchm.inc"
c
c --- Arguments:
      integer ny, nr
      real    loss(ny+1), gain(ny+1)
      real    r(nr)
c
c --- Entry Point:
c
c
c-----Calculate the species rates
c
c
c  H2O2  HNO3   SO2  SULF  MEO2   RO2  PACD  AACD  CXO3  ALD2
c  XO2H  PANX  FORM  MEPX  MEOH  ROOH   XO2  XO2N  NTR1  NTR2
c  FACD    CO  HCO3  ALDX  GLYD   GLY  MGLY  ETHA  ETOH   KET
c   PAR  ACET  PRPA  XPRP  XPAR   ROR  ETHY   ETH   OLE  IOLE
c  ISOP  ISO2  INTR  ISPD  ISPX  HPLD  OPO3  EPOX  EPX2  TERP
c  BENZ  CRES  BZO2  OPEN   TOL   TO2  XOPN   XYL  XLO2   CRO
c  CAT1  CRON  OPAN  ECH4    I2   HOI  I2O2  INO3  HIO3  IXOY
c   DMS
c
        Loss(lH2O2 )= +         r( 21)+         r( 22)+         r( 23)
c
        Gain(lH2O2 )= +         r( 17)+         r( 19)+         r( 20)
     &                +( 0.040)*r(138)+( 0.080)*r(141)
c
        Loss(lHNO3 )= +         r( 46)+         r( 47)
c
        Gain(lHNO3 )= +( 2.000)*r( 39)+         r( 45)+         r( 99)
     &                +         r(105)+         r(108)+         r(112)
     &                +         r(115)+         r(117)+( 0.717)*r(153)
     &                +         r(157)+         r(183)+         r(187)
     &                +         r(196)+         r(198)+         r(207)
     &                +         r(224)+         r(229)+         r(233)
c
        Loss(lSO2  )= +         r( 52)+         r(230)
c
        Gain(lSO2  )= +         r(231)+         r(233)
c
        Loss(lSULF )= 0.0
c
        Gain(lSULF )= +         r( 52)+         r(230)+         r(232)
c
        Loss(lMEO2 )= +         r( 71)+         r( 72)+         r( 73)
     &                +         r( 74)
c
        Gain(lMEO2 )= +         r( 53)+( 0.400)*r( 56)+( 0.440)*r( 57)
     &                +( 2.000)*r( 59)+         r( 60)+( 0.900)*r( 73)
     &                +( 0.800)*r( 77)+( 0.800)*r( 81)+( 0.800)*r( 85)
     &                +( 0.600)*r( 87)+         r( 88)+         r( 94)
     &                +         r(106)+         r(121)+( 0.500)*r(125)
     &                +( 1.380)*r(126)+( 0.800)*r(146)+( 0.115)*r(151)
     &                +( 0.340)*r(154)+( 0.800)*r(161)+         r(169)
     &                +         r(174)+         r(180)+         r(203)
     &                +         r(208)+         r(231)+         r(232)
        Gain(lMEO2 ) = Gain(lMEO2 )
     &                +         r(233)
c
        Loss(lRO2  )= +         r( 58)+         r( 66)+         r( 68)
     &                +         r( 69)+( 2.000)*r( 70)+         r( 74)
     &                +         r( 78)+         r( 82)+         r( 86)
     &                +         r(147)+         r(162)+         r(171)
     &                +         r(176)+         r(181)+         r(204)
c
        Gain(lRO2  )= +         r( 53)+( 0.400)*r( 56)+( 0.440)*r( 57)
     &                +( 2.000)*r( 59)+( 2.000)*r( 60)+         r( 61)
     &                +( 0.400)*r( 64)+( 0.440)*r( 65)+( 0.800)*r( 66)
     &                +( 2.000)*r( 67)+( 0.900)*r( 73)+         r( 74)
     &                +( 0.800)*r( 77)+         r( 78)+( 0.800)*r( 81)
     &                +         r( 82)+( 0.800)*r( 85)+         r( 86)
     &                +( 0.600)*r( 87)+         r( 88)+( 0.600)*r( 89)
     &                +         r( 94)+         r(106)+         r(109)
     &                +( 0.110)*r(111)+( 0.200)*r(113)+( 0.500)*r(115)
        Gain(lRO2  ) = Gain(lRO2  )
     &                +         r(117)+         r(121)+         r(122)
     &                +( 0.100)*r(124)+         r(125)+( 1.380)*r(126)
     &                +         r(127)+( 0.980)*r(130)+         r(134)
     &                +         r(136)+( 1.195)*r(137)+( 0.150)*r(138)
     &                +         r(139)+         r(140)+( 0.300)*r(141)
     &                +         r(142)+         r(143)+( 0.082)*r(144)
     &                +( 0.872)*r(146)+( 1.072)*r(147)+( 0.200)*r(149)
     &                +         r(150)+( 0.658)*r(151)+( 0.284)*r(153)
        Gain(lRO2  ) = Gain(lRO2  )
     &                +( 0.840)*r(154)+( 0.067)*r(155)+         r(158)
     &                +( 0.800)*r(161)+         r(162)+         r(163)
     &                +( 1.500)*r(164)+( 0.940)*r(165)+( 1.280)*r(166)
     &                +( 0.352)*r(167)+         r(169)+         r(171)
     &                +( 0.720)*r(172)+         r(174)+         r(176)
     &                +( 0.602)*r(177)+         r(180)+         r(181)
     &                +( 0.020)*r(182)+( 0.700)*r(183)+( 2.000)*r(190)
     &                +( 0.300)*r(191)+         r(192)+( 0.400)*r(194)
        Gain(lRO2  ) = Gain(lRO2  )
     &                +( 0.440)*r(202)+( 2.000)*r(203)+( 1.800)*r(204)
     &                +         r(208)+         r(225)+         r(226)
     &                +         r(227)+         r(228)
c
        Loss(lPACD )= +         r( 95)
c
        Gain(lPACD )= +( 0.410)*r( 57)+( 0.410)*r( 65)+( 0.410)*r(202)
c
        Loss(lAACD )= +         r( 94)
c
        Gain(lAACD )= +( 0.150)*r( 57)+( 0.150)*r( 65)+( 0.100)*r( 73)
     &                +( 0.200)*r( 77)+( 0.200)*r( 81)+( 0.200)*r( 85)
     &                +( 0.130)*r(138)+( 0.080)*r(141)+( 0.200)*r(146)
     &                +( 0.200)*r(161)+( 0.150)*r(202)+( 0.200)*r(204)
c
        Loss(lCXO3 )= +         r( 60)+         r( 61)+         r( 62)
     &                +         r( 65)+         r( 66)+( 2.000)*r( 67)
c
        Gain(lCXO3 )= +         r( 63)+( 0.600)*r( 64)+         r(107)
     &                +         r(108)+( 0.500)*r(125)+( 0.200)*r(149)
     &                +( 0.717)*r(153)+( 0.390)*r(165)+( 0.200)*r(199)
c
        Loss(lALD2 )= +         r(104)+         r(105)+         r(106)
c
        Gain(lALD2 )= +         r( 60)+         r( 61)+( 0.400)*r( 64)
     &                +( 0.440)*r( 65)+( 0.800)*r( 66)+( 2.000)*r( 67)
     &                +         r(109)+( 0.991)*r(122)+( 0.950)*r(124)
     &                +( 0.500)*r(125)+( 0.740)*r(130)+( 0.488)*r(137)
     &                +( 0.295)*r(138)+( 0.250)*r(139)+( 1.300)*r(140)
     &                +( 0.732)*r(141)+( 0.500)*r(142)+( 0.040)*r(152)
     &                +( 0.100)*r(191)+( 0.020)*r(195)+         r(206)
c
        Loss(lXO2H )= +         r( 75)+         r( 76)+         r( 77)
     &                +         r( 78)
c
        Gain(lXO2H )= +         r( 60)+         r( 61)+( 0.400)*r( 64)
     &                +( 0.440)*r( 65)+( 0.800)*r( 66)+( 2.000)*r( 67)
     &                +( 0.540)*r( 89)+         r(109)+( 0.110)*r(111)
     &                +( 0.991)*r(122)+( 0.100)*r(124)+( 0.500)*r(125)
     &                +( 0.940)*r(130)+         r(134)+( 0.500)*r(136)
     &                +( 0.976)*r(137)+( 0.150)*r(138)+( 0.480)*r(139)
     &                +         r(140)+( 0.300)*r(141)+( 0.480)*r(142)
     &                +( 0.082)*r(144)+( 0.072)*r(146)+( 0.072)*r(147)
     &                +( 0.640)*r(150)+( 0.142)*r(153)+( 0.340)*r(154)
        Gain(lXO2H ) = Gain(lXO2H )
     &                +( 0.370)*r(163)+( 0.750)*r(164)+( 0.070)*r(165)
     &                +( 0.280)*r(166)+( 0.070)*r(172)+( 0.058)*r(177)
     &                +( 0.120)*r(183)+         r(189)+( 2.000)*r(190)
     &                +( 0.300)*r(191)+( 0.450)*r(192)+( 0.400)*r(194)
     &                +( 0.440)*r(202)+( 0.800)*r(204)+         r(226)
     &                +( 0.126)*r(228)
c
        Loss(lPANX )= +         r( 63)+         r( 64)+         r(206)
c
        Gain(lPANX )= +         r( 62)
c
        Loss(lFORM )= +         r( 96)+         r( 97)+         r( 98)
     &                +         r( 99)+         r(100)
c
        Gain(lFORM )= +         r( 71)+( 0.100)*r( 72)+         r( 73)
     &                +( 0.685)*r( 74)+( 0.400)*r( 87)+         r(101)
     &                +( 0.740)*r(111)+         r(123)+( 0.078)*r(124)
     &                +         r(127)+( 1.560)*r(134)+         r(135)
     &                +( 1.125)*r(136)+( 0.781)*r(137)+( 0.555)*r(138)
     &                +( 0.500)*r(139)+( 0.128)*r(141)+( 0.673)*r(144)
     &                +( 0.120)*r(145)+( 0.598)*r(146)+( 0.598)*r(147)
     &                +( 0.600)*r(149)+( 0.350)*r(150)+( 0.231)*r(152)
     &                +( 0.260)*r(154)+( 0.375)*r(159)+( 0.375)*r(160)
        Gain(lFORM ) = Gain(lFORM )
     &                +( 0.300)*r(161)+( 0.375)*r(162)+( 0.592)*r(163)
     &                +( 0.280)*r(164)+( 0.240)*r(165)+         r(188)
     &                +( 0.080)*r(195)+( 0.140)*r(197)+         r(231)
     &                +         r(233)
c
        Loss(lMEPX )= +         r( 87)+         r( 88)
c
        Gain(lMEPX )= +( 0.900)*r( 72)+( 0.500)*r(103)
c
        Loss(lMEOH )= +         r(123)
c
        Gain(lMEOH )= +( 0.315)*r( 74)+( 0.150)*r(111)
c
        Loss(lROOH )= +         r( 89)+         r( 90)
c
        Gain(lROOH )= +         r( 76)+         r( 80)+         r( 84)
c
        Loss(lXO2  )= +         r( 79)+         r( 80)+         r( 81)
     &                +         r( 82)
c
        Gain(lXO2  )= +( 0.200)*r(113)+( 0.500)*r(115)+         r(117)
     &                +         r(127)+( 0.500)*r(136)+( 0.195)*r(137)
     &                +( 0.480)*r(139)+( 0.480)*r(142)+( 0.200)*r(149)
     &                +( 0.330)*r(150)+( 0.521)*r(151)+( 0.142)*r(153)
     &                +( 0.160)*r(154)+( 0.630)*r(163)+( 0.500)*r(164)
     &                +( 0.690)*r(165)+( 0.750)*r(166)+( 0.480)*r(183)
     &                +( 0.450)*r(192)+         r(203)+( 0.874)*r(228)
c
        Loss(lXO2N )= +         r( 83)+         r( 84)+         r( 85)
     &                +         r( 86)
c
        Gain(lXO2N )= +( 0.060)*r( 89)+( 0.009)*r(122)+( 0.040)*r(130)
     &                +( 0.024)*r(137)+( 0.040)*r(139)+( 0.040)*r(142)
     &                +( 0.030)*r(150)+( 0.022)*r(151)+( 0.250)*r(164)
     &                +( 0.180)*r(165)+( 0.250)*r(166)+( 0.020)*r(182)
     &                +( 0.100)*r(183)+( 0.100)*r(192)+         r(225)
     &                +         r(227)
c
        Loss(lNTR1 )= +         r( 91)+         r( 92)
c
        Gain(lNTR1 )= +( 0.500)*r( 83)+         r(132)+( 0.500)*r(136)
     &                +( 0.500)*r(139)+( 0.500)*r(142)
c
        Loss(lNTR2 )= +         r(207)
c
        Gain(lNTR2 )= +( 0.500)*r( 83)+         r( 91)+( 0.650)*r(150)
     &                +( 0.142)*r(153)+( 0.266)*r(163)+( 0.530)*r(166)
     &                +( 0.082)*r(168)+( 0.140)*r(173)+( 0.140)*r(178)
     &                +         r(186)+         r(187)+( 0.500)*r(192)
     &                +( 0.500)*r(205)
c
        Loss(lFACD )= +         r( 93)
c
        Gain(lFACD )= +         r(102)+( 0.500)*r(103)+( 0.300)*r(133)
     &                +( 0.370)*r(135)+( 0.090)*r(138)+( 0.150)*r(152)
     &                +( 0.074)*r(159)+( 0.185)*r(163)
c
        Loss(lCO   )= +         r(120)
c
        Gain(lCO   )= +         r( 96)+         r( 97)+         r( 98)
     &                +         r( 99)+         r(106)+         r(109)
     &                +( 0.890)*r(111)+( 1.800)*r(113)+( 2.000)*r(114)
     &                +( 1.500)*r(115)+         r(116)+         r(118)
     &                +( 0.380)*r(126)+( 0.300)*r(133)+( 0.510)*r(135)
     &                +( 0.378)*r(138)+( 0.245)*r(141)+( 0.066)*r(149)
     &                +( 0.137)*r(151)+( 0.543)*r(152)+( 0.251)*r(159)
     &                +( 0.251)*r(160)+( 0.200)*r(161)+( 0.251)*r(162)
     &                +( 0.001)*r(165)+( 0.700)*r(189)+( 0.500)*r(191)
        Gain(lCO   ) = Gain(lCO   )
     &                +         r(193)+( 1.980)*r(195)+( 0.500)*r(199)
     &                +         r(205)
c
        Loss(lHCO3 )= +         r(101)+         r(102)+         r(103)
c
        Gain(lHCO3 )= +         r(100)
c
        Loss(lALDX )= +         r(107)+         r(108)+         r(109)
c
        Gain(lALDX )= +( 0.370)*r(130)+( 0.488)*r(137)+( 0.270)*r(138)
     &                +( 0.375)*r(139)+( 0.700)*r(140)+( 0.442)*r(141)
     &                +( 0.625)*r(142)+( 0.150)*r(149)+( 0.029)*r(155)
     &                +( 0.078)*r(163)+( 0.470)*r(164)+( 0.210)*r(165)
     &                +( 0.470)*r(166)+( 0.440)*r(202)+         r(203)
     &                +( 0.800)*r(204)+( 0.268)*r(226)+( 0.126)*r(228)
c
        Loss(lGLYD )= +         r(110)+         r(111)+         r(112)
c
        Gain(lGLYD )= +( 0.011)*r(124)+( 0.220)*r(134)+( 0.269)*r(151)
     &                +( 0.113)*r(153)+( 0.128)*r(154)+( 0.275)*r(159)
     &                +( 0.275)*r(160)+( 0.220)*r(161)+( 0.275)*r(162)
     &                +( 0.331)*r(163)
c
        Loss(lGLY  )= +         r(113)+         r(114)+         r(115)
c
        Gain(lGLY  )= +( 0.200)*r(110)+( 0.110)*r(111)+( 0.700)*r(133)
     &                +( 0.075)*r(138)+( 0.240)*r(141)+( 0.170)*r(152)
     &                +( 0.275)*r(159)+( 0.275)*r(160)+( 0.220)*r(161)
     &                +( 0.275)*r(162)+( 0.918)*r(168)+         r(169)
     &                +         r(171)+( 0.417)*r(173)+( 0.480)*r(174)
     &                +( 0.480)*r(176)+( 0.221)*r(178)+( 0.260)*r(180)
     &                +( 0.260)*r(181)+( 0.025)*r(182)+( 0.240)*r(183)
     &                +( 0.400)*r(189)+( 0.400)*r(190)+( 0.400)*r(194)
     &                +( 1.400)*r(195)+( 0.500)*r(199)+( 0.500)*r(205)
c
        Loss(lMGLY )= +         r(116)+         r(117)+         r(118)
c
        Gain(lMGLY )= +( 0.075)*r(138)+( 0.060)*r(141)+( 0.115)*r(151)
     &                +( 0.531)*r(152)+( 0.113)*r(153)+( 0.275)*r(159)
     &                +( 0.275)*r(160)+( 0.220)*r(161)+( 0.275)*r(162)
     &                +( 0.443)*r(173)+( 0.520)*r(174)+( 0.520)*r(176)
     &                +( 0.675)*r(178)+( 0.770)*r(180)+( 0.770)*r(181)
     &                +( 0.240)*r(183)+         r(190)+( 1.200)*r(191)
     &                +( 0.250)*r(192)+( 0.240)*r(195)
c
        Loss(lETHA )= +         r(122)
c
        Gain(lETHA )= 0.0
c
        Loss(lETOH )= +         r(124)
c
        Gain(lETOH )= 0.0
c
        Loss(lKET  )= +         r(125)
c
        Gain(lKET  )= +( 0.200)*r(130)+         r(131)
c
        Loss(lPAR  )= +         r(129)
c
        Gain(lPAR  )= +(-2.500)*r(125)+(-2.700)*r(130)+(-0.730)*r(137)
     &                +(-0.790)*r(138)+(-1.000)*r(139)+( 0.290)*r(141)
     &                +         r(142)+( 0.350)*r(149)+( 0.117)*r(151)
     &                +( 0.717)*r(153)+( 0.240)*r(154)+( 2.175)*r(159)
     &                +( 2.175)*r(160)+( 1.740)*r(161)+( 2.175)*r(162)
     &                +( 2.700)*r(163)+( 1.660)*r(164)+( 7.000)*r(165)
     &                +( 0.268)*r(226)+(-0.126)*r(228)
c
        Loss(lACET )= +         r(126)+         r(127)
c
        Gain(lACET )= +( 0.420)*r(130)+( 0.137)*r(151)+( 0.170)*r(152)
     &                +( 0.170)*r(154)+( 0.732)*r(226)
c
        Loss(lPRPA )= +         r(128)
c
        Gain(lPRPA )= 0.0
c
        Loss(lXPRP )= +         r(225)+         r(226)
c
        Gain(lXPRP )= +         r(128)
c
        Loss(lXPAR )= +         r(227)+         r(228)
c
        Gain(lXPAR )= +         r(129)
c
        Loss(lROR  )= +         r(130)+         r(131)+         r(132)
c
        Gain(lROR  )= +( 0.020)*r(130)+( 0.874)*r(228)
c
        Loss(lETHY )= +         r(133)
c
        Gain(lETHY )= 0.0
c
        Loss(lETH  )= +         r(134)+         r(135)+         r(136)
c
        Gain(lETH  )= 0.0
c
        Loss(lOLE  )= +         r(137)+         r(138)+         r(139)
c
        Gain(lOLE  )= +( 0.240)*r(154)+( 0.098)*r(163)
c
        Loss(lIOLE )= +         r(140)+         r(141)+         r(142)
c
        Gain(lIOLE )= +( 0.029)*r(155)
c
        Loss(lISOP )= +         r(143)+         r(149)+         r(150)
c
        Gain(lISOP )= 0.0
c
        Loss(lISO2 )= +         r(144)+         r(145)+         r(146)
     &                +         r(147)+         r(148)
c
        Gain(lISO2 )= +         r(143)+( 0.067)*r(155)
c
        Loss(lINTR )= +         r(163)+         r(229)
c
        Gain(lINTR )= +( 0.100)*r(144)+( 0.104)*r(163)
c
        Loss(lISPD )= +         r(151)+         r(152)+         r(153)
     &                +         r(154)
c
        Gain(lISPD )= +( 0.900)*r(144)+( 0.120)*r(145)+         r(146)
     &                +         r(147)+( 0.650)*r(149)+( 0.350)*r(150)
     &                +         r(156)+         r(157)
c
        Loss(lISPX )= +         r(155)
c
        Gain(lISPX )= +( 0.880)*r(145)
c
        Loss(lHPLD )= +         r(156)+         r(157)
c
        Gain(lHPLD )= +         r(148)
c
        Loss(lOPO3 )= +         r(199)+         r(200)+         r(202)
     &                +         r(203)+         r(204)
c
        Gain(lOPO3 )= +( 0.457)*r(151)+( 0.480)*r(183)+         r(193)
     &                +( 0.600)*r(194)+         r(196)+         r(201)
c
        Loss(lEPOX )= +         r(158)
c
        Gain(lEPOX )= +( 0.904)*r(155)
c
        Loss(lEPX2 )= +         r(159)+         r(160)+         r(161)
     &                +         r(162)
c
        Gain(lEPX2 )= +         r(158)
c
        Loss(lTERP )= +         r(164)+         r(165)+         r(166)
c
        Gain(lTERP )= 0.0
c
        Loss(lBENZ )= +         r(167)
c
        Gain(lBENZ )= 0.0
c
        Loss(lCRES )= +         r(182)+         r(183)
c
        Gain(lCRES )= +( 0.530)*r(167)+( 0.180)*r(172)+( 0.155)*r(177)
     &                +         r(185)
c
        Loss(lBZO2 )= +         r(168)+         r(169)+         r(170)
     &                +         r(171)
c
        Gain(lBZO2 )= +( 0.352)*r(167)
c
        Loss(lOPEN )= +         r(193)+         r(194)+         r(195)
     &                +         r(196)
c
        Gain(lOPEN )= +( 0.118)*r(167)+( 0.918)*r(168)+         r(169)
     &                +         r(171)+( 0.100)*r(172)+( 0.660)*r(173)
     &                +( 0.770)*r(174)+( 0.770)*r(176)+( 0.300)*r(178)
     &                +( 0.350)*r(180)+( 0.350)*r(181)+( 0.025)*r(182)
     &                +         r(188)+( 0.250)*r(192)
c
        Loss(lTOL  )= +         r(172)
c
        Gain(lTOL  )= 0.0
c
        Loss(lTO2  )= +         r(173)+         r(174)+         r(175)
     &                +         r(176)
c
        Gain(lTO2  )= +( 0.650)*r(172)
c
        Loss(lXOPN )= +         r(189)+         r(190)+         r(191)
     &                +         r(192)
c
        Gain(lXOPN )= +( 0.200)*r(173)+( 0.230)*r(174)+( 0.230)*r(176)
     &                +( 0.244)*r(177)+( 0.560)*r(178)+( 0.650)*r(180)
     &                +( 0.650)*r(181)
c
        Loss(lXYL  )= +         r(177)
c
        Gain(lXYL  )= 0.0
c
        Loss(lXLO2 )= +         r(178)+         r(179)+         r(180)
     &                +         r(181)
c
        Gain(lXLO2 )= +( 0.544)*r(177)
c
        Loss(lCRO  )= +         r(184)+         r(185)
c
        Gain(lCRO  )= +( 0.200)*r(182)+( 0.300)*r(183)+( 0.500)*r(186)
     &                +( 0.500)*r(187)+( 0.500)*r(197)+         r(198)
c
        Loss(lCAT1 )= +         r(197)+         r(198)
c
        Gain(lCAT1 )= +( 0.732)*r(182)
c
        Loss(lCRON )= +         r(186)+         r(187)+         r(188)
c
        Gain(lCRON )= +         r(184)
c
        Loss(lOPAN )= +         r(201)+         r(205)
c
        Gain(lOPAN )= +         r(200)
c
        Loss(lECH4 )= +         r(208)
c
        Gain(lECH4 )= 0.0
c
        Loss(lI2   )= +         r(209)
c
        Gain(lI2   )= 0.0
c
        Loss(lHOI  )= +         r(210)
c
        Gain(lHOI  )= +         r(214)+         r(224)
c
        Loss(lI2O2 )= +         r(221)+         r(222)
c
        Gain(lI2O2 )= +( 0.600)*r(213)
c
        Loss(lINO3 )= +         r(223)+         r(224)
c
        Gain(lINO3 )= +         r(216)
c
        Loss(lHIO3 )= 0.0
c
        Gain(lHIO3 )= +         r(218)
c
        Loss(lIXOY )= 0.0
c
        Gain(lIXOY )= +         r(219)+         r(222)
c
        Loss(lDMS  )= +         r(231)+         r(232)+         r(233)
c
        Gain(lDMS  )= 0.0
c
c
      return
      end

