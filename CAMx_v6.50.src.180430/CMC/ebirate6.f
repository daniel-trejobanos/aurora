      subroutine ebirate6(ny,nr,r,gain,loss)
      implicit none
c
c----CAMx v6.50 180430
c
c     EBIRATE6 computes species production and loss
c     for the EBI solver
c
c     Copyright 1996 - 2018
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
c  HNO3  H2O2   XO2  XO2N   NTR  ROOH  FORM  ALD2  ALDX   PAR
c   SO2  SULF    CO  MEO2  MEPX  MEOH  HCO3  FACD  PACD  AACD
c  CXO3  PANX  ETHA  ETOH   ROR   OLE   ETH  IOLE   TOL  CRES
c   TO2  OPEN   CRO  MGLY   XYL  ISOP  ISPD  TERP
c
        Loss(lHNO3 )= +         r( 29)+         r( 52)
c
        Gain(lHNO3 )= +( 2.000)*r( 19)+( 2.000)*r( 20)+         r( 28)
     &                +         r( 48)+         r( 61)+         r( 78)
     &                +         r( 86)+         r(101)+         r(135)
     &                +( 0.150)*r(151)
c
        Loss(lH2O2 )= +         r( 36)+         r( 37)+         r( 45)
c
        Gain(lH2O2 )= +         r( 34)+         r( 35)+         r( 42)
c
        Loss(lXO2  )= +         r( 54)+         r( 56)+( 2.000)*r( 58)
     &                +         r( 60)+         r( 94)+         r(110)
c
        Gain(lXO2  )= +         r( 64)+( 0.300)*r( 71)+         r(103)
     &                +( 0.900)*r(109)+( 2.000)*r(111)+         r(112)
     &                +( 0.991)*r(113)+( 0.100)*r(114)+( 0.870)*r(115)
     &                +( 0.960)*r(116)+( 0.200)*r(119)+( 0.800)*r(120)
     &                +( 0.220)*r(121)+( 0.910)*r(122)+( 0.700)*r(123)
     &                +         r(124)+         r(126)+( 0.100)*r(127)
     &                +         r(128)+( 0.080)*r(131)+( 0.600)*r(134)
     &                +         r(139)+( 0.030)*r(140)+( 0.500)*r(141)
     &                +         r(142)+( 0.250)*r(144)+( 0.991)*r(145)
        Gain(lXO2  ) = Gain(lXO2  )
     &                +( 0.200)*r(146)+         r(147)+         r(148)
     &                +( 0.713)*r(149)+( 0.064)*r(150)+( 0.075)*r(151)
     &                +( 0.700)*r(152)+( 1.250)*r(154)+( 0.760)*r(155)
     &                +( 1.030)*r(156)
c
        Loss(lXO2N )= +         r( 55)+         r( 57)+( 2.000)*r( 59)
     &                +         r( 60)
c
        Gain(lXO2N )= +( 0.009)*r(113)+( 0.130)*r(115)+( 0.040)*r(116)
     &                +( 0.010)*r(119)+( 0.090)*r(122)+( 0.088)*r(145)
     &                +( 0.250)*r(154)+( 0.180)*r(155)+( 0.250)*r(156)
c
        Loss(lNTR  )= +         r( 61)+         r( 62)
c
        Gain(lNTR  )= +         r( 55)+         r(118)+( 0.100)*r(132)
     &                +         r(136)+( 0.800)*r(147)+( 0.800)*r(148)
     &                +( 0.850)*r(151)+( 0.530)*r(156)
c
        Loss(lROOH )= +         r( 64)+         r( 65)
c
        Gain(lROOH )= +         r( 56)+         r( 57)
c
        Loss(lFORM )= +         r( 74)+         r( 75)+         r( 76)
     &                +         r( 77)+         r( 78)+         r( 79)
c
        Gain(lFORM )= +( 0.330)*r( 61)+( 0.330)*r( 62)+         r( 68)
     &                +( 1.370)*r( 70)+         r( 72)+         r( 73)
     &                +         r( 80)+         r( 93)+( 0.100)*r(109)
     &                +( 0.100)*r(114)+( 0.200)*r(119)+( 0.800)*r(120)
     &                +( 0.740)*r(121)+         r(122)+         r(123)
     &                +( 1.560)*r(124)+         r(125)+( 2.000)*r(126)
     &                +( 0.250)*r(129)+         r(139)+( 0.700)*r(140)
     &                +( 0.500)*r(144)+( 0.629)*r(145)+( 0.600)*r(146)
     &                +( 0.167)*r(149)+( 0.150)*r(150)+( 0.282)*r(151)
        Gain(lFORM ) = Gain(lFORM )
     &                +( 0.900)*r(152)+( 0.280)*r(154)+( 0.240)*r(155)
c
        Loss(lALD2 )= +         r( 84)+         r( 85)+         r( 86)
     &                +         r( 87)
c
        Gain(lALD2 )= +( 0.330)*r( 61)+( 0.330)*r( 62)+( 0.500)*r( 64)
     &                +( 0.500)*r( 65)+         r(103)+         r(107)
     &                +( 0.900)*r(109)+( 0.900)*r(110)+( 2.000)*r(111)
     &                +         r(112)+( 0.991)*r(113)+( 0.900)*r(114)
     &                +( 0.060)*r(115)+( 0.600)*r(116)+( 0.200)*r(119)
     &                +( 0.330)*r(120)+( 0.180)*r(121)+( 0.350)*r(122)
     &                +( 1.240)*r(127)+( 1.300)*r(128)+( 0.650)*r(129)
     &                +( 1.180)*r(130)+( 0.252)*r(149)+( 0.020)*r(150)
     &                +( 0.067)*r(152)
c
        Loss(lALDX )= +         r( 99)+         r(100)+         r(101)
     &                +         r(102)
c
        Gain(lALDX )= +( 0.330)*r( 61)+( 0.330)*r( 62)+( 0.500)*r( 64)
     &                +( 0.500)*r( 65)+( 0.050)*r(114)+( 0.050)*r(115)
     &                +( 0.500)*r(116)+( 0.300)*r(119)+( 0.620)*r(120)
     &                +( 0.320)*r(121)+( 0.560)*r(122)+( 0.220)*r(124)
     &                +( 0.660)*r(127)+( 0.700)*r(128)+( 0.350)*r(129)
     &                +( 0.640)*r(130)+( 0.030)*r(140)+( 0.150)*r(146)
     &                +( 0.800)*r(147)+( 0.800)*r(148)+( 0.120)*r(149)
     &                +( 0.357)*r(151)+( 0.150)*r(153)+( 0.470)*r(154)
     &                +( 0.210)*r(155)+( 0.470)*r(156)
c
        Loss(lPAR  )= +         r(115)
c
        Gain(lPAR  )= +(-0.660)*r( 61)+(-0.660)*r( 62)+(-0.110)*r(115)
     &                +(-2.100)*r(116)+( 0.200)*r(119)+(-0.700)*r(120)
     &                +(-1.000)*r(121)+(-1.000)*r(122)+( 0.100)*r(127)
     &                +( 1.100)*r(141)+( 0.250)*r(144)+( 0.350)*r(146)
     &                +( 2.400)*r(147)+( 2.400)*r(148)+( 1.565)*r(149)
     &                +( 0.360)*r(150)+( 1.282)*r(151)+( 0.832)*r(152)
     &                +( 5.120)*r(153)+( 1.660)*r(154)+( 7.000)*r(155)
c
        Loss(lSO2  )= +         r( 63)
c
        Gain(lSO2  )= 0.0
c
        Loss(lSULF )= 0.0
c
        Gain(lSULF )= +         r( 63)
c
        Loss(lCO   )= +         r( 66)
c
        Gain(lCO   )= +         r( 74)+         r( 75)+         r( 76)
     &                +         r( 77)+         r( 78)+         r( 87)
     &                +         r(102)+( 0.200)*r(119)+( 0.330)*r(121)
     &                +         r(123)+( 0.630)*r(125)+( 0.100)*r(127)
     &                +( 0.250)*r(129)+         r(138)+( 2.000)*r(139)
     &                +( 0.690)*r(140)+         r(143)+( 0.066)*r(146)
     &                +( 0.334)*r(149)+( 0.225)*r(150)+( 0.643)*r(151)
     &                +( 0.333)*r(152)+( 0.001)*r(155)
c
        Loss(lMEO2 )= +         r( 68)+         r( 69)+( 2.000)*r( 70)
     &                +         r( 93)+         r(109)
c
        Gain(lMEO2 )= +         r( 67)+( 0.700)*r( 71)+         r( 87)
     &                +         r( 88)+( 0.900)*r( 93)+( 0.900)*r( 94)
     &                +( 2.000)*r( 95)+         r( 97)+         r( 98)
     &                +         r(102)+         r(112)
c
        Loss(lMEPX )= +         r( 71)+         r( 72)
c
        Gain(lMEPX )= +         r( 69)+         r( 82)
c
        Loss(lMEOH )= +         r( 73)
c
        Gain(lMEOH )= +( 0.630)*r( 70)
c
        Loss(lHCO3 )= +         r( 80)+         r( 81)+         r( 82)
c
        Gain(lHCO3 )= +         r( 79)
c
        Loss(lFACD )= +         r( 83)
c
        Gain(lFACD )= +         r( 81)+( 0.370)*r(125)
c
        Loss(lPACD )= +         r( 96)+         r( 97)
c
        Gain(lPACD )= +( 0.800)*r( 92)+( 0.800)*r(108)
c
        Loss(lAACD )= +         r( 98)
c
        Gain(lAACD )= +( 0.200)*r( 92)+( 0.100)*r( 93)+( 0.100)*r( 94)
     &                +( 0.200)*r(108)+( 0.100)*r(109)+( 0.100)*r(110)
c
        Loss(lCXO3 )= +         r(103)+         r(104)+         r(108)
     &                +         r(109)+         r(110)+( 2.000)*r(111)
     &                +         r(112)
c
        Gain(lCXO3 )= +         r( 99)+         r(100)+         r(101)
     &                +         r(105)+         r(106)+( 0.250)*r(144)
     &                +( 0.200)*r(146)+( 0.250)*r(149)+( 0.075)*r(151)
     &                +( 0.390)*r(155)
c
        Loss(lPANX )= +         r(105)+         r(106)+         r(107)
c
        Gain(lPANX )= +         r(104)
c
        Loss(lETHA )= +         r(113)
c
        Gain(lETHA )= 0.0
c
        Loss(lETOH )= +         r(114)
c
        Gain(lETOH )= 0.0
c
        Loss(lROR  )= +         r(116)+         r(117)+         r(118)
c
        Gain(lROR  )= +( 0.760)*r(115)+( 0.020)*r(116)
c
        Loss(lOLE  )= +         r(119)+         r(120)+         r(121)
     &                +         r(122)
c
        Gain(lOLE  )= 0.0
c
        Loss(lETH  )= +         r(123)+         r(124)+         r(125)
     &                +         r(126)
c
        Gain(lETH  )= 0.0
c
        Loss(lIOLE )= +         r(127)+         r(128)+         r(129)
     &                +         r(130)
c
        Gain(lIOLE )= 0.0
c
        Loss(lTOL  )= +         r(131)
c
        Gain(lTOL  )= 0.0
c
        Loss(lCRES )= +         r(134)+         r(135)
c
        Gain(lCRES )= +( 0.360)*r(131)+         r(133)+         r(137)
     &                +( 0.200)*r(141)
c
        Loss(lTO2  )= +         r(132)+         r(133)
c
        Gain(lTO2  )= +( 0.560)*r(131)+( 0.300)*r(141)
c
        Loss(lOPEN )= +         r(138)+         r(139)+         r(140)
c
        Gain(lOPEN )= +( 0.900)*r(132)+( 0.300)*r(134)
c
        Loss(lCRO  )= +         r(136)+         r(137)
c
        Gain(lCRO  )= +( 0.400)*r(134)+         r(135)
c
        Loss(lMGLY )= +         r(142)+         r(143)
c
        Gain(lMGLY )= +( 0.200)*r(140)+( 0.800)*r(141)+( 0.168)*r(149)
     &                +( 0.850)*r(150)
c
        Loss(lXYL  )= +         r(141)
c
        Gain(lXYL  )= 0.0
c
        Loss(lISOP )= +         r(144)+         r(145)+         r(146)
     &                +         r(147)+         r(148)
c
        Gain(lISOP )= 0.0
c
        Loss(lISPD )= +         r(149)+         r(150)+         r(151)
     &                +         r(152)
c
        Gain(lISPD )= +( 0.750)*r(144)+( 0.912)*r(145)+( 0.650)*r(146)
     &                +( 0.200)*r(147)+( 0.200)*r(148)
c
        Loss(lTERP )= +         r(153)+         r(154)+         r(155)
     &                +         r(156)
c
        Gain(lTERP )= 0.0
c
c
      return
      end

