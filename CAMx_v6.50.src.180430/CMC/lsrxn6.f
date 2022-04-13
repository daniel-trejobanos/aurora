      subroutine lsrxn6(H2O,M,O2,CH4,H2,y,dbrk,r)
      implicit none
c
c----CAMx v6.50 180430
c
c     LSRXN6 computes double precision fluxes for each reaction
c
c     Copyright 1996 - 2018
c     Ramboll
c     Created by the CMC version 5.2.6
c
c     Routines Called:
c        none
c
c     Called by:
c        LSRATE6
c
      include "camx.prm"
      include "chmdat.inc"
      include "ddmchm.inc"
c
      double precision H2O, M, O2, CH4, H2, N2
      double precision y(*)
      double precision dbrk(*)
      double precision r(*)
c
c --- Entry point
c
      N2  = M - O2
      r(  1) = dbrk(  1)*y(lNO2)
      r(  2) = dbrk(  2)*y(lO)*O2*M
      r(  3) = dbrk(  3)*y(lO3)*y(lNO)
      r(  4) = dbrk(  4)*y(lO)*y(lNO2)
      r(  5) = dbrk(  5)*y(lO)*y(lNO2)
      r(  6) = dbrk(  6)*y(lO)*y(lNO)
      r(  7) = dbrk(  7)*y(lNO2)*y(lO3)
      r(  8) = dbrk(  8)*y(lO3)
      r(  9) = dbrk(  9)*y(lO3)
      r( 10) = dbrk( 10)*y(lO1D)*M
      r( 11) = dbrk( 11)*y(lO1D)*H2O
      r( 12) = dbrk( 12)*y(lO3)*y(lOH)
      r( 13) = dbrk( 13)*y(lO3)*y(lHO2)
      r( 14) = dbrk( 14)*y(lNO3)
      r( 15) = dbrk( 15)*y(lNO3)
      r( 16) = dbrk( 16)*y(lNO3)*y(lNO)
      r( 17) = dbrk( 17)*y(lNO3)*y(lNO2)
      r( 18) = dbrk( 18)*y(lNO3)*y(lNO2)
      r( 19) = dbrk( 19)*y(lN2O5)*H2O
      r( 20) = dbrk( 20)*y(lN2O5)*H2O*H2O
      r( 21) = dbrk( 21)*y(lN2O5)
      r( 22) = dbrk( 22)*y(lNO)*y(lNO)*O2
      r( 23) = dbrk( 23)*y(lNO)*y(lNO2)*H2O
      r( 24) = dbrk( 24)*y(lNO)*y(lOH)
      r( 25) = dbrk( 25)*y(lHONO)
      r( 26) = dbrk( 26)*y(lOH)*y(lHONO)
      r( 27) = dbrk( 27)*y(lHONO)*y(lHONO)
      r( 28) = dbrk( 28)*y(lNO2)*y(lOH)
      r( 29) = dbrk( 29)*y(lOH)*y(lHNO3)
      r( 30) = dbrk( 30)*y(lHO2)*y(lNO)
      r( 31) = dbrk( 31)*y(lHO2)*y(lNO2)
      r( 32) = dbrk( 32)*y(lPNA)
      r( 33) = dbrk( 33)*y(lOH)*y(lPNA)
      r( 34) = dbrk( 34)*y(lHO2)*y(lHO2)
      r( 35) = dbrk( 35)*y(lHO2)*y(lHO2)*H2O
      r( 36) = dbrk( 36)*y(lH2O2)
      r( 37) = dbrk( 37)*y(lOH)*y(lH2O2)
      r( 38) = dbrk( 38)*y(lO1D)*H2
      r( 39) = dbrk( 39)*y(lOH)*H2
      r( 40) = dbrk( 40)*y(lOH)*y(lO)
      r( 41) = dbrk( 41)*y(lOH)*y(lOH)
      r( 42) = dbrk( 42)*y(lOH)*y(lOH)
      r( 43) = dbrk( 43)*y(lOH)*y(lHO2)
      r( 44) = dbrk( 44)*y(lHO2)*y(lO)
      r( 45) = dbrk( 45)*y(lH2O2)*y(lO)
      r( 46) = dbrk( 46)*y(lNO3)*y(lO)
      r( 47) = dbrk( 47)*y(lNO3)*y(lOH)
      r( 48) = dbrk( 48)*y(lNO3)*y(lHO2)
      r( 49) = dbrk( 49)*y(lNO3)*y(lO3)
      r( 50) = dbrk( 50)*y(lNO3)*y(lNO3)
      r( 51) = dbrk( 51)*y(lPNA)
      r( 52) = dbrk( 52)*y(lHNO3)
      r( 53) = dbrk( 53)*y(lN2O5)
      r( 54) = dbrk( 54)*y(lXO2)*y(lNO)
      r( 55) = dbrk( 55)*y(lXO2N)*y(lNO)
      r( 56) = dbrk( 56)*y(lXO2)*y(lHO2)
      r( 57) = dbrk( 57)*y(lXO2N)*y(lHO2)
      r( 58) = dbrk( 58)*y(lXO2)*y(lXO2)
      r( 59) = dbrk( 59)*y(lXO2N)*y(lXO2N)
      r( 60) = dbrk( 60)*y(lXO2)*y(lXO2N)
      r( 61) = dbrk( 61)*y(lNTR)*y(lOH)
      r( 62) = dbrk( 62)*y(lNTR)
      r( 63) = dbrk( 63)*y(lSO2)*y(lOH)
      r( 64) = dbrk( 64)*y(lROOH)*y(lOH)
      r( 65) = dbrk( 65)*y(lROOH)
      r( 66) = dbrk( 66)*y(lOH)*y(lCO)
      r( 67) = dbrk( 67)*y(lOH)*CH4
      r( 68) = dbrk( 68)*y(lMEO2)*y(lNO)
      r( 69) = dbrk( 69)*y(lMEO2)*y(lHO2)
      r( 70) = dbrk( 70)*y(lMEO2)*y(lMEO2)
      r( 71) = dbrk( 71)*y(lMEPX)*y(lOH)
      r( 72) = dbrk( 72)*y(lMEPX)
      r( 73) = dbrk( 73)*y(lMEOH)*y(lOH)
      r( 74) = dbrk( 74)*y(lFORM)*y(lOH)
      r( 75) = dbrk( 75)*y(lFORM)
      r( 76) = dbrk( 76)*y(lFORM)
      r( 77) = dbrk( 77)*y(lFORM)*y(lO)
      r( 78) = dbrk( 78)*y(lFORM)*y(lNO3)
      r( 79) = dbrk( 79)*y(lFORM)*y(lHO2)
      r( 80) = dbrk( 80)*y(lHCO3)
      r( 81) = dbrk( 81)*y(lHCO3)*y(lNO)
      r( 82) = dbrk( 82)*y(lHCO3)*y(lHO2)
      r( 83) = dbrk( 83)*y(lFACD)*y(lOH)
      r( 84) = dbrk( 84)*y(lALD2)*y(lO)
      r( 85) = dbrk( 85)*y(lALD2)*y(lOH)
      r( 86) = dbrk( 86)*y(lALD2)*y(lNO3)
      r( 87) = dbrk( 87)*y(lALD2)
      r( 88) = dbrk( 88)*y(lC2O3)*y(lNO)
      r( 89) = dbrk( 89)*y(lC2O3)*y(lNO2)
      r( 90) = dbrk( 90)*y(lPAN)
      r( 91) = dbrk( 91)*y(lPAN)
      r( 92) = dbrk( 92)*y(lC2O3)*y(lHO2)
      r( 93) = dbrk( 93)*y(lC2O3)*y(lMEO2)
      r( 94) = dbrk( 94)*y(lC2O3)*y(lXO2)
      r( 95) = dbrk( 95)*y(lC2O3)*y(lC2O3)
      r( 96) = dbrk( 96)*y(lPACD)*y(lOH)
      r( 97) = dbrk( 97)*y(lPACD)
      r( 98) = dbrk( 98)*y(lAACD)*y(lOH)
      r( 99) = dbrk( 99)*y(lALDX)*y(lO)
      r(100) = dbrk(100)*y(lALDX)*y(lOH)
      r(101) = dbrk(101)*y(lALDX)*y(lNO3)
      r(102) = dbrk(102)*y(lALDX)
      r(103) = dbrk(103)*y(lCXO3)*y(lNO)
      r(104) = dbrk(104)*y(lCXO3)*y(lNO2)
      r(105) = dbrk(105)*y(lPANX)
      r(106) = dbrk(106)*y(lPANX)
      r(107) = dbrk(107)*y(lPANX)*y(lOH)
      r(108) = dbrk(108)*y(lCXO3)*y(lHO2)
      r(109) = dbrk(109)*y(lCXO3)*y(lMEO2)
      r(110) = dbrk(110)*y(lCXO3)*y(lXO2)
      r(111) = dbrk(111)*y(lCXO3)*y(lCXO3)
      r(112) = dbrk(112)*y(lCXO3)*y(lC2O3)
      r(113) = dbrk(113)*y(lOH)*y(lETHA)
      r(114) = dbrk(114)*y(lOH)*y(lETOH)
      r(115) = dbrk(115)*y(lPAR)*y(lOH)
      r(116) = dbrk(116)*y(lROR)
      r(117) = dbrk(117)*y(lROR)
      r(118) = dbrk(118)*y(lROR)*y(lNO2)
      r(119) = dbrk(119)*y(lO)*y(lOLE)
      r(120) = dbrk(120)*y(lOH)*y(lOLE)
      r(121) = dbrk(121)*y(lO3)*y(lOLE)
      r(122) = dbrk(122)*y(lNO3)*y(lOLE)
      r(123) = dbrk(123)*y(lO)*y(lETH)
      r(124) = dbrk(124)*y(lOH)*y(lETH)
      r(125) = dbrk(125)*y(lO3)*y(lETH)
      r(126) = dbrk(126)*y(lNO3)*y(lETH)
      r(127) = dbrk(127)*y(lIOLE)*y(lO)
      r(128) = dbrk(128)*y(lIOLE)*y(lOH)
      r(129) = dbrk(129)*y(lIOLE)*y(lO3)
      r(130) = dbrk(130)*y(lIOLE)*y(lNO3)
      r(131) = dbrk(131)*y(lTOL)*y(lOH)
      r(132) = dbrk(132)*y(lTO2)*y(lNO)
      r(133) = dbrk(133)*y(lTO2)
      r(134) = dbrk(134)*y(lOH)*y(lCRES)
      r(135) = dbrk(135)*y(lCRES)*y(lNO3)
      r(136) = dbrk(136)*y(lCRO)*y(lNO2)
      r(137) = dbrk(137)*y(lCRO)*y(lHO2)
      r(138) = dbrk(138)*y(lOPEN)
      r(139) = dbrk(139)*y(lOPEN)*y(lOH)
      r(140) = dbrk(140)*y(lOPEN)*y(lO3)
      r(141) = dbrk(141)*y(lOH)*y(lXYL)
      r(142) = dbrk(142)*y(lOH)*y(lMGLY)
      r(143) = dbrk(143)*y(lMGLY)
      r(144) = dbrk(144)*y(lO)*y(lISOP)
      r(145) = dbrk(145)*y(lOH)*y(lISOP)
      r(146) = dbrk(146)*y(lO3)*y(lISOP)
      r(147) = dbrk(147)*y(lNO3)*y(lISOP)
      r(148) = dbrk(148)*y(lNO2)*y(lISOP)
      r(149) = dbrk(149)*y(lOH)*y(lISPD)
      r(150) = dbrk(150)*y(lO3)*y(lISPD)
      r(151) = dbrk(151)*y(lNO3)*y(lISPD)
      r(152) = dbrk(152)*y(lISPD)
      r(153) = dbrk(153)*y(lTERP)*y(lO)
      r(154) = dbrk(154)*y(lTERP)*y(lOH)
      r(155) = dbrk(155)*y(lTERP)*y(lO3)
      r(156) = dbrk(156)*y(lTERP)*y(lNO3)
c
      return
      end
