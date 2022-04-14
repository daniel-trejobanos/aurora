      subroutine lsrxn5(H2O,M,O2,CH4,H2,y,dbrk,r)
      implicit none
c
c----CAMx v7.10 210105
c
c     LSRXN5 computes double precision fluxes for each reaction
c
c     Copyright 1996 - 2021
c     Ramboll
c     Created by the CMC version 5.2.6
c
c     Routines Called:
c        none
c
c     Called by:
c        LSRATE5
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
      r(  2) = dbrk(  2)*y(lO3P)*O2*M
      r(  3) = dbrk(  3)*y(lO3P)*y(lO3)
      r(  4) = dbrk(  4)*y(lO3P)*y(lNO)
      r(  5) = dbrk(  5)*y(lO3P)*y(lNO2)
      r(  6) = dbrk(  6)*y(lO3P)*y(lNO2)
      r(  7) = dbrk(  7)*y(lO3)*y(lNO)
      r(  8) = dbrk(  8)*y(lO3)*y(lNO2)
      r(  9) = dbrk(  9)*y(lNO)*y(lNO3)
      r( 10) = dbrk( 10)*y(lNO)*y(lNO)*O2
      r( 11) = dbrk( 11)*y(lNO2)*y(lNO3)
      r( 12) = dbrk( 12)*y(lN2O5)
      r( 13) = dbrk( 13)*y(lN2O5)*H2O
      r( 14) = dbrk( 14)*y(lN2O5)*H2O*H2O
      r( 15) = dbrk( 15)*y(lNO2)*y(lNO3)
      r( 16) = dbrk( 16)*y(lNO3)
      r( 17) = dbrk( 17)*y(lNO3)
      r( 18) = dbrk( 18)*y(lO3)
      r( 19) = dbrk( 19)*y(lO3)
      r( 20) = dbrk( 20)*y(lO1D)*H2O
      r( 21) = dbrk( 21)*y(lO1D)*M
      r( 22) = dbrk( 22)*y(lOH)*y(lNO)
      r( 23) = dbrk( 23)*y(lHONO)
      r( 24) = dbrk( 24)*y(lOH)*y(lHONO)
      r( 25) = dbrk( 25)*y(lOH)*y(lNO2)
      r( 26) = dbrk( 26)*y(lOH)*y(lNO3)
      r( 27) = dbrk( 27)*y(lOH)*y(lHNO3)
      r( 28) = dbrk( 28)*y(lHNO3)
      r( 29) = dbrk( 29)*y(lOH)*y(lCO)
      r( 30) = dbrk( 30)*y(lOH)*y(lO3)
      r( 31) = dbrk( 31)*y(lHO2)*y(lNO)
      r( 32) = dbrk( 32)*y(lHO2)*y(lNO2)
      r( 33) = dbrk( 33)*y(lPNA)
      r( 34) = dbrk( 34)*y(lPNA)
      r( 35) = dbrk( 35)*y(lPNA)*y(lOH)
      r( 36) = dbrk( 36)*y(lHO2)*y(lO3)
      r( 37) = dbrk( 37)*y(lHO2)*y(lHO2)
      r( 38) = dbrk( 38)*y(lHO2)*y(lHO2)*H2O
      r( 39) = dbrk( 39)*y(lNO3)*y(lHO2)
      r( 40) = dbrk( 40)*y(lNO3)*y(lNO3)
      r( 41) = dbrk( 41)*y(lH2O2)
      r( 42) = dbrk( 42)*y(lH2O2)*y(lOH)
      r( 43) = dbrk( 43)*y(lOH)*y(lHO2)
      r( 44) = dbrk( 44)*y(lOH)*y(lSO2)
      r( 45) = dbrk( 45)*y(lOH)*H2
      r( 46) = dbrk( 46)*y(lMEO2)*y(lNO)
      r( 47) = dbrk( 47)*y(lMEO2)*y(lHO2)
      r( 48) = dbrk( 48)*y(lMEO2)*y(lHO2)
      r( 49) = dbrk( 49)*y(lMEO2)*y(lNO3)
      r( 50) = dbrk( 50)*y(lMEO2)*y(lMEO2)
      r( 51) = dbrk( 51)*y(lMEO2)*y(lMEO2)
      r( 52) = dbrk( 52)*y(lRO2C)*y(lNO)
      r( 53) = dbrk( 53)*y(lRO2C)*y(lHO2)
      r( 54) = dbrk( 54)*y(lRO2C)*y(lNO3)
      r( 55) = dbrk( 55)*y(lRO2C)*y(lMEO2)
      r( 56) = dbrk( 56)*y(lRO2C)*y(lRO2C)
      r( 57) = dbrk( 57)*y(lRO2X)*y(lNO)
      r( 58) = dbrk( 58)*y(lRO2X)*y(lHO2)
      r( 59) = dbrk( 59)*y(lRO2X)*y(lNO3)
      r( 60) = dbrk( 60)*y(lRO2X)*y(lMEO2)
      r( 61) = dbrk( 61)*y(lRO2X)*y(lRO2C)
      r( 62) = dbrk( 62)*y(lRO2X)*y(lRO2X)
      r( 63) = dbrk( 63)*y(lMCO3)*y(lNO2)
      r( 64) = dbrk( 64)*y(lPAN)
      r( 65) = dbrk( 65)*y(lPAN)
      r( 66) = dbrk( 66)*y(lMCO3)*y(lNO)
      r( 67) = dbrk( 67)*y(lMCO3)*y(lHO2)
      r( 68) = dbrk( 68)*y(lMCO3)*y(lNO3)
      r( 69) = dbrk( 69)*y(lMCO3)*y(lMEO2)
      r( 70) = dbrk( 70)*y(lMCO3)*y(lRO2C)
      r( 71) = dbrk( 71)*y(lMCO3)*y(lRO2X)
      r( 72) = dbrk( 72)*y(lMCO3)*y(lMCO3)
      r( 73) = dbrk( 73)*y(lRCO3)*y(lNO2)
      r( 74) = dbrk( 74)*y(lPAN2)
      r( 75) = dbrk( 75)*y(lPAN2)
      r( 76) = dbrk( 76)*y(lRCO3)*y(lNO)
      r( 77) = dbrk( 77)*y(lRCO3)*y(lHO2)
      r( 78) = dbrk( 78)*y(lRCO3)*y(lNO3)
      r( 79) = dbrk( 79)*y(lRCO3)*y(lMEO2)
      r( 80) = dbrk( 80)*y(lRCO3)*y(lRO2C)
      r( 81) = dbrk( 81)*y(lRCO3)*y(lRO2X)
      r( 82) = dbrk( 82)*y(lRCO3)*y(lMCO3)
      r( 83) = dbrk( 83)*y(lRCO3)*y(lRCO3)
      r( 84) = dbrk( 84)*y(lBZC3)*y(lNO2)
      r( 85) = dbrk( 85)*y(lPBZN)
      r( 86) = dbrk( 86)*y(lPBZN)
      r( 87) = dbrk( 87)*y(lBZC3)*y(lNO)
      r( 88) = dbrk( 88)*y(lBZC3)*y(lHO2)
      r( 89) = dbrk( 89)*y(lBZC3)*y(lNO3)
      r( 90) = dbrk( 90)*y(lBZC3)*y(lMEO2)
      r( 91) = dbrk( 91)*y(lBZC3)*y(lRO2C)
      r( 92) = dbrk( 92)*y(lBZC3)*y(lRO2X)
      r( 93) = dbrk( 93)*y(lBZC3)*y(lMCO3)
      r( 94) = dbrk( 94)*y(lBZC3)*y(lRCO3)
      r( 95) = dbrk( 95)*y(lBZC3)*y(lBZC3)
      r( 96) = dbrk( 96)*y(lMAC3)*y(lNO2)
      r( 97) = dbrk( 97)*y(lMPAN)
      r( 98) = dbrk( 98)*y(lMPAN)
      r( 99) = dbrk( 99)*y(lMAC3)*y(lNO)
      r(100) = dbrk(100)*y(lMAC3)*y(lHO2)
      r(101) = dbrk(101)*y(lMAC3)*y(lNO3)
      r(102) = dbrk(102)*y(lMAC3)*y(lMEO2)
      r(103) = dbrk(103)*y(lMAC3)*y(lRO2C)
      r(104) = dbrk(104)*y(lMAC3)*y(lRO2X)
      r(105) = dbrk(105)*y(lMAC3)*y(lMCO3)
      r(106) = dbrk(106)*y(lMAC3)*y(lRCO3)
      r(107) = dbrk(107)*y(lMAC3)*y(lBZC3)
      r(108) = dbrk(108)*y(lMAC3)*y(lMAC3)
      r(109) = dbrk(109)*y(lTBUO)*y(lNO2)
      r(110) = dbrk(110)*y(lTBUO)
      r(111) = dbrk(111)*y(lBZO)*y(lNO2)
      r(112) = dbrk(112)*y(lBZO)*y(lHO2)
      r(113) = dbrk(113)*y(lBZO)
      r(114) = dbrk(114)*y(lXHO2)*y(lNO)
      r(115) = dbrk(115)*y(lXHO2)*y(lHO2)
      r(116) = dbrk(116)*y(lXHO2)*y(lNO3)
      r(117) = dbrk(117)*y(lXHO2)*y(lMEO2)
      r(118) = dbrk(118)*y(lXHO2)*y(lRO2C)
      r(119) = dbrk(119)*y(lXHO2)*y(lRO2X)
      r(120) = dbrk(120)*y(lXHO2)*y(lMCO3)
      r(121) = dbrk(121)*y(lXHO2)*y(lRCO3)
      r(122) = dbrk(122)*y(lXHO2)*y(lBZC3)
      r(123) = dbrk(123)*y(lXHO2)*y(lMAC3)
      r(124) = dbrk(124)*y(lXOH)*y(lNO)
      r(125) = dbrk(125)*y(lXOH)*y(lHO2)
      r(126) = dbrk(126)*y(lXOH)*y(lNO3)
      r(127) = dbrk(127)*y(lXOH)*y(lMEO2)
      r(128) = dbrk(128)*y(lXOH)*y(lRO2C)
      r(129) = dbrk(129)*y(lXOH)*y(lRO2X)
      r(130) = dbrk(130)*y(lXOH)*y(lMCO3)
      r(131) = dbrk(131)*y(lXOH)*y(lRCO3)
      r(132) = dbrk(132)*y(lXOH)*y(lBZC3)
      r(133) = dbrk(133)*y(lXOH)*y(lMAC3)
      r(134) = dbrk(134)*y(lXNO2)*y(lNO)
      r(135) = dbrk(135)*y(lXNO2)*y(lHO2)
      r(136) = dbrk(136)*y(lXNO2)*y(lNO3)
      r(137) = dbrk(137)*y(lXNO2)*y(lMEO2)
      r(138) = dbrk(138)*y(lXNO2)*y(lRO2C)
      r(139) = dbrk(139)*y(lXNO2)*y(lRO2X)
      r(140) = dbrk(140)*y(lXNO2)*y(lMCO3)
      r(141) = dbrk(141)*y(lXNO2)*y(lRCO3)
      r(142) = dbrk(142)*y(lXNO2)*y(lBZC3)
      r(143) = dbrk(143)*y(lXNO2)*y(lMAC3)
      r(144) = dbrk(144)*y(lXMEO)*y(lNO)
      r(145) = dbrk(145)*y(lXMEO)*y(lHO2)
      r(146) = dbrk(146)*y(lXMEO)*y(lNO3)
      r(147) = dbrk(147)*y(lXMEO)*y(lMEO2)
      r(148) = dbrk(148)*y(lXMEO)*y(lRO2C)
      r(149) = dbrk(149)*y(lXMEO)*y(lRO2X)
      r(150) = dbrk(150)*y(lXMEO)*y(lMCO3)
      r(151) = dbrk(151)*y(lXMEO)*y(lRCO3)
      r(152) = dbrk(152)*y(lXMEO)*y(lBZC3)
      r(153) = dbrk(153)*y(lXMEO)*y(lMAC3)
      r(154) = dbrk(154)*y(lXMC3)*y(lNO)
      r(155) = dbrk(155)*y(lXMC3)*y(lHO2)
      r(156) = dbrk(156)*y(lXMC3)*y(lNO3)
      r(157) = dbrk(157)*y(lXMC3)*y(lMEO2)
      r(158) = dbrk(158)*y(lXMC3)*y(lRO2C)
      r(159) = dbrk(159)*y(lXMC3)*y(lRO2X)
      r(160) = dbrk(160)*y(lXMC3)*y(lMCO3)
      r(161) = dbrk(161)*y(lXMC3)*y(lRCO3)
      r(162) = dbrk(162)*y(lXMC3)*y(lBZC3)
      r(163) = dbrk(163)*y(lXMC3)*y(lMAC3)
      r(164) = dbrk(164)*y(lXRC3)*y(lNO)
      r(165) = dbrk(165)*y(lXRC3)*y(lHO2)
      r(166) = dbrk(166)*y(lXRC3)*y(lNO3)
      r(167) = dbrk(167)*y(lXRC3)*y(lMEO2)
      r(168) = dbrk(168)*y(lXRC3)*y(lRO2C)
      r(169) = dbrk(169)*y(lXRC3)*y(lRO2X)
      r(170) = dbrk(170)*y(lXRC3)*y(lMCO3)
      r(171) = dbrk(171)*y(lXRC3)*y(lRCO3)
      r(172) = dbrk(172)*y(lXRC3)*y(lBZC3)
      r(173) = dbrk(173)*y(lXRC3)*y(lMAC3)
      r(174) = dbrk(174)*y(lXMA3)*y(lNO)
      r(175) = dbrk(175)*y(lXMA3)*y(lHO2)
      r(176) = dbrk(176)*y(lXMA3)*y(lNO3)
      r(177) = dbrk(177)*y(lXMA3)*y(lMEO2)
      r(178) = dbrk(178)*y(lXMA3)*y(lRO2C)
      r(179) = dbrk(179)*y(lXMA3)*y(lRO2X)
      r(180) = dbrk(180)*y(lXMA3)*y(lMCO3)
      r(181) = dbrk(181)*y(lXMA3)*y(lRCO3)
      r(182) = dbrk(182)*y(lXMA3)*y(lBZC3)
      r(183) = dbrk(183)*y(lXMA3)*y(lMAC3)
      r(184) = dbrk(184)*y(lXTBU)*y(lNO)
      r(185) = dbrk(185)*y(lXTBU)*y(lHO2)
      r(186) = dbrk(186)*y(lXTBU)*y(lNO3)
      r(187) = dbrk(187)*y(lXTBU)*y(lMEO2)
      r(188) = dbrk(188)*y(lXTBU)*y(lRO2C)
      r(189) = dbrk(189)*y(lXTBU)*y(lRO2X)
      r(190) = dbrk(190)*y(lXTBU)*y(lMCO3)
      r(191) = dbrk(191)*y(lXTBU)*y(lRCO3)
      r(192) = dbrk(192)*y(lXTBU)*y(lBZC3)
      r(193) = dbrk(193)*y(lXTBU)*y(lMAC3)
      r(194) = dbrk(194)*y(lXCO)*y(lNO)
      r(195) = dbrk(195)*y(lXCO)*y(lHO2)
      r(196) = dbrk(196)*y(lXCO)*y(lNO3)
      r(197) = dbrk(197)*y(lXCO)*y(lMEO2)
      r(198) = dbrk(198)*y(lXCO)*y(lRO2C)
      r(199) = dbrk(199)*y(lXCO)*y(lRO2X)
      r(200) = dbrk(200)*y(lXCO)*y(lMCO3)
      r(201) = dbrk(201)*y(lXCO)*y(lRCO3)
      r(202) = dbrk(202)*y(lXCO)*y(lBZC3)
      r(203) = dbrk(203)*y(lXCO)*y(lMAC3)
      r(204) = dbrk(204)*y(lHCHO)
      r(205) = dbrk(205)*y(lHCHO)
      r(206) = dbrk(206)*y(lHCHO)*y(lOH)
      r(207) = dbrk(207)*y(lHCHO)*y(lNO3)
      r(208) = dbrk(208)*y(lCCHO)*y(lOH)
      r(209) = dbrk(209)*y(lCCHO)
      r(210) = dbrk(210)*y(lCCHO)*y(lNO3)
      r(211) = dbrk(211)*y(lRCHO)*y(lOH)
      r(212) = dbrk(212)*y(lRCHO)
      r(213) = dbrk(213)*y(lRCHO)*y(lNO3)
      r(214) = dbrk(214)*y(lACET)*y(lOH)
      r(215) = dbrk(215)*y(lACET)
      r(216) = dbrk(216)*y(lMEK)*y(lOH)
      r(217) = dbrk(217)*y(lMEK)
      r(218) = dbrk(218)*y(lMEOH)*y(lOH)
      r(219) = dbrk(219)*y(lFACD)*y(lOH)
      r(220) = dbrk(220)*y(lAACD)*y(lOH)
      r(221) = dbrk(221)*y(lPACD)*y(lOH)
      r(222) = dbrk(222)*y(lCOOH)*y(lOH)
      r(223) = dbrk(223)*y(lCOOH)
      r(224) = dbrk(224)*y(lROOH)*y(lOH)
      r(225) = dbrk(225)*y(lROOH)
      r(226) = dbrk(226)*y(lR6PX)*y(lOH)
      r(227) = dbrk(227)*y(lR6PX)
      r(228) = dbrk(228)*y(lRAPX)*y(lOH)
      r(229) = dbrk(229)*y(lRAPX)
      r(230) = dbrk(230)*y(lGLY)
      r(231) = dbrk(231)*y(lGLY)
      r(232) = dbrk(232)*y(lGLY)*y(lOH)
      r(233) = dbrk(233)*y(lGLY)*y(lNO3)
      r(234) = dbrk(234)*y(lMGLY)
      r(235) = dbrk(235)*y(lMGLY)*y(lOH)
      r(236) = dbrk(236)*y(lMGLY)*y(lNO3)
      r(237) = dbrk(237)*y(lBACL)
      r(238) = dbrk(238)*y(lCRES)*y(lOH)
      r(239) = dbrk(239)*y(lCRES)*y(lNO3)
      r(240) = dbrk(240)*y(lNPHE)*y(lOH)
      r(241) = dbrk(241)*y(lNPHE)
      r(242) = dbrk(242)*y(lNPHE)
      r(243) = dbrk(243)*y(lBALD)*y(lOH)
      r(244) = dbrk(244)*y(lBALD)
      r(245) = dbrk(245)*y(lBALD)*y(lNO3)
      r(246) = dbrk(246)*y(lAFG1)*y(lOH)
      r(247) = dbrk(247)*y(lAFG1)*y(lO3)
      r(248) = dbrk(248)*y(lAFG1)
      r(249) = dbrk(249)*y(lAFG2)*y(lOH)
      r(250) = dbrk(250)*y(lAFG2)*y(lO3)
      r(251) = dbrk(251)*y(lAFG2)
      r(252) = dbrk(252)*y(lAFG3)*y(lOH)
      r(253) = dbrk(253)*y(lAFG3)*y(lO3)
      r(254) = dbrk(254)*y(lMACR)*y(lOH)
      r(255) = dbrk(255)*y(lMACR)*y(lO3)
      r(256) = dbrk(256)*y(lMACR)*y(lNO3)
      r(257) = dbrk(257)*y(lMACR)*y(lO3P)
      r(258) = dbrk(258)*y(lMACR)
      r(259) = dbrk(259)*y(lMVK)*y(lOH)
      r(260) = dbrk(260)*y(lMVK)*y(lO3)
      r(261) = dbrk(261)*y(lMVK)*y(lO3P)
      r(262) = dbrk(262)*y(lMVK)
      r(263) = dbrk(263)*y(lIPRD)*y(lOH)
      r(264) = dbrk(264)*y(lIPRD)*y(lO3)
      r(265) = dbrk(265)*y(lIPRD)*y(lNO3)
      r(266) = dbrk(266)*y(lIPRD)
      r(267) = dbrk(267)*y(lPRD2)*y(lOH)
      r(268) = dbrk(268)*y(lPRD2)
      r(269) = dbrk(269)*y(lRNO3)*y(lOH)
      r(270) = dbrk(270)*y(lRNO3)
      r(271) = dbrk(271)*y(lGLYD)*y(lOH)
      r(272) = dbrk(272)*y(lGLYD)
      r(273) = dbrk(273)*y(lGLYD)*y(lNO3)
      r(274) = dbrk(274)*y(lACRO)*y(lOH)
      r(275) = dbrk(275)*y(lACRO)*y(lO3)
      r(276) = dbrk(276)*y(lACRO)*y(lNO3)
      r(277) = dbrk(277)*y(lACRO)*y(lO3P)
      r(278) = dbrk(278)*y(lACRO)
      r(279) = dbrk(279)*y(lCO3H)*y(lOH)
      r(280) = dbrk(280)*y(lCO3H)
      r(281) = dbrk(281)*y(lRO3H)*y(lOH)
      r(282) = dbrk(282)*y(lRO3H)
      r(283) = dbrk(283)*y(lXHCH)*y(lNO)
      r(284) = dbrk(284)*y(lXHCH)*y(lHO2)
      r(285) = dbrk(285)*y(lXHCH)*y(lNO3)
      r(286) = dbrk(286)*y(lXHCH)*y(lMEO2)
      r(287) = dbrk(287)*y(lXHCH)*y(lRO2C)
      r(288) = dbrk(288)*y(lXHCH)*y(lRO2X)
      r(289) = dbrk(289)*y(lXHCH)*y(lMCO3)
      r(290) = dbrk(290)*y(lXHCH)*y(lRCO3)
      r(291) = dbrk(291)*y(lXHCH)*y(lBZC3)
      r(292) = dbrk(292)*y(lXHCH)*y(lMAC3)
      r(293) = dbrk(293)*y(lXCCH)*y(lNO)
      r(294) = dbrk(294)*y(lXCCH)*y(lHO2)
      r(295) = dbrk(295)*y(lXCCH)*y(lNO3)
      r(296) = dbrk(296)*y(lXCCH)*y(lMEO2)
      r(297) = dbrk(297)*y(lXCCH)*y(lRO2C)
      r(298) = dbrk(298)*y(lXCCH)*y(lRO2X)
      r(299) = dbrk(299)*y(lXCCH)*y(lMCO3)
      r(300) = dbrk(300)*y(lXCCH)*y(lRCO3)
      r(301) = dbrk(301)*y(lXCCH)*y(lBZC3)
      r(302) = dbrk(302)*y(lXCCH)*y(lMAC3)
      r(303) = dbrk(303)*y(lXRCH)*y(lNO)
      r(304) = dbrk(304)*y(lXRCH)*y(lHO2)
      r(305) = dbrk(305)*y(lXRCH)*y(lNO3)
      r(306) = dbrk(306)*y(lXRCH)*y(lMEO2)
      r(307) = dbrk(307)*y(lXRCH)*y(lRO2C)
      r(308) = dbrk(308)*y(lXRCH)*y(lRO2X)
      r(309) = dbrk(309)*y(lXRCH)*y(lMCO3)
      r(310) = dbrk(310)*y(lXRCH)*y(lRCO3)
      r(311) = dbrk(311)*y(lXRCH)*y(lBZC3)
      r(312) = dbrk(312)*y(lXRCH)*y(lMAC3)
      r(313) = dbrk(313)*y(lXACE)*y(lNO)
      r(314) = dbrk(314)*y(lXACE)*y(lHO2)
      r(315) = dbrk(315)*y(lXACE)*y(lNO3)
      r(316) = dbrk(316)*y(lXACE)*y(lMEO2)
      r(317) = dbrk(317)*y(lXACE)*y(lRO2C)
      r(318) = dbrk(318)*y(lXACE)*y(lRO2X)
      r(319) = dbrk(319)*y(lXACE)*y(lMCO3)
      r(320) = dbrk(320)*y(lXACE)*y(lRCO3)
      r(321) = dbrk(321)*y(lXACE)*y(lBZC3)
      r(322) = dbrk(322)*y(lXACE)*y(lMAC3)
      r(323) = dbrk(323)*y(lXMEK)*y(lNO)
      r(324) = dbrk(324)*y(lXMEK)*y(lHO2)
      r(325) = dbrk(325)*y(lXMEK)*y(lNO3)
      r(326) = dbrk(326)*y(lXMEK)*y(lMEO2)
      r(327) = dbrk(327)*y(lXMEK)*y(lRO2C)
      r(328) = dbrk(328)*y(lXMEK)*y(lRO2X)
      r(329) = dbrk(329)*y(lXMEK)*y(lMCO3)
      r(330) = dbrk(330)*y(lXMEK)*y(lRCO3)
      r(331) = dbrk(331)*y(lXMEK)*y(lBZC3)
      r(332) = dbrk(332)*y(lXMEK)*y(lMAC3)
      r(333) = dbrk(333)*y(lXPD2)*y(lNO)
      r(334) = dbrk(334)*y(lXPD2)*y(lHO2)
      r(335) = dbrk(335)*y(lXPD2)*y(lNO3)
      r(336) = dbrk(336)*y(lXPD2)*y(lMEO2)
      r(337) = dbrk(337)*y(lXPD2)*y(lRO2C)
      r(338) = dbrk(338)*y(lXPD2)*y(lRO2X)
      r(339) = dbrk(339)*y(lXPD2)*y(lMCO3)
      r(340) = dbrk(340)*y(lXPD2)*y(lRCO3)
      r(341) = dbrk(341)*y(lXPD2)*y(lBZC3)
      r(342) = dbrk(342)*y(lXPD2)*y(lMAC3)
      r(343) = dbrk(343)*y(lXGLY)*y(lNO)
      r(344) = dbrk(344)*y(lXGLY)*y(lHO2)
      r(345) = dbrk(345)*y(lXGLY)*y(lNO3)
      r(346) = dbrk(346)*y(lXGLY)*y(lMEO2)
      r(347) = dbrk(347)*y(lXGLY)*y(lRO2C)
      r(348) = dbrk(348)*y(lXGLY)*y(lRO2X)
      r(349) = dbrk(349)*y(lXGLY)*y(lMCO3)
      r(350) = dbrk(350)*y(lXGLY)*y(lRCO3)
      r(351) = dbrk(351)*y(lXGLY)*y(lBZC3)
      r(352) = dbrk(352)*y(lXGLY)*y(lMAC3)
      r(353) = dbrk(353)*y(lXMGL)*y(lNO)
      r(354) = dbrk(354)*y(lXMGL)*y(lHO2)
      r(355) = dbrk(355)*y(lXMGL)*y(lNO3)
      r(356) = dbrk(356)*y(lXMGL)*y(lMEO2)
      r(357) = dbrk(357)*y(lXMGL)*y(lRO2C)
      r(358) = dbrk(358)*y(lXMGL)*y(lRO2X)
      r(359) = dbrk(359)*y(lXMGL)*y(lMCO3)
      r(360) = dbrk(360)*y(lXMGL)*y(lRCO3)
      r(361) = dbrk(361)*y(lXMGL)*y(lBZC3)
      r(362) = dbrk(362)*y(lXMGL)*y(lMAC3)
      r(363) = dbrk(363)*y(lXBAC)*y(lNO)
      r(364) = dbrk(364)*y(lXBAC)*y(lHO2)
      r(365) = dbrk(365)*y(lXBAC)*y(lNO3)
      r(366) = dbrk(366)*y(lXBAC)*y(lMEO2)
      r(367) = dbrk(367)*y(lXBAC)*y(lRO2C)
      r(368) = dbrk(368)*y(lXBAC)*y(lRO2X)
      r(369) = dbrk(369)*y(lXBAC)*y(lMCO3)
      r(370) = dbrk(370)*y(lXBAC)*y(lRCO3)
      r(371) = dbrk(371)*y(lXBAC)*y(lBZC3)
      r(372) = dbrk(372)*y(lXBAC)*y(lMAC3)
      r(373) = dbrk(373)*y(lXBAL)*y(lNO)
      r(374) = dbrk(374)*y(lXBAL)*y(lHO2)
      r(375) = dbrk(375)*y(lXBAL)*y(lNO3)
      r(376) = dbrk(376)*y(lXBAL)*y(lMEO2)
      r(377) = dbrk(377)*y(lXBAL)*y(lRO2C)
      r(378) = dbrk(378)*y(lXBAL)*y(lRO2X)
      r(379) = dbrk(379)*y(lXBAL)*y(lMCO3)
      r(380) = dbrk(380)*y(lXBAL)*y(lRCO3)
      r(381) = dbrk(381)*y(lXBAL)*y(lBZC3)
      r(382) = dbrk(382)*y(lXBAL)*y(lMAC3)
      r(383) = dbrk(383)*y(lXAF1)*y(lNO)
      r(384) = dbrk(384)*y(lXAF1)*y(lHO2)
      r(385) = dbrk(385)*y(lXAF1)*y(lNO3)
      r(386) = dbrk(386)*y(lXAF1)*y(lMEO2)
      r(387) = dbrk(387)*y(lXAF1)*y(lRO2C)
      r(388) = dbrk(388)*y(lXAF1)*y(lRO2X)
      r(389) = dbrk(389)*y(lXAF1)*y(lMCO3)
      r(390) = dbrk(390)*y(lXAF1)*y(lRCO3)
      r(391) = dbrk(391)*y(lXAF1)*y(lBZC3)
      r(392) = dbrk(392)*y(lXAF1)*y(lMAC3)
      r(393) = dbrk(393)*y(lXAF2)*y(lNO)
      r(394) = dbrk(394)*y(lXAF2)*y(lHO2)
      r(395) = dbrk(395)*y(lXAF2)*y(lNO3)
      r(396) = dbrk(396)*y(lXAF2)*y(lMEO2)
      r(397) = dbrk(397)*y(lXAF2)*y(lRO2C)
      r(398) = dbrk(398)*y(lXAF2)*y(lRO2X)
      r(399) = dbrk(399)*y(lXAF2)*y(lMCO3)
      r(400) = dbrk(400)*y(lXAF2)*y(lRCO3)
      r(401) = dbrk(401)*y(lXAF2)*y(lBZC3)
      r(402) = dbrk(402)*y(lXAF2)*y(lMAC3)
      r(403) = dbrk(403)*y(lXAF3)*y(lNO)
      r(404) = dbrk(404)*y(lXAF3)*y(lHO2)
      r(405) = dbrk(405)*y(lXAF3)*y(lNO3)
      r(406) = dbrk(406)*y(lXAF3)*y(lMEO2)
      r(407) = dbrk(407)*y(lXAF3)*y(lRO2C)
      r(408) = dbrk(408)*y(lXAF3)*y(lRO2X)
      r(409) = dbrk(409)*y(lXAF3)*y(lMCO3)
      r(410) = dbrk(410)*y(lXAF3)*y(lRCO3)
      r(411) = dbrk(411)*y(lXAF3)*y(lBZC3)
      r(412) = dbrk(412)*y(lXAF3)*y(lMAC3)
      r(413) = dbrk(413)*y(lXMAC)*y(lNO)
      r(414) = dbrk(414)*y(lXMAC)*y(lHO2)
      r(415) = dbrk(415)*y(lXMAC)*y(lNO3)
      r(416) = dbrk(416)*y(lXMAC)*y(lMEO2)
      r(417) = dbrk(417)*y(lXMAC)*y(lRO2C)
      r(418) = dbrk(418)*y(lXMAC)*y(lRO2X)
      r(419) = dbrk(419)*y(lXMAC)*y(lMCO3)
      r(420) = dbrk(420)*y(lXMAC)*y(lRCO3)
      r(421) = dbrk(421)*y(lXMAC)*y(lBZC3)
      r(422) = dbrk(422)*y(lXMAC)*y(lMAC3)
      r(423) = dbrk(423)*y(lXMVK)*y(lNO)
      r(424) = dbrk(424)*y(lXMVK)*y(lHO2)
      r(425) = dbrk(425)*y(lXMVK)*y(lNO3)
      r(426) = dbrk(426)*y(lXMVK)*y(lMEO2)
      r(427) = dbrk(427)*y(lXMVK)*y(lRO2C)
      r(428) = dbrk(428)*y(lXMVK)*y(lRO2X)
      r(429) = dbrk(429)*y(lXMVK)*y(lMCO3)
      r(430) = dbrk(430)*y(lXMVK)*y(lRCO3)
      r(431) = dbrk(431)*y(lXMVK)*y(lBZC3)
      r(432) = dbrk(432)*y(lXMVK)*y(lMAC3)
      r(433) = dbrk(433)*y(lXIPR)*y(lNO)
      r(434) = dbrk(434)*y(lXIPR)*y(lHO2)
      r(435) = dbrk(435)*y(lXIPR)*y(lNO3)
      r(436) = dbrk(436)*y(lXIPR)*y(lMEO2)
      r(437) = dbrk(437)*y(lXIPR)*y(lRO2C)
      r(438) = dbrk(438)*y(lXIPR)*y(lRO2X)
      r(439) = dbrk(439)*y(lXIPR)*y(lMCO3)
      r(440) = dbrk(440)*y(lXIPR)*y(lRCO3)
      r(441) = dbrk(441)*y(lXIPR)*y(lBZC3)
      r(442) = dbrk(442)*y(lXIPR)*y(lMAC3)
      r(443) = dbrk(443)*y(lXRN3)*y(lNO)
      r(444) = dbrk(444)*y(lXRN3)*y(lHO2)
      r(445) = dbrk(445)*y(lXRN3)*y(lNO3)
      r(446) = dbrk(446)*y(lXRN3)*y(lMEO2)
      r(447) = dbrk(447)*y(lXRN3)*y(lRO2C)
      r(448) = dbrk(448)*y(lXRN3)*y(lRO2X)
      r(449) = dbrk(449)*y(lXRN3)*y(lMCO3)
      r(450) = dbrk(450)*y(lXRN3)*y(lRCO3)
      r(451) = dbrk(451)*y(lXRN3)*y(lBZC3)
      r(452) = dbrk(452)*y(lXRN3)*y(lMAC3)
      r(453) = dbrk(453)*y(lYRPX)*y(lNO)
      r(454) = dbrk(454)*y(lYRPX)*y(lHO2)
      r(455) = dbrk(455)*y(lYRPX)*y(lNO3)
      r(456) = dbrk(456)*y(lYRPX)*y(lMEO2)
      r(457) = dbrk(457)*y(lYRPX)*y(lRO2C)
      r(458) = dbrk(458)*y(lYRPX)*y(lRO2X)
      r(459) = dbrk(459)*y(lYRPX)*y(lMCO3)
      r(460) = dbrk(460)*y(lYRPX)*y(lRCO3)
      r(461) = dbrk(461)*y(lYRPX)*y(lBZC3)
      r(462) = dbrk(462)*y(lYRPX)*y(lMAC3)
      r(463) = dbrk(463)*y(lY6PX)*y(lNO)
      r(464) = dbrk(464)*y(lY6PX)*y(lHO2)
      r(465) = dbrk(465)*y(lY6PX)*y(lNO3)
      r(466) = dbrk(466)*y(lY6PX)*y(lMEO2)
      r(467) = dbrk(467)*y(lY6PX)*y(lRO2C)
      r(468) = dbrk(468)*y(lY6PX)*y(lRO2X)
      r(469) = dbrk(469)*y(lY6PX)*y(lMCO3)
      r(470) = dbrk(470)*y(lY6PX)*y(lRCO3)
      r(471) = dbrk(471)*y(lY6PX)*y(lBZC3)
      r(472) = dbrk(472)*y(lY6PX)*y(lMAC3)
      r(473) = dbrk(473)*y(lYAPX)*y(lNO)
      r(474) = dbrk(474)*y(lYAPX)*y(lHO2)
      r(475) = dbrk(475)*y(lYAPX)*y(lNO3)
      r(476) = dbrk(476)*y(lYAPX)*y(lMEO2)
      r(477) = dbrk(477)*y(lYAPX)*y(lRO2C)
      r(478) = dbrk(478)*y(lYAPX)*y(lRO2X)
      r(479) = dbrk(479)*y(lYAPX)*y(lMCO3)
      r(480) = dbrk(480)*y(lYAPX)*y(lRCO3)
      r(481) = dbrk(481)*y(lYAPX)*y(lBZC3)
      r(482) = dbrk(482)*y(lYAPX)*y(lMAC3)
      r(483) = dbrk(483)*y(lZRN3)*y(lNO)
      r(484) = dbrk(484)*y(lZRN3)*y(lHO2)
      r(485) = dbrk(485)*y(lZRN3)*y(lNO3)
      r(486) = dbrk(486)*y(lZRN3)*y(lMEO2)
      r(487) = dbrk(487)*y(lZRN3)*y(lRO2C)
      r(488) = dbrk(488)*y(lZRN3)*y(lRO2X)
      r(489) = dbrk(489)*y(lZRN3)*y(lMCO3)
      r(490) = dbrk(490)*y(lZRN3)*y(lRCO3)
      r(491) = dbrk(491)*y(lZRN3)*y(lBZC3)
      r(492) = dbrk(492)*y(lZRN3)*y(lMAC3)
      r(493) = dbrk(493)*y(lXGLD)*y(lNO)
      r(494) = dbrk(494)*y(lXGLD)*y(lHO2)
      r(495) = dbrk(495)*y(lXGLD)*y(lNO3)
      r(496) = dbrk(496)*y(lXGLD)*y(lMEO2)
      r(497) = dbrk(497)*y(lXGLD)*y(lRO2C)
      r(498) = dbrk(498)*y(lXGLD)*y(lRO2X)
      r(499) = dbrk(499)*y(lXGLD)*y(lMCO3)
      r(500) = dbrk(500)*y(lXGLD)*y(lRCO3)
      r(501) = dbrk(501)*y(lXGLD)*y(lBZC3)
      r(502) = dbrk(502)*y(lXGLD)*y(lMAC3)
      r(503) = dbrk(503)*y(lXACR)*y(lNO)
      r(504) = dbrk(504)*y(lXACR)*y(lHO2)
      r(505) = dbrk(505)*y(lXACR)*y(lNO3)
      r(506) = dbrk(506)*y(lXACR)*y(lMEO2)
      r(507) = dbrk(507)*y(lXACR)*y(lRO2C)
      r(508) = dbrk(508)*y(lXACR)*y(lRO2X)
      r(509) = dbrk(509)*y(lXACR)*y(lMCO3)
      r(510) = dbrk(510)*y(lXACR)*y(lRCO3)
      r(511) = dbrk(511)*y(lXACR)*y(lBZC3)
      r(512) = dbrk(512)*y(lXACR)*y(lMAC3)
      r(513) = dbrk(513)*CH4*y(lOH)
      r(514) = dbrk(514)*y(lETHE)*y(lOH)
      r(515) = dbrk(515)*y(lETHE)*y(lO3)
      r(516) = dbrk(516)*y(lETHE)*y(lNO3)
      r(517) = dbrk(517)*y(lETHE)*y(lO3P)
      r(518) = dbrk(518)*y(lPRPE)*y(lOH)
      r(519) = dbrk(519)*y(lPRPE)*y(lO3)
      r(520) = dbrk(520)*y(lPRPE)*y(lNO3)
      r(521) = dbrk(521)*y(lPRPE)*y(lO3P)
      r(522) = dbrk(522)*y(lBD13)*y(lOH)
      r(523) = dbrk(523)*y(lBD13)*y(lO3)
      r(524) = dbrk(524)*y(lBD13)*y(lNO3)
      r(525) = dbrk(525)*y(lBD13)*y(lO3P)
      r(526) = dbrk(526)*y(lISOP)*y(lOH)
      r(527) = dbrk(527)*y(lISOP)*y(lO3)
      r(528) = dbrk(528)*y(lISOP)*y(lNO3)
      r(529) = dbrk(529)*y(lISOP)*y(lO3P)
      r(530) = dbrk(530)*y(lAPIN)*y(lOH)
      r(531) = dbrk(531)*y(lAPIN)*y(lO3)
      r(532) = dbrk(532)*y(lAPIN)*y(lNO3)
      r(533) = dbrk(533)*y(lAPIN)*y(lO3P)
      r(534) = dbrk(534)*y(lACYE)*y(lOH)
      r(535) = dbrk(535)*y(lACYE)*y(lO3)
      r(536) = dbrk(536)*y(lBENZ)*y(lOH)
      r(537) = dbrk(537)*y(lTOLU)*y(lOH)
      r(538) = dbrk(538)*y(lMXYL)*y(lOH)
      r(539) = dbrk(539)*y(lOXYL)*y(lOH)
      r(540) = dbrk(540)*y(lPXYL)*y(lOH)
      r(541) = dbrk(541)*y(lB124)*y(lOH)
      r(542) = dbrk(542)*y(lETOH)*y(lOH)
      r(543) = dbrk(543)*y(lALK1)*y(lOH)
      r(544) = dbrk(544)*y(lALK2)*y(lOH)
      r(545) = dbrk(545)*y(lALK3)*y(lOH)
      r(546) = dbrk(546)*y(lALK4)*y(lOH)
      r(547) = dbrk(547)*y(lALK5)*y(lOH)
      r(548) = dbrk(548)*y(lOLE1)*y(lOH)
      r(549) = dbrk(549)*y(lOLE1)*y(lO3)
      r(550) = dbrk(550)*y(lOLE1)*y(lNO3)
      r(551) = dbrk(551)*y(lOLE1)*y(lO3P)
      r(552) = dbrk(552)*y(lOLE2)*y(lOH)
      r(553) = dbrk(553)*y(lOLE2)*y(lO3)
      r(554) = dbrk(554)*y(lOLE2)*y(lNO3)
      r(555) = dbrk(555)*y(lOLE2)*y(lO3P)
      r(556) = dbrk(556)*y(lARO1)*y(lOH)
      r(557) = dbrk(557)*y(lARO2)*y(lOH)
      r(558) = dbrk(558)*y(lTERP)*y(lOH)
      r(559) = dbrk(559)*y(lTERP)*y(lO3)
      r(560) = dbrk(560)*y(lTERP)*y(lNO3)
      r(561) = dbrk(561)*y(lTERP)*y(lO3P)
      r(562) = dbrk(562)*y(lSESQ)*y(lOH)
      r(563) = dbrk(563)*y(lSESQ)*y(lO3)
      r(564) = dbrk(564)*y(lSESQ)*y(lNO3)
      r(565) = dbrk(565)*y(lSESQ)*y(lO3P)
c
      return
      end
