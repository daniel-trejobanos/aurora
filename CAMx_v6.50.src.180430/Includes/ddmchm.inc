c----CAMx v6.50 180430
c
c     DDMCHM.INC sets species pointers for the DDM chemistry
c     These equivalences must be consistent with the internal
c     species lists defined in data statements in READCHM
c
c     Copyright 1996 - 2018
c     Ramboll
c
      integer lmap(NSPNAM)
c
      common /lname/ lmap
c
      integer      laacd,   lacet,   lacro
      integer      lacye,   lafg1,   lafg2
      integer      lafg3,   lald2,   laldx
      integer      lalk1,   lalk2,   lalk3
      integer      lalk4,   lalk5,   lapin
      integer      laro1,   laro2,   lb124
      integer      lbacl,   lbald,   lbd13
      integer      lbenz,     lbr,    lbr2
      integer      lbrn2,   lbrn3,    lbro
      integer      lbzc3,    lbzo,   lbzo2
      integer      lc2o3,   lcat1,   lccho
      integer       lcg1,    lcg2,    lcg3
      integer       lcg4,   lch3i,     lcl
      integer       lcl2,   lcln2,   lcln3
      integer       lclo,     lco,   lco3h
      integer      lcooh,   lcres,    lcro
      integer      lcron,   lcxo3,   lech4
      integer      lepox,   lepx2,    leth
      integer      letha,   lethe,   lethy
      integer      letoh,   lfacd,   lfmbr
      integer      lfmcl,   lform,    lgly
      integer      lglyd,   lh2o2,    lhbr
      integer      lhcho,    lhcl,   lhco3
      integer       lhg0,    lhg2,     lhi
      integer      lhio3,   lhno3,    lho2
      integer      lhobr,   lhocl,    lhoi
      integer      lhono,   lhpld,      li
      integer        li2,   li2o2,   lialk
      integer      lino2,   lino3,   lintr
      integer        lio,   liole,   liprd
      integer      liso2,   lisop,   lispd
      integer      lispx,   livoa,   livob
      integer      livod,   livog,   lixoy
      integer       lket,   lmac3,   lmacr
      integer       lmb2,   lmb2c,    lmb3
      integer       lmbc,   lmbc2,   lmco3
      integer       lmek,   lmeo2,   lmeoh
      integer      lmepx,   lmgly,    lmi2
      integer       lmib,    lmic,   lmpan
      integer       lmvk,   lmxyl,   ln2o5
      integer       lnh3,     lno,    lno2
      integer       lno3,   lnphe,    lntr
      integer      lntr1,   lntr2,      lo
      integer       lo1d,     lo3,    lo3p
      integer        loh,    loio,    lole
      integer      lole1,   lole2,   lopan
      integer      lopen,   lopo3,   loxyl
      integer      lpacd,    lpan,   lpan2
      integer      lpanx,    lpar,   lpbzn
      integer       lpna,   lprd2,   lprpa
      integer      lprpe,   lpxyl,   lr6px
      integer      lrapx,   lrcho,   lrco3
      integer      lrno3,    lro2,   lro2c
      integer      lro2x,   lro3h,   lrooh
      integer       lror,   lsesq,    lso2
      integer       lsqt,   lssbr,   lsscl
      integer      lssn3,   lsulf,   ltbuo
      integer      lterp,    lto2,    ltol
      integer      ltolu,   lvap1,   lvap2
      integer      lvap3,   lvap4,   lvas1
      integer      lvas2,   lvas3,   lvas4
      integer      lvbs1,   lvbs2,   lvbs3
      integer      lvbs4,   lvcp1,   lvcp2
      integer      lvcp3,   lvcp4,   lvfp1
      integer      lvfp2,   lvfp3,   lvfp4
      integer      lxace,   lxacr,   lxaf1
      integer      lxaf2,   lxaf3,   lxbac
      integer      lxbal,   lxcch,    lxco
      integer      lxgld,   lxgly,   lxhch
      integer      lxho2,   lxipr,   lxlo2
      integer      lxma3,   lxmac,   lxmc3
      integer      lxmek,   lxmeo,   lxmgl
      integer      lxmvk,     lxn,   lxno2
      integer       lxo2,   lxo2h,   lxo2n
      integer       lxoh,   lxopn,   lxpar
      integer      lxpd2,   lxprp,   lxrc3
      integer      lxrch,   lxrn3,   lxtbu
      integer       lxyl,   ly6px,   lyapx
      integer      lyrpx,   lzrn3,   lpso4
      integer      lpno3,   lpnh4,    lpoa
      integer      lsoa1,   lsoa2,   lsoa3
      integer      lsoa4,   lsopa,   lsopb
      integer       lpec,   lfcrs,   lfprm
      integer      lccrs,   lcprm,     lna
      integer       lpcl,   lph2o,    lpfe
      integer       lpmn,     lpk,    lpca
      integer       lpmg,  lhgiip, lhgiipc
      integer       lhgp,   lpap0,   lpap1
      integer      lpap2,   lpap3,   lpap4
      integer      lpas0,   lpas1,   lpas2
      integer      lpas3,   lpas4,   lpbs0
      integer      lpbs1,   lpbs2,   lpbs3
      integer      lpbs4,   lpcp0,   lpcp1
      integer      lpcp2,   lpcp3,   lpcp4
      integer      lpfp0,   lpfp1,   lpfp2
      integer      lpfp3,   lpfp4, lpso4_1
      integer    lpso4_2, lpso4_3, lpso4_4
      integer    lpso4_5, lpso4_6, lpso4_7
      integer    lpso4_8, lpso4_9,lpso4_10
      integer    lpno3_1, lpno3_2, lpno3_3
      integer    lpno3_4, lpno3_5, lpno3_6
      integer    lpno3_7, lpno3_8, lpno3_9
      integer   lpno3_10, lpnh4_1, lpnh4_2
      integer    lpnh4_3, lpnh4_4, lpnh4_5
      integer    lpnh4_6, lpnh4_7, lpnh4_8
      integer    lpnh4_9,lpnh4_10,  lpoa_1
      integer     lpoa_2,  lpoa_3,  lpoa_4
      integer     lpoa_5,  lpoa_6,  lpoa_7
      integer     lpoa_8,  lpoa_9, lpoa_10
      integer    lsoa1_1, lsoa1_2, lsoa1_3
      integer    lsoa1_4, lsoa1_5, lsoa1_6
      integer    lsoa1_7, lsoa1_8, lsoa1_9
      integer   lsoa1_10, lsoa2_1, lsoa2_2
      integer    lsoa2_3, lsoa2_4, lsoa2_5
      integer    lsoa2_6, lsoa2_7, lsoa2_8
      integer    lsoa2_9,lsoa2_10, lsoa3_1
      integer    lsoa3_2, lsoa3_3, lsoa3_4
      integer    lsoa3_5, lsoa3_6, lsoa3_7
      integer    lsoa3_8, lsoa3_9,lsoa3_10
      integer    lsoa4_1, lsoa4_2, lsoa4_3
      integer    lsoa4_4, lsoa4_5, lsoa4_6
      integer    lsoa4_7, lsoa4_8, lsoa4_9
      integer   lsoa4_10, lsopa_1, lsopa_2
      integer    lsopa_3, lsopa_4, lsopa_5
      integer    lsopa_6, lsopa_7, lsopa_8
      integer    lsopa_9,lsopa_10, lsopb_1
      integer    lsopb_2, lsopb_3, lsopb_4
      integer    lsopb_5, lsopb_6, lsopb_7
      integer    lsopb_8, lsopb_9,lsopb_10
      integer     lpec_1,  lpec_2,  lpec_3
      integer     lpec_4,  lpec_5,  lpec_6
      integer     lpec_7,  lpec_8,  lpec_9
      integer    lpec_10, lcrst_1, lcrst_2
      integer    lcrst_3, lcrst_4, lcrst_5
      integer    lcrst_6, lcrst_7, lcrst_8
      integer    lcrst_9,lcrst_10,   lna_1
      integer      lna_2,   lna_3,   lna_4
      integer      lna_5,   lna_6,   lna_7
      integer      lna_8,   lna_9,  lna_10
      integer     lpcl_1,  lpcl_2,  lpcl_3
      integer     lpcl_4,  lpcl_5,  lpcl_6
      integer     lpcl_7,  lpcl_8,  lpcl_9
      integer    lpcl_10, lph2o_1, lph2o_2
      integer    lph2o_3, lph2o_4, lph2o_5
      integer    lph2o_6, lph2o_7, lph2o_8
      integer    lph2o_9,lph2o_10
c
      equivalence
     &   (lmap(  1),   laacd),(lmap(  2),   lacet),(lmap(  3),   lacro),
     &   (lmap(  4),   lacye),(lmap(  5),   lafg1),(lmap(  6),   lafg2),
     &   (lmap(  7),   lafg3),(lmap(  8),   lald2),(lmap(  9),   laldx),
     &   (lmap( 10),   lalk1),(lmap( 11),   lalk2),(lmap( 12),   lalk3),
     &   (lmap( 13),   lalk4),(lmap( 14),   lalk5),(lmap( 15),   lapin),
     &   (lmap( 16),   laro1),(lmap( 17),   laro2),(lmap( 18),   lb124),
     &   (lmap( 19),   lbacl),(lmap( 20),   lbald),(lmap( 21),   lbd13),
     &   (lmap( 22),   lbenz),(lmap( 23),     lbr),(lmap( 24),    lbr2),
     &   (lmap( 25),   lbrn2),(lmap( 26),   lbrn3),(lmap( 27),    lbro),
     &   (lmap( 28),   lbzc3),(lmap( 29),    lbzo),(lmap( 30),   lbzo2),
     &   (lmap( 31),   lc2o3),(lmap( 32),   lcat1),(lmap( 33),   lccho),
     &   (lmap( 34),    lcg1),(lmap( 35),    lcg2),(lmap( 36),    lcg3),
     &   (lmap( 37),    lcg4),(lmap( 38),   lch3i),(lmap( 39),     lcl),
     &   (lmap( 40),    lcl2),(lmap( 41),   lcln2),(lmap( 42),   lcln3),
     &   (lmap( 43),    lclo),(lmap( 44),     lco),(lmap( 45),   lco3h),
     &   (lmap( 46),   lcooh),(lmap( 47),   lcres),(lmap( 48),    lcro),
     &   (lmap( 49),   lcron),(lmap( 50),   lcxo3),(lmap( 51),   lech4),
     &   (lmap( 52),   lepox),(lmap( 53),   lepx2),(lmap( 54),    leth),
     &   (lmap( 55),   letha),(lmap( 56),   lethe),(lmap( 57),   lethy),
     &   (lmap( 58),   letoh),(lmap( 59),   lfacd),(lmap( 60),   lfmbr),
     &   (lmap( 61),   lfmcl),(lmap( 62),   lform),(lmap( 63),    lgly),
     &   (lmap( 64),   lglyd),(lmap( 65),   lh2o2),(lmap( 66),    lhbr),
     &   (lmap( 67),   lhcho),(lmap( 68),    lhcl),(lmap( 69),   lhco3),
     &   (lmap( 70),    lhg0),(lmap( 71),    lhg2),(lmap( 72),     lhi),
     &   (lmap( 73),   lhio3),(lmap( 74),   lhno3),(lmap( 75),    lho2),
     &   (lmap( 76),   lhobr),(lmap( 77),   lhocl),(lmap( 78),    lhoi),
     &   (lmap( 79),   lhono),(lmap( 80),   lhpld),(lmap( 81),      li),
     &   (lmap( 82),     li2),(lmap( 83),   li2o2),(lmap( 84),   lialk),
     &   (lmap( 85),   lino2),(lmap( 86),   lino3),(lmap( 87),   lintr),
     &   (lmap( 88),     lio),(lmap( 89),   liole),(lmap( 90),   liprd),
     &   (lmap( 91),   liso2),(lmap( 92),   lisop),(lmap( 93),   lispd),
     &   (lmap( 94),   lispx),(lmap( 95),   livoa),(lmap( 96),   livob),
     &   (lmap( 97),   livod),(lmap( 98),   livog),(lmap( 99),   lixoy),
     &   (lmap(100),    lket),(lmap(101),   lmac3),(lmap(102),   lmacr),
     &   (lmap(103),    lmb2),(lmap(104),   lmb2c),(lmap(105),    lmb3),
     &   (lmap(106),    lmbc),(lmap(107),   lmbc2),(lmap(108),   lmco3),
     &   (lmap(109),    lmek),(lmap(110),   lmeo2),(lmap(111),   lmeoh),
     &   (lmap(112),   lmepx),(lmap(113),   lmgly),(lmap(114),    lmi2),
     &   (lmap(115),    lmib),(lmap(116),    lmic),(lmap(117),   lmpan),
     &   (lmap(118),    lmvk),(lmap(119),   lmxyl),(lmap(120),   ln2o5),
     &   (lmap(121),    lnh3),(lmap(122),     lno),(lmap(123),    lno2),
     &   (lmap(124),    lno3),(lmap(125),   lnphe),(lmap(126),    lntr),
     &   (lmap(127),   lntr1),(lmap(128),   lntr2),(lmap(129),      lo),
     &   (lmap(130),    lo1d),(lmap(131),     lo3),(lmap(132),    lo3p),
     &   (lmap(133),     loh),(lmap(134),    loio),(lmap(135),    lole),
     &   (lmap(136),   lole1),(lmap(137),   lole2),(lmap(138),   lopan),
     &   (lmap(139),   lopen),(lmap(140),   lopo3),(lmap(141),   loxyl),
     &   (lmap(142),   lpacd),(lmap(143),    lpan),(lmap(144),   lpan2),
     &   (lmap(145),   lpanx),(lmap(146),    lpar),(lmap(147),   lpbzn),
     &   (lmap(148),    lpna),(lmap(149),   lprd2),(lmap(150),   lprpa),
     &   (lmap(151),   lprpe),(lmap(152),   lpxyl),(lmap(153),   lr6px),
     &   (lmap(154),   lrapx),(lmap(155),   lrcho),(lmap(156),   lrco3),
     &   (lmap(157),   lrno3),(lmap(158),    lro2),(lmap(159),   lro2c),
     &   (lmap(160),   lro2x),(lmap(161),   lro3h),(lmap(162),   lrooh),
     &   (lmap(163),    lror),(lmap(164),   lsesq),(lmap(165),    lso2),
     &   (lmap(166),    lsqt),(lmap(167),   lssbr),(lmap(168),   lsscl),
     &   (lmap(169),   lssn3),(lmap(170),   lsulf),(lmap(171),   ltbuo),
     &   (lmap(172),   lterp),(lmap(173),    lto2),(lmap(174),    ltol),
     &   (lmap(175),   ltolu),(lmap(176),   lvap1),(lmap(177),   lvap2),
     &   (lmap(178),   lvap3),(lmap(179),   lvap4),(lmap(180),   lvas1),
     &   (lmap(181),   lvas2),(lmap(182),   lvas3),(lmap(183),   lvas4),
     &   (lmap(184),   lvbs1),(lmap(185),   lvbs2),(lmap(186),   lvbs3),
     &   (lmap(187),   lvbs4),(lmap(188),   lvcp1),(lmap(189),   lvcp2),
     &   (lmap(190),   lvcp3),(lmap(191),   lvcp4),(lmap(192),   lvfp1),
     &   (lmap(193),   lvfp2),(lmap(194),   lvfp3),(lmap(195),   lvfp4),
     &   (lmap(196),   lxace),(lmap(197),   lxacr),(lmap(198),   lxaf1),
     &   (lmap(199),   lxaf2),(lmap(200),   lxaf3),(lmap(201),   lxbac),
     &   (lmap(202),   lxbal),(lmap(203),   lxcch),(lmap(204),    lxco),
     &   (lmap(205),   lxgld),(lmap(206),   lxgly),(lmap(207),   lxhch),
     &   (lmap(208),   lxho2),(lmap(209),   lxipr),(lmap(210),   lxlo2),
     &   (lmap(211),   lxma3),(lmap(212),   lxmac),(lmap(213),   lxmc3),
     &   (lmap(214),   lxmek),(lmap(215),   lxmeo),(lmap(216),   lxmgl),
     &   (lmap(217),   lxmvk),(lmap(218),     lxn),(lmap(219),   lxno2),
     &   (lmap(220),    lxo2),(lmap(221),   lxo2h),(lmap(222),   lxo2n),
     &   (lmap(223),    lxoh),(lmap(224),   lxopn),(lmap(225),   lxpar),
     &   (lmap(226),   lxpd2),(lmap(227),   lxprp),(lmap(228),   lxrc3),
     &   (lmap(229),   lxrch),(lmap(230),   lxrn3),(lmap(231),   lxtbu),
     &   (lmap(232),    lxyl),(lmap(233),   ly6px),(lmap(234),   lyapx),
     &   (lmap(235),   lyrpx),(lmap(236),   lzrn3),(lmap(237),   lpso4),
     &   (lmap(238),   lpno3),(lmap(239),   lpnh4),(lmap(240),    lpoa),
     &   (lmap(241),   lsoa1),(lmap(242),   lsoa2),(lmap(243),   lsoa3),
     &   (lmap(244),   lsoa4),(lmap(245),   lsopa),(lmap(246),   lsopb),
     &   (lmap(247),    lpec),(lmap(248),   lfcrs),(lmap(249),   lfprm),
     &   (lmap(250),   lccrs),(lmap(251),   lcprm),(lmap(252),     lna),
     &   (lmap(253),    lpcl),(lmap(254),   lph2o),(lmap(255),    lpfe),
     &   (lmap(256),    lpmn),(lmap(257),     lpk),(lmap(258),    lpca),
     &   (lmap(259),    lpmg),(lmap(260),  lhgiip),(lmap(261), lhgiipc),
     &   (lmap(262),    lhgp),(lmap(263),   lpap0),(lmap(264),   lpap1),
     &   (lmap(265),   lpap2),(lmap(266),   lpap3),(lmap(267),   lpap4),
     &   (lmap(268),   lpas0),(lmap(269),   lpas1),(lmap(270),   lpas2)
      equivalence
     &   (lmap(271),   lpas3),(lmap(272),   lpas4),(lmap(273),   lpbs0),
     &   (lmap(274),   lpbs1),(lmap(275),   lpbs2),(lmap(276),   lpbs3),
     &   (lmap(277),   lpbs4),(lmap(278),   lpcp0),(lmap(279),   lpcp1),
     &   (lmap(280),   lpcp2),(lmap(281),   lpcp3),(lmap(282),   lpcp4),
     &   (lmap(283),   lpfp0),(lmap(284),   lpfp1),(lmap(285),   lpfp2),
     &   (lmap(286),   lpfp3),(lmap(287),   lpfp4),(lmap(288), lpso4_1),
     &   (lmap(289), lpso4_2),(lmap(290), lpso4_3),(lmap(291), lpso4_4),
     &   (lmap(292), lpso4_5),(lmap(293), lpso4_6),(lmap(294), lpso4_7),
     &   (lmap(295), lpso4_8),(lmap(296), lpso4_9),(lmap(297),lpso4_10),
     &   (lmap(298), lpno3_1),(lmap(299), lpno3_2),(lmap(300), lpno3_3),
     &   (lmap(301), lpno3_4),(lmap(302), lpno3_5),(lmap(303), lpno3_6),
     &   (lmap(304), lpno3_7),(lmap(305), lpno3_8),(lmap(306), lpno3_9),
     &   (lmap(307),lpno3_10),(lmap(308), lpnh4_1),(lmap(309), lpnh4_2),
     &   (lmap(310), lpnh4_3),(lmap(311), lpnh4_4),(lmap(312), lpnh4_5),
     &   (lmap(313), lpnh4_6),(lmap(314), lpnh4_7),(lmap(315), lpnh4_8),
     &   (lmap(316), lpnh4_9),(lmap(317),lpnh4_10),(lmap(318),  lpoa_1),
     &   (lmap(319),  lpoa_2),(lmap(320),  lpoa_3),(lmap(321),  lpoa_4),
     &   (lmap(322),  lpoa_5),(lmap(323),  lpoa_6),(lmap(324),  lpoa_7),
     &   (lmap(325),  lpoa_8),(lmap(326),  lpoa_9),(lmap(327), lpoa_10),
     &   (lmap(328), lsoa1_1),(lmap(329), lsoa1_2),(lmap(330), lsoa1_3),
     &   (lmap(331), lsoa1_4),(lmap(332), lsoa1_5),(lmap(333), lsoa1_6),
     &   (lmap(334), lsoa1_7),(lmap(335), lsoa1_8),(lmap(336), lsoa1_9),
     &   (lmap(337),lsoa1_10),(lmap(338), lsoa2_1),(lmap(339), lsoa2_2),
     &   (lmap(340), lsoa2_3),(lmap(341), lsoa2_4),(lmap(342), lsoa2_5),
     &   (lmap(343), lsoa2_6),(lmap(344), lsoa2_7),(lmap(345), lsoa2_8),
     &   (lmap(346), lsoa2_9),(lmap(347),lsoa2_10),(lmap(348), lsoa3_1),
     &   (lmap(349), lsoa3_2),(lmap(350), lsoa3_3),(lmap(351), lsoa3_4),
     &   (lmap(352), lsoa3_5),(lmap(353), lsoa3_6),(lmap(354), lsoa3_7),
     &   (lmap(355), lsoa3_8),(lmap(356), lsoa3_9),(lmap(357),lsoa3_10),
     &   (lmap(358), lsoa4_1),(lmap(359), lsoa4_2),(lmap(360), lsoa4_3),
     &   (lmap(361), lsoa4_4),(lmap(362), lsoa4_5),(lmap(363), lsoa4_6),
     &   (lmap(364), lsoa4_7),(lmap(365), lsoa4_8),(lmap(366), lsoa4_9),
     &   (lmap(367),lsoa4_10),(lmap(368), lsopa_1),(lmap(369), lsopa_2),
     &   (lmap(370), lsopa_3),(lmap(371), lsopa_4),(lmap(372), lsopa_5),
     &   (lmap(373), lsopa_6),(lmap(374), lsopa_7),(lmap(375), lsopa_8),
     &   (lmap(376), lsopa_9),(lmap(377),lsopa_10),(lmap(378), lsopb_1),
     &   (lmap(379), lsopb_2),(lmap(380), lsopb_3),(lmap(381), lsopb_4),
     &   (lmap(382), lsopb_5),(lmap(383), lsopb_6),(lmap(384), lsopb_7),
     &   (lmap(385), lsopb_8),(lmap(386), lsopb_9),(lmap(387),lsopb_10),
     &   (lmap(388),  lpec_1),(lmap(389),  lpec_2),(lmap(390),  lpec_3),
     &   (lmap(391),  lpec_4),(lmap(392),  lpec_5),(lmap(393),  lpec_6),
     &   (lmap(394),  lpec_7),(lmap(395),  lpec_8),(lmap(396),  lpec_9),
     &   (lmap(397), lpec_10),(lmap(398), lcrst_1),(lmap(399), lcrst_2),
     &   (lmap(400), lcrst_3),(lmap(401), lcrst_4),(lmap(402), lcrst_5),
     &   (lmap(403), lcrst_6),(lmap(404), lcrst_7),(lmap(405), lcrst_8),
     &   (lmap(406), lcrst_9),(lmap(407),lcrst_10),(lmap(408),   lna_1),
     &   (lmap(409),   lna_2),(lmap(410),   lna_3),(lmap(411),   lna_4),
     &   (lmap(412),   lna_5),(lmap(413),   lna_6),(lmap(414),   lna_7),
     &   (lmap(415),   lna_8),(lmap(416),   lna_9),(lmap(417),  lna_10),
     &   (lmap(418),  lpcl_1),(lmap(419),  lpcl_2),(lmap(420),  lpcl_3),
     &   (lmap(421),  lpcl_4),(lmap(422),  lpcl_5),(lmap(423),  lpcl_6),
     &   (lmap(424),  lpcl_7),(lmap(425),  lpcl_8),(lmap(426),  lpcl_9),
     &   (lmap(427), lpcl_10),(lmap(428), lph2o_1),(lmap(429), lph2o_2),
     &   (lmap(430), lph2o_3),(lmap(431), lph2o_4),(lmap(432), lph2o_5),
     &   (lmap(433), lph2o_6),(lmap(434), lph2o_7),(lmap(435), lph2o_8),
     &   (lmap(436), lph2o_9),(lmap(437),lph2o_10)
