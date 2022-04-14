c----CAMx v7.10 210105
c
c     DDMCHM.INC sets species pointers for the DDM chemistry
c     These equivalences must be consistent with the internal
c     species lists defined in data statements in READCHM
c
c     Copyright 1996 - 2021
c     Ramboll
c
      integer lmap(NSPNAM)
c
      common /lname/ lmap
c
      integer         lo,    lo1d,     loh
      integer       lho2,   lbzo2,   lc2o3
      integer       lcro,   lcxo3,   lepx2
      integer      lhco3,   liso2,   lmeo2
      integer      lopo3,    lro2,    lror
      integer       lto2,   lxlo2,    lxo2
      integer      lxo2h,   lxo2n,     lbr
      integer       lbro,     lcl,    lclo
      integer         li,     lio,    loio
      integer      lxpar,   lxprp,   laacd
      integer      lacet,   lald2,   laldx
      integer      lbenz,   lcat1,     lco
      integer      lcres,   lcron,   lepox
      integer       leth,   letha,   lethy
      integer      letoh,   lfacd,   lform
      integer       lgly,   lglyd,   lh2o2
      integer      lhno3,   lhono,   lhpld
      integer      lintr,   liole,   lisop
      integer      lispd,   lispx,    lket
      integer      lmeoh,   lmepx,   lmgly
      integer      ln2o5,     lno,    lno2
      integer       lno3,   lntr1,   lntr2
      integer        lo3,    lole,   lopan
      integer      lopen,   lpacd,    lpan
      integer      lpanx,    lpar,    lpna
      integer      lprpa,   lrooh,    lso2
      integer      lsulf,   lterp,    ltol
      integer      lxopn,    lxyl,   lech4
      integer        li2,   li2o2,   lixoy
      integer       lhoi,   lhio3,   lino3
      integer       lbr2,   lbrn2,   lbrn3
      integer      lch3i,    lcl2,   lcln2
      integer      lcln3,   lfmbr,   lfmcl
      integer       lhbr,    lhcl,    lhg0
      integer       lhg2,     lhi,   lhobr
      integer      lhocl,   lialk,   lino2
      integer       lmb2,   lmb2c,    lmb3
      integer       lmbc,   lmbc2,    lmi2
      integer       lmib,    lmic,    lnh3
      integer       lsqt,   lssbr,   lsscl
      integer      lssn3,    ldms,    lntr
      integer       lcg1,    lcg2,    lcg3
      integer       lcg4,   livoa,   livob
      integer      lpso4,   lpno3,   lpnh4
      integer       lpoa,   lsoa1,   lsoa2
      integer      lsoa3,   lsoa4,   lsopa
      integer      lsopb,    lpec,   lfcrs
      integer      lfprm,   lccrs,   lcprm
      integer        lna,    lpcl,   lph2o
      integer       lpfe,    lpmn,     lpk
      integer       lpca,    lpmg,    lpal
      integer       lpsi,    lpti,    lo3p
      integer      lbzc3,    lbzo,   lmac3
      integer      lmco3,   lrco3,   lro2c
      integer      lro2x,   ltbuo,   lxace
      integer      lxacr,   lxaf1,   lxaf2
      integer      lxaf3,   lxbac,   lxbal
      integer      lxcch,    lxco,   lxgld
      integer      lxgly,   lxhch,   lxho2
      integer      lxipr,   lxma3,   lxmac
      integer      lxmc3,   lxmek,   lxmeo
      integer      lxmgl,   lxmvk,   lxno2
      integer       lxoh,   lxpd2,   lxrc3
      integer      lxrch,   lxrn3,   lxtbu
      integer      ly6px,   lyapx,   lyrpx
      integer      lzrn3,   lacro,   lacye
      integer      lafg1,   lafg2,   lafg3
      integer      lalk1,   lalk2,   lalk3
      integer      lalk4,   lalk5,   lapin
      integer      laro1,   laro2,   lb124
      integer      lbacl,   lbald,   lbd13
      integer      lccho,   lco3h,   lcooh
      integer      lethe,   lhcho,   liprd
      integer      lmacr,    lmek,   lmpan
      integer       lmvk,   lmxyl,   lnphe
      integer      lole1,   lole2,   loxyl
      integer      lpan2,   lpbzn,   lprd2
      integer      lprpe,   lpxyl,   lr6px
      integer      lrapx,   lrcho,   lrno3
      integer      lro3h,   lsesq,   ltolu
      integer        lxn,   livod,   livog
      integer      lvap1,   lvap2,   lvap3
      integer      lvap4,   lvas1,   lvas2
      integer      lvas3,   lvas4,   lvbs1
      integer      lvbs2,   lvbs3,   lvbs4
      integer      lvcp1,   lvcp2,   lvcp3
      integer      lvcp4,   lvfp1,   lvfp2
      integer      lvfp3,   lvfp4,   lpap0
      integer      lpap1,   lpap2,   lpap3
      integer      lpap4,   lpas0,   lpas1
      integer      lpas2,   lpas3,   lpas4
      integer      lpbs0,   lpbs1,   lpbs2
      integer      lpbs3,   lpbs4,   lpcp0
      integer      lpcp1,   lpcp2,   lpcp3
      integer      lpcp4,   lpfp0,   lpfp1
      integer      lpfp2,   lpfp3,   lpfp4
      integer     lhgiip, lhgiipc,    lhgp
      integer    lpso4_1, lpso4_2, lpso4_3
      integer    lpso4_4, lpso4_5, lpso4_6
      integer    lpso4_7, lpso4_8, lpso4_9
      integer   lpso4_10, lpno3_1, lpno3_2
      integer    lpno3_3, lpno3_4, lpno3_5
      integer    lpno3_6, lpno3_7, lpno3_8
      integer    lpno3_9,lpno3_10, lpnh4_1
      integer    lpnh4_2, lpnh4_3, lpnh4_4
      integer    lpnh4_5, lpnh4_6, lpnh4_7
      integer    lpnh4_8, lpnh4_9,lpnh4_10
      integer     lpoa_1,  lpoa_2,  lpoa_3
      integer     lpoa_4,  lpoa_5,  lpoa_6
      integer     lpoa_7,  lpoa_8,  lpoa_9
      integer    lpoa_10, lsoa1_1, lsoa1_2
      integer    lsoa1_3, lsoa1_4, lsoa1_5
      integer    lsoa1_6, lsoa1_7, lsoa1_8
      integer    lsoa1_9,lsoa1_10, lsoa2_1
      integer    lsoa2_2, lsoa2_3, lsoa2_4
      integer    lsoa2_5, lsoa2_6, lsoa2_7
      integer    lsoa2_8, lsoa2_9,lsoa2_10
      integer    lsoa3_1, lsoa3_2, lsoa3_3
      integer    lsoa3_4, lsoa3_5, lsoa3_6
      integer    lsoa3_7, lsoa3_8, lsoa3_9
      integer   lsoa3_10, lsoa4_1, lsoa4_2
      integer    lsoa4_3, lsoa4_4, lsoa4_5
      integer    lsoa4_6, lsoa4_7, lsoa4_8
      integer    lsoa4_9,lsoa4_10, lsopa_1
      integer    lsopa_2, lsopa_3, lsopa_4
      integer    lsopa_5, lsopa_6, lsopa_7
      integer    lsopa_8, lsopa_9,lsopa_10
      integer    lsopb_1, lsopb_2, lsopb_3
      integer    lsopb_4, lsopb_5, lsopb_6
      integer    lsopb_7, lsopb_8, lsopb_9
      integer   lsopb_10,  lpec_1,  lpec_2
      integer     lpec_3,  lpec_4,  lpec_5
      integer     lpec_6,  lpec_7,  lpec_8
      integer     lpec_9, lpec_10, lcrst_1
      integer    lcrst_2, lcrst_3, lcrst_4
      integer    lcrst_5, lcrst_6, lcrst_7
      integer    lcrst_8, lcrst_9,lcrst_10
      integer      lna_1,   lna_2,   lna_3
      integer      lna_4,   lna_5,   lna_6
      integer      lna_7,   lna_8,   lna_9
      integer     lna_10,  lpcl_1,  lpcl_2
      integer     lpcl_3,  lpcl_4,  lpcl_5
      integer     lpcl_6,  lpcl_7,  lpcl_8
      integer     lpcl_9, lpcl_10, lph2o_1
      integer    lph2o_2, lph2o_3, lph2o_4
      integer    lph2o_5, lph2o_6, lph2o_7
      integer    lph2o_8, lph2o_9,lph2o_10
c
      equivalence
     &   (lmap(  1),      lo),(lmap(  2),    lo1d),(lmap(  3),     loh),
     &   (lmap(  4),    lho2),(lmap(  5),   lbzo2),(lmap(  6),   lc2o3),
     &   (lmap(  7),    lcro),(lmap(  8),   lcxo3),(lmap(  9),   lepx2),
     &   (lmap( 10),   lhco3),(lmap( 11),   liso2),(lmap( 12),   lmeo2),
     &   (lmap( 13),   lopo3),(lmap( 14),    lro2),(lmap( 15),    lror),
     &   (lmap( 16),    lto2),(lmap( 17),   lxlo2),(lmap( 18),    lxo2),
     &   (lmap( 19),   lxo2h),(lmap( 20),   lxo2n),(lmap( 21),     lbr),
     &   (lmap( 22),    lbro),(lmap( 23),     lcl),(lmap( 24),    lclo),
     &   (lmap( 25),      li),(lmap( 26),     lio),(lmap( 27),    loio),
     &   (lmap( 28),   lxpar),(lmap( 29),   lxprp),(lmap( 30),   laacd),
     &   (lmap( 31),   lacet),(lmap( 32),   lald2),(lmap( 33),   laldx),
     &   (lmap( 34),   lbenz),(lmap( 35),   lcat1),(lmap( 36),     lco),
     &   (lmap( 37),   lcres),(lmap( 38),   lcron),(lmap( 39),   lepox),
     &   (lmap( 40),    leth),(lmap( 41),   letha),(lmap( 42),   lethy),
     &   (lmap( 43),   letoh),(lmap( 44),   lfacd),(lmap( 45),   lform),
     &   (lmap( 46),    lgly),(lmap( 47),   lglyd),(lmap( 48),   lh2o2),
     &   (lmap( 49),   lhno3),(lmap( 50),   lhono),(lmap( 51),   lhpld),
     &   (lmap( 52),   lintr),(lmap( 53),   liole),(lmap( 54),   lisop),
     &   (lmap( 55),   lispd),(lmap( 56),   lispx),(lmap( 57),    lket),
     &   (lmap( 58),   lmeoh),(lmap( 59),   lmepx),(lmap( 60),   lmgly),
     &   (lmap( 61),   ln2o5),(lmap( 62),     lno),(lmap( 63),    lno2),
     &   (lmap( 64),    lno3),(lmap( 65),   lntr1),(lmap( 66),   lntr2),
     &   (lmap( 67),     lo3),(lmap( 68),    lole),(lmap( 69),   lopan),
     &   (lmap( 70),   lopen),(lmap( 71),   lpacd),(lmap( 72),    lpan),
     &   (lmap( 73),   lpanx),(lmap( 74),    lpar),(lmap( 75),    lpna),
     &   (lmap( 76),   lprpa),(lmap( 77),   lrooh),(lmap( 78),    lso2),
     &   (lmap( 79),   lsulf),(lmap( 80),   lterp),(lmap( 81),    ltol),
     &   (lmap( 82),   lxopn),(lmap( 83),    lxyl),(lmap( 84),   lech4),
     &   (lmap( 85),     li2),(lmap( 86),   li2o2),(lmap( 87),   lixoy),
     &   (lmap( 88),    lhoi),(lmap( 89),   lhio3),(lmap( 90),   lino3),
     &   (lmap( 91),    lbr2),(lmap( 92),   lbrn2),(lmap( 93),   lbrn3),
     &   (lmap( 94),   lch3i),(lmap( 95),    lcl2),(lmap( 96),   lcln2),
     &   (lmap( 97),   lcln3),(lmap( 98),   lfmbr),(lmap( 99),   lfmcl),
     &   (lmap(100),    lhbr),(lmap(101),    lhcl),(lmap(102),    lhg0),
     &   (lmap(103),    lhg2),(lmap(104),     lhi),(lmap(105),   lhobr),
     &   (lmap(106),   lhocl),(lmap(107),   lialk),(lmap(108),   lino2),
     &   (lmap(109),    lmb2),(lmap(110),   lmb2c),(lmap(111),    lmb3),
     &   (lmap(112),    lmbc),(lmap(113),   lmbc2),(lmap(114),    lmi2),
     &   (lmap(115),    lmib),(lmap(116),    lmic),(lmap(117),    lnh3),
     &   (lmap(118),    lsqt),(lmap(119),   lssbr),(lmap(120),   lsscl),
     &   (lmap(121),   lssn3),(lmap(122),    ldms),(lmap(123),    lntr),
     &   (lmap(124),    lcg1),(lmap(125),    lcg2),(lmap(126),    lcg3),
     &   (lmap(127),    lcg4),(lmap(128),   livoa),(lmap(129),   livob),
     &   (lmap(130),   lpso4),(lmap(131),   lpno3),(lmap(132),   lpnh4),
     &   (lmap(133),    lpoa),(lmap(134),   lsoa1),(lmap(135),   lsoa2),
     &   (lmap(136),   lsoa3),(lmap(137),   lsoa4),(lmap(138),   lsopa),
     &   (lmap(139),   lsopb),(lmap(140),    lpec),(lmap(141),   lfcrs),
     &   (lmap(142),   lfprm),(lmap(143),   lccrs),(lmap(144),   lcprm),
     &   (lmap(145),     lna),(lmap(146),    lpcl),(lmap(147),   lph2o),
     &   (lmap(148),    lpfe),(lmap(149),    lpmn),(lmap(150),     lpk),
     &   (lmap(151),    lpca),(lmap(152),    lpmg),(lmap(153),    lpal),
     &   (lmap(154),    lpsi),(lmap(155),    lpti),(lmap(156),    lo3p),
     &   (lmap(157),   lbzc3),(lmap(158),    lbzo),(lmap(159),   lmac3),
     &   (lmap(160),   lmco3),(lmap(161),   lrco3),(lmap(162),   lro2c),
     &   (lmap(163),   lro2x),(lmap(164),   ltbuo),(lmap(165),   lxace),
     &   (lmap(166),   lxacr),(lmap(167),   lxaf1),(lmap(168),   lxaf2),
     &   (lmap(169),   lxaf3),(lmap(170),   lxbac),(lmap(171),   lxbal),
     &   (lmap(172),   lxcch),(lmap(173),    lxco),(lmap(174),   lxgld),
     &   (lmap(175),   lxgly),(lmap(176),   lxhch),(lmap(177),   lxho2),
     &   (lmap(178),   lxipr),(lmap(179),   lxma3),(lmap(180),   lxmac),
     &   (lmap(181),   lxmc3),(lmap(182),   lxmek),(lmap(183),   lxmeo),
     &   (lmap(184),   lxmgl),(lmap(185),   lxmvk),(lmap(186),   lxno2),
     &   (lmap(187),    lxoh),(lmap(188),   lxpd2),(lmap(189),   lxrc3),
     &   (lmap(190),   lxrch),(lmap(191),   lxrn3),(lmap(192),   lxtbu),
     &   (lmap(193),   ly6px),(lmap(194),   lyapx),(lmap(195),   lyrpx),
     &   (lmap(196),   lzrn3),(lmap(197),   lacro),(lmap(198),   lacye),
     &   (lmap(199),   lafg1),(lmap(200),   lafg2),(lmap(201),   lafg3),
     &   (lmap(202),   lalk1),(lmap(203),   lalk2),(lmap(204),   lalk3),
     &   (lmap(205),   lalk4),(lmap(206),   lalk5),(lmap(207),   lapin),
     &   (lmap(208),   laro1),(lmap(209),   laro2),(lmap(210),   lb124),
     &   (lmap(211),   lbacl),(lmap(212),   lbald),(lmap(213),   lbd13),
     &   (lmap(214),   lccho),(lmap(215),   lco3h),(lmap(216),   lcooh),
     &   (lmap(217),   lethe),(lmap(218),   lhcho),(lmap(219),   liprd),
     &   (lmap(220),   lmacr),(lmap(221),    lmek),(lmap(222),   lmpan),
     &   (lmap(223),    lmvk),(lmap(224),   lmxyl),(lmap(225),   lnphe),
     &   (lmap(226),   lole1),(lmap(227),   lole2),(lmap(228),   loxyl),
     &   (lmap(229),   lpan2),(lmap(230),   lpbzn),(lmap(231),   lprd2),
     &   (lmap(232),   lprpe),(lmap(233),   lpxyl),(lmap(234),   lr6px),
     &   (lmap(235),   lrapx),(lmap(236),   lrcho),(lmap(237),   lrno3),
     &   (lmap(238),   lro3h),(lmap(239),   lsesq),(lmap(240),   ltolu),
     &   (lmap(241),     lxn),(lmap(242),   livod),(lmap(243),   livog),
     &   (lmap(244),   lvap1),(lmap(245),   lvap2),(lmap(246),   lvap3),
     &   (lmap(247),   lvap4),(lmap(248),   lvas1),(lmap(249),   lvas2),
     &   (lmap(250),   lvas3),(lmap(251),   lvas4),(lmap(252),   lvbs1),
     &   (lmap(253),   lvbs2),(lmap(254),   lvbs3),(lmap(255),   lvbs4),
     &   (lmap(256),   lvcp1),(lmap(257),   lvcp2),(lmap(258),   lvcp3),
     &   (lmap(259),   lvcp4),(lmap(260),   lvfp1),(lmap(261),   lvfp2),
     &   (lmap(262),   lvfp3),(lmap(263),   lvfp4),(lmap(264),   lpap0),
     &   (lmap(265),   lpap1),(lmap(266),   lpap2),(lmap(267),   lpap3),
     &   (lmap(268),   lpap4),(lmap(269),   lpas0),(lmap(270),   lpas1)
      equivalence
     &   (lmap(271),   lpas2),(lmap(272),   lpas3),(lmap(273),   lpas4),
     &   (lmap(274),   lpbs0),(lmap(275),   lpbs1),(lmap(276),   lpbs2),
     &   (lmap(277),   lpbs3),(lmap(278),   lpbs4),(lmap(279),   lpcp0),
     &   (lmap(280),   lpcp1),(lmap(281),   lpcp2),(lmap(282),   lpcp3),
     &   (lmap(283),   lpcp4),(lmap(284),   lpfp0),(lmap(285),   lpfp1),
     &   (lmap(286),   lpfp2),(lmap(287),   lpfp3),(lmap(288),   lpfp4),
     &   (lmap(289),  lhgiip),(lmap(290), lhgiipc),(lmap(291),    lhgp),
     &   (lmap(292), lpso4_1),(lmap(293), lpso4_2),(lmap(294), lpso4_3),
     &   (lmap(295), lpso4_4),(lmap(296), lpso4_5),(lmap(297), lpso4_6),
     &   (lmap(298), lpso4_7),(lmap(299), lpso4_8),(lmap(300), lpso4_9),
     &   (lmap(301),lpso4_10),(lmap(302), lpno3_1),(lmap(303), lpno3_2),
     &   (lmap(304), lpno3_3),(lmap(305), lpno3_4),(lmap(306), lpno3_5),
     &   (lmap(307), lpno3_6),(lmap(308), lpno3_7),(lmap(309), lpno3_8),
     &   (lmap(310), lpno3_9),(lmap(311),lpno3_10),(lmap(312), lpnh4_1),
     &   (lmap(313), lpnh4_2),(lmap(314), lpnh4_3),(lmap(315), lpnh4_4),
     &   (lmap(316), lpnh4_5),(lmap(317), lpnh4_6),(lmap(318), lpnh4_7),
     &   (lmap(319), lpnh4_8),(lmap(320), lpnh4_9),(lmap(321),lpnh4_10),
     &   (lmap(322),  lpoa_1),(lmap(323),  lpoa_2),(lmap(324),  lpoa_3),
     &   (lmap(325),  lpoa_4),(lmap(326),  lpoa_5),(lmap(327),  lpoa_6),
     &   (lmap(328),  lpoa_7),(lmap(329),  lpoa_8),(lmap(330),  lpoa_9),
     &   (lmap(331), lpoa_10),(lmap(332), lsoa1_1),(lmap(333), lsoa1_2),
     &   (lmap(334), lsoa1_3),(lmap(335), lsoa1_4),(lmap(336), lsoa1_5),
     &   (lmap(337), lsoa1_6),(lmap(338), lsoa1_7),(lmap(339), lsoa1_8),
     &   (lmap(340), lsoa1_9),(lmap(341),lsoa1_10),(lmap(342), lsoa2_1),
     &   (lmap(343), lsoa2_2),(lmap(344), lsoa2_3),(lmap(345), lsoa2_4),
     &   (lmap(346), lsoa2_5),(lmap(347), lsoa2_6),(lmap(348), lsoa2_7),
     &   (lmap(349), lsoa2_8),(lmap(350), lsoa2_9),(lmap(351),lsoa2_10),
     &   (lmap(352), lsoa3_1),(lmap(353), lsoa3_2),(lmap(354), lsoa3_3),
     &   (lmap(355), lsoa3_4),(lmap(356), lsoa3_5),(lmap(357), lsoa3_6),
     &   (lmap(358), lsoa3_7),(lmap(359), lsoa3_8),(lmap(360), lsoa3_9),
     &   (lmap(361),lsoa3_10),(lmap(362), lsoa4_1),(lmap(363), lsoa4_2),
     &   (lmap(364), lsoa4_3),(lmap(365), lsoa4_4),(lmap(366), lsoa4_5),
     &   (lmap(367), lsoa4_6),(lmap(368), lsoa4_7),(lmap(369), lsoa4_8),
     &   (lmap(370), lsoa4_9),(lmap(371),lsoa4_10),(lmap(372), lsopa_1),
     &   (lmap(373), lsopa_2),(lmap(374), lsopa_3),(lmap(375), lsopa_4),
     &   (lmap(376), lsopa_5),(lmap(377), lsopa_6),(lmap(378), lsopa_7),
     &   (lmap(379), lsopa_8),(lmap(380), lsopa_9),(lmap(381),lsopa_10),
     &   (lmap(382), lsopb_1),(lmap(383), lsopb_2),(lmap(384), lsopb_3),
     &   (lmap(385), lsopb_4),(lmap(386), lsopb_5),(lmap(387), lsopb_6),
     &   (lmap(388), lsopb_7),(lmap(389), lsopb_8),(lmap(390), lsopb_9),
     &   (lmap(391),lsopb_10),(lmap(392),  lpec_1),(lmap(393),  lpec_2),
     &   (lmap(394),  lpec_3),(lmap(395),  lpec_4),(lmap(396),  lpec_5),
     &   (lmap(397),  lpec_6),(lmap(398),  lpec_7),(lmap(399),  lpec_8),
     &   (lmap(400),  lpec_9),(lmap(401), lpec_10),(lmap(402), lcrst_1),
     &   (lmap(403), lcrst_2),(lmap(404), lcrst_3),(lmap(405), lcrst_4),
     &   (lmap(406), lcrst_5),(lmap(407), lcrst_6),(lmap(408), lcrst_7),
     &   (lmap(409), lcrst_8),(lmap(410), lcrst_9),(lmap(411),lcrst_10),
     &   (lmap(412),   lna_1),(lmap(413),   lna_2),(lmap(414),   lna_3),
     &   (lmap(415),   lna_4),(lmap(416),   lna_5),(lmap(417),   lna_6),
     &   (lmap(418),   lna_7),(lmap(419),   lna_8),(lmap(420),   lna_9),
     &   (lmap(421),  lna_10),(lmap(422),  lpcl_1),(lmap(423),  lpcl_2),
     &   (lmap(424),  lpcl_3),(lmap(425),  lpcl_4),(lmap(426),  lpcl_5),
     &   (lmap(427),  lpcl_6),(lmap(428),  lpcl_7),(lmap(429),  lpcl_8),
     &   (lmap(430),  lpcl_9),(lmap(431), lpcl_10),(lmap(432), lph2o_1),
     &   (lmap(433), lph2o_2),(lmap(434), lph2o_3),(lmap(435), lph2o_4),
     &   (lmap(436), lph2o_5),(lmap(437), lph2o_6),(lmap(438), lph2o_7),
     &   (lmap(439), lph2o_8),(lmap(440), lph2o_9),(lmap(441),lph2o_10)
