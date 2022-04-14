c----CAMx v6.50 180430
c  
c     CHMDAT.INC contains all chemistry variables 
c                            
c     Copyright 1996 - 2018
c     Ramboll 
c            
c     Modifications:  
c       4/4/00     Added aerosol deposition variables to /aerochm/
c       1/9/02     Aerosol size cut points and density now a function of
c                  species
c       8/20/02    Added minimum CWC to define presence of clouds
c      12/12/02    Expanded species list for Mechanism 4
c       1/10/03    Added array for deposition output species names
c      10/14/04    Modifications for mechanism 10
c      11/30/06    Created by taking Static variables out of chmstry.com
c      12/29/06 -bkoo-     Expanded species list for the updated SOA scheme
c      01/08/07 -bkoo-     Expanded species list for Mechanism 6 (CB05)
c                          Now ipigrxn is a scalar pointer to NO self-reaction
c      07/04/07 -bkoo-     Added pointer to hydrolysis of N2O5
c      04/28/08 -gyarwood- Added EBI solver
c      04/12/12 -cemery-   Added T and P adjustments to photolysis rates
c      07/20/12 -ou-       Added Mechanism 1 (CB06 with Iodine chemistry)
c      10/08/12 -jjung-    Added Mechanism 2 (CB6r1)
c      04/30/13 -cemery-   Added surface model
c      06/28/13 -bkoo-     Added indexes for new GREASD PiG chemistry
c      10/14/13 -bkoo-     Added pointer to hydrolysis of organic nitrate
c      10/24/13 -bkoo-     Revised Mechanism 2 (CB6r1 -> CB6r2)
c      03/18/14 -bkoo-     Added species for benzene SOA
c      04/07/14 -cemery-   Added top con file
c      06/27/14 -bkoo-     Added Mechanism 3 (CB6r2h)
c      11/11/14 -bkoo-     Added Mechanism 4 (CB6r3)
c      12/07/14 -bkoo-     Added species for VBS
c      01/08/16 -bkoo-     Updated for SAPRC99->SAPRC07TC
c                                      Revised VBS
c                                      Removal of IEH solver
c      02/29/16 -bkoo-     Added pointer to heterogeneous rxn of N2O5 + HCL
c      07/20/16 -bkoo-     Added pointer to heterogeneous hydrolysis of INTR
c      09/02/16 -bkoo-     Revised heterogeneous rxn pointers
c      11/28/16 -bkoo-     Revised VBS precursors
c      11/22/17 -bkoo-     Added PFE/PMN/PK/PCA/PMG
c      01/12/18 -bkoo-     Removed BNZA/TOLA/XYLA/ISP/TRP
c 
c-----------------------------------------------------------------------
c     Parameters for some of the switches:
c
c     CDCMC  -- string for requesting the CMC chemistry solver
c     CDEBI  -- string for requesting the EBI chemistry solver
c     CDIEH  -- string for requesting the IEH chemistry solver
c     CDLSOD -- string for requesting the LSODE chemistry solver
c     IDCMC  -- code for using the CMC chemistry solver
c     IDEBI  -- code for using the EBI chemistry solver
c     IDIEH  -- code for using the IEH chemistry solver
c     IDLSOD -- code for using the LSODE chemistry solver
c
c-----------------------------------------------------------------------
c
      character*10 CDCMC
      character*10 CDEBI
      character*10 CDIEH
      character*10 CDLSOD
      integer      IDCMC
      integer      IDEBI
      integer      IDIEH
      integer      IDLSOD
c
      parameter( CDCMC  = "CMC       " )
      parameter( CDEBI  = "EBI       " )
      parameter( CDIEH  = "IEH       " )
      parameter( CDLSOD = "LSODE     " )
      parameter( IDCMC  = 1 )
      parameter( IDEBI  = 2 )
      parameter( IDIEH  = 3 )
      parameter( IDLSOD = 4 )
c 
c-----------------------------------------------------------------------
c    Variables for the number of species in input files:
c
c    ngas    --  number of gas species being modeled
c    naero   --  number of aersol species being modeled
c    nspec   --  total number of modeled species
c    nrad    --  number of radical species being modeled
c    nreact  --  number of chemical reactions
c    idmech  --  the code which determines which chemical mechanism is used
c    idsolv  --  the code which determines which chemstry solver to use
c    navspc  --  number of species to write to output average file
c    ndepspc --  number of species to write to deposition file
c    nicspc  --  number of species in the initial conditions file
c    nbcspc  --  number of species in the boundary conditions file
c    ntcspc  --  number of species in the top concentrations file
c    nptspc  --  number of species in the point source emissions file
c    nsmspc  --  number of species in surface model
c    nsmrxn  --  number of reactions in surface model
c
c-----------------------------------------------------------------------
c
       integer      ngas
       integer      naero
       integer      nspec
       integer      nrad
       integer      nreact
       integer      idmech
       integer      idsolv
       integer      navspc
       integer      ndepspc
       integer      nicspc
       integer      nbcspc
       integer      ntcspc
       integer      nptspc
       integer      nsmspc
       integer      nsmrxn
c
      common /chm1/ ngas, naero, nspec, nrad, nreact,
     &                         idmech, idsolv, navspc, ndepspc, nicspc,
     &                           nbcspc, ntcspc, nptspc, nsmspc, nsmrxn
c
c-----------------------------------------------------------------------
c
c    aeropt --  keyword for aerosol scheme ('NONE', 'INERT', 'CF' or 'CMU')
c    leqsam --  logical flag to use EQSAM chemistry
c
c-----------------------------------------------------------------------
c
      character*10 aeropt
      logical leqsam
c
      common /chm2/ aeropt
      common /chm2dat/ leqsam
c
c-----------------------------------------------------------------------
c     Variables for photolysis data:
c
c     nphot1   -- number of primary photolysis reactions
c     nphot2   -- number of secondary (scaled) photolysis reactions
c
c-----------------------------------------------------------------------
c
      integer nphot1
      integer nphot2
c
      common /photmap/ nphot1, nphot2
c 
c-----------------------------------------------------------------------
c     Mercury chemistry variables 
c     Variables for vertical profiles of Cl2, Br and BrO for Hg chemistry
c
c     NHTHAL   --  number of heights for halogen concentrations 
c     hthal    --  heights (km) for the top of each concentration range
c     cl2day  --  profile of Cl2 (ppm) with height during day over ocean
c     cl2nite --  profile of Cl2 (ppm) with height at night over ocean
c     brlprof  --  profile of Br (ppm) with height over land
c     brwprof  --  profile of Br (ppm) with height over ocean
c     brolprof --  profile of BrO (ppm) with height over land
c     browprof --  profile of BrO (ppm) with height over ocean
c
c     MWHG     --  Hg molecular weight 
c-----------------------------------------------------------------------
c
      integer, parameter :: NHTHAL = 6
      real, parameter :: MWHG = 200.59
c
      real, dimension(NHTHAL) :: hthal,brlprof,brwprof,brolprof,
     &                           browprof,cl2day,cl2nite
c
      common /halprofs/ hthal, brlprof, brwprof, brolprof, browprof,
     &                  cl2day, cl2nite
c 
c-----------------------------------------------------------------------
c     Parameters for mapping input species to internal model order:
c
c      NSPNAM  -- number of recognizable state species
c
      integer   NSPNAM
c
      parameter ( NSPNAM  =   437 )
c 
c-----------------------------------------------------------------------
c     Variables for mapping input species to internal model order:
c
c     kmap     -- mapping of species on chemistry parameters file to
c                 internal order
c
c-----------------------------------------------------------------------
c
      integer kmap(NSPNAM)
c
      common /kname/ kmap
c
      integer      kaacd,   kacet,   kacro
      integer      kacye,   kafg1,   kafg2
      integer      kafg3,   kald2,   kaldx
      integer      kalk1,   kalk2,   kalk3
      integer      kalk4,   kalk5,   kapin
      integer      karo1,   karo2,   kb124
      integer      kbacl,   kbald,   kbd13
      integer      kbenz,     kbr,    kbr2
      integer      kbrn2,   kbrn3,    kbro
      integer      kbzc3,    kbzo,   kbzo2
      integer      kc2o3,   kcat1,   kccho
      integer       kcg1,    kcg2,    kcg3
      integer       kcg4,   kch3i,     kcl
      integer       kcl2,   kcln2,   kcln3
      integer       kclo,     kco,   kco3h
      integer      kcooh,   kcres,    kcro
      integer      kcron,   kcxo3,   kech4
      integer      kepox,   kepx2,    keth
      integer      ketha,   kethe,   kethy
      integer      ketoh,   kfacd,   kfmbr
      integer      kfmcl,   kform,    kgly
      integer      kglyd,   kh2o2,    khbr
      integer      khcho,    khcl,   khco3
      integer       khg0,    khg2,     khi
      integer      khio3,   khno3,    kho2
      integer      khobr,   khocl,    khoi
      integer      khono,   khpld,      ki
      integer        ki2,   ki2o2,   kialk
      integer      kino2,   kino3,   kintr
      integer        kio,   kiole,   kiprd
      integer      kiso2,   kisop,   kispd
      integer      kispx,   kivoa,   kivob
      integer      kivod,   kivog,   kixoy
      integer       kket,   kmac3,   kmacr
      integer       kmb2,   kmb2c,    kmb3
      integer       kmbc,   kmbc2,   kmco3
      integer       kmek,   kmeo2,   kmeoh
      integer      kmepx,   kmgly,    kmi2
      integer       kmib,    kmic,   kmpan
      integer       kmvk,   kmxyl,   kn2o5
      integer       knh3,     kno,    kno2
      integer       kno3,   knphe,    kntr
      integer      kntr1,   kntr2,      ko
      integer       ko1d,     ko3,    ko3p
      integer        koh,    koio,    kole
      integer      kole1,   kole2,   kopan
      integer      kopen,   kopo3,   koxyl
      integer      kpacd,    kpan,   kpan2
      integer      kpanx,    kpar,   kpbzn
      integer       kpna,   kprd2,   kprpa
      integer      kprpe,   kpxyl,   kr6px
      integer      krapx,   krcho,   krco3
      integer      krno3,    kro2,   kro2c
      integer      kro2x,   kro3h,   krooh
      integer       kror,   ksesq,    kso2
      integer       ksqt,   kssbr,   ksscl
      integer      kssn3,   ksulf,   ktbuo
      integer      kterp,    kto2,    ktol
      integer      ktolu,   kvap1,   kvap2
      integer      kvap3,   kvap4,   kvas1
      integer      kvas2,   kvas3,   kvas4
      integer      kvbs1,   kvbs2,   kvbs3
      integer      kvbs4,   kvcp1,   kvcp2
      integer      kvcp3,   kvcp4,   kvfp1
      integer      kvfp2,   kvfp3,   kvfp4
      integer      kxace,   kxacr,   kxaf1
      integer      kxaf2,   kxaf3,   kxbac
      integer      kxbal,   kxcch,    kxco
      integer      kxgld,   kxgly,   kxhch
      integer      kxho2,   kxipr,   kxlo2
      integer      kxma3,   kxmac,   kxmc3
      integer      kxmek,   kxmeo,   kxmgl
      integer      kxmvk,     kxn,   kxno2
      integer       kxo2,   kxo2h,   kxo2n
      integer       kxoh,   kxopn,   kxpar
      integer      kxpd2,   kxprp,   kxrc3
      integer      kxrch,   kxrn3,   kxtbu
      integer       kxyl,   ky6px,   kyapx
      integer      kyrpx,   kzrn3,   kpso4
      integer      kpno3,   kpnh4,    kpoa
      integer      ksoa1,   ksoa2,   ksoa3
      integer      ksoa4,   ksopa,   ksopb
      integer       kpec,   kfcrs,   kfprm
      integer      kccrs,   kcprm,     kna
      integer       kpcl,   kph2o,    kpfe
      integer       kpmn,     kpk,    kpca
      integer       kpmg,  khgiip, khgiipc
      integer       khgp,   kpap0,   kpap1
      integer      kpap2,   kpap3,   kpap4
      integer      kpas0,   kpas1,   kpas2
      integer      kpas3,   kpas4,   kpbs0
      integer      kpbs1,   kpbs2,   kpbs3
      integer      kpbs4,   kpcp0,   kpcp1
      integer      kpcp2,   kpcp3,   kpcp4
      integer      kpfp0,   kpfp1,   kpfp2
      integer      kpfp3,   kpfp4, kpso4_1
      integer    kpso4_2, kpso4_3, kpso4_4
      integer    kpso4_5, kpso4_6, kpso4_7
      integer    kpso4_8, kpso4_9,kpso4_10
      integer    kpno3_1, kpno3_2, kpno3_3
      integer    kpno3_4, kpno3_5, kpno3_6
      integer    kpno3_7, kpno3_8, kpno3_9
      integer   kpno3_10, kpnh4_1, kpnh4_2
      integer    kpnh4_3, kpnh4_4, kpnh4_5
      integer    kpnh4_6, kpnh4_7, kpnh4_8
      integer    kpnh4_9,kpnh4_10,  kpoa_1
      integer     kpoa_2,  kpoa_3,  kpoa_4
      integer     kpoa_5,  kpoa_6,  kpoa_7
      integer     kpoa_8,  kpoa_9, kpoa_10
      integer    ksoa1_1, ksoa1_2, ksoa1_3
      integer    ksoa1_4, ksoa1_5, ksoa1_6
      integer    ksoa1_7, ksoa1_8, ksoa1_9
      integer   ksoa1_10, ksoa2_1, ksoa2_2
      integer    ksoa2_3, ksoa2_4, ksoa2_5
      integer    ksoa2_6, ksoa2_7, ksoa2_8
      integer    ksoa2_9,ksoa2_10, ksoa3_1
      integer    ksoa3_2, ksoa3_3, ksoa3_4
      integer    ksoa3_5, ksoa3_6, ksoa3_7
      integer    ksoa3_8, ksoa3_9,ksoa3_10
      integer    ksoa4_1, ksoa4_2, ksoa4_3
      integer    ksoa4_4, ksoa4_5, ksoa4_6
      integer    ksoa4_7, ksoa4_8, ksoa4_9
      integer   ksoa4_10, ksopa_1, ksopa_2
      integer    ksopa_3, ksopa_4, ksopa_5
      integer    ksopa_6, ksopa_7, ksopa_8
      integer    ksopa_9,ksopa_10, ksopb_1
      integer    ksopb_2, ksopb_3, ksopb_4
      integer    ksopb_5, ksopb_6, ksopb_7
      integer    ksopb_8, ksopb_9,ksopb_10
      integer     kpec_1,  kpec_2,  kpec_3
      integer     kpec_4,  kpec_5,  kpec_6
      integer     kpec_7,  kpec_8,  kpec_9
      integer    kpec_10, kcrst_1, kcrst_2
      integer    kcrst_3, kcrst_4, kcrst_5
      integer    kcrst_6, kcrst_7, kcrst_8
      integer    kcrst_9,kcrst_10,   kna_1
      integer      kna_2,   kna_3,   kna_4
      integer      kna_5,   kna_6,   kna_7
      integer      kna_8,   kna_9,  kna_10
      integer     kpcl_1,  kpcl_2,  kpcl_3
      integer     kpcl_4,  kpcl_5,  kpcl_6
      integer     kpcl_7,  kpcl_8,  kpcl_9
      integer    kpcl_10, kph2o_1, kph2o_2
      integer    kph2o_3, kph2o_4, kph2o_5
      integer    kph2o_6, kph2o_7, kph2o_8
      integer    kph2o_9,kph2o_10
c
      equivalence
     &   (kmap(  1),   kaacd),(kmap(  2),   kacet),(kmap(  3),   kacro),
     &   (kmap(  4),   kacye),(kmap(  5),   kafg1),(kmap(  6),   kafg2),
     &   (kmap(  7),   kafg3),(kmap(  8),   kald2),(kmap(  9),   kaldx),
     &   (kmap( 10),   kalk1),(kmap( 11),   kalk2),(kmap( 12),   kalk3),
     &   (kmap( 13),   kalk4),(kmap( 14),   kalk5),(kmap( 15),   kapin),
     &   (kmap( 16),   karo1),(kmap( 17),   karo2),(kmap( 18),   kb124),
     &   (kmap( 19),   kbacl),(kmap( 20),   kbald),(kmap( 21),   kbd13),
     &   (kmap( 22),   kbenz),(kmap( 23),     kbr),(kmap( 24),    kbr2),
     &   (kmap( 25),   kbrn2),(kmap( 26),   kbrn3),(kmap( 27),    kbro),
     &   (kmap( 28),   kbzc3),(kmap( 29),    kbzo),(kmap( 30),   kbzo2),
     &   (kmap( 31),   kc2o3),(kmap( 32),   kcat1),(kmap( 33),   kccho),
     &   (kmap( 34),    kcg1),(kmap( 35),    kcg2),(kmap( 36),    kcg3),
     &   (kmap( 37),    kcg4),(kmap( 38),   kch3i),(kmap( 39),     kcl),
     &   (kmap( 40),    kcl2),(kmap( 41),   kcln2),(kmap( 42),   kcln3),
     &   (kmap( 43),    kclo),(kmap( 44),     kco),(kmap( 45),   kco3h),
     &   (kmap( 46),   kcooh),(kmap( 47),   kcres),(kmap( 48),    kcro),
     &   (kmap( 49),   kcron),(kmap( 50),   kcxo3),(kmap( 51),   kech4),
     &   (kmap( 52),   kepox),(kmap( 53),   kepx2),(kmap( 54),    keth),
     &   (kmap( 55),   ketha),(kmap( 56),   kethe),(kmap( 57),   kethy),
     &   (kmap( 58),   ketoh),(kmap( 59),   kfacd),(kmap( 60),   kfmbr),
     &   (kmap( 61),   kfmcl),(kmap( 62),   kform),(kmap( 63),    kgly),
     &   (kmap( 64),   kglyd),(kmap( 65),   kh2o2),(kmap( 66),    khbr),
     &   (kmap( 67),   khcho),(kmap( 68),    khcl),(kmap( 69),   khco3),
     &   (kmap( 70),    khg0),(kmap( 71),    khg2),(kmap( 72),     khi),
     &   (kmap( 73),   khio3),(kmap( 74),   khno3),(kmap( 75),    kho2),
     &   (kmap( 76),   khobr),(kmap( 77),   khocl),(kmap( 78),    khoi),
     &   (kmap( 79),   khono),(kmap( 80),   khpld),(kmap( 81),      ki),
     &   (kmap( 82),     ki2),(kmap( 83),   ki2o2),(kmap( 84),   kialk),
     &   (kmap( 85),   kino2),(kmap( 86),   kino3),(kmap( 87),   kintr),
     &   (kmap( 88),     kio),(kmap( 89),   kiole),(kmap( 90),   kiprd),
     &   (kmap( 91),   kiso2),(kmap( 92),   kisop),(kmap( 93),   kispd),
     &   (kmap( 94),   kispx),(kmap( 95),   kivoa),(kmap( 96),   kivob),
     &   (kmap( 97),   kivod),(kmap( 98),   kivog),(kmap( 99),   kixoy),
     &   (kmap(100),    kket),(kmap(101),   kmac3),(kmap(102),   kmacr),
     &   (kmap(103),    kmb2),(kmap(104),   kmb2c),(kmap(105),    kmb3),
     &   (kmap(106),    kmbc),(kmap(107),   kmbc2),(kmap(108),   kmco3),
     &   (kmap(109),    kmek),(kmap(110),   kmeo2),(kmap(111),   kmeoh),
     &   (kmap(112),   kmepx),(kmap(113),   kmgly),(kmap(114),    kmi2),
     &   (kmap(115),    kmib),(kmap(116),    kmic),(kmap(117),   kmpan),
     &   (kmap(118),    kmvk),(kmap(119),   kmxyl),(kmap(120),   kn2o5),
     &   (kmap(121),    knh3),(kmap(122),     kno),(kmap(123),    kno2),
     &   (kmap(124),    kno3),(kmap(125),   knphe),(kmap(126),    kntr),
     &   (kmap(127),   kntr1),(kmap(128),   kntr2),(kmap(129),      ko),
     &   (kmap(130),    ko1d),(kmap(131),     ko3),(kmap(132),    ko3p),
     &   (kmap(133),     koh),(kmap(134),    koio),(kmap(135),    kole),
     &   (kmap(136),   kole1),(kmap(137),   kole2),(kmap(138),   kopan),
     &   (kmap(139),   kopen),(kmap(140),   kopo3),(kmap(141),   koxyl),
     &   (kmap(142),   kpacd),(kmap(143),    kpan),(kmap(144),   kpan2),
     &   (kmap(145),   kpanx),(kmap(146),    kpar),(kmap(147),   kpbzn),
     &   (kmap(148),    kpna),(kmap(149),   kprd2),(kmap(150),   kprpa),
     &   (kmap(151),   kprpe),(kmap(152),   kpxyl),(kmap(153),   kr6px),
     &   (kmap(154),   krapx),(kmap(155),   krcho),(kmap(156),   krco3),
     &   (kmap(157),   krno3),(kmap(158),    kro2),(kmap(159),   kro2c),
     &   (kmap(160),   kro2x),(kmap(161),   kro3h),(kmap(162),   krooh),
     &   (kmap(163),    kror),(kmap(164),   ksesq),(kmap(165),    kso2),
     &   (kmap(166),    ksqt),(kmap(167),   kssbr),(kmap(168),   ksscl),
     &   (kmap(169),   kssn3),(kmap(170),   ksulf),(kmap(171),   ktbuo),
     &   (kmap(172),   kterp),(kmap(173),    kto2),(kmap(174),    ktol),
     &   (kmap(175),   ktolu),(kmap(176),   kvap1),(kmap(177),   kvap2),
     &   (kmap(178),   kvap3),(kmap(179),   kvap4),(kmap(180),   kvas1),
     &   (kmap(181),   kvas2),(kmap(182),   kvas3),(kmap(183),   kvas4),
     &   (kmap(184),   kvbs1),(kmap(185),   kvbs2),(kmap(186),   kvbs3),
     &   (kmap(187),   kvbs4),(kmap(188),   kvcp1),(kmap(189),   kvcp2),
     &   (kmap(190),   kvcp3),(kmap(191),   kvcp4),(kmap(192),   kvfp1),
     &   (kmap(193),   kvfp2),(kmap(194),   kvfp3),(kmap(195),   kvfp4),
     &   (kmap(196),   kxace),(kmap(197),   kxacr),(kmap(198),   kxaf1),
     &   (kmap(199),   kxaf2),(kmap(200),   kxaf3),(kmap(201),   kxbac),
     &   (kmap(202),   kxbal),(kmap(203),   kxcch),(kmap(204),    kxco),
     &   (kmap(205),   kxgld),(kmap(206),   kxgly),(kmap(207),   kxhch),
     &   (kmap(208),   kxho2),(kmap(209),   kxipr),(kmap(210),   kxlo2),
     &   (kmap(211),   kxma3),(kmap(212),   kxmac),(kmap(213),   kxmc3),
     &   (kmap(214),   kxmek),(kmap(215),   kxmeo),(kmap(216),   kxmgl),
     &   (kmap(217),   kxmvk),(kmap(218),     kxn),(kmap(219),   kxno2),
     &   (kmap(220),    kxo2),(kmap(221),   kxo2h),(kmap(222),   kxo2n),
     &   (kmap(223),    kxoh),(kmap(224),   kxopn),(kmap(225),   kxpar),
     &   (kmap(226),   kxpd2),(kmap(227),   kxprp),(kmap(228),   kxrc3),
     &   (kmap(229),   kxrch),(kmap(230),   kxrn3),(kmap(231),   kxtbu),
     &   (kmap(232),    kxyl),(kmap(233),   ky6px),(kmap(234),   kyapx),
     &   (kmap(235),   kyrpx),(kmap(236),   kzrn3),(kmap(237),   kpso4),
     &   (kmap(238),   kpno3),(kmap(239),   kpnh4),(kmap(240),    kpoa),
     &   (kmap(241),   ksoa1),(kmap(242),   ksoa2),(kmap(243),   ksoa3),
     &   (kmap(244),   ksoa4),(kmap(245),   ksopa),(kmap(246),   ksopb),
     &   (kmap(247),    kpec),(kmap(248),   kfcrs),(kmap(249),   kfprm),
     &   (kmap(250),   kccrs),(kmap(251),   kcprm),(kmap(252),     kna),
     &   (kmap(253),    kpcl),(kmap(254),   kph2o),(kmap(255),    kpfe),
     &   (kmap(256),    kpmn),(kmap(257),     kpk),(kmap(258),    kpca),
     &   (kmap(259),    kpmg),(kmap(260),  khgiip),(kmap(261), khgiipc),
     &   (kmap(262),    khgp),(kmap(263),   kpap0),(kmap(264),   kpap1),
     &   (kmap(265),   kpap2),(kmap(266),   kpap3),(kmap(267),   kpap4),
     &   (kmap(268),   kpas0),(kmap(269),   kpas1),(kmap(270),   kpas2)
      equivalence
     &   (kmap(271),   kpas3),(kmap(272),   kpas4),(kmap(273),   kpbs0),
     &   (kmap(274),   kpbs1),(kmap(275),   kpbs2),(kmap(276),   kpbs3),
     &   (kmap(277),   kpbs4),(kmap(278),   kpcp0),(kmap(279),   kpcp1),
     &   (kmap(280),   kpcp2),(kmap(281),   kpcp3),(kmap(282),   kpcp4),
     &   (kmap(283),   kpfp0),(kmap(284),   kpfp1),(kmap(285),   kpfp2),
     &   (kmap(286),   kpfp3),(kmap(287),   kpfp4),(kmap(288), kpso4_1),
     &   (kmap(289), kpso4_2),(kmap(290), kpso4_3),(kmap(291), kpso4_4),
     &   (kmap(292), kpso4_5),(kmap(293), kpso4_6),(kmap(294), kpso4_7),
     &   (kmap(295), kpso4_8),(kmap(296), kpso4_9),(kmap(297),kpso4_10),
     &   (kmap(298), kpno3_1),(kmap(299), kpno3_2),(kmap(300), kpno3_3),
     &   (kmap(301), kpno3_4),(kmap(302), kpno3_5),(kmap(303), kpno3_6),
     &   (kmap(304), kpno3_7),(kmap(305), kpno3_8),(kmap(306), kpno3_9),
     &   (kmap(307),kpno3_10),(kmap(308), kpnh4_1),(kmap(309), kpnh4_2),
     &   (kmap(310), kpnh4_3),(kmap(311), kpnh4_4),(kmap(312), kpnh4_5),
     &   (kmap(313), kpnh4_6),(kmap(314), kpnh4_7),(kmap(315), kpnh4_8),
     &   (kmap(316), kpnh4_9),(kmap(317),kpnh4_10),(kmap(318),  kpoa_1),
     &   (kmap(319),  kpoa_2),(kmap(320),  kpoa_3),(kmap(321),  kpoa_4),
     &   (kmap(322),  kpoa_5),(kmap(323),  kpoa_6),(kmap(324),  kpoa_7),
     &   (kmap(325),  kpoa_8),(kmap(326),  kpoa_9),(kmap(327), kpoa_10),
     &   (kmap(328), ksoa1_1),(kmap(329), ksoa1_2),(kmap(330), ksoa1_3),
     &   (kmap(331), ksoa1_4),(kmap(332), ksoa1_5),(kmap(333), ksoa1_6),
     &   (kmap(334), ksoa1_7),(kmap(335), ksoa1_8),(kmap(336), ksoa1_9),
     &   (kmap(337),ksoa1_10),(kmap(338), ksoa2_1),(kmap(339), ksoa2_2),
     &   (kmap(340), ksoa2_3),(kmap(341), ksoa2_4),(kmap(342), ksoa2_5),
     &   (kmap(343), ksoa2_6),(kmap(344), ksoa2_7),(kmap(345), ksoa2_8),
     &   (kmap(346), ksoa2_9),(kmap(347),ksoa2_10),(kmap(348), ksoa3_1),
     &   (kmap(349), ksoa3_2),(kmap(350), ksoa3_3),(kmap(351), ksoa3_4),
     &   (kmap(352), ksoa3_5),(kmap(353), ksoa3_6),(kmap(354), ksoa3_7),
     &   (kmap(355), ksoa3_8),(kmap(356), ksoa3_9),(kmap(357),ksoa3_10),
     &   (kmap(358), ksoa4_1),(kmap(359), ksoa4_2),(kmap(360), ksoa4_3),
     &   (kmap(361), ksoa4_4),(kmap(362), ksoa4_5),(kmap(363), ksoa4_6),
     &   (kmap(364), ksoa4_7),(kmap(365), ksoa4_8),(kmap(366), ksoa4_9),
     &   (kmap(367),ksoa4_10),(kmap(368), ksopa_1),(kmap(369), ksopa_2),
     &   (kmap(370), ksopa_3),(kmap(371), ksopa_4),(kmap(372), ksopa_5),
     &   (kmap(373), ksopa_6),(kmap(374), ksopa_7),(kmap(375), ksopa_8),
     &   (kmap(376), ksopa_9),(kmap(377),ksopa_10),(kmap(378), ksopb_1),
     &   (kmap(379), ksopb_2),(kmap(380), ksopb_3),(kmap(381), ksopb_4),
     &   (kmap(382), ksopb_5),(kmap(383), ksopb_6),(kmap(384), ksopb_7),
     &   (kmap(385), ksopb_8),(kmap(386), ksopb_9),(kmap(387),ksopb_10),
     &   (kmap(388),  kpec_1),(kmap(389),  kpec_2),(kmap(390),  kpec_3),
     &   (kmap(391),  kpec_4),(kmap(392),  kpec_5),(kmap(393),  kpec_6),
     &   (kmap(394),  kpec_7),(kmap(395),  kpec_8),(kmap(396),  kpec_9),
     &   (kmap(397), kpec_10),(kmap(398), kcrst_1),(kmap(399), kcrst_2),
     &   (kmap(400), kcrst_3),(kmap(401), kcrst_4),(kmap(402), kcrst_5),
     &   (kmap(403), kcrst_6),(kmap(404), kcrst_7),(kmap(405), kcrst_8),
     &   (kmap(406), kcrst_9),(kmap(407),kcrst_10),(kmap(408),   kna_1),
     &   (kmap(409),   kna_2),(kmap(410),   kna_3),(kmap(411),   kna_4),
     &   (kmap(412),   kna_5),(kmap(413),   kna_6),(kmap(414),   kna_7),
     &   (kmap(415),   kna_8),(kmap(416),   kna_9),(kmap(417),  kna_10),
     &   (kmap(418),  kpcl_1),(kmap(419),  kpcl_2),(kmap(420),  kpcl_3),
     &   (kmap(421),  kpcl_4),(kmap(422),  kpcl_5),(kmap(423),  kpcl_6),
     &   (kmap(424),  kpcl_7),(kmap(425),  kpcl_8),(kmap(426),  kpcl_9),
     &   (kmap(427), kpcl_10),(kmap(428), kph2o_1),(kmap(429), kph2o_2),
     &   (kmap(430), kph2o_3),(kmap(431), kph2o_4),(kmap(432), kph2o_5),
     &   (kmap(433), kph2o_6),(kmap(434), kph2o_7),(kmap(435), kph2o_8),
     &   (kmap(436), kph2o_9),(kmap(437),kph2o_10)
c
c-----------------------------------------------------------------------
c     Variables to define parameters for each chemical species:
c
c     henso20  -- Henry's Law constant at STP for SO2 (molar/atm)
c     tfactso2 -- Temperature dependence of SO2 Henry's Law constant (1/K)
c     nbin     -- Number of aerosol size bins
c     cwmin    -- Minimum cloud water threshold (g/m3)
c     tamin    -- Cloud water freezing threshold (K)
c
c-----------------------------------------------------------------------
c
      real henso20
      real tfactso2
      real cwmin
      real tamin
c
      common /depchm/ henso20, tfactso2, cwmin, tamin
c
      integer nbin
c
      common /aerochm/ nbin
c
c-----------------------------------------------------------------------
c     Pointers to photolysis reactions that receive T,P adjustments
c
      integer jno2rxn,jo3rxn,jhcho1rxn,jhcho2rxn,jch3chorxn
      common /phtrxn/ jno2rxn,jo3rxn,jhcho1rxn,jhcho2rxn,jch3chorxn
c
c-----------------------------------------------------------------------
c     Pointers to heterogeneous reactions to be adjusted in KHETERO
c                    N2O5 + H2O -> 2 HNO3       (1)
c                    N2O5 + HCL -> CLN2 + HNO3  (2)
c                    NTR2 + H2O -> HNO3         (3)
c                    INTR + H2O -> HNO3         (4)
c                    SO2        -> SULF         (5)
c
      integer, parameter :: NHETRXN = 5
      integer, dimension(NHETRXN) :: ihetrxn
      common /hetrxn/ ihetrxn
c
c-----------------------------------------------------------------------
c     Pointer used to lookup pig chemistry rate constant
c
c     ipigrxn  -- pointer to NO self-reaction
c                    NO + NO -> 2 NO2
c
      integer ipigrxn
c
      common /pigrxn/ ipigrxn

      integer, parameter :: l_O    = 1,
     &                      l_O1D  = 2,
     &                      l_OH   = 3,
     &                      l_HO2  = 4,
     &                      l_NO3  = 5,
     &                      l_N2O5 = 6,
     &                      l_NO   = 7,
     &                      l_NO2  = 8,
     &                      l_O3   = 9,
     &                      l_HONO = 10,
     &                      l_HNO3 = 11,
     &                      l_CO   = 12,
     &                      l_FORM = 13,
     &                      l_SO2  = 14,
     &                      l_SULF = 15
      integer, parameter :: NRPIGCHEM = 23
      integer, parameter :: NSPIGCHEM = 15
      integer, dimension(NSPIGCHEM) :: lsmap
      integer, dimension(NRPIGCHEM) :: lrmap
      common /pigchemmap/ lsmap, lrmap
c
c----------------------------------------------------------------------
c    Variables for controlling calls to aerosol routines
c
c     dtaero     -- user input time interval (min) between calls to aerosol 
c                   routines
c     dt_aero    -- adjusted (for I/O frequency) time interval (min) between
c                   calls to aerosol routines
c
      real dtaero
      real dt_aero
c
      common /aero_t/ dtaero, dt_aero
