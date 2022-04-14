c----CAMx v7.10 210105
c  
c     CHMDAT.INC contains all chemistry variables 
c                            
c     Copyright 1996 - 2021
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
c      01/06/19 -cemery-   Added PAL/PSI/PTI
c      01/09/19 -cemery-   Added DMS to CB6r4 (remains Mech 4)
c      11/17/20 -cemery-   Added definition of TUV photolysis file labels
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
c    nemspc  --  number of species in the ALL emissions files
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
       integer      nemspc
       integer      nsmspc
       integer      nsmrxn
c
      common /chm1/ ngas, naero, nspec, nrad, nreact,
     &                         idmech, idsolv, navspc, ndepspc, nicspc,
     &                           nbcspc, ntcspc, nsmspc, nsmrxn
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
      character*20 tuvlabl(10)
c
      common /photmap/ nphot1, nphot2, tuvlabl
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
      parameter ( NSPNAM  =   441 )
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
      integer         ko,    ko1d,     koh
      integer       kho2,   kbzo2,   kc2o3
      integer       kcro,   kcxo3,   kepx2
      integer      khco3,   kiso2,   kmeo2
      integer      kopo3,    kro2,    kror
      integer       kto2,   kxlo2,    kxo2
      integer      kxo2h,   kxo2n,     kbr
      integer       kbro,     kcl,    kclo
      integer         ki,     kio,    koio
      integer      kxpar,   kxprp,   kaacd
      integer      kacet,   kald2,   kaldx
      integer      kbenz,   kcat1,     kco
      integer      kcres,   kcron,   kepox
      integer       keth,   ketha,   kethy
      integer      ketoh,   kfacd,   kform
      integer       kgly,   kglyd,   kh2o2
      integer      khno3,   khono,   khpld
      integer      kintr,   kiole,   kisop
      integer      kispd,   kispx,    kket
      integer      kmeoh,   kmepx,   kmgly
      integer      kn2o5,     kno,    kno2
      integer       kno3,   kntr1,   kntr2
      integer        ko3,    kole,   kopan
      integer      kopen,   kpacd,    kpan
      integer      kpanx,    kpar,    kpna
      integer      kprpa,   krooh,    kso2
      integer      ksulf,   kterp,    ktol
      integer      kxopn,    kxyl,   kech4
      integer        ki2,   ki2o2,   kixoy
      integer       khoi,   khio3,   kino3
      integer       kbr2,   kbrn2,   kbrn3
      integer      kch3i,    kcl2,   kcln2
      integer      kcln3,   kfmbr,   kfmcl
      integer       khbr,    khcl,    khg0
      integer       khg2,     khi,   khobr
      integer      khocl,   kialk,   kino2
      integer       kmb2,   kmb2c,    kmb3
      integer       kmbc,   kmbc2,    kmi2
      integer       kmib,    kmic,    knh3
      integer       ksqt,   kssbr,   ksscl
      integer      kssn3,    kdms,    kntr
      integer       kcg1,    kcg2,    kcg3
      integer       kcg4,   kivoa,   kivob
      integer      kpso4,   kpno3,   kpnh4
      integer       kpoa,   ksoa1,   ksoa2
      integer      ksoa3,   ksoa4,   ksopa
      integer      ksopb,    kpec,   kfcrs
      integer      kfprm,   kccrs,   kcprm
      integer        kna,    kpcl,   kph2o
      integer       kpfe,    kpmn,     kpk
      integer       kpca,    kpmg,    kpal
      integer       kpsi,    kpti,    ko3p
      integer      kbzc3,    kbzo,   kmac3
      integer      kmco3,   krco3,   kro2c
      integer      kro2x,   ktbuo,   kxace
      integer      kxacr,   kxaf1,   kxaf2
      integer      kxaf3,   kxbac,   kxbal
      integer      kxcch,    kxco,   kxgld
      integer      kxgly,   kxhch,   kxho2
      integer      kxipr,   kxma3,   kxmac
      integer      kxmc3,   kxmek,   kxmeo
      integer      kxmgl,   kxmvk,   kxno2
      integer       kxoh,   kxpd2,   kxrc3
      integer      kxrch,   kxrn3,   kxtbu
      integer      ky6px,   kyapx,   kyrpx
      integer      kzrn3,   kacro,   kacye
      integer      kafg1,   kafg2,   kafg3
      integer      kalk1,   kalk2,   kalk3
      integer      kalk4,   kalk5,   kapin
      integer      karo1,   karo2,   kb124
      integer      kbacl,   kbald,   kbd13
      integer      kccho,   kco3h,   kcooh
      integer      kethe,   khcho,   kiprd
      integer      kmacr,    kmek,   kmpan
      integer       kmvk,   kmxyl,   knphe
      integer      kole1,   kole2,   koxyl
      integer      kpan2,   kpbzn,   kprd2
      integer      kprpe,   kpxyl,   kr6px
      integer      krapx,   krcho,   krno3
      integer      kro3h,   ksesq,   ktolu
      integer        kxn,   kivod,   kivog
      integer      kvap1,   kvap2,   kvap3
      integer      kvap4,   kvas1,   kvas2
      integer      kvas3,   kvas4,   kvbs1
      integer      kvbs2,   kvbs3,   kvbs4
      integer      kvcp1,   kvcp2,   kvcp3
      integer      kvcp4,   kvfp1,   kvfp2
      integer      kvfp3,   kvfp4,   kpap0
      integer      kpap1,   kpap2,   kpap3
      integer      kpap4,   kpas0,   kpas1
      integer      kpas2,   kpas3,   kpas4
      integer      kpbs0,   kpbs1,   kpbs2
      integer      kpbs3,   kpbs4,   kpcp0
      integer      kpcp1,   kpcp2,   kpcp3
      integer      kpcp4,   kpfp0,   kpfp1
      integer      kpfp2,   kpfp3,   kpfp4
      integer     khgiip, khgiipc,    khgp
      integer    kpso4_1, kpso4_2, kpso4_3
      integer    kpso4_4, kpso4_5, kpso4_6
      integer    kpso4_7, kpso4_8, kpso4_9
      integer   kpso4_10, kpno3_1, kpno3_2
      integer    kpno3_3, kpno3_4, kpno3_5
      integer    kpno3_6, kpno3_7, kpno3_8
      integer    kpno3_9,kpno3_10, kpnh4_1
      integer    kpnh4_2, kpnh4_3, kpnh4_4
      integer    kpnh4_5, kpnh4_6, kpnh4_7
      integer    kpnh4_8, kpnh4_9,kpnh4_10
      integer     kpoa_1,  kpoa_2,  kpoa_3
      integer     kpoa_4,  kpoa_5,  kpoa_6
      integer     kpoa_7,  kpoa_8,  kpoa_9
      integer    kpoa_10, ksoa1_1, ksoa1_2
      integer    ksoa1_3, ksoa1_4, ksoa1_5
      integer    ksoa1_6, ksoa1_7, ksoa1_8
      integer    ksoa1_9,ksoa1_10, ksoa2_1
      integer    ksoa2_2, ksoa2_3, ksoa2_4
      integer    ksoa2_5, ksoa2_6, ksoa2_7
      integer    ksoa2_8, ksoa2_9,ksoa2_10
      integer    ksoa3_1, ksoa3_2, ksoa3_3
      integer    ksoa3_4, ksoa3_5, ksoa3_6
      integer    ksoa3_7, ksoa3_8, ksoa3_9
      integer   ksoa3_10, ksoa4_1, ksoa4_2
      integer    ksoa4_3, ksoa4_4, ksoa4_5
      integer    ksoa4_6, ksoa4_7, ksoa4_8
      integer    ksoa4_9,ksoa4_10, ksopa_1
      integer    ksopa_2, ksopa_3, ksopa_4
      integer    ksopa_5, ksopa_6, ksopa_7
      integer    ksopa_8, ksopa_9,ksopa_10
      integer    ksopb_1, ksopb_2, ksopb_3
      integer    ksopb_4, ksopb_5, ksopb_6
      integer    ksopb_7, ksopb_8, ksopb_9
      integer   ksopb_10,  kpec_1,  kpec_2
      integer     kpec_3,  kpec_4,  kpec_5
      integer     kpec_6,  kpec_7,  kpec_8
      integer     kpec_9, kpec_10, kcrst_1
      integer    kcrst_2, kcrst_3, kcrst_4
      integer    kcrst_5, kcrst_6, kcrst_7
      integer    kcrst_8, kcrst_9,kcrst_10
      integer      kna_1,   kna_2,   kna_3
      integer      kna_4,   kna_5,   kna_6
      integer      kna_7,   kna_8,   kna_9
      integer     kna_10,  kpcl_1,  kpcl_2
      integer     kpcl_3,  kpcl_4,  kpcl_5
      integer     kpcl_6,  kpcl_7,  kpcl_8
      integer     kpcl_9, kpcl_10, kph2o_1
      integer    kph2o_2, kph2o_3, kph2o_4
      integer    kph2o_5, kph2o_6, kph2o_7
      integer    kph2o_8, kph2o_9,kph2o_10
c
      equivalence
     &   (kmap(  1),      ko),(kmap(  2),    ko1d),(kmap(  3),     koh),
     &   (kmap(  4),    kho2),(kmap(  5),   kbzo2),(kmap(  6),   kc2o3),
     &   (kmap(  7),    kcro),(kmap(  8),   kcxo3),(kmap(  9),   kepx2),
     &   (kmap( 10),   khco3),(kmap( 11),   kiso2),(kmap( 12),   kmeo2),
     &   (kmap( 13),   kopo3),(kmap( 14),    kro2),(kmap( 15),    kror),
     &   (kmap( 16),    kto2),(kmap( 17),   kxlo2),(kmap( 18),    kxo2),
     &   (kmap( 19),   kxo2h),(kmap( 20),   kxo2n),(kmap( 21),     kbr),
     &   (kmap( 22),    kbro),(kmap( 23),     kcl),(kmap( 24),    kclo),
     &   (kmap( 25),      ki),(kmap( 26),     kio),(kmap( 27),    koio),
     &   (kmap( 28),   kxpar),(kmap( 29),   kxprp),(kmap( 30),   kaacd),
     &   (kmap( 31),   kacet),(kmap( 32),   kald2),(kmap( 33),   kaldx),
     &   (kmap( 34),   kbenz),(kmap( 35),   kcat1),(kmap( 36),     kco),
     &   (kmap( 37),   kcres),(kmap( 38),   kcron),(kmap( 39),   kepox),
     &   (kmap( 40),    keth),(kmap( 41),   ketha),(kmap( 42),   kethy),
     &   (kmap( 43),   ketoh),(kmap( 44),   kfacd),(kmap( 45),   kform),
     &   (kmap( 46),    kgly),(kmap( 47),   kglyd),(kmap( 48),   kh2o2),
     &   (kmap( 49),   khno3),(kmap( 50),   khono),(kmap( 51),   khpld),
     &   (kmap( 52),   kintr),(kmap( 53),   kiole),(kmap( 54),   kisop),
     &   (kmap( 55),   kispd),(kmap( 56),   kispx),(kmap( 57),    kket),
     &   (kmap( 58),   kmeoh),(kmap( 59),   kmepx),(kmap( 60),   kmgly),
     &   (kmap( 61),   kn2o5),(kmap( 62),     kno),(kmap( 63),    kno2),
     &   (kmap( 64),    kno3),(kmap( 65),   kntr1),(kmap( 66),   kntr2),
     &   (kmap( 67),     ko3),(kmap( 68),    kole),(kmap( 69),   kopan),
     &   (kmap( 70),   kopen),(kmap( 71),   kpacd),(kmap( 72),    kpan),
     &   (kmap( 73),   kpanx),(kmap( 74),    kpar),(kmap( 75),    kpna),
     &   (kmap( 76),   kprpa),(kmap( 77),   krooh),(kmap( 78),    kso2),
     &   (kmap( 79),   ksulf),(kmap( 80),   kterp),(kmap( 81),    ktol),
     &   (kmap( 82),   kxopn),(kmap( 83),    kxyl),(kmap( 84),   kech4),
     &   (kmap( 85),     ki2),(kmap( 86),   ki2o2),(kmap( 87),   kixoy),
     &   (kmap( 88),    khoi),(kmap( 89),   khio3),(kmap( 90),   kino3),
     &   (kmap( 91),    kbr2),(kmap( 92),   kbrn2),(kmap( 93),   kbrn3),
     &   (kmap( 94),   kch3i),(kmap( 95),    kcl2),(kmap( 96),   kcln2),
     &   (kmap( 97),   kcln3),(kmap( 98),   kfmbr),(kmap( 99),   kfmcl),
     &   (kmap(100),    khbr),(kmap(101),    khcl),(kmap(102),    khg0),
     &   (kmap(103),    khg2),(kmap(104),     khi),(kmap(105),   khobr),
     &   (kmap(106),   khocl),(kmap(107),   kialk),(kmap(108),   kino2),
     &   (kmap(109),    kmb2),(kmap(110),   kmb2c),(kmap(111),    kmb3),
     &   (kmap(112),    kmbc),(kmap(113),   kmbc2),(kmap(114),    kmi2),
     &   (kmap(115),    kmib),(kmap(116),    kmic),(kmap(117),    knh3),
     &   (kmap(118),    ksqt),(kmap(119),   kssbr),(kmap(120),   ksscl),
     &   (kmap(121),   kssn3),(kmap(122),    kdms),(kmap(123),    kntr),
     &   (kmap(124),    kcg1),(kmap(125),    kcg2),(kmap(126),    kcg3),
     &   (kmap(127),    kcg4),(kmap(128),   kivoa),(kmap(129),   kivob),
     &   (kmap(130),   kpso4),(kmap(131),   kpno3),(kmap(132),   kpnh4),
     &   (kmap(133),    kpoa),(kmap(134),   ksoa1),(kmap(135),   ksoa2),
     &   (kmap(136),   ksoa3),(kmap(137),   ksoa4),(kmap(138),   ksopa),
     &   (kmap(139),   ksopb),(kmap(140),    kpec),(kmap(141),   kfcrs),
     &   (kmap(142),   kfprm),(kmap(143),   kccrs),(kmap(144),   kcprm),
     &   (kmap(145),     kna),(kmap(146),    kpcl),(kmap(147),   kph2o),
     &   (kmap(148),    kpfe),(kmap(149),    kpmn),(kmap(150),     kpk),
     &   (kmap(151),    kpca),(kmap(152),    kpmg),(kmap(153),    kpal),
     &   (kmap(154),    kpsi),(kmap(155),    kpti),(kmap(156),    ko3p),
     &   (kmap(157),   kbzc3),(kmap(158),    kbzo),(kmap(159),   kmac3),
     &   (kmap(160),   kmco3),(kmap(161),   krco3),(kmap(162),   kro2c),
     &   (kmap(163),   kro2x),(kmap(164),   ktbuo),(kmap(165),   kxace),
     &   (kmap(166),   kxacr),(kmap(167),   kxaf1),(kmap(168),   kxaf2),
     &   (kmap(169),   kxaf3),(kmap(170),   kxbac),(kmap(171),   kxbal),
     &   (kmap(172),   kxcch),(kmap(173),    kxco),(kmap(174),   kxgld),
     &   (kmap(175),   kxgly),(kmap(176),   kxhch),(kmap(177),   kxho2),
     &   (kmap(178),   kxipr),(kmap(179),   kxma3),(kmap(180),   kxmac),
     &   (kmap(181),   kxmc3),(kmap(182),   kxmek),(kmap(183),   kxmeo),
     &   (kmap(184),   kxmgl),(kmap(185),   kxmvk),(kmap(186),   kxno2),
     &   (kmap(187),    kxoh),(kmap(188),   kxpd2),(kmap(189),   kxrc3),
     &   (kmap(190),   kxrch),(kmap(191),   kxrn3),(kmap(192),   kxtbu),
     &   (kmap(193),   ky6px),(kmap(194),   kyapx),(kmap(195),   kyrpx),
     &   (kmap(196),   kzrn3),(kmap(197),   kacro),(kmap(198),   kacye),
     &   (kmap(199),   kafg1),(kmap(200),   kafg2),(kmap(201),   kafg3),
     &   (kmap(202),   kalk1),(kmap(203),   kalk2),(kmap(204),   kalk3),
     &   (kmap(205),   kalk4),(kmap(206),   kalk5),(kmap(207),   kapin),
     &   (kmap(208),   karo1),(kmap(209),   karo2),(kmap(210),   kb124),
     &   (kmap(211),   kbacl),(kmap(212),   kbald),(kmap(213),   kbd13),
     &   (kmap(214),   kccho),(kmap(215),   kco3h),(kmap(216),   kcooh),
     &   (kmap(217),   kethe),(kmap(218),   khcho),(kmap(219),   kiprd),
     &   (kmap(220),   kmacr),(kmap(221),    kmek),(kmap(222),   kmpan),
     &   (kmap(223),    kmvk),(kmap(224),   kmxyl),(kmap(225),   knphe),
     &   (kmap(226),   kole1),(kmap(227),   kole2),(kmap(228),   koxyl),
     &   (kmap(229),   kpan2),(kmap(230),   kpbzn),(kmap(231),   kprd2),
     &   (kmap(232),   kprpe),(kmap(233),   kpxyl),(kmap(234),   kr6px),
     &   (kmap(235),   krapx),(kmap(236),   krcho),(kmap(237),   krno3),
     &   (kmap(238),   kro3h),(kmap(239),   ksesq),(kmap(240),   ktolu),
     &   (kmap(241),     kxn),(kmap(242),   kivod),(kmap(243),   kivog),
     &   (kmap(244),   kvap1),(kmap(245),   kvap2),(kmap(246),   kvap3),
     &   (kmap(247),   kvap4),(kmap(248),   kvas1),(kmap(249),   kvas2),
     &   (kmap(250),   kvas3),(kmap(251),   kvas4),(kmap(252),   kvbs1),
     &   (kmap(253),   kvbs2),(kmap(254),   kvbs3),(kmap(255),   kvbs4),
     &   (kmap(256),   kvcp1),(kmap(257),   kvcp2),(kmap(258),   kvcp3),
     &   (kmap(259),   kvcp4),(kmap(260),   kvfp1),(kmap(261),   kvfp2),
     &   (kmap(262),   kvfp3),(kmap(263),   kvfp4),(kmap(264),   kpap0),
     &   (kmap(265),   kpap1),(kmap(266),   kpap2),(kmap(267),   kpap3),
     &   (kmap(268),   kpap4),(kmap(269),   kpas0),(kmap(270),   kpas1)
      equivalence
     &   (kmap(271),   kpas2),(kmap(272),   kpas3),(kmap(273),   kpas4),
     &   (kmap(274),   kpbs0),(kmap(275),   kpbs1),(kmap(276),   kpbs2),
     &   (kmap(277),   kpbs3),(kmap(278),   kpbs4),(kmap(279),   kpcp0),
     &   (kmap(280),   kpcp1),(kmap(281),   kpcp2),(kmap(282),   kpcp3),
     &   (kmap(283),   kpcp4),(kmap(284),   kpfp0),(kmap(285),   kpfp1),
     &   (kmap(286),   kpfp2),(kmap(287),   kpfp3),(kmap(288),   kpfp4),
     &   (kmap(289),  khgiip),(kmap(290), khgiipc),(kmap(291),    khgp),
     &   (kmap(292), kpso4_1),(kmap(293), kpso4_2),(kmap(294), kpso4_3),
     &   (kmap(295), kpso4_4),(kmap(296), kpso4_5),(kmap(297), kpso4_6),
     &   (kmap(298), kpso4_7),(kmap(299), kpso4_8),(kmap(300), kpso4_9),
     &   (kmap(301),kpso4_10),(kmap(302), kpno3_1),(kmap(303), kpno3_2),
     &   (kmap(304), kpno3_3),(kmap(305), kpno3_4),(kmap(306), kpno3_5),
     &   (kmap(307), kpno3_6),(kmap(308), kpno3_7),(kmap(309), kpno3_8),
     &   (kmap(310), kpno3_9),(kmap(311),kpno3_10),(kmap(312), kpnh4_1),
     &   (kmap(313), kpnh4_2),(kmap(314), kpnh4_3),(kmap(315), kpnh4_4),
     &   (kmap(316), kpnh4_5),(kmap(317), kpnh4_6),(kmap(318), kpnh4_7),
     &   (kmap(319), kpnh4_8),(kmap(320), kpnh4_9),(kmap(321),kpnh4_10),
     &   (kmap(322),  kpoa_1),(kmap(323),  kpoa_2),(kmap(324),  kpoa_3),
     &   (kmap(325),  kpoa_4),(kmap(326),  kpoa_5),(kmap(327),  kpoa_6),
     &   (kmap(328),  kpoa_7),(kmap(329),  kpoa_8),(kmap(330),  kpoa_9),
     &   (kmap(331), kpoa_10),(kmap(332), ksoa1_1),(kmap(333), ksoa1_2),
     &   (kmap(334), ksoa1_3),(kmap(335), ksoa1_4),(kmap(336), ksoa1_5),
     &   (kmap(337), ksoa1_6),(kmap(338), ksoa1_7),(kmap(339), ksoa1_8),
     &   (kmap(340), ksoa1_9),(kmap(341),ksoa1_10),(kmap(342), ksoa2_1),
     &   (kmap(343), ksoa2_2),(kmap(344), ksoa2_3),(kmap(345), ksoa2_4),
     &   (kmap(346), ksoa2_5),(kmap(347), ksoa2_6),(kmap(348), ksoa2_7),
     &   (kmap(349), ksoa2_8),(kmap(350), ksoa2_9),(kmap(351),ksoa2_10),
     &   (kmap(352), ksoa3_1),(kmap(353), ksoa3_2),(kmap(354), ksoa3_3),
     &   (kmap(355), ksoa3_4),(kmap(356), ksoa3_5),(kmap(357), ksoa3_6),
     &   (kmap(358), ksoa3_7),(kmap(359), ksoa3_8),(kmap(360), ksoa3_9),
     &   (kmap(361),ksoa3_10),(kmap(362), ksoa4_1),(kmap(363), ksoa4_2),
     &   (kmap(364), ksoa4_3),(kmap(365), ksoa4_4),(kmap(366), ksoa4_5),
     &   (kmap(367), ksoa4_6),(kmap(368), ksoa4_7),(kmap(369), ksoa4_8),
     &   (kmap(370), ksoa4_9),(kmap(371),ksoa4_10),(kmap(372), ksopa_1),
     &   (kmap(373), ksopa_2),(kmap(374), ksopa_3),(kmap(375), ksopa_4),
     &   (kmap(376), ksopa_5),(kmap(377), ksopa_6),(kmap(378), ksopa_7),
     &   (kmap(379), ksopa_8),(kmap(380), ksopa_9),(kmap(381),ksopa_10),
     &   (kmap(382), ksopb_1),(kmap(383), ksopb_2),(kmap(384), ksopb_3),
     &   (kmap(385), ksopb_4),(kmap(386), ksopb_5),(kmap(387), ksopb_6),
     &   (kmap(388), ksopb_7),(kmap(389), ksopb_8),(kmap(390), ksopb_9),
     &   (kmap(391),ksopb_10),(kmap(392),  kpec_1),(kmap(393),  kpec_2),
     &   (kmap(394),  kpec_3),(kmap(395),  kpec_4),(kmap(396),  kpec_5),
     &   (kmap(397),  kpec_6),(kmap(398),  kpec_7),(kmap(399),  kpec_8),
     &   (kmap(400),  kpec_9),(kmap(401), kpec_10),(kmap(402), kcrst_1),
     &   (kmap(403), kcrst_2),(kmap(404), kcrst_3),(kmap(405), kcrst_4),
     &   (kmap(406), kcrst_5),(kmap(407), kcrst_6),(kmap(408), kcrst_7),
     &   (kmap(409), kcrst_8),(kmap(410), kcrst_9),(kmap(411),kcrst_10),
     &   (kmap(412),   kna_1),(kmap(413),   kna_2),(kmap(414),   kna_3),
     &   (kmap(415),   kna_4),(kmap(416),   kna_5),(kmap(417),   kna_6),
     &   (kmap(418),   kna_7),(kmap(419),   kna_8),(kmap(420),   kna_9),
     &   (kmap(421),  kna_10),(kmap(422),  kpcl_1),(kmap(423),  kpcl_2),
     &   (kmap(424),  kpcl_3),(kmap(425),  kpcl_4),(kmap(426),  kpcl_5),
     &   (kmap(427),  kpcl_6),(kmap(428),  kpcl_7),(kmap(429),  kpcl_8),
     &   (kmap(430),  kpcl_9),(kmap(431), kpcl_10),(kmap(432), kph2o_1),
     &   (kmap(433), kph2o_2),(kmap(434), kph2o_3),(kmap(435), kph2o_4),
     &   (kmap(436), kph2o_5),(kmap(437), kph2o_6),(kmap(438), kph2o_7),
     &   (kmap(439), kph2o_8),(kmap(440), kph2o_9),(kmap(441),kph2o_10)
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
