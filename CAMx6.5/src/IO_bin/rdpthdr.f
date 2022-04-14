      subroutine rdpthdr(begtim,begdate,endtim,enddate)
      use filunit
      use grid
      use chmstry
      use bndary
      use ptemiss
      use tracer
c 
c----CAMx v6.50 180430
c 
c     RDPTHDR reads the header of binary point source emissions file,
c     initializes time-invariant point source variables, and maps the point
c     source species list to the internal CAMx species list
c                           
c     Copyright 1996 - 2018
c     Ramboll
c           
c     Modifications: 
c        10/24/01  Removed BSWAP and converted integer strings to character*4
c        11/06/01  Input dates are now Julian
c        02/09/02  Added code to handle end of year dates
c        12/21/13  Added compact point source file
c        12/07/14  Revised for VBS emissions
c        01/08/16  Updated for Revised VBS emissions
c        11/22/17  Added checks for PFE/PMN/PK/PCA/PMG emissions (disabled)
c 
c     Input arguments: 
c        begtim              model start time (HHMM) 
c        begdate             model start date (YYJJJ) 
c        endtim              model end time (HHMM) 
c        enddate             model end date (YYJJJ)
c             
c     Output arguments: 
c             
c     Routines Called: 
c        none
c             
c     Called by: 
c        PNTPREP
c 
      include 'camx.prm'
      include 'flags.inc'
      include 'vbs.inc'
c
      integer begdate
      integer enddate
c
      character*10 ptfil, infil, ptspc, ptcompact
      character*4  ifile(10), note(60)
c
      character*4 ptspec(10,MXSPEC)
c
      logical l_exist_ivoc, l_exist_poa_xx
c
      integer, parameter :: num_elem = 5
      integer :: idx_elem(num_elem)
c
      data ptfil /'PTSOURCE  '/
      data ptcompact /'PTSOURCECP'/
c
c-----Entry point
c
c-----Initialize the mapping array ---
c
      do l=1,nspec
        lptmap(l) = 0
      enddo
c
c-----Read 1st PT header record and check inputs 
c             
      rewind(iptem)
      read(iptem) ifile,note,nseg,nptspc,idat1,tim1,idat2,tim2
c             
      if(INT(tim2) .EQ. 24) then
        tim2 = 0.
        idat2 = idat2 + 1
        if( MOD(idat2,1000) .GT. 365 ) then
           if( MOD(INT(idat2/1000),4) .EQ. 0 ) then
              if( MOD(idat2,1000) .EQ. 367 ) 
     &                       idat2 = (INT(idat2/1000)+1)*1000 + 1
           else
              idat2 = (INT(idat2/1000)+1)*1000 + 1
           endif
        endif
      endif
      write(infil,'(10a1)') (ifile(n),n=1,10) 
      if (infil.ne.ptfil .AND. infil .ne. ptcompact) then 
        write(iout,'(//,a)') 'ERROR in RDPTHDR:'
        write(iout,*)'PT input file is not labelled PTSOURCE or PTSOURCECP' 
        call camxerr()
      endif   
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      if (idat1.gt.begdate) then 
        write(iout,'(//,a)') 'WARNING in RDPTHDR:'
        write(iout,*)'PT start date > simulation start date' 
        write(iout,*)'  PT file: ',idat1 
        write(iout,*)'Sim start: ',begdate 
        if (.not.le1day) then
          write(iout,*)'CAMx expecting day-specific emissions: Stopping'
          call camxerr()
        endif
      elseif (idat1.eq.begdate .and. tim1.gt.begtim) then 
        write(iout,'(//,a)') 'WARNING in RDPTHDR:'
        write(iout,*)'PT start time > simulation start time' 
        write(iout,*)'  PT file: ',tim1 
        write(iout,*)'Sim start: ',begtim 
        call camxerr()
      elseif (idat2.lt.enddate) then 
        write(iout,'(//,a)') 'WARNING in RDPTHDR:'
        write(iout,*)'PT end date < simulation end date' 
        write(iout,*)'PT file: ',idat2
        write(iout,*)'Sim end: ',enddate 
        if (.not.le1day) then
          write(iout,*)'CAMx expecting day-specific emissions: Stopping'
          call camxerr()
        endif
      elseif (idat2.eq.enddate .and. tim2.lt.endtim) then 
        write(iout,'(//,a)') 'ERROR in RDPTHDR:'
        write(iout,*)'PT end time < simulation end time' 
        write(iout,*)'PT file: ',tim2
        write(iout,*)'Sim end: ',endtim 
        call camxerr()
      endif 
c 
c-----Read 2nd PT header
c 
      read(iptem) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz 
c 
c-----Read 3rd & 4th PT header 
c 
      read(iptem)
      read(iptem) ((ptspec(n,l),n=1,10),l=1,nptspc) 
c 
c-----Map PT species to model species 
c 
      if (lvbs .and. LVBSPREPROC) then ! assign fake positive idx to count all the primary VBS spc in
        do l = 0, NVOLBIN
          lptmap(kpap_c(l)) = nptspc + 1
          lptmap(kpfp_c(l)) = nptspc + 1
        enddo
        l_exist_ivoc   = .false.
        l_exist_poa_xx = .false.
      endif
      do 15 lpt = 1,nptspc 
        write(ptspc,'(10a1)') (ptspec(n,lpt),n=1,10) 
        if (ptspc.eq.'HNO2      ') ptspc = 'HONO      '
        if (ptspc.eq.'HCHO      ' .and. kHCHO.eq.nspec+1)
     &                                        ptspc = 'FORM      '

        if (lvbs .and. LVBSPREPROC) then
          if (ptspc.eq.'IVOA      ' .or.
     &        ptspc.eq.'IVOB      ') l_exist_ivoc = .true.
          if (ptspc.eq.'POA_OP    ') then
            l_exist_poa_xx = .true.
            lptmap(kpap_c(0)) = lpt ! temporarily assign to PAP0
            write(idiag,'(a,i5,2x,2a)') 'Point source species ',
     &           lpt,ptspc,' mapped to model species PAP0-PAP5 (VBS)'
            goto 15
          endif
          if (ptspc.eq.'POA_BB    ') then
            l_exist_poa_xx = .true.
            lptmap(kpfp_c(0)) = lpt ! temporarily assign to PFP0
            write(idiag,'(a,i5,2x,2a)') 'Point source species ',
     &           lpt,ptspc,' mapped to model species PFP0-PFP5 (VBS)'
            goto 15
          endif
        endif

        do 20 l = 1,nspec 
          if (ptspc.eq.spname(l)) then 
            lptmap(l) = lpt
            write(idiag,'(2(a,i5,2x,a))')
     &                   'Point source species ',lpt,ptspc, 
     &                   ' mapped to model species ',l,spname(l) 
            goto 15 
          endif 
 20     continue
        write(idiag,*)'PT species: ',ptspc,' not modeled'
 15   continue 
c
      if (lvbs .and. LVBSPREPROC) then
        if ( .not.l_exist_ivoc .or. .not.l_exist_poa_xx ) then
          write(iout,'(//,a)') 'ERROR in RDPTHDR:'
          write(iout,'(2A)')'Point source species are not compatible',
     &                      ' with VBS.'
          write(iout,'(2A)')'VBS requires IVOC and sector-specific',
     &                      ' POA emissions.'
          call camxerr()
        endif
      endif
c
!bk      idx_elem = (/ kPFE, kPMN, kPK, kPCA, kPMG /)
!bk      do l = 1, num_elem
!bk        if ( idx_elem(l).lt.nspec+1 ) then
!bk          if ( lptmap(idx_elem(l)).le.0 ) then
!bk            write(iout,'(//,a)') 'ERROR in RDPTHDR:'
!bk            write(iout,'(2A)') 'Point source species missing - ',
!bk     &                          spname(idx_elem(l))
!bk            write(iout,'(2A)') 'You must provide the emissions or ',
!bk     &                         'remove it from the CHEMPARAM input.'
!bk            call camxerr()
!bk          endif
!bk        endif
!bk      enddo
c
c-----Read time invariant data (assume nseg=1)
c     check number of sources against max 
c
      read(iptem) idum,nptsrc
c
c-----Allocate the arrays ---
c
      allocate( xloc(nptsrc) )
      allocate( yloc(nptsrc) )
c
c----Call routine to allocate the arrays 
c
      call alloc_ptemiss(nspec,ngrid)
      if( ltrace .OR. lddm .OR. lhddm ) then
        call alloc_tracer_pts(nptsrc)
      endif
c
      read(iptem) (xloc(n),yloc(n),hstk(n),dstk(n),tstk(n),vstk(n),
     &             n=1,nptsrc)
c
      do 30 n = 1,nptsrc
        if (dstk(n).lt.0.) then
          lpiglet(n) = .true.
c         dstk(n) = -dstk(n)
        else
          lpiglet(n) = .false.
        endif
        vstk(n) = vstk(n)/3600.
c
c======================== Source Apportion Begin =======================
c
c
c   --- put point source locations into array for
c       probing tools verification ---
c
      if( ltrace  .OR. lddm .OR. lhddm ) then
        xlocpt(n) = xloc(n)
        ylocpt(n) = yloc(n)
      endif
c
c========================= Source Apportion End ========================
c
  30  continue
      write(idiag,*)
c
      return
      end
