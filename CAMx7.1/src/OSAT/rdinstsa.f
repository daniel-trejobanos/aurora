c*** RDINSTSA
c
      subroutine rdinstsa(idate,btim,nox,noy,noz,nspsa)
      use filunit
      use grid
      use tracer
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c   Description:
c     This routine reads the instantaneous file for the tracer
c     species.  A file is read for each grid nest and the data is
c     stored in the tracer concentration array.
c
c     Copyright 1996 - 2021
c     Ramboll
c
c    Argument description:
c     Outputs:
c     Inputs: 
c        idate   I   begining date of simulation (YYJJJ)
c        btim    R   begining time of simulation
c        nox     I   number of X cells in grid
c        noy     I   number of Y cells in grid
c        noz     I   number of layers in grid
c        nspsa   I   number of species in conc array
c
c-----------------------------------------------------------------------
c   LOG:
c     1/20/99   Grid cell size from file should be meters for all
c               cartesian projections (UTM, LCP, PSP)
c     10/24/01  Removed BSWAP and converted integer strings to character*4
c     11/06/01  Input dates are now Julian
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   idate
      real      btim
      integer   nox
      integer   noy
      integer   noz
      integer   nspsa
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 cname
      character*4  fname(10), fnote(60)
      integer      nsegs, nspcs, ibgdat, iendat
      integer      nzonin, noxgin, noygin, nzclin
      integer      ntoday, isegmt, ndate, numold
      integer      i, j, k, l
      real         begtim, endtim, xutmin, yutmin, xorgin, yorgin
      real         delxin, delyin, ttime
      logical      lmatch
c
      character*10 savename(MXTRSP)
      character*4  ispec(10,MXTRSP)
      real         cnctmp(MXCELLS,MXCELLS)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      numold = ntotsp
      ndate = idate
      ttime = btim / 100. 
c
c   --- open the coarse grid instantaneous file ---
c
      open(unit=iorini(IDXCRS),file=inifil(IDXCRS),
     &                    form='UNFORMATTED',ERR=7006,status='UNKNOWN')
c
c   --- file description header ---
c
      read(iorini(IDXCRS),ERR=7000,END=7000) fname, fnote, nsegs, 
     &                           nspcs, ibgdat, begtim, iendat, endtim
      if( nspcs .GT. MXTRSP ) then
        write(iout,'(//,A)') 'ERROR in RDINSTSA:'
        write(iout,*) 'A parameter in the camx.prm is not ',
     &                                        'sufficiently large.'
        write(iout,*) 'Please change the value for parameter: MXTRSP'
        write(iout,*) 'It should be set to a value of at least: ',nspcs
        call flush(iout)
        call camxerr()
      endif
c
c  --- make sure this is the right file ---
c
      write(cname,'(10A1)') (fname(i),i=1,10) 
      if( cname .NE. 'AIRQUALITY' ) goto 7001
c
c  --- make sure time span is correct ---
c
      if( begtim .EQ. 24.0 ) then
          begtim = 0.
          ibgdat = ibgdat + 1
      endif
      if( MOD(ibgdat,1000) .GT. 365 ) then
          if( MOD(INT(ibgdat/1000),4) .EQ. 0 ) then
             if( MOD(ibgdat,1000) .EQ. 367 )
     &                     ibgdat = (INT(ibgdat/1000)+1)*1000 + 1
          else
             ibgdat = (INT(ibgdat/1000)+1)*1000 + 1
          endif
      endif
      if( ibgdat .NE. ndate .OR. 
     &                        ABS(begtim-ttime) .GT. 0.00001 ) goto 7002
c
c   --- region description header ----
c
      read(iorini(IDXCRS),ERR=7000) xutmin, yutmin, nzonin, 
     &            xorgin, yorgin, delxin, delyin, noxgin, noygin, nzclin
      lmatch = .TRUE.
      if( nzonin .NE. iuzon ) lmatch = .FALSE.
c
c   --- set scaling factor for coordinates ---
c
      if( .NOT.llatlon) then
         factr = 1000.0
      else
         factr = 1.0
      endif
      xorgin = xorgin/factr
      yorgin = yorgin/factr
      delxin = delxin/factr
      delyin = delyin/factr
      if( ABS(xorgin-xorg) .GT. 0.0001 ) lmatch = .FALSE.
      if( ABS(yorgin-yorg) .GT. 0.0001 ) lmatch = .FALSE.
      if( ABS(delxin-delx) .GT. 0.0001 ) lmatch = .FALSE.
      if( ABS(delyin-dely) .GT. 0.0001 ) lmatch = .FALSE.
      if( noxgin .NE. nox ) lmatch = .FALSE.
      if( noygin .NE. noy ) lmatch = .FALSE.
      if( nzclin .NE. noz ) lmatch = .FALSE.
      if( .NOT. lmatch ) goto 7003
c
c  --- segment description header ---
c
      read(iorini(IDXCRS),ERR=7000) 
c
c  ---- species list ---
c
      read(iorini(IDXCRS),ERR=7000) ((ispec(i,j),i=1,10),j=1,nspcs)
c
c  --- make sure species list is compatable, up to timing tracers ---
c
      do l=1,ipttim-1
         write(cname,'(10A1)') (ispec(i,l),i=1,10)
         if( cname .NE. ptname(l) ) goto 7004
      enddo
c
c  --- shift the species list around to get the previous timing tracers
c      in the list as well as the timing tracers for this run ---
c
      if( ntrtim .GT. 0 ) then
         ntoday = ntotsp - ipttim + 1
         ntotsp = nspcs + ntoday
         if( ntotsp .GT. MXTRSP ) then
           write(iout,'(//,A)') 'ERROR in RDINSTSA:'
           write(iout,*) 'A parameter in the camx.prm is not ',
     &                                        'sufficiently large.'
           write(iout,'(2A)') ' You are using timing tracers but ',
     &               'have not allocated space for the timing releases.'
           write(iout,*) 'Please change the value for parameter: MXTRSP'
           write(iout,*) 'It should be set to a value of at least: ',
     &                                                            ntotsp
           call flush(iout)
           call camxerr()
         endif
c
c   --- call the pointer routine again to update for tracer arrays ---
c
         call iniptr(ncol,nrow)
c
c  --- call routine to re-allocate based on the real total
c      number of species ----
c
         do l=1,nspcs
            savename(l) = ptname(l)
         enddo
         call realloc_tracer_specs(ngrid,ncol,nrow,nlay,nlayers_ems,
     &                                                   numold,iout)
c 
         npttim = npttim + ntoday
         nreles = (nspcs - ipttim +1) / (2*nregin) 
         do l=ntoday,1,-1
            ptname(nspcs+l) = savename(ipttim-1+l)
            loutsa(nspcs+l) = .TRUE.
         enddo
         do l=1,ipttim-1
            ptname(l) = savename(l)
         enddo
         do l=ipttim,nspcs
            write(cname,'(10A1)') (ispec(i,l),i=1,10)
            ptname(l) = cname
         enddo
         nsaspc = nspcs
      endif
c
c   --- check for consistency with previous days's run ----
c
      if( nspcs .GT. ntotsp ) then
        write(iout,'(//,A)') 'ERROR in RDINSTSA:'
        write(iout,*) 'The number of species in the SA restart file ',
     &                                  'is inconsistent with this run.'
        write(iout,*) 'The are more species in the file ',
     &                             'than necessary for this applicaton.'
        write(iout,*) '  Number of species on file: ',nspcs
        write(iout,*) '  Number of tracer species requested: ',ntotsp
        write(iout,*) 'Make sure you used the correct options in the ',
     &                                              'CAMx control file.'
        call flush(iout)
        call camxerr()
      endif

c
c   --- read the data for this hour ----
c
      read(iorini(IDXCRS),ERR=7000) ibgdat, begtim, iendat, endtim
      do l=1,nspcs
         do k=1,noz
            read(iorini(IDXCRS)) isegmt, (ispec(i,l),i=1,10), 
     &                            ((cnctmp(i,j),i=1,nox),j=1,noy)
            call loadinstsa(nox,noy,noz,ntotsp,k,l,ptconc,cnctmp)
         enddo
      enddo
c
c  --- initialize all of the timing tracers concs to zero to start off ---
c
      do j=1,noy
        do i=1,nox
           cnctmp(i,j) = 0.
        enddo
      enddo
      do l=nspcs+1,nsaspc
         do k=1,noz
             call loadinstsa(nox,noy,noz,ntotsp,k,l,ptconc,cnctmp)
         enddo
         do i=1,MXRECP
            conrcp(l,i) = 0.
         enddo
      enddo
c
c  --- close file and return to calling routine ---
c
      close(iorini(IDXCRS))
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in RDINSTSA:'
      write(iout,9000,ERR=9999)'Reading the header of the ',
     &    'tracer concentration initialization file for coarse grid: ',
     &                          inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in RDINSTSA:'
      write(iout,9000,ERR=9999) 'Tracer concentration ',
     &    'initilization file for coarse grid appears to be type: ',
     &                                                        cname
      write(iout,9000,ERR=9999) 'It should be type: AIRQUALITY'
      write(iout,9000,ERR=9999) 'Filename: ',
     &                          inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
      call camxerr()
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in RDINSTSA:'
      write(iout,9000,ERR=9999) 'Tracer concentration ',
     &   'initialization file for coarse grid is not for correct ',
     &                               'time period; ',
     &                          inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
      write(iout,9001,ERR=9999) 'Simulation Start: ',ndate,ttime
      write(iout,9001,ERR=9999) 'Initialization File: ',ibgdat,begtim
      call camxerr()
c
 7003 continue
      write(iout,'(//,a)') 'ERROR in RDINSTSA:'
      write(iout,9000,ERR=9999) 'Region definition from ',
     &  ' tracer concentration file for coarse grid does not match ',
     &                                           'simulation control.'
      write(iout,9000,ERR=9999) '      Initialization file: ',
     &                          inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
      write(iout,9003,ERR=9999) 'UTM zone',nzonin
      write(iout,9002,ERR=9999) 'Region origin',xorgin,yorgin
      write(iout,9002,ERR=9999) 'Cell spacing',delxin, delyin
      write(iout,9004,ERR=9999) 'Number of cells',noxgin,noygin
      write(iout,9003,ERR=9999) 'Vertical cells',nzclin
      write(iout,9000,ERR=9999) 
      write(iout,9000,ERR=9999) '      Simulation Control'
      write(iout,9003,ERR=9999) 'UTM zone',iuzon
      write(iout,9002,ERR=9999) 'Region origin',xorg,yorg
      write(iout,9002,ERR=9999) 'Cell spacing',delx, dely
      write(iout,9004,ERR=9999) 'Number of cells',nox,noy
      write(iout,9003,ERR=9999) 'Vertical cells',noz
      call camxerr()
c
 7004 continue
      write(iout,'(//,a)') 'ERROR in RDINSTSA:'
      write(iout,9000,ERR=9999) 'Species list for tracer ',
     &            'concentration initialization file for coarse grid',
     &          ' is inconsistent with user parameters ',
     &                          inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
      write(iout,9006,ERR=9999) 'Species number: ',l,' should be ',
     &                      ptname(l),' and ',cname,' was read instead.'
      call camxerr()
c
 7006 continue
      write(iout,'(//,a)') 'ERROR in RDINSTSA:'
      write(iout,9000,ERR=9999) 'Opening the ',
     &    'tracer concentration initialization file for coarse grid: ',
     &                          inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
      write(iout,9005,ERR=9999) 'Make sure the name for output files ',
     &                           'reflects the correct simulation day.'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(/,1X,6A)
 9001 format(1X,A,I10.5,F10.1)
 9002 format(1X,A,T20,': (',F10.2,',',F10.2,')')
 9003 format(1X,A,T20,': ',I10)
 9004 format(1X,A,T20,': (',I10,',',I10,')')
 9005 format(10X,2A)
 9006 format(1X,A,I5,5A)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
