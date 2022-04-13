      subroutine cncprep(endtim,enddate,lfirst)
      use camxcom
      use camxfld
      use filunit
      use grid
      use chmstry
      use ncf_iomod
c 
c----CAMx v6.50 180430
c 
c     CNCPREP reads the AIRQUALITY or INSTANT file header (depending on
c     whether this is a restart or not) and maps file species names to
c     the internal CAMx species list. The routine then writes headers
c     to new AVERAGE files
c 
c     Copyright 1996 - 2018
c     Ramboll
c           
c     Modifications: 
c        1/20/99   Grid cell size on file should be meters for all cartesian
c                  projections (UTM, LCP, PSP)
c        10/24/01  Removed BSWAP and converted integer strings to character*4
c        10/31/01  Added logic to ensure that a cold start reads an AIRQUALITY
c                  file and a restart reads an INSTANT file
c        11/06/01  Input dates are now Julian
c        8/25/06   Average output files now all UAM format, one file per grid
c        11/16/06  Now rewinds the files before writing the header
c        01/04/11  Revised for new header format
c 
c     Input arguments: 
c        endtim  - model end time (HHMM)
c        enddate - model end date (YYJJJ)
c        lfirst  - .TRUE. if this is the first time this routine is called
c             
c     Output arguments: 
c        none
c             
c     Routines Called: 
c        none
c             
c     Called by: 
c        STARTUP 
c
      implicit none
      include 'flags.inc'
c
      integer enddate
      real endtim
      logical lfirst
c
      character*200 action
      character*20 spec_units(MXSPEC)
      character*20 spec_long_name(MXSPEC)
      character*60 spec_desc(MXSPEC)
      character*60 spec_coords(MXSPEC)
      character*4 ifile(10),note(60)
      character*4 icspec(10,MXSPEC)
      character*10 aqfil,infil,cnfil,avfil,icspc
      integer izero,ione,iunit,iseg,idat1,idat2,iutm,
     &        nx,ny,nz,iproj,idum,i,n,l,lic,nlayer,num_dims
      real zero,tim1,tim2,plon,plat,orgx,orgy,dx,dy,t1,t2,
     &     dxf,dyf,orgxf,orgyf
c
      data aqfil /'AIRQUALITY'/
      data cnfil /'INSTANT   '/
      data avfil /'AVERAGE   '/
      data izero,ione /0,1/
      data zero /0./
c
      integer istrln   !--- external function
c
c-----Entry point
c
      lairqul = .false.
      iunit = iic
      if (lrstrt) iunit = irstc
c
c-----Read 1st IC header record and check inputs
c
      read(iunit,ERR=7000,END=7000) ifile,note,iseg,nicspc,idat1,
     &                                             tim1,idat2,tim2
      if (INT(tim2) .EQ. 24 ) then
        idat2 = idat2 + 1
        tim2 = 0.
        if( MOD(idat2,1000) .GT. 365 ) then
           if( MOD(INT(idat2/1000),4) .EQ. 0 ) then
              if( MOD(idat2,1000) .EQ. 367 )
     &                     idat2 = (INT(idat2/1000)+1)*1000 + 1
           else
              idat2 = (INT(idat2/1000)+1)*1000 + 1
           endif
        endif
      endif
      write(infil,'(10a1)') (ifile(n),n=1,10)
      if (.not.lrstrt .and. infil.ne.aqfil) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'This is a cold start from Initial Conditions'
        write(iout,*)'IC input file is not labelled AIRQUALITY'
        call camxerr()
      endif
      if (lrstrt .and. infil.ne.cnfil) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'This is a restart from a Restart File'
        write(iout,*)'IC input file is not labelled INSTANT'
        call camxerr()
      endif
      if (infil.eq.aqfil) lairqul = .true.
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      if (idat1.gt.begdate) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'IC start date > simulation start date'
        write(iout,*)'  IC file: ',idat1
        write(iout,*)'Sim start: ',begdate
        call camxerr()
      elseif (idat1.eq.begdate .and. tim1.gt.begtim) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'IC start time > simulation start time'
        write(iout,*)'  IC file: ',tim1
        write(iout,*)'Sim start: ',begtim
        call camxerr()
      endif
c
c-----Read 2nd IC header record and check inputs
c
      read(iunit,ERR=7001) plon,plat,iutm,orgx,orgy,dx,dy,nx,ny,nz,
     &                     iproj,idum,t1,t2
      if (.NOT.llatlon) then
        orgx = orgx/1000.
        orgy = orgy/1000.
        dx = dx/1000.
        dy = dy/1000.
      endif
      if (abs(orgx-xorg).gt.0.001 .or. abs(orgy-yorg).gt.0.001) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,'(a)') 'Reading IC/Restart file'
        write(iout,*)'Met origin not equal to model origin'
        write(iout,'(a,2f12.4)')'  IC file: ',orgx,orgy
        write(iout,'(a,2f12.4)')'    Model: ',xorg,yorg
        write(iout,*)
        call camxerr()
      endif
      if (abs(dx-delx).gt.0.001 .or. abs(dy-dely).gt.0.001) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,'(a)') 'Reading IC/Restart file'
        write(iout,*)'IC cell size not equal to model cell size'
        write(iout,'(a,2f10.4)')'  IC file: ',dx,dy
        write(iout,'(a,2f10.4)')'    model: ',delx,dely
        write(iout,*)
        call camxerr()
      elseif (nx.ne.ncol(1) .or. ny.ne.nrow(1) 
     &                          .or. nz.ne.nlay(1)) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,'(a)') 'Reading IC/Restart file'
        write(iout,*)'IC grid size not equal to model grid size'
        write(iout,*)'IC file: ',nx,ny,nz
        write(iout,*)'  model: ',ncol(1),nrow(1),nlay(1)
        write(iout,*)
        call camxerr()
      endif 
c
c-----Read 3rd & 4th IC header 
c
      read(iunit,ERR=7001) 
      read(iunit,ERR=7001) ((icspec(n,l),n=1,10),l=1,nicspc)
c
c-----Map IC species to model species
c
      do 20 l = 1,nspec
        licmap(l,1) = 0
        do 15 lic = 1,nicspc
          write(icspc,'(10a1)') (icspec(n,lic),n=1,10)
          if (icspc.eq.'HNO2      ') icspc = 'HONO      '
          if (icspc.eq.'HCHO      ' .and. kHCHO.eq.nspec+1)
     &                                        icspc = 'FORM      '
          if (icspc.eq.spname(l)) then
            licmap(l,1) = lic
            write(idiag,'(2(a,i5,2x,a))')
     &                   'Initial species ',lic,icspc,
     &                   ' mapped to model species ',l,spname(l)
            goto 20
          endif
 15     continue
        write(idiag,*)'Did not find species: ',spname(l),' on IC file'
        if (.not.lairqul) then
          write(iout,'(//,a)') 'ERROR in CNCPREP:'
          write(iout,*)'The INSTANT file must contain the same ',
     &                 'species as specified in the CHEMPARAM file'
          call camxerr()
        endif
 20   continue
      write(idiag,*)
c 
c-----Write average output concentration file headers
c
      idat1 = begdate
      idat2 = enddate
      tim1 = begtim/100.
      tim2 = endtim/100.
      iutm = iuzon
      plon = polelon
      plat = polelat
      t1   = tlat1
      t2   = tlat2
      if (llatlon) then
        iproj = 0
        orgx = xorg
        orgy = yorg
        dx = delx
        dy = dely
      else
        orgx = 1000.*xorg
        orgy = 1000.*yorg
        dx = 1000.*delx
        dy = 1000.*dely
        if (lutm) then
          iproj = 1
        elseif (lambrt) then
          iproj = 2
        elseif (lrpolar) then
          iproj = 3
        elseif (lpolar) then
          iproj = 4
        elseif (lmerc) then
          iproj = 5
        endif
      endif
      read(runmsg(1:60),'(60a1)') (note(n),n=1,60)
      read(avfil,'(10a1)') (ifile(n),n=1,10)
      do l = 1,navspc
        if(lavmap(l) .GT. 0 ) then 
          read(spname(lavmap(l)),'(10a1)') (icspec(n,l),n=1,10)
          spavnam(l) = spname(lavmap(l))
        endif
      enddo
      if( l3davg(1) ) then
        nlayer = nlay(1)
        num_dims = 4
      else
        nlayer = 1 
        num_dims = 4
      endif
c
c-----Master grid average header
c
      if( lcdfout .AND. lfirst ) then
          call ncf_set_vars_base()
          call ncf_set_tstep(begdate,begtim,enddate,endtim)
      endif
      if( .NOT. lcdfout) then
          rewind(iavg(1))
          write(iavg(1)) ifile,note,itzon,navspc,idat1,tim1,idat2,tim2
          write(iavg(1)) plon,plat,iutm,orgx,orgy,dx,dy,nx,ny,nlayer,
     &                                       iproj,izero,t1,t2,zero
          write(iavg(1)) ione,ione,nx,ny
          write(iavg(1)) ((icspec(n,l),n=1,10),l=1,navspc)
      else if( lcdfout .AND. lfirst ) then
          action = 'Writing master grid output average file.'
          call ncf_set_specatt_avrg(spec_units,spec_long_name,spec_desc,
     &                                                        spec_coords)
          call ncf_set_global('AVERAGE   ',1,begdate,begtim,enddate,endtim,
     &                                                       nlayer,navspc)
          call ncf_wrt_dim(action,iavg(1),1,ncol(1),nrow(1),nlayer,navspc)
          call ncf_wrt_global(action,iavg(1),navspc,spavnam,.FALSE.)
          call ncf_wrt_vars_base(action,iavg(1))
          call ncf_wrt_vars_species(action,iavg(1),ncol(1),nrow(1),nlayer,
     &                              navspc,spavnam,spec_units,spec_long_name,
     &                                        spec_desc,spec_coords,num_dims)
          call ncf_enddef_file(action,iavg(1))
          call ncf_wrt_data_grid(action,iavg(1),1,ncol(1),nrow(1),
     &                           orgx,orgy,dx,dy,nlayer,cellat(iptr2d(1)),
     &                                   cellon(iptr2d(1)),topo(iptr2d(1)))
      endif
      if( ngrid .EQ. 1 ) goto 9999
c
c-----Fine grid average headers
c
      do i = 2,ngrid 
        dxf   = dx/float(meshold(i))
        dyf   = dy/float(meshold(i))
        orgxf = orgx + dx*(inst1(i)-1) - dxf
        orgyf = orgy + dy*(jnst1(i)-1) - dyf
        if( l3davg(i) ) then
          nlayer = nlay(i)
        else
          nlayer = 1
        endif
        if( .NOT. lcdfout ) then
           rewind(iavg(i))
           write(iavg(i)) ifile,note,itzon,navspc,idat1,tim1,idat2,tim2
           write(iavg(i)) plon,plat,iutm,orgxf,orgyf,dxf,dyf,ncol(i),
     &                       nrow(i),nlayer,iproj,izero,t1,t2,zero
           write(iavg(i)) ione,ione,ncol(i),nrow(i)
           write(iavg(i)) ((icspec(n,l),n=1,10),l=1,navspc)
        else if( lcdfout .AND. lfirst ) then
          write(action,'(A,I2)') 'Writing output average file for grid: ',i
          call ncf_set_specatt_avrg(spec_units,spec_long_name,spec_desc,
     &                                                        spec_coords)
          call ncf_set_global('AVERAGE   ',i,begdate,begtim,enddate,endtim,
     &                                                      nlayer,navspc)
          call ncf_wrt_dim(action,iavg(i),i,ncol(i),nrow(i),nlayer,navspc)
          call ncf_wrt_global(action,iavg(i),navspc,spavnam,.FALSE.)
          call ncf_wrt_vars_base(action,iavg(i))
          call ncf_wrt_vars_species(action,iavg(i),ncol(i),nrow(i),nlayer,
     &                            navspc,spavnam,spec_units,spec_long_name,
     &                                       spec_desc,spec_coords,num_dims)
          call ncf_enddef_file(action,iavg(i))
          call ncf_wrt_data_grid(action,iavg(i),i,ncol(i),nrow(i),
     &                           orgx,orgy,dx,dy,nlayer,cellat(iptr2d(i)),
     &                                  cellon(iptr2d(i)),topo(iptr2d(i)))
        endif
      enddo 
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in CNCPREP:'
      write(iout,'(A)',ERR=9999)'Reading restart file for coarse grid.'
      write(iout,'(2A)',ERR=9999)'Make sure the filename is specified ',
     &          'correctly and the previous day finished correctly.'
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in CNCPREP:'
      write(iout,'(2A)',ERR=9999)'Reading the header of restart file ',
     &                                  'for coarse grid.'      
      call camxerr()
c
 9999 continue
      lfirst = .FALSE.
      return
      end
