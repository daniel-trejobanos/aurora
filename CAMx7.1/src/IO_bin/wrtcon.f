      subroutine wrtcon(iflag,tim2,idat2,iunit,igrd,nox,noy,noz,
     &                  nsptmp,cncfld)
      use grid
      use chmstry
      use camxcom
c
c----CAMx v7.10 210105
c 
c     WRTCON writes average and instantaneous concentration fields; for
c     average files, optionally writes only layer 1; for instantaneous files,
c     writes all layers.
c 
c     Copyright 1996 - 2021
c     Ramboll
c           
c     Modifications: 
c        1/20/99   Grid cell size on file should be meters for all cartesian
c                  projections (UTM, LCP, PSP)
c        10/24/01  Removed BSWAP and converted integer strings to character*4
c        8/23/06   Only a single instant restart file is written at end of
c                  simulation
c        12/15/08  Added code to handle averaging of radicals
c        01/04/11  Revised for new header format
c 
c     Input arguments:
c        iflag               output type flag (0=average/1=instantaneous)
c        tim2                output time (HHMM)
c        idat2               output date (YYJJJ)
c        iunit               output unit
c        igrd                grid number
c        nox                 number of cells in x-direction
c        noy                 number of cells in y-direction
c        noz                 number of layers
c        nsptmp              number of species
c        cncfld              concentration field to output (ppm or umol/m3)
c             
c     Output arguments: 
c        none
c             
c     Routines Called: 
c        none
c             
c     Called by: 
c        CAMx
c 
      include 'camx.prm'
      include 'flags.inc'
c
      real cncfld(nox,noy,noz,nsptmp)
c
      character*4 ifile(10), note(60)
      character*10 cnfil
c
      character*4 ispec(10,MXSPEC)
      integer     buffer_offset
c
c-----Data statements
c
      data cnfil /'INSTANT   '/
      data nseg,izero,ione /1,0,1/
      data zero /0./
c
c-----Entry point
c
       buffer_offset = 0
c
c-----Determine time/date range
c
      idat1 = idat2
      etim = AINT(ANINT(tim2)/100.) + amod(ANINT(tim2),100.)/60.
      btim = ANINT( 1000*(etim - ANINT(dtout)/60.) )/1000.
      if (btim.lt.0.) then
        btim = btim + 24.
        idat1 = idat1 - 1
        if (MOD(idat1,1000) .EQ. 0 ) then
          if ( MOD(INT(idat1/1000)-1,4) .EQ. 0 ) then
            idat1 = (INT(idat1/1000)-1)*1000 + 366
          else
            idat1 = (INT(idat1/1000)-1)*1000 + 365
          endif
        endif
      endif
      idat3 = idat2
      etim3 = etim + 0.1
      if (etim3.gt.24.) then
        etim3 = etim3 - 24.
        idat3 = idat3 + 1
        if( MOD(idat3,1000) .GT. 365 ) then
            if( MOD(INT(idat3/1000),4) .EQ. 0 ) then
               if( MOD(idat3,1000) .EQ. 367 )
     &                     idat3 = (INT(idat3/1000)+1)*1000 + 1
            else
               idat3 = (INT(idat3/1000)+1)*1000 + 1
            endif
         endif
      endif
c
c-----For instantaneous files, write the header
c
      nlayer = noz
      if (iflag.eq.1) then
        read(cnfil,'(10a1)') (ifile(n),n=1,10)
        read(runmsg(1:60),'(60a1)') (note(n),n=1,60)
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
        do l=1,nsptmp
          read(spname(l),'(10a1)') (ispec(n,l),n=1,10) 
        enddo
c
        write(iunit) ifile,note,itzon,nsptmp,idat2,etim,idat3,etim3
        write(iunit) plon,plat,iutm,orgx,orgy,dx,dy,
     &               nox,noy,noz,iproj,izero,t1,t2,zero
        write(iunit) ione,ione,nox,noy
        write(iunit) ((ispec(n,l),n=1,10),l=1,nsptmp)
        write(iunit) idat2,etim,idat3,etim3
      else
        if( igrd. GT. 1 ) buffer_offset = 1
        do l = 1,nsptmp
          if( lavmap(l) .NE. 0 ) read(spname(lavmap(l)),'(10a1)') 
     &                                             (ispec(n,l),n=1,10)
        enddo
        if( .NOT. l3davg(igrd) ) nlayer = 1 
        write(iunit) idat1,btim,idat2,etim
      endif
c
c-----Write gridded concentration field
c
      do l = 1,nsptmp
        do k = 1,nlayer
          write(iunit) nseg,(ispec(n,l),n=1,10),
     &                 ((cncfld(i,j,k,l),i=1+buffer_offset,nox-buffer_offset),
     &                                    j=1+buffer_offset,noy-buffer_offset)
        enddo
      enddo
c
      return
      end
