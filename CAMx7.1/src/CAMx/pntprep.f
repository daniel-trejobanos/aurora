      subroutine pntprep(begtim,begdate,endtim,enddate)
      use filunit
      use grid
      use chmstry
      use bndary
      use ptemiss
      use tracer
c 
c----CAMx v7.10 210105
c 
c     PNTPREP reads the header of binary point source emissions file,
c     initializes time-invariant point source variables, and maps the point
c     source species list to the internal CAMx species list
c                           
c     Copyright 1996 - 2021
c     Ramboll
c           
c     Modifications: 
c        11/06/01  Input dates are now Julian
c        05/01/03  Fixed bug that lost sources in first modeled cell
c        04/15/09  Now checks number of point sources against the 
c                  parameter.
c 
c     Input arguments: 
c        begtim              model start time (HHMM) 
c        begdate             model start date (YYJJJ) 
c        endtim              model end time (HHMM) 
c        enddate             model end date (YYJJJ)
c             
c     Output arguments: 
c        none
c             
c     Routines Called: 
c        RDPTHDR
c        GEOPSP 
c             
c     Called by: 
c        STARTUP 
c 
      include 'camx.prm'
      include 'flags.inc'
c
      integer begdate,enddate
      integer ifle, icount
c
      integer indxpt(MXPTSRC)
c
c-----Entry point
c
c-----Read the header
c
      nptsrc = 0
      if( ltrace .AND. tectyp .EQ. SA ) then
         call get_nptsrc_sa()
      else
         call get_nptsrc()
      endif
      if( ltrace ) then 
         if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) call get_nptsrc_rt()
      endif
      if( lddm .OR. lhddm ) call get_nptsrc_rt()
      if( ltrace .AND. tectyp .EQ. SA ) then
         call ncf_rdpthdr_sa(begtim,begdate,endtim,enddate)
         call rdpthdr_sa(begtim,begdate,endtim,enddate)
      else
         call ncf_rdpthdr(begtim,begdate,endtim,enddate)
         call rdpthdr(begtim,begdate,endtim,enddate)
      endif
      if( nptsrc .GT. MXPTSRC ) then
        write(iout,'(//,A)') 'ERROR in PNTPREP:'
        write(iout,*) 'A parameter in the camx.prm is not ',
     &                                        'sufficiently large.'
        write(iout,*) 'Please change the value for parameter: MXPTSRC'
        write(iout,*) 'It should be set to a value of at least: ',nptsrc
        call flush(iout)
        call camxerr()
      endif
      if( ltrace .AND. tectyp .EQ. SA ) then
         call ncf_rdstacks_sa()
      else
         call ncf_rdstacks()
      endif
c
c-----Call routine to allocate arrays by point source 
c
      call alloc_grid_ptsrc(MAX(1,nptsrc))
c
c-----Convert location to grid coordinates, convert vexit from m/hr to m/s 
c     and make sure stack diameter is positive (PiG flag is negative diameter)
c  
c-----Coarse grid
c
      if (llatlon) then
        do n = 1,nptsrc
          xstk(n,1) = xloc(n) - xorg
          ystk(n,1) = yloc(n) - yorg
        enddo
      else
        do n = 1,nptsrc
          xstk(n,1) = xloc(n)/1000. - xorg
          ystk(n,1) = yloc(n)/1000. - yorg
        enddo
      endif
c
      do 30 n=1,nptsrc
        ii = 1 + FLOOR(xstk(n,1)/delx)
        jj = 1 + FLOOR(ystk(n,1)/dely)
        isrc(n,1) = ii
        jsrc(n,1) = jj
        indxpt(n) = 1
c
c-----Note sources outside domain
c
        if (ii.lt.1 .or. ii.gt.ncol(1) .or. jj.lt.1 .or.
     &      jj.gt.nrow(1)) then
          indxpt(n) = 0
          write(idiag,'(a,i10,2x,2i7,a)')'Source outside domain: ',
     &           n,ii,jj,'  --- not modeled'
        elseif (ii.le.1 .or. ii.ge.ncol(1) .or.
     &          jj.le.1 .or. jj.ge.nrow(1)) then
          indxpt(n) = 0
        endif
 30   continue
c
c-----Map source to coarse grid
c
      nsrc = 0
      do n=1,nptsrc
        if (indxpt(n).eq.1) then
          nsrc = nsrc + 1
          idsrc(nsrc,1) = n
          isrc(nsrc,1) = isrc(n,1) 
          jsrc(nsrc,1) = jsrc(n,1) 
        endif
      enddo
      nosrc(1) = nsrc
c
c-----Fine grids
c
      do igrd = 2,ngrid
        nsrc = 0
        do n = 1,nptsrc
          if (indxpt(n).eq.1) then
            xstk(n,igrd) = xstk(n,1) - (inst1(igrd) - 1)*delx
            ystk(n,igrd) = ystk(n,1) - (jnst1(igrd) - 1)*dely
            ii = 2 + FLOOR(xstk(n,igrd)/delx*FLOAT( meshold(igrd) ) )
            jj = 2 + FLOOR(ystk(n,igrd)/dely*FLOAT( meshold(igrd) ) )
c
            if (ii.gt.1 .and. ii.lt.ncol(igrd) .and.
     &          jj.gt.1 .and. jj.lt.nrow(igrd)) then
              nsrc = nsrc + 1
              idsrc(nsrc,igrd) = n
              isrc(nsrc,igrd) = ii
              jsrc(nsrc,igrd) = jj
            endif
          endif
          nosrc(igrd) = nsrc
        enddo
      enddo
c
      write(idiag,*)
      do igrd = 1,ngrid
        write(idiag,*) 'Point sources in grid #',igrd,' = ',nosrc(igrd)
c       write(idiag,'(i10,3i5)') (n,idsrc(n,igrd),isrc(n,igrd),jsrc(n,igrd),
c    &                          n=1,nosrc(igrd))
      enddo
      write(idiag,*)
c
      return
      end