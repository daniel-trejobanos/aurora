      subroutine nstprep()
      use grid
      use filunit
      use tracer
c
c----CAMx v7.10 210105
c
c     NSTPREP sets up parameters for nested grids
c
c     Copyright 1996 - 2021
c     Ramboll 
c          
c     Modifications:
c        07/22/02  --gwilson--  Added code to fill in the source 
c                               area mappings for the nests
c        03/01/16  --gwilson--  Added partial source area map
c
c     Input arguments:
c        none
c
c     Output arguments:
c        none
c
c     Routines Called:
c        PARNTCHD
c
c     Called by:
c        STARTUP
c
      include "camx.prm"
c                                   
      integer idprt(MXCELLS*MXCELLS*MXGRID)
c
      integer i, ipart, igrp
      logical long
c
c-----Entry point
c
c-----Identify parent-children relationship
c
      call  parntchd(ngrid,ncol,nrow,inst1,inst2,jnst1,jnst2,meshold,
     &               mapgrd,i1,i2,j1,j2,nmesh,nchdrn,idchdrn)
c
c---- echo the new order of grids ---
c
      write(idiag,*)
      write(idiag,*) '|',('-',i=1,74),'|'
      write(idiag,*) '|',(' ',i=1,74),'|'
      write(idiag,'(2A)') ' | NOTE:  The following table shows the ',
     &        'internal order of the nests as      |'
      write(idiag,'(2A)') 
     &          ' |        defined by the model.                ',
     &        '                             |'
      write(idiag,'(2A)') 
     &          ' |        The grid ID may have been re-calculated ',
     &                                  'by the model.             |'
      write(idiag,'(2A)') 
     &            ' |        The internal model order is the order in ',
     &    'which the nests will     |'
      write(idiag,'(2A)')
     &            ' |        appear in the fine grid output files.',
     &        '                             |'
      write(idiag,*) '|',(' ',i=1,74),'|'
      write(idiag,*) '|',('-',i=1,74),'|'
      write(idiag,*)
      write(idiag,*) '     Nest ID'
      write(idiag,*) ' Internal  Original   x-range      ncol     ',
     &                  '  y-range      nrow   mesh factor'
      do n = 2,ngrid
          i = mapgrd(n)
          write(idiag,'(1X,I5,4X,I5,2(3X,I5,3X,I5,3X,I5),5X,I5)')
     &          n,i,inst1(i),inst2(i),ncol(i),jnst1(i),jnst2(i),
     &                                              nrow(i),meshold(i)
      enddo
      write(idiag,*)
c
c-----Calculation of dx and dy for children grids
c
      do ip=1,ngrid
        do ic=1,nchdrn(ip)
          ig = idchdrn(ic,ip)
          nm = nmesh(ig)
          deltay(ig) = deltay(ip)/nm
          do j=j1(ig),j2(ig)
            do l=1,nm
              deltax((j-j1(ig))*nm + l + 1,ig) = deltax(j,ip)/nm
            enddo
          enddo
          deltax(1,ig) = deltax(j1(ig)-1,ip)/nm
          deltax(nrow(ig),ig) = deltax(j2(ig)+1,ip)/nm
        enddo
      enddo
c
c-----Define idfine for the area where fine cells exist
c
c-----Default: no fine cells
c
      idx = 1
      do igrid = 1,ngrid
        do n=1,ncol(igrid)*nrow(igrid)
          idfin(idx) = 0
          idx = idx + 1
        enddo
      enddo
c
c-----Determine idfin where fine grids exist
c
      do ip=1,ngrid
        ip_map = mapgrd(ip)
        long = .FALSE.
        do j = nrow(ip_map),1,-1 
          is = 1 + (j-1)*ncol(ip_map)
          ie = j*ncol(ip_map)
          do i=is,ie
           idprt(iptr2d(ip_map)-1+i) = ip
          enddo
        enddo
        do ic=1,nchdrn(ip)
          igrd = idchdrn(ic,ip)
          do j = j1(igrd),j2(igrd)
            do i = i1(igrd),i2(igrd) 
              idx = i + (j-1)*ncol(ip_map)
              idfin(iptr2d(ip_map)-1+idx) = igrd 
              idprt(iptr2d(ip_map)-1+idx) = igrd 
              if( ip .GE. 10 .OR. igrd .GE. 10 ) long = .TRUE.
            enddo 
          enddo 
        enddo   
        if( nchdrn(ip) .GT. 0 ) then 
          write(idiag,*)
          write(idiag,'(2A,I3)') ' The following map shows the ',
     &            'location of nests in grid #',ip
          do j = nrow(ip_map),1,-1 
            is = 1 + (j-1)*ncol(ip_map)
            ie = j*ncol(ip_map)
            if( .NOT. long ) then
                write(idiag,'(300i1)') 
     &                        (idprt(iptr2d(ip_map)-1+i),i=is,ie)
            else
                write(idiag,'(300i2)') 
     &                        (idprt(iptr2d(ip_map)-1+i),i=is,ie)
            endif
          enddo
        endif
      enddo
      write(idiag,*)
c        
c======================== Source Apportion Begin =======================
c                                   
c
c========================= Source Apportion End ========================
c
      return
      end
