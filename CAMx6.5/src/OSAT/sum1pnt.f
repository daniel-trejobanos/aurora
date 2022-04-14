      subroutine sum1pnt(numcols,numrows,nspmod,nsptrac,igroup,idx,
     &      nptsrc,emsbas,emsoth,emslft,emstot,emspnt,emssum,izcel,
     &                                             idcompact,lemit)
      use grid
      use chmstry
      use pigsty
      use filunit
      use tracer
c
c     Copyright 1996 - 2018
c     Ramboll
c
c
c----CAMx v6.50 180430
c
c     SUM1PNT sums up the point emission of one species for a given group
c
c       07/19/02  --gwilson-- Added seperate source area map for each grid.
c       08/25/05  --cemery--  Revamped PiG pointer arrays for source group
c                             and region
c       11/16/06  --gwilson-- fixed bug in point source override for PiG sources
c       11/27/06  --gwilson-- fixed bug in calculating emissions table
c       10/28/09  --gwilson-- Changed dimension of variables to accomodate 
c                             the dynamic memory allocation
c       12/20/13  --gwilson-- Added compact point source file
c       03/01/16  --gwilson-- Added partial source area map
c
c     Input argument:
c        numcols           max number of columns in any grid
c        numrows           max number of columns in any grid
c        nspmod            number of model species
c        nsptrac           number of tracer species
c        igroup            group ID
c        idx               specie ID
c        emspnt            the species emission for the group
c        idcompact         index of the source in master list
c
c     Output arguments:
c        emssum            emission summed over grid
c        emsbas            base emission
c        emsoth            "otherwise" emission
c        emslft            leftover emission
c        emstot            total emission
c        lemit             flag to determine if tracer class is emitted
c
      include "camx.prm"
      include "flags.inc"
c
      character*200 fname
      integer   numcols
      integer   numrows
      integer   nspmod
      integer   nsptrac
      integer   igroup
      integer   idx
      integer   nptsrc
      real      emsbas(nspmod,nsptrac)
      real      emsoth(nspmod,nsptrac)
      real*8    emslft(numcols,numrows,nspmod)
      real*8    emstot(numcols,numrows,nspmod)
      real      emspnt(MXPTSRC)
      real      emssum(nspmod,nsptrac)
      integer   izcel(*)
      integer   idcompact(*)
      logical   lemit(*)
      logical   luse
      integer   ipart
      real      frac
c
      real, allocatable, dimension(:) :: xloctmp
      real, allocatable, dimension(:) :: yloctmp
c
c  --- allocate the local arrays ---
c
      allocate( xloctmp(nptsrc) )
      allocate( yloctmp(nptsrc) )
c
c   --- set the file name for elevated points emissions file ---
c
      if( igroup .EQ. 0 ) then
        write(fname,'(A,I3)') 'PTSOURCE -- UNIT ',iptem
      else
        fname = tptfil(igroup)
      endif
c
c  --- make sure this species is needed ---
c
      luse = .FALSE.
      do icls=1,ntrcls
        if( trspmap(idx,icls) .NE. 0.  .OR.
     &                         yhratmap(idx,icls) .NE. 0. .OR.
     &                         ylratmap(idx,icls) .NE. 0. ) then
          if( trspmap(idx,icls) .NE. 0. ) lemit(icls) = .TRUE.
          luse = .TRUE.
        endif
      enddo
      if( .NOT. luse ) goto 9999
c
c   --- sum up the emissions for each point ---
c
        if (llatlon) then
          do n = 1,nptsrc
            idxpt = idcompact(n)
            xloctmp(n) = xlocpt(idxpt) - xorg
            yloctmp(n) = ylocpt(idxpt) - yorg
          enddo
        else
          do n = 1,nptsrc
            idxpt = idcompact(n)
            xloctmp(n) = xlocpt(idxpt)/1000. - xorg
            yloctmp(n) = ylocpt(idxpt)/1000. - yorg
          enddo
        endif
c
      do 70 i=1,nptsrc
         idxpt = idcompact(i)
         icel = 1 + FLOOR( xloctmp(i)/delx )
         jcel = 1 + FLOOR( yloctmp(i)/dely )
         if(icel .LE. 0 .OR. icel .GT. ncol(1)) goto 70
         if(jcel .LE. 0 .OR. jcel .GT. nrow(1)) goto 70
         icrs = icel
         jcrs = jcel
c
c   --- find out if a nest contains this source  ---
c
         igrd = 1
         do ig = 2,ngrid
           xlocnst = xloctmp(i) - (inst1(ig)-1)*delx
           ylocnst = yloctmp(i) - (jnst1(ig)-1)*dely
           ii = 2 + FLOOR( xlocnst/delx * FLOAT( meshold(ig) ) )
           jj = 2 + FLOOR( ylocnst/dely * FLOAT( meshold(ig) ) )
           if( ii .GT. 1 .AND. jj .GT. 1 .AND. ii .LT. ncol(ig) .AND.
     &                                           jj .LT. nrow(ig) ) then
              igrd = ig
              icel = ii
              jcel = jj
            endif
         enddo
c
c  --- get the region for this cell from mapping array ----
c
        imap = igrmap(0,1,igrd,icel,jcel)
        frac = 1.0
c
c  --- change the region if the override is set ---
c
        if( izcel(idcompact(i)) .LT. 0 ) then
            if( .NOT. lptoverride ) then
               write(iout,'(//,a)') 'ERROR in SUM1PNT:'
               write(iout,'(/,1X,2A )') 'A source was read that has the point ',
     &                   'source override trigger turned on.'
               write(iout,'(1X,2A)') 'If you intend to use point source ',
     &                     'override please set the namelist variable'
               write(iout,'(1X,2A)') 'SA_PT_Override to TRUE in the CAMx_Control namelist.'
               write(iout,'(1X,2A)') 'If you do not want point source override ',
     &                         'you need to set the kcell variable '
               write(iout,'(1X,2A)') 'in your point source file to zero.'
               call camxerr()
            endif
            imap = ABS( izcel(idcompact(i)) )
            frac = 1.0
        endif
        if( imap .LE. 0 .OR. imap .GT. nregin ) then
           write(iout,'(//,a)') 'ERROR in SUM1PNT:'
           write(iout,'(/,1X,A,A,I4)') 'Invalid region found in',
     &             ' point source override when reading point source file.'
           write(iout,'(1X,A,I4)') 'Region code      : ',imap
           write(iout,'(1X,A,I4)') 'Number of regions: ',nregin
           write(iout,'(10X,A,/,A)') 'Point source filename: ',
     &                                                 fname(:istrln(fname))
           write(iout,'(1X,2A)') 'Check the values in the point ',
     &                                                     'source overide.'
           call camxerr()
        endif
c
c  --- calculate the index into the tracer species for this group/region ---
c
        if( ngroup .GT. 0 ) then
c
c   --- if group is base emissions, add to "leftover" group ----
c
           if( igroup .EQ. 0 ) then
              if( leftovr ) then
                 do icls=1,ntrcls
                    if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yhratmap(idx,icls) .NE. 0. .OR.
     &                                 ylratmap(idx,icls) .NE. 0. ) then
                        ipt = iemcls(icls)-1 + imap+ngroup*nregin
                        emsbas(idx,ipt) = emsbas(idx,ipt) + emspnt(i) * frac
                    endif
                 enddo
              endif
c
c  --- if doing PiG and source is a PIG source, set the PiG map and group
c      pointers ---
c
              if( lpigsa(idxpt) .AND. emspnt(i) .GT. 0. ) then
                 ipigmap(idxpt) = imap
                 ipiggrp(idxpt) = ngroup
              endif
c
              do icls=1,ntrcls
                 if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yhratmap(idx,icls) .NE. 0. .OR.
     &                                 ylratmap(idx,icls) .NE. 0. ) then
                    emstot(icrs,jcrs,idx) = 
     &                              emstot(icrs,jcrs,idx) +  emspnt(i) * frac
                 endif
              enddo
c
c   --- otherwise, add to this group/region and subtract from "leftover" ---
c
           else
c
c  --- if doing PiG and source is a PIG source, set the PiG map and group
c      pointers ---
c
              if( lpigsa(idxpt) .AND. emspnt(i) .GT. 0. ) then
                 ipigmap(idxpt) = imap
                 ipiggrp(idxpt) = igroup-1
              endif
c
              do icls=1,ntrcls
                 if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yhratmap(idx,icls) .NE. 0. .OR.
     &                                 ylratmap(idx,icls) .NE. 0. ) then
                    ipt = iemcls(icls)-1 + imap +(igroup-1)*nregin
                    emssum(idx,ipt) = emssum(idx,ipt) + emspnt(i) * frac
                    if( leftovr ) then
                      ipt = iemcls(icls)-1 + imap+ngroup*nregin
                      emsoth(idx,ipt) = emsoth(idx,ipt) + emspnt(i) * frac
                    endif
                 endif
              enddo
              do icls=1,ntrcls
                  if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yhratmap(idx,icls) .NE. 0. .OR.
     &                                 ylratmap(idx,icls) .NE. 0. ) then
                    emslft(icrs,jcrs,idx) = 
     &                         emslft(icrs,jcrs,idx) + emspnt(i) * frac
                 endif
              enddo
           endif
c
c   --- only using regular model emissions ---
c
        else
           do icls=1,ntrcls
              if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yhratmap(idx,icls) .NE. 0. .OR.
     &                                 ylratmap(idx,icls) .NE. 0. ) then
                 ipt = iemcls(icls) - 1 + imap
                 emssum(idx,ipt) = emssum(idx,ipt) + emspnt(i) * frac
              endif
           enddo
c
c  --- if doing PiG and source is a PIG source, set the PiG map and group
c      pointers ---
c
           if( lpigsa(idxpt) .AND. emspnt(i) .GT. 0. ) then
              ipigmap(idxpt) = imap
              ipiggrp(idxpt) = 0
           endif
        endif
  70  continue
c
 9999 continue
c
c  --- deallocate the local arrays ---
c
      deallocate( xloctmp )
      deallocate( yloctmp )
c
      return
      end
