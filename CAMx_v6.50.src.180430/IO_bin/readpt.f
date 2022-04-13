      subroutine readpt()
      use filunit
      use grid
      use chmstry
      use camxcom
      use ptemiss
c
c----CAMx v6.50 180430
c
c     READPT reads the time-variant records of the point source file and
c     cycles through to current time/date to load point emission rates
c
c     Copyright 1996 - 2018
c     Ramboll
c          
c     Modifications:
c     05/01/03  Time span of emissions must now match emiss update interval
c     04/21/04  Now reads effective plume height which may overide plume
c               height calculation
c     04/14/04  Removed uncessary and errant check for plume rise distribution
c               flags
c     12/07/14  Revised for VBS emissions
c     01/08/16  Updated for Revised VBS emissions
c
c     Input arguments:
c        none
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
      include 'vbs.inc'
c
      character*4 ptspec(10)
c
      real emispts(MXPTSRC)
      real poa_op_pt(MXPTSRC),poa_bb_pt(MXPTSRC)
c
      data pi /3.1415927/
c
c-----Entry point
c
      kount = 1
 100  continue
      read(iptem,end=900) idat1,tim1,idat2,tim2
      ichktm1 = NINT( 1000*(tim1) )
      if( le1day ) then
         ichktm2 = NINT( 1000*(tim2) )
      else
         ichktm2 = NINT( 1000*(tim2)+24000*(idat2-idat1) )
      endif
      if( ichktm2 .EQ. 0 ) ichktm2 = 24000
      ichkems = NINT( 1000*(dtems/60.) )
      if( (ichktm2 - ichktm1) .NE. ichkems ) then
          write(iout,'(//,a)')'ERROR in READPT:'
          write(iout,*) 'Time interval in point emissions file does'
          write(iout,*)  ' not match emissions update time interval.'
          write(iout,*) '   Beginning Date/Time (Hour): ',idat1,tim1
          write(iout,*) '   Ending Date/Time    (Hour): ',idat2,tim2
          write(iout,*) '   Emiss Input interval(Hour): ',dtems/60.
          call camxerr()
      endif
      if(NINT(1000*tim2) .EQ. 0) then
        tim2 = 24.
        idat2 = idat2 - 1
      endif
      tim1 = 100.*aint(tim1) + 60.*amod(tim1,1.)
      tim2 = 100.*aint(tim2) + 60.*amod(tim2,1.)
      read(iptem) idum,npts 
      if (npts.ne.nptsrc) then 
        write(iout,'(//,a)') 'ERROR in READPT:'
        write(iout,*)'Number of points read from point source file',
     &               ' differs from header value'
        write(iout,*)'Record: ',npts,' Header: ',nptsrc 
        call camxerr()
      endif 
      read (iptem) (idum,idum,idum,flowrat(n),effph(n),n=1,npts)
c
c----Verify that plume distribution override is valid
c
      do n=1,npts
        if(flowrat(n) .LT. 0. .AND. effph(n) .LT. 0. ) then
           if( ABS(effph(n)) .LT. ABS(flowrat(n)) ) then
              write(iout,'(//,a)')'ERROR in READPT:'
              write(iout,*) 'Invalid values found for flow rate and plume height.'
              write(iout,*) 'The plume distribution override is triggered. But the '
              write(iout,*) 'value for flow rate (base) is larger than value for ',
     &                      'plume height (top).'
              call camxerr()
           endif
        endif
      enddo
c
      if (lvbs .and. LVBSPREPROC) then ! initialize temporary VBS POA emiss array
        poa_op_pt = 0.0
        poa_bb_pt = 0.0
      endif
      do 50 ll = 1,nptspc 
        read(iptem) idum,(ptspec(i),i=1,10),(emispts(n),n=1,npts) 
c
c----Load into the global array ---
c
        if (lvbs .and. LVBSPREPROC) then
          if( lptmap(kpap_c(0)) .EQ. ll ) then ! POA_OP in VBS emiss
             poa_op_pt(:npts) = emispts(:npts)
             goto 50
          endif
          if( lptmap(kpfp_c(0)) .EQ. ll ) then ! POA_BB in VBS emiss
             poa_bb_pt(:npts) = emispts(:npts)
             goto 50
          endif
        endif

        do l=1,nspec
          if( lptmap(l) .EQ. ll ) then
             do n=1,npts
                ptemis(n,l) = emispts(n)
             enddo
           endif
        enddo
 50   continue
      if (lvbs .and. LVBSPREPROC) then
        do l = 0, NVOLBIN
          ptemis(:,kpap_c(l)) = poa_op_pt(:npts) * poa_op_ef(l)
          ptemis(:,kpfp_c(l)) = poa_bb_pt(:npts) * poa_bb_ef(l)
        enddo
      endif
      write(iout,'(a40,2(f7.0,i8.5))')
     &       'Read point source file at ',tim1,idat1,tim2,idat2 
      call flush(iout)
c
c-----Check times only if LE1DAY = T, otherwise check both time and date
c
      if (le1day) then
        if (abs(tim1-time).lt.0.01 .and. tim2.gt.time) goto 200
        if (tim1-time.ge.0.01) goto 900
      else
        if ((idat1.lt.date .or.
     &      (idat1.eq.date .and. abs(tim1-time).lt.0.01)) .and.
     &      (idat2.gt.date .or.
     &      (idat2.eq.date .and. tim2.gt.time)))
     &    goto 200
      endif
      goto 100
c 
c-----Convert emission rates from moles/(dtems-hours) to moles/s for gas 
c     or g/(dtems-hours) to g/s for aero species 
c 
 200  do 10 l = 1,nspec
        if( lptmap(l) .EQ. 0 ) goto 10
        do n = 1,npts 
          ptemis(n,l) = ptemis(n,l)/(60.*dtems) 
        enddo 
 10   continue 
c 
c-----Convert flow rate to new exit velocity in m/s  
c 
      do n = 1,npts 
        if (flowrat(n).GT.0. .AND. dstk(n) .NE. 0. )
     &    vstk(n) = flowrat(n)/(3600.*pi*(abs(dstk(n))/2)**2) 
      enddo 
      goto 999
c
c-----End of file reached; if 1-day emissions requested, rewind and read 
c     through header once more.  Otherwise, report error and exit
c
 900  continue
      if (le1day) then
        if (kount.ge.2) then
          write(iout,'(//,a)')'ERROR in READPT:'
          write(iout,*)'Cannot match model time with point source time.'
          call camxerr()
        endif
        rewind(iptem)
        read(iptem) idum 
        read(iptem) dum  
        read(iptem) idum  
        read(iptem) idum 
        read(iptem) idum 
        read(iptem) dum
        kount = kount + 1
        goto 100
      else
        write(iout,'(//,a)')'ERROR in READPT:'
        write(iout,*)'End of point source file reached.  Make sure the '
        write(iout,*)
     &            'file is for the correct day and contains all hours.'
        call camxerr()
      endif
c
 999  continue
c
      return
      end
