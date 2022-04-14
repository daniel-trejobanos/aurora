      subroutine readar(igrd,ncol,nrow,iarem,iout,aremis,nspcar,nspcs)
      use chmstry
      use camxcom
c
c----CAMx v6.50 180430
c
c     READAR reads the time-variant records of the area source file for
c     the given grid and cycles through to current time/date to load 
c     area emission rates
c
c     Copyright 1996 - 2018
c     Ramboll
c          
c     Modifications:
c        10/20/00  Added check for negative emission rates
c        05/01/03  Time span of emissions must now match emiss update interval
c        12/07/14  Revised for VBS emissions
c        01/08/16  Updated for Revised VBS emissions
c
c     Input arguments:
c        igrd                grid index
c        ncol                number of columns
c        nrow                number of rows
c        iarem               file unit for area emissions file
c        iout                file unit number for output file
c        nspcar              number of input gridded emission species
c        nspcs               number of model species
c
c     Output arguments:
c        aremis              area emissions rate (mole/s)
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
      character*4 arspec(10)
      real aremis(ncol,nrow,nspcs)
      real poa_gv_em(ncol,nrow),poa_dv_em(ncol,nrow),
     &     poa_mc_em(ncol,nrow),poa_op_em(ncol,nrow),
     &     poa_bb_em(ncol,nrow)
c
      real argrid(MXCELLS,MXCELLS)
c
c-----Entry point
c
      if (iarem.eq.0) goto 999
      kount = 1
 100  read(iarem,end=900) idat1,tim1,idat2,tim2 
      ichktm1 = NINT( 1000*(tim1) )
      if( le1day ) then
         ichktm2 = NINT( 1000*(tim2) )
      else
         ichktm2 = NINT( 1000*(tim2)+24000*(idat2-idat1) )
      endif
      if( ichktm2 .EQ. 0 ) ichktm2 = 24000
      ichkems = NINT( 1000*(dtems/60.) )
      if( (ichktm2 - ichktm1) .NE. ichkems ) then
          write(iout,'(//,a)')'ERROR in READAR:'
          write(iout,*) 'Time interval in surface emissions file does'
          write(iout,*)  ' not match emissions update time interval.'
          write(iout,*) '   Beginning Date/Time (HHMM): ',idat1,tim1
          write(iout,*) '   Ending Date/Time    (HHMM): ',idat2,tim2
          write(iout,*) '   Emiss Input interval (min): ',dtems
          call camxerr()
      endif
      if( NINT(1000*tim2) .EQ. 0) then
        tim2 = 24.
        idat2 = idat2 - 1
      endif
      tim1 = 100.*aint(tim1) + 60.*amod(tim1,1.)
      tim2 = 100.*aint(tim2) + 60.*amod(tim2,1.)
      if (lvbs .and. LVBSPREPROC) then ! initialize temporary VBS POA emiss array
        poa_gv_em = 0.0
        poa_dv_em = 0.0
        poa_mc_em = 0.0
        poa_op_em = 0.0
        poa_bb_em = 0.0
      endif
      do 50 ll = 1,nspcar
        read(iarem) idum,(arspec(i),i=1,10),
     &                          ((argrid(i,j),i=1,ncol),j=1,nrow)
c
c-----Put the data into the global array ----
c
        if (lvbs .and. LVBSPREPROC) then
           if( larmap(kpap_c(0),igrd) .EQ. ll ) then ! POA_OP in VBS emiss
              poa_op_em(:,:) = argrid(:ncol,:nrow)
              goto 50
           endif
           if( larmap(kpap_c(1),igrd) .EQ. ll ) then ! POA_GV in VBS emiss
              poa_gv_em(:,:) = argrid(:ncol,:nrow)
              goto 50
           endif
           if( larmap(kpap_c(2),igrd) .EQ. ll ) then ! POA_DV in VBS emiss
              poa_dv_em(:,:) = argrid(:ncol,:nrow)
              goto 50
           endif
           if( larmap(kpcp_c(0),igrd) .EQ. ll ) then ! POA_MC in VBS emiss
              poa_mc_em(:,:) = argrid(:ncol,:nrow)
              goto 50
           endif
           if( larmap(kpfp_c(0),igrd) .EQ. ll ) then ! POA_BB in VBS emiss
              poa_bb_em(:,:) = argrid(:ncol,:nrow)
              goto 50
           endif
        endif

        do l=1,nspcs
           if( larmap(l,igrd) .EQ. ll ) then
               do i=1,ncol
                  do j=1,nrow
                    aremis(i,j,l) = argrid(i,j)
                  enddo
               enddo
           endif
        enddo
c
 50   continue
      if (lvbs .and. LVBSPREPROC) then
        do l = 0, NVOLBIN
          aremis(:,:,kpap_c(l)) = poa_op_em(:,:) * poa_op_ef(l)
     &                          + poa_gv_em(:,:) * poa_gv_ef(l)
     &                          + poa_dv_em(:,:) * poa_dv_ef(l)
          aremis(:,:,kpcp_c(l)) = poa_mc_em(:,:) * poa_mc_ef(l)
          aremis(:,:,kpfp_c(l)) = poa_bb_em(:,:) * poa_bb_ef(l)
        enddo
      endif
      write(iout,'(a40,2(f7.0,i8.5),a,i3)')
     &      'Read area source file at ',tim1,idat1,tim2,idat2,
     &      ' grid',igrd
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
 200  continue
      do 10 l = 1,nspcs
        if( larmap(l,igrd) .LE. 0 ) goto 10
        do j = 1,nrow
          do i = 1,ncol
            if (aremis(i,j,l).lt.0.) then
              write(iout,'(//,a)') 'ERROR in READAR:'
              write(iout,'(a,i3)') 'Negative emissions for grid:',igrd
              write(iout,'(a,3i3)') '(i,j,l): ',i,j,l
              call camxerr()
            endif
            aremis(i,j,l) = aremis(i,j,l)/(60.*dtems) 
          enddo 
        enddo
 10   continue 
      goto 999
c
c-----End of file reached; if 1-day emissions requested, rewind and read 
c     through header once more.  Otherwise, report error and exit
c
 900  continue
      if (le1day) then
        if (kount.ge.2) then
          write(iout,'(//,a)') 'ERROR in READAR:'
          write(iout,*)'Cannot match model time with area source time'
          call camxerr()
        endif 
        rewind(iarem)
        read(iarem) idum 
        read(iarem) dum  
        read(iarem) idum  
        read(iarem) idum 
        kount = kount + 1
        goto 100
      else
        write(iout,'(//,a)') 'ERROR in READAR:'
        write(iout,*)'End of area source file reached'
        call camxerr()
      endif
c
 999  continue
c
      return
      end
