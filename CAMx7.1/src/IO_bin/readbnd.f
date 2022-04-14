      subroutine readbnd(bndtim,bnddate)
      use filunit
      use grid
      use chmstry
      use bndary
      use camxfld
      use camxcom
c 
c----CAMx v7.10 210105
c 
c     READBND reads and cycles through the BOUNDARY file to the
c     current time/date, and loads coarse grid boundary concentrations
c 
c     Copyright 1996 - 2021
c     Ramboll
c           
c     Modifications: 
c        none
c 
c     Input arguments: 
c        bndtim                 model simulation time (HHMM)
c        bnddate                model simulation date (YYJJJ)
c             
c     Output arguments: 
c        bndtim                 next boundary update time (HHMM)
c        bnddate                next boundary update date (YYJJJ)
c             
c     Routines Called: 
c        none
c             
c     Called by: 
c        CAMx
c
      include 'camx.prm'
c
      character*4 bcspec(10)
      integer bnddate, ncells
c
      real, allocatable, dimension(:,:,:,:) :: bctmp
c
c-----Entry point
c
      ncells = MAX(ncol(1),nrow(1))
      nz = nlay(1)
      allocate( bctmp(ncells,nz,4,MXSPEC) )
c
c-----Read through coarse grid concentration records until current time/date
c
 100  read(ibc,end=900) idat1,tim1,idat2,tim2
      if (INT(tim2) .EQ. 24) then
        tim2 = 0.
        idat2 = idat2 + 1
        if( MOD(idat2,1000) .GT. 365 ) then
           if( MOD(INT(idat2/1000),4) .EQ. 0 ) then
              if( MOD(idat2,1000) .EQ. 367 )
     &                     idat2 = (INT(idat2/1000)+1)*1000 + 1
           else
              idat2 = (INT(idat2/1000)+1)*1000 + 1
           endif
        endif
      endif
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      do l = 1,nbcspc
        do n = 1,4
          nc = nrow(1)
          if (n.gt.2) nc = ncol(1)
          read(ibc) idum,(bcspec(j),j=1,10),iedge,
     &              ((bctmp(i,k,n,l),k=1,nz),i=1,nc) 
        enddo
      enddo
      write(iout,'(a40,2(f7.0,i8.5))')
     &  'Read boundary condition file at ',tim1,idat1,tim2,idat2
      call flush(iout)
      if ((idat1.lt.date .or. (idat1.eq.date .and. tim1.le.time)) .and.
     &    (idat2.gt.date .or. (idat2.eq.date .and. tim2.gt.time))) then
c
c-----Load boundary concentrations; convert gasses from ppm to umol/m3,
c     PM stays at ug/m3
c
        do 60 l = 1,nspec
          lbc = lbcmap(l)
          do 50 k = 1,nz
c
            do 40 j = 2,nrow(1)-1
              i = 1
              n3d = i + ncol(1)*(j - 1) + ncol(1)*nrow(1)*(k - 1)
              n4d = n3d + ncol(1)*nrow(1)*nlay(1)*(l - 1)
              if (l.le.ngas) then
                convfac = densfac*273./tempk(n3d)*press(n3d)/1013.
              else
                convfac = 1.
              endif
              conc(n4d) = convfac*bdnl(l)
              if (lbc.gt.0) then
                 if( bctmp(j,k,1,lbc).gt.bdnl(l) ) 
     &                   conc(n4d) = convfac*bctmp(j,k,1,lbc)
              endif
c
              i = ncol(1)
              n3d = i + ncol(1)*(j - 1) + ncol(1)*nrow(1)*(k - 1)
              n4d = n3d + ncol(1)*nrow(1)*nlay(1)*(l - 1)
              if (l.le.ngas) then
                convfac = densfac*273./tempk(n3d)*press(n3d)/1013.
              else
                convfac = 1.
              endif
              conc(n4d) = convfac*bdnl(l)
              if (lbc.gt.0 )then
                if( bctmp(j,k,2,lbc).gt.bdnl(l) ) 
     &                  conc(n4d) = convfac*bctmp(j,k,2,lbc)
              endif
 40         continue
c
            do 45 i = 2,ncol(1)-1
              j = 1
              n3d = i + ncol(1)*(j - 1) + ncol(1)*nrow(1)*(k - 1) 
              n4d = n3d + ncol(1)*nrow(1)*nlay(1)*(l - 1)
              if (l.le.ngas) then
                convfac = densfac*273./tempk(n3d)*press(n3d)/1013.
              else
                convfac = 1.
              endif
              conc(n4d) = convfac*bdnl(l)
              if (lbc.gt.0) then
                 if( bctmp(i,k,3,lbc).gt.bdnl(l) ) 
     &              conc(n4d) = convfac*bctmp(i,k,3,lbc)
              endif
c
              j = nrow(1)
              n3d = i + ncol(1)*(j - 1) + ncol(1)*nrow(1)*(k - 1) 
              n4d = n3d + ncol(1)*nrow(1)*nlay(1)*(l - 1)
              if (l.le.ngas) then
                convfac = densfac*273./tempk(n3d)*press(n3d)/1013.
              else
                convfac = 1.
              endif
              conc(n4d) = convfac*bdnl(l)
              if (lbc.gt.0) then
                  if( bctmp(i,k,4,lbc).gt.bdnl(l) ) then
                       conc(n4d) = convfac*bctmp(i,k,4,lbc) 
                  endif
              endif
 45         continue 
 50       continue
 60     continue
      else
        goto 100
      endif
c
c-----Set next boundary update time
c
      bndtim = tim2
      bnddate = idat2
      if (bndtim.ge.2400.) then
        bndtim = bndtim - 2400.
        bnddate = bnddate + 1
        if( MOD(bnddate,1000) .GT. 365 ) then
           if( MOD(INT(bnddate/1000),4) .EQ. 0 ) then
              if( MOD(bnddate,1000) .EQ. 367 )
     &                     bnddate = (INT(bnddate/1000)+1)*1000 + 1
           else
              bnddate = (INT(bnddate/1000)+1)*1000 + 1
           endif
        endif
      endif
      goto 999
c
c-----End of BC file reached
c
 900  write(iout,'(//,a)') 'ERROR in READBND:'
      write(iout,*)'Premature End of BC file reached.'
      write(iout,*)'Make sure boundary file contains simulation ',
     &                                               'time period.'
      call camxerr()
c
 999  continue
c
c  --- dealocate the local array ---
c
      deallocate( bctmp )
      return
      end
