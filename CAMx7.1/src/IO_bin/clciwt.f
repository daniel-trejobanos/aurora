c**** CLCIWT
c
      subroutine clciwt(idate,btim,jdate,etim,ncolx,nrowy,nlays,
     &                  height)
      use filunit
      use grid
      use chmstry
      use bndary
      use tracer
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine calculates the weighted reactivity factor for VOC
c   species for the initial conditions.  The mass is weighted by layer
c   thickness giving the weighted average for the cell.  The average
c   over all cells is then calculated.
c
c     Copyright 1996 - 2021
c     Ramboll     
c
c      Argument description:
c        idate   I   beginning date of simulation (YYJJJ)
c        btim    R   beginning time of simulation
c        jdate   I   ending date of simulation (YYJJJ)
c        etim    R   ending time of simulation
c        ncolx   I   number of columns in coarse grid
c        nrowy   I   number of rows in coarse grid
c        nlays   I   number of layers in coarse grid
c        height  R   vertical grid structure at current time
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     01/04/96   --gwilson--    Original development
c     11/06/01   --cemery--     Input dates are now Julian
c     11/20/03   --gwilson--    Fixed bug when using the species map
c                               to get the index of modeled species
c     11/16/06   --gwilson--    Removed the rewind of the output file
c     01/04/11   --cemery--     Revised for new met input format
c
c-----------------------------------------------------------------------
c    Include files:
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
      integer   jdate
      real      etim
      integer   ncolx
      integer   nrowy
      integer   nlays
      real height(ncolx,nrowy,nlays)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*4  iname(10)
      integer      idx, iseg, ispc, izcl, i, j, k, numic_tracers
      integer      ibegic, iendic, ibgdhp, idtnow, iunit, ioff
      real         btimic, etimic, btimhp, timnow
      logical      lfound
c
      real sumvoc(MXTRCLS)
      real sumkoh(MXTRCLS)
      real summir(MXTRCLS)
      real sumyld(MXTRCLS)
      real yldhvoc(MXTRCLS), yldlvoc(MXTRCLS)
      real consum(MXSPEC)
      real congrd(MXCELLS*MXCELLS*MXLAYER)
c
      real, allocatable, dimension(:,:,:) :: tmp3d
      real, allocatable, dimension(:,:,:) :: depth
c
c-----------------------------------------------------------------------
c    Common Blocks:
c-----------------------------------------------------------------------
c
       common /comclciwt/ congrd
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- call routine to get the file pointers to the proper place ---
c
      if( .NOT. lrstrt ) then
         iunit = iic
      else
         iunit = irstc
      endif
      ibgdhp = 0
      btimhp = 0.0
c
c  ---- allocate the temp array ---
c
      allocate( tmp3d(ncolx,nrowy,nlays) )
      allocate( depth(ncolx,nrowy,nlays) )
c
c  --- set the current date and time ----
c
      idtnow = idate
      timnow = btim/100.0
c
c   --- determine depth array from height ---
c
      do k = 1,nlays
        do j = 1,nrowy
          do i = 1,ncolx
            depth(i,j,k) = height(i,j,k)
            if( k .GT. 1 ) 
     &         depth(i,j,k) = height(i,j,k) - height(i,j,k-1)
          enddo
        enddo
      enddo
c
c   --- initialize the array to zero ---
c
      do i=1,nspec
        consum(i) = 0.
      enddo
c
c   --- read the initial conditions ----
c
  111 continue
      read(iunit,ERR=7001,END=7002) ibegic, btimic, iendic, etimic
      lfound = .TRUE.
      if( ibegic .EQ. idtnow .AND. btimic .GT. timnow) lfound = .FALSE.
      if( ibegic .GT. idtnow ) lfound = .FALSE.
      do 10 ispc=1,nicspc
          do izcl = 1,nlays
              read(iunit,ERR=7001,END=7002) iseg, (iname(i),i=1,10), 
     &                    ((tmp3d(i,j,izcl),i=1,ncolx),j=1,nrowy)
          enddo
          do k=1,nlays
            do j=1,nrowy
              do i=1,ncolx
                n3d = i + (j-1)*ncolx + (k-1)*ncolx*nrowy
                congrd(n3d) = tmp3d(i,j,k)
              enddo
            enddo
          enddo
c
c   --- if record does not span this hour, skip it ----
c
         if( .NOT. lfound ) goto 10
c
c   --- if the species is a not modelled or not a VOC species skip it ---
c
          idx = 0
          do i=1,nspec
            if( licmap(i) .EQ. ispc ) idx = i
          enddo
          if( idx .LE. 0 ) goto 10
          call sumicwt(ncolx,nrowy,nlays,deltax,
     &                                     depth,congrd,consum(idx))
c
c   --- next species ---
c
   10 continue
c
c  --- if hour not found, go back and read it ---
c
      if( .NOT. lfound ) goto 111
c
c  --- all concentrations are summed, calculate the weghted fraction ----
c
      do icls=1,ntrcls
         sumvoc(icls) = 0.
         sumkoh(icls) = 0.
         summir(icls) = 0.
         sumyld(icls) = 0.
         yldhvoc(icls) = 0.
         yldlvoc(icls) = 0.
      enddo
      do i=1,nspec
         if( (lvocsp(i) .OR. lvocsoa(i)) .AND. consum(i) .GT. 0. ) then
            do icls=1,ntrcls
               sumvoc(icls) = sumvoc(icls) + consum(i) * trspmap(i,icls)
               sumkoh(icls) = sumkoh(icls) + consum(i) * rkohrt(i) * 
     &                                                   trspmap(i,icls)
               summir(icls) = summir(icls) + consum(i) * rmirrt(i) * 
     &                                                   trspmap(i,icls)
               if( yhratmap(i,icls) .GT. 0. .OR.
     &             ylratmap(i,icls) .GT. 0. ) then
                    sumyld(icls) = sumyld(icls) + consum(i)
                    yldhvoc(icls) = yldhvoc(icls) +
     &                                      consum(i) * yhratmap(i,icls)
                    yldlvoc(icls) = yldlvoc(icls) +
     &                                      consum(i) * ylratmap(i,icls)
               endif
            enddo 
         endif
      enddo
      numic_tracers = 1
      if( lsa_ioric ) numic_tracers = ncls_ioric
      do icls=1,ntrcls 
         do ioff=1,numic_tracers
            if( sumvoc(icls) .GT. 0. ) then
                wtkoh(iptcls(icls)+ioff-1) = sumkoh(icls) / sumvoc(icls)
                wtmir(iptcls(icls)+ioff-1) = summir(icls) / sumvoc(icls)
            else
                wtkoh(iptcls(icls)+ioff-1) = 0.
                wtmir(iptcls(icls)+ioff-1) = 0.
            endif
            if( sumyld(icls) .GT. 0. ) then
                yhrates(iptcls(icls)+ioff-1) = yldhvoc(icls) / sumyld(icls)
                ylrates(iptcls(icls)+ioff-1) = yldlvoc(icls) / sumyld(icls)
            else       
                yhrates(iptcls(icls)+ioff-1) = 0.
                ylrates(iptcls(icls)+ioff-1) = 0.
            endif
        enddo
      enddo
c
c  --- rewind the files to be used by the regular model ----
c
      rewind(iunit)
      rewind(i3dmet(1))
c
c  --- return to the calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
c
 7001 continue
      write(iout,'(//,A)') 'ERROR in CLCIWT:'
      write(iout,'(/,1X,A)') 'Reading initial conditions.'
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in CLCIWT:'
      write(iout,'(/,1X,2A)') 'ERROR: Premature end-of-file reading ',
     &                                            'initial conditions.'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
c
c  ---- deallocate the temp array ---
c
      deallocate( tmp3d )
      deallocate( depth )
c
      return
      end
