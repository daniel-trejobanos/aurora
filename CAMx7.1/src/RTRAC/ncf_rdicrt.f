c**** NCF_RDICRT.F
c
      subroutine ncf_rdicrt(nox,noy,noz,nspsa,saconc)
      use tracer
      use grid
      use chmstry
      use rtracchm
      use filunit
      implicit none
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine fils one hour of initial conditions for the RTRAC
c   species. It then places these concentrations in the  appropriate 
c   place in the gridded array used for tracer concentrations.  
c   The values are left in PPM so that they can be interpolated to
c   the nests in a later step. This version is for NetCDF files.
c
c     Copyright 1996 - 2021
c     Ramboll
c
c      Argument description:
c       Outputs:
c           saconc   R  tracer concentrations
c       Inputs:
c           noy      I  number of Y cells in the grid
c           noz      I  number of layers in the grid
c           nspsa    I  number of tracer species
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'camx.inc'
      include 'netcdf.inc'         
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer   nox
      integer   noy
      integer   noz
      integer   nspsa
      real      saconc(nox,noy,noz,nspsa)
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
      integer ncf_chkfile
      integer ncf_get_tstep
      integer istrln
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 action
      character*10  aqfil, cfile, this_var, name_in
      integer       ierr, jerr, this_tstep, i, j, k, ispc
      integer       data_start(4), data_count(4), this_varid
      integer       iicdate_time_tflag, iicdate_time_etflag
      integer       irt, ifirst, itmp
      logical       lexist, luse
c
      real, allocatable, dimension(:,:,:) :: cinit
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- open the IC ---
c
      inquire(file=icfil,exist=lexist)
      if( .NOT. lexist ) goto 7000
      action = 'Opening RTRAC Initial Conditions file.'
      aqfil = 'AIRQUALITY'
      ierr = ncf_chkfile(ioric,icfil,action,aqfil)
      if( ierr .NE. ISUCES ) then
         jerr = nf_open(icfil,NF_NOWRITE,ioric)
         if( jerr .NE. NF_NOERR ) goto 7001
         aqfil = 'INITIAL   '
      else
         return
      endif
c
c  --- initailize to lower bound ---
c
      do ispc=1,nspsa
         do k=1,noz
            do j=1,noy
               do i=1,nox
                    saconc(i,j,k,ispc) = rtlbnd(ispc)
               enddo
            enddo
         enddo
      enddo
c
c --- get the type of file to make sure it is a IC file ---
c
      this_var = 'CAMx_NAME'
      ierr = nf_get_att_text(ioric, NF_GLOBAL, this_var, name_in)
      if( ierr .NE. NF_NOERR ) then
         this_var = 'NAME'
         ierr = nf_get_att_text(ioric, NF_GLOBAL, this_var, name_in)
         if( ierr .NE. NF_NOERR ) goto 7000
      endif
      if( name_in(:10) .NE. aqfil(:10) ) goto 7002
c
c --- call routine to make sure grid defintion is consistent ---
c
      call ncf_chk_griddef(ioric,action,1,.TRUE.,.TRUE.,.FALSE.,.FALSE.,itmp)
c
c --- call routine to make sure file spans the episode ---
c
      call ncf_chk_tstep(ioric,action,begdate,begtim,begdate,begtim,.FALSE.)
c
c  --- call routine to zero out the array ---
c
      allocate( cinit(ncol(1),nrow(1),nlay(1)) )
c
c  --- set the parameters for how to read the this data ---
c
      data_start(1) = 1
      data_count(1) = ncol(1)
      data_start(2) = 1
      data_count(2) = nrow(1)
      data_start(3) = 1
      data_count(3) = nlay(1)
c
c  --- get the timestep for the first hour ---
c
      this_tstep = ncf_get_tstep(ioric,action,begdate,begtim,
     &          iicdate_time_tflag,iicdate_time_etflag,.FALSE.,.TRUE.)
      data_start(4) = this_tstep
      data_count(4) = 1
c
c  --- read the concentrations for each species ---
c
      do ispc=1,ntotsp
c
c  ---- load this species into local array ---
c
         this_var = ptname(ispc)
         ierr = nf_inq_varid(ioric,this_var,this_varid)
         if( ierr .NE. NF_NOERR ) cycle
         ierr = nf_get_vara_real(ioric,this_varid,data_start,data_count,cinit)
         if( ierr .NE. NF_NOERR) then
           write(iout,'(//,a)') 'ERROR in NCF_RDICRT:'
           write(iout,*)'Cannot read initial conditions data for species: ',
     &                                   ptname(ispc)(:istrln(ptname(ispc)))
           call camxerr()
         endif
c
c  --- put into global array ---
c
         do k=1,nlay(1)
            do j=2,nrow(1)-1
               do i=2,ncol(1)-1
                 if( cinit(i,j,k) .GE. rtlbnd(ispc) )
     &                       saconc(i,j,k,ispc) = cinit(i,j,k)
               enddo
            enddo
         enddo
c
c  --- check the next RTRAC species ---
c
      enddo
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue 
      write(iout,'(//,A)') 'ERROR in NCF_RDICRT:'
      write(iout,*) 'ERROR:  IC file for RTRAC does not exist: ',
     &                                             icfil(:istrln(icfil))
      call camxerr()
c
 7001 continue
      write(iout,'(//,A)') 'ERROR in NCF_RDICRT:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(2A)') 'Could not open file: ',
     &                                   icfil(:istrln(icfil))
      write(iout,'(3A)') 'If this is a NCF file, check that its',
     &                   ' format is consistent with the NCF library',
     &                   ' used to build CAMx.'
      write(iout,'(2A)') 'The CAMx makefile supports netCDF3-Classic,',
     &                   ' and netCDF4 compressed or uncompressed.'
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in NCF_RDICRT:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(2A)',ERR=9999) 'Filetype is not correct.'
      write(iout,'(2A)',ERR=9999) 'Expecting    : ',aqfil(:istrln(aqfil))
      write(iout,'(2A)',ERR=9999) 'Value in file: ',name_in(:istrln(name_in))
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
      return
      end
