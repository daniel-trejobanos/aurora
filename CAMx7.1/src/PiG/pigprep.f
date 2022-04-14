      subroutine pigprep(begdat,begtim,numprocs)
      use grid
      use pigsty
      use filunit
      use ptemiss
      use chmstry
      use camxfld
      use tracer
      use rtracchm
      use node_mod
c
c----CAMx v7.10 210105
c
c     PIGPREP prepares the PiG submodel
c
c     Copyright 1996 - 2021
c     Ramboll
c          
c     Modifications:
c        6/27/00 - Added some diagnostics to ensure that the point source PiG
c                  list is consistent on a restart with that used in previous 
c                  run.
c                - Now writing PiG point source information to PiG restart
c                  file, and echoing this information to diagnostic file on 
c                  model startup.
c       11/06/01   Input dates are now Julian
c        7/05/02   Added code to accommodate IRON-PiG
c       06/24/03   Removed thetapig
c       02/16/05   Added code to accomodate RTRAC mass
c       08/24/05   Removed diagnostics that ensure consistent PiG lists on
c                  restarts; removed read/write of PiG header containing 
c                  point source parameters; read of GREASD restart file now 
c                  includes OSAT pointer to region and group
c        2/02/06   Removed GREASD-PiG specific file formats; both PiG options
c                  use the IRON file formats
c       06/04/09   Added sigmax
c
c     Input arguments:
c        begdat              model beginning date (YYJJJ)
c        begtim              model beginning time
c        numprocs            number of processors for MPI
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
      include "camx.prm"
      include "flags.inc"
      include "netcdf.inc"
c
      character*200 action
      character*10 this_var
      integer begdat, this_varid, ierr
      real begtim
      integer numprocs, igroup
      integer idatpig,nsrc,irec,n,i,nr,is,m,ifile
      real timpig
c
      real xx(MXPTSRC)
      real yy(MXPTSRC)
      real hh(MXPTSRC)
      real dd(MXPTSRC)
      real tt(MXPTSRC)
      real vv(MXPTSRC)
      integer ipigflag(MXPTSRC)
c
      common /compigprep/ xx,yy,hh,dd,tt,vv,ipigflag

      integer istrln
c
c-----Entry point
c
      action = 'Reading NetCDF point source file.'
c
      nkill(1) = 0
      nkill(2) = 0
      nkill(3) = 0
      nkill(4) = 0
      nkill(5) = 0
      nkill(6) = 0
      nkill(7) = 0
      nkill(8) = 0
      nkill(9) = 0
      npig = 0
      nreactr = 1
      loverlap = .false.
      if( ipigflg .EQ. IRONPIG ) then
        nreactr = MXRECTR
        loverlap = OVERLAP
      endif
c
c  ---if this is a restart, then call routine to 
c     allocate arrays for puff concs ----
c

      if( lrstrt ) then
        call alloc_pigsty(nspec,nreactr,ngrid,numprocs,ipigflg)
      endif
      do n = 1,ngrid
        nage(n) = 0
        pigage(n) = 0.
        do is = 1,nspec
          pgmserr(is,n) = 0.
        enddo
      enddo
c
c-----Check FLEAK param
c
      if ((OVERLAP .or. LEAKON) .and. FLEAK .GT. 1.) then
        write(iout,'(//,a)') 'ERROR in PIGPREP:'
        write(iout,*) 'FLEAK must be <= 1.0'
        write(iout,*) 'FLEAK = ',FLEAK
        call camxerr()
      endif
c 
c-----Read PiG information if it is a restart run and if PiG file is
c     provided 
c
      if (lrstrt .AND. irstp.ne.0) then
c
c-----Read hourly data to current date/time
c
        irec = 0
 100    continue
        irec = irec + 1
        read(irstp,ERR=7000,END=7001) idatpig,timpig,npig,nreactr
c
        irec = irec + 1
        if (ltrace .AND. (tectyp .EQ. RTRAC .OR.
     &                                  tectyp .EQ. RTCMC) ) then
          read(irstp,ERR=7002,END=7001)
     &           (ingrd(n),idpig(n),xpigf(n),xpigb(n),ypigf(n),
     &           ypigb(n),zpig(n),axisy(n),axisz(n),sigy(n),sigx(n),
     &           sigz(n),pufftop(n),puffbot(n),htfms(n),htfmb(n),
     &           vtfms(n),vtfmb(n),agepigf(n),agepigb(n),fmspig(n),
     &           ((puffmass(i,nr,n),i=1,nspec),nr=1,nreactr),n=1,npig),
     &           (((puffrt(i,nr,n),i=1,nrtrac),nr=1,nreactr),n=1,npig)
        else
          read(irstp,ERR=7002,END=7001)
     &           (ingrd(n),idpig(n),xpigf(n),xpigb(n),ypigf(n),
     &           ypigb(n),zpig(n),axisy(n),axisz(n),sigy(n),sigx(n),
     &           sigz(n),pufftop(n),puffbot(n),htfms(n),htfmb(n),
     &           vtfms(n),vtfmb(n),agepigf(n),agepigb(n),fmspig(n),
     &           ipufmap(n),ipufgrp(n),
     &           ((puffmass(i,nr,n),i=1,nspec),nr=1,nreactr),n=1,npig)
        endif
        if (timpig.ge.2400.) then
          timpig = timpig - 2400.
          idatpig = idatpig + 1
          if (MOD(idatpig,1000) .GT. 365) then
            if (MOD(INT(idatpig/1000),4) .EQ. 0) then
              if (MOD(idatpig,1000) .EQ. 367)
     &          idatpig = (INT(idatpig/1000)+1)*1000 + 1
            else
              idatpig = (INT(idatpig/1000)+1)*1000 + 1
            endif
          endif
        endif
        write(iout,'(a,F10.1,i10.5,i10)') 'Read PiG file at  ',
     &                                timpig,idatpig,npig
        if (idatpig.lt.begdat .or. 
     &     (idatpig.eq.begdat .and. timpig.lt.begtim - 0.01)) goto 100
        if (idatpig.gt.begdat .or. 
     &     (idatpig.eq.begdat .and. timpig.gt.begtim + 0.01)) then
           write(iout,'(//,a)') 'ERROR in PIGPREP:'
           write(iout,*) 'Date or time in PiG file > beginning time'
           write(iout,'(2i10.5)') idatpig, timpig
           call camxerr()
        endif
      endif
c
c-----Echo PiG point source information to diagnostic file
c
      write(idiag,'(//,a)') 'PiG source information'
      write(idiag,'(80a)') ('-',m=1,80)
      write(idiag,'(a,a)') ' Pig Src   Pt Src     Xloc      Yloc   ',
     &                     ' Height   Diameter  Temperature  Velocity'
      write(idiag,'(a,a)') '    #         #        (km or deg)  ',
     &                '     (m)       (m)         (K)       (m/hr)'
      write(idiag,'(80a)') ('-',m=1,80)
      nsrc = 0
      do ifile=1,npoint_files
         if( is_netcdf_iptem(ifile) ) then
c
            this_var = "xcoord"
            ierr = nf_inq_varid(iptem(ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iptem(ifile),this_varid,xx)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "ycoord"
            ierr = nf_inq_varid(iptem(ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iptem(ifile),this_varid,yy)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "stkheight"
            ierr = nf_inq_varid(iptem(ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iptem(ifile),this_varid,hh)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "stkdiam"
            ierr = nf_inq_varid(iptem(ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iptem(ifile),this_varid,dd)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "stktemp"
            ierr = nf_inq_varid(iptem(ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iptem(ifile),this_varid,tt)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "stkspeed"
            ierr = nf_inq_varid(iptem(ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iptem(ifile),this_varid,vv)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "pigflag"
            ierr = nf_inq_varid(iptem(ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_int(iptem(ifile),this_varid,ipigflag)
            if( ierr .NE. NF_NOERR ) goto 7004
         else
            rewind(iptem(ifile))
            do n = 1,5
              read(iptem(ifile))
            enddo
            read(iptem(ifile)) (xx(n),yy(n),hh(n),dd(n),tt(n),
     &                              vv(n),n=1,nptsrc_files(ifile))
            do n = 1,nptsrc_files(ifile)
                ipigflag(n) = 0
                if(dd(n).lt.0.) ipigflag(n) = 1
            enddo
         endif
         do n = 1,nptsrc_files(ifile)
           if (ipigflag(n) .GT. 0 ) then
                nsrc = nsrc + 1
                write(idiag,'(i5,2x,i10,2f10.3,f9.1,f9.2,f12.1,f13.0)')
     &            nsrc,n,xx(n)/1000.,yy(n)/1000.,hh(n),ABS(dd(n)),tt(n),vv(n)
           endif
         enddo
      enddo
c
c   ---- what if this is SA run ---
c 
      nsrc = 0
      do igroup=1,ngroup
      do ifile=1,num_iortpt(igroup)
         if( is_netcdf_iortpt(igroup,ifile) ) then
c
            this_var = "xcoord"
            ierr = nf_inq_varid(iortpt(igroup,ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iortpt(igroup,ifile),this_varid,xx)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "ycoord"
            ierr = nf_inq_varid(iortpt(igroup,ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iortpt(igroup,ifile),this_varid,yy)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "stkheight"
            ierr = nf_inq_varid(iortpt(igroup,ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iortpt(igroup,ifile),this_varid,hh)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "stkdiam"
            ierr = nf_inq_varid(iortpt(igroup,ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iortpt(igroup,ifile),this_varid,dd)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "stktemp"
            ierr = nf_inq_varid(iortpt(igroup,ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iortpt(igroup,ifile),this_varid,tt)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "stkspeed"
            ierr = nf_inq_varid(iortpt(igroup,ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_real(iortpt(igroup,ifile),this_varid,vv)
            if( ierr .NE. NF_NOERR ) goto 7004
c
            this_var = "pigflag"
            ierr = nf_inq_varid(iortpt(igroup,ifile),this_var,this_varid)
            if( ierr .NE. NF_NOERR ) goto 7003
            ierr = nf_get_var_int(iortpt(igroup,ifile),this_varid,ipigflag)
            if( ierr .NE. NF_NOERR ) goto 7004
         else
            rewind(iortpt(igroup,ifile))
            do n = 1,5
              read(iortpt(igroup,ifile))
            enddo
            read(iortpt(igroup,ifile)) (xx(n),yy(n),hh(n),dd(n),tt(n),
     &                              vv(n),n=1,nptsrc_safile(igroup,ifile))
            do n = 1,nptsrc_safile(igroup,ifile)
                ipigflag(n) = 0
                if(dd(n).lt.0.) ipigflag(n) = 1
            enddo
         endif
         do n = 1,nptsrc_safile(igroup,ifile)
           if (ipigflag(n) .GT. 0 ) then
                nsrc = nsrc + 1
                write(idiag,'(i5,2x,i10,2f10.3,f9.1,f9.2,f12.1,f13.0)')
     &            nsrc,n+idx_start_sapts(igroup,ifile),xx(n)/1000.,
     &                      yy(n)/1000.,hh(n),ABS(dd(n)),tt(n),vv(n)
           endif
         enddo
      enddo
      enddo
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in PIGPREP:'
      write(iout,*) 'Cannot read PiG restart file at record: ',irec
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in PIGPREP:'
      write(iout,*) 'Premature end-of-file found in PiG ',
     &              'restart file at record: ',irec
      call camxerr()
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in PIGPREP:'
      write(iout,'(a,i10.5,f10.1)') 
     &      'Cannot read data in PiG restart file at hour: ',
     &      idatpig,timpig
      call camxerr()
c
 7003 continue
      write(iout,'(//,a)') 'ERROR in PIGPREP:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(2A)') 'Cannot find variable id for: ',
     &                                      this_var(:istrln(this_var))
      call camxerr()
c
 7004 continue
      write(iout,'(//,a)') 'ERROR in PIGPREP:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(2A)') 'Cannot read data for variable: ',
     &                                      this_var(:istrln(this_var))
      call camxerr()

c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
c
      return
      end
