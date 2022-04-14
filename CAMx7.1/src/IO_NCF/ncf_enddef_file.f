c**** NCF_ENDDEF_FILE
c
      subroutine ncf_enddef_file(action,iounit)
      use grid
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine ends the definition mode of a NetCDF file
c
c     Copyright 1996 - 2021
c     Ramboll
c      Argument description:
c       Inputs:
c           action  C  name of file to open
c           iounit  I  NetCDF file ID of file
c       Outputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     02/20/17   --gwilson--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      character*(*) action
      integer       iounit
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer istrln
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer ierr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ierr = nf_enddef(iounit)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in NCF_ENDDEF_FILE:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(A)') 'Cannot terminate definition mode.'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
