      subroutine nodes_send_pig_sample(numprocs,iproc_id)
      use pigsty
      use filunit
c
c----CAMx v6.50 180430
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    This routine passes the PiG sampling grid data back to the master
c    grid.
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by: 
c       MASTER_UPDATE
c    Subroutines called:
c       MPI_SEND
c       MPI_RECV
c
c     Copyright 1996 - 2018
c     Ramboll
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i,j,ierr
      integer :: status(MPI_STATUS_SIZE)
      real    :: Larray( MXCOLSMP*MXROWSMP*MXSAMPLE*MXSPEC )
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.NOT. lmpi .AND. iproc_id .GT. 0) return
c
c  --- process ID non-zero sends the messages, to process ID zero  ---
c
      if (iproc_id .GT. 0) then
         call MPI_SEND(smpcnc,nsmpcels,MPI_REAL,0,itag,MPI_COMM_WORLD,ierr)
c
c  --- the master process will recieve the message ---
c
      else
         do j=1, nsmpcels
           smpcnc(j) = 0.
         enddo
         do i=1,numprocs
            call MPI_RECV(Larray,nsmpcels,MPI_REAL,i,itag,
     &                                     MPI_COMM_WORLD,status,ierr)
            do j=1,nsmpcels
               smpcnc(j) = smpcnc(j) + Larray(j) 
            enddo
         enddo
      endif
      itag = itag+1
c
      end
