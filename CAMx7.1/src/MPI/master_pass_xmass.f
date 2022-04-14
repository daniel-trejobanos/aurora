      subroutine master_pass_xmass(array,nbytes,mpi_type,itag,
     &                                              numprocs,iproc_id)
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
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
c     Copyright 1996 - 2021
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
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real*8  :: array(nbytes)
c
      integer :: nbytes
      integer :: mpi_type
      integer :: itag
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real*8  :: Larray(nbytes)
c
      integer :: ierr
      integer :: i
      integer :: j
      integer :: status(MPI_STATUS_SIZE)
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
         do j=1, nbytes
            Larray(j) = array(j)
         enddo
         call MPI_SEND(Larray,nbytes,mpi_type,0,itag,MPI_COMM_WORLD,ierr)
c
c  --- the master process will recieve the message ---
c
      else
         do j=1, nbytes
           array(j) = 0.
         enddo
         do i=1,numprocs
            call MPI_RECV(Larray,nbytes,mpi_type,i,itag,
     &                    MPI_COMM_WORLD,status,ierr    )
            do j=1, nbytes
               array(j) = array(j) + Larray(j) 
            enddo
         enddo
      endif
      itag = itag+1
c
      end
