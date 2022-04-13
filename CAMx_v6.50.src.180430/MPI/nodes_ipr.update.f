      subroutine nodes_ipr_update(numprocs,iproc_id,numcells,numspecs)
      use procan
      use filunit
      implicit none
c
c
c----CAMx v6.50 180430
c
c     This routine will pass the cipr data for Process Analysis
c     from the compute nodes, where it is calculated to the master
c     where it will be written to the output file. The data can be
c     summed over all compute nodes because it will be zero in a PA
c     cell outside of the computational slice. This routine will be
c     called at each output time.
c     
c
c     Copyright 1996 - 2018
c     Ramboll
c
c
c     Modifications:
c
c     Input arguments:
c        numprocs            the number of processes
c        iproc_id            the process ID for this processor
c        numcells            then number of total Process Aanalysis cells
c        numspecs            then number of total Process Aanalysis "species"
c
c     Output arguments:
c        none
c
c     Routines called:
c     
c     Called by:
c
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c     
c-----Argmument declarations
c
      integer numprocs
      integer iproc_id
      integer numcells
      integer numspecs
c     
c-----Local variables
c
      integer iprc, icl, ispc
      integer nbytes, ierr, status(MPI_STATUS_SIZE), i
      integer itmp_npastep(numcells,numspecs)
      real    tmp_cipr(numcells,numspecs)
c     
c-----Entry point
c
      if( .NOT. lmpi .AND. iproc_id .GT. 0 ) return
c
c  --- calculate the  number of values ---
c
      nbytes = numcells * numspecs
c
c  --- pass the actual PA data,
c      process ID zero recieves data into temp array ---
c  --- do each process seperately ---
c
      do iprc=1,NPAPRC
         if( iproc_id .EQ. 0 ) then
            do icl=1,numcells
              do ispc=1,numspecs
                 npastep(icl,ispc) = 0
                 cipr(iprc,icl,ispc) = 0.
              enddo
            enddo
            do i=1,numprocs 
               call MPI_RECV( tmp_cipr, nbytes, MPI_REAL, i, itag,
     &                                      MPI_COMM_WORLD, status, ierr)

               do icl=1,numcells
                 do ispc=1,numspecs
                    cipr(iprc,icl,ispc) = cipr(iprc,icl,ispc) +
     &                                       tmp_cipr(icl,ispc)
                 enddo 
               enddo 
            enddo
c
c  --- if this is a compute process, send the global array ---
c
         else
            do icl=1,numcells
              do ispc=1,numspecs
                 tmp_cipr(icl,ispc) = cipr(iprc,icl,ispc)
              enddo 
            enddo 
            call MPI_SEND( tmp_cipr, nbytes, MPI_REAL, 0, itag,
     &                                            MPI_COMM_WORLD, ierr)
         endif
         itag = itag + 1
      enddo
c
c  --- calculate the  number of counters ---
c
      nbytes = numcells * numspecs
c
c  --- pass the timestep counts,
c      process ID zero recieves data into temp array ---
c
      if( iproc_id .EQ. 0 ) then
         do i=1,numprocs 
            call MPI_RECV( itmp_npastep, nbytes , MPI_INTEGER, i, itag,
     &                                      MPI_COMM_WORLD, status, ierr)
            do icl=1,numcells
              do ispc=1,numspecs
                   npastep(icl,ispc) = npastep(icl,ispc) +
     &                                            itmp_npastep(icl,ispc)
              enddo 
            enddo 
         enddo
c
c  --- if this is a compute process, send the global array ---
c
      else
         call MPI_SEND( npastep, nbytes, MPI_INTEGER, 0, itag,
     &                                            MPI_COMM_WORLD, ierr)
c
c  --- zero out the arrays ---
c
      endif
c
c  --- return to the calling routine ---
c
      itag = itag + 1
      end
