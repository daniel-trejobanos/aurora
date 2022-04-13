      subroutine emassign(ncol,nrow,io,jo,nmesh,ncolf,nrowf,nspcems,
     &                                                  cgems,fgems)
c
c----CAMx v6.50 180430
c
c     EMASSIGN assigns emissions to fine grid values from coarse grid
c
c     Copyright 1996 - 2018
c     Ramboll
c          
c     Modifications:
c        none
c
c     Input arguments:
c        ncol              number of columns in the parent grid
c        nrow              number of rows in the parent grid
c        io                starting i index for the fine grid
c        jo                starting j index for the fine grid
c        nmesh             mesh number
c        ncolf             number of columns in fine grid
c        nrowf             number of rows in fine grid
c        nspcems           number of emissions species
c        cgems             emissions value on coarse grid
c
c     Output arguments:
c        fgems             emissions value on fine grid
c
c     Subroutine called:
c        none
c
c     Called by:
c
      real cgems(ncol,nrow,nspcems)
      real fgems(ncolf,nrowf,nspcems)
c
c-----Entry point
c
        do k=1,nspcems
           do 40 jfin = 2,nrowf-1
             j = (jfin - 2)/nmesh + jo
             do 30 ifin = 2,ncolf-1
               i = (ifin - 2)/nmesh + io
               fgems(ifin,jfin,k) = cgems(i,j,k)/(nmesh*nmesh)
  30         continue
  40       continue
        enddo
c
      return
      end
