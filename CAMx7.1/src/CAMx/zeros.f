      subroutine zeros(a,n)
c
c----CAMx v7.10 210105
c
c     Copyright 1996 - 2021
c     Ramboll
c
c     ZEROS sets input array to zero
c
      dimension a(n)
c
      do i=1,n
        a(i) = 0.
      enddo
c
      return
      end
