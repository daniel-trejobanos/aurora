      subroutine banner( )
c
c----CAMx v7.10 210105
c
c     BANNER writes to the screen the license blurb for CAMx.
c
c     Copyright 1996 - 2021
c     Ramboll
c
c     Modifications:
c        none
c
c     Input arguments:
c        none
c
c     Output arguments:
c        iounit
c
c     Routines called:
c        none
c
c     Called by:
c         MAIN
c
c-----Entry point
c
      write(*,'(/,1X,2A)') 'By running CAMx you accept the terms ',
     &                                           'of the CAMx LICENSE.'
      write(*,'(1X,2A)') 'The CAMx LICENSE is included with the ',
     &                     'CAMx source code and also available here:'
      write(*,'(2X,2A,//)') 'http://www.camx.com/getmedia/',
     &          '397003b1-5168-4920-915d-d448b4777eb6/LICENSE_v7-10.txt' 
      call system("sleep 5")
 
      return
      end
