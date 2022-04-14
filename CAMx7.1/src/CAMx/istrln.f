C**** ISTRLN
c
c----CAMx v7.10 210105
c
c     Copyright 1996 - 2021
c     Ramboll
c
      function istrln( string )
      integer   istrln
c
c-----------------------------------------------------------------------
c
c     This routine returns the non-blank length of a string.
c
c   Arguments:
c     Inputs:
c       string   C   string for determining length
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer   i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize length to zero ----
c
      istrln = 0
      do 10 i=LEN( string ),1,-1
         if( string(i:i) .NE. ' ' ) then
             istrln = i
             goto 9999
         endif
   10 continue
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
