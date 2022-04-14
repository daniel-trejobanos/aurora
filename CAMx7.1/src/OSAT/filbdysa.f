c**** FILBDYSA.F
c
      subroutine filbdysa(igrid,nox,noy,noz,nspec,nspsa,conc,saconc)
      use bndary
      use tracer
      use node_mod
c
      implicit none
c
c----CAMx v7.10 210105
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine fills one hour of boundary conditions and calculates 
c   the NOx and VOC levels.  It then places these concentrations in the 
c   appropriate place in the gridded array used for tracer concentrations.  
c   The O3 concentrations are placed into the concentration arrays
c   for the Ozone tracer species.
c
c     Copyright 1996 - 2021
c     Ramboll
c
c      Argument description:
c           saconc   R  array of tracer concentrations
c       Inputs:
c           igrid    I  grid number of this grid
c           nox      I  number of cells in X direction
c           noy      I  number of cells in Y direction
c           noz      I  number of layers 
c           conc     R  array of regular model concentrations
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     06/06/96   --gwilson--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer   igrid
      integer   nox
      integer   noy
      integer   noz
      integer   nspec
      integer   nspsa
      real      conc(nox,noy,noz,nspec)
      real      saconc(nox,noy,noz,nspsa)
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      logical, external :: isbound
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer ioff, icl, jcl, izcl, ispc, icls
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  ---- read concentrations for each species ----
c
      do 10 ispc=1,nspec
c
c   --- if the species is a not a tracer species skip it ---
c
          if( .NOT. lusespc(ispc) ) goto 10
c
c  --- do the WEST and EAST boundaries ----
c
          do 20 jcl=2,noy-1
c
c   --- if stratifying by boundary, put in seperate position ---
c
             if( lbndry ) then
                ioff = ncls_ioric-1 + IDXBWS
             else
                ioff = ncls_ioric
             endif
c
c  --- West boundary ---
c
            if( .NOT. lmpi .OR. 
     &                 isbound( mibcon(1),mmxp(1),mmyp(1),1,0 ) ) then
                do 30 izcl=1,noz
                   if( conc(1,jcl,izcl,ispc) .LE. 0 ) goto 30
c
c   --- loop over the tracer classes ---
c
                   do icls=1,ntrcls
                      saconc(1,jcl,izcl,iptcls(icls)+ioff) = 
     &                  saconc(1,jcl,izcl,iptcls(icls)+ioff) + 
     &                      conc(1,jcl,izcl,ispc) * trspmap(ispc,icls)
                   enddo
   30           continue
            endif
c
c  --- East boundary ---
c
c
c   --- if stratifying by boundary, put in seperate position ---
c
            if( lbndry ) then
               ioff = ncls_ioric-1 + IDXBES
            else
               ioff = ncls_ioric
            endif
            if( .NOT. lmpi .OR. 
     &             isbound( mibcon(1),mmxp(1),mmyp(1),mmxp(1),0 ) ) then
                do 40 izcl=1,noz
                   if( conc(nox,jcl,izcl,ispc) .LE. 0. ) goto 40
c
c   --- loop over the tracer classes ---
c
                   do icls=1,ntrcls
                       saconc(nox,jcl,izcl,iptcls(icls)+ioff) = 
     &                    saconc(nox,jcl,izcl,iptcls(icls)+ioff) + 
     &                      conc(nox,jcl,izcl,ispc) * trspmap(ispc,icls)
                   enddo
c
   40           continue
            endif
   20     continue
c
c  --- do the SOUTH and NORTH boundaries ----
c
          do 50 icl=2,nox-1
c
c  --- South boundary ---
c
c
c   --- stratifying by boundary, put in seperate position ---
c
             if( lbndry ) then
                 ioff = ncls_ioric-1 + IDXBST
             else
                 ioff = ncls_ioric
             endif
c
             if( .NOT. lmpi .OR. 
     &                  isbound( mibcon(1),mmxp(1),mmyp(1),0,1 ) ) then
                do 60 izcl=1,noz
                   if( conc(icl,1,izcl,ispc) .LE. 0. ) goto 60
c
c   --- loop over the tracer classes ---
c
                   do icls=1,ntrcls
                       saconc(icl,1,izcl,iptcls(icls)+ioff) = 
     &                    saconc(icl,1,izcl,iptcls(icls)+ioff) + 
     &                      conc(icl,1,izcl,ispc) * trspmap(ispc,icls)
                   enddo
c
   60           continue
              endif
c
c  --- North boundary ---
c
c
c   --- if stratifying by boundary, put in seperate position ---
c
             if( lbndry ) then
                 ioff = ncls_ioric-1 + IDXBNT
             else
                 ioff = ncls_ioric
             endif
c
             if( .NOT. lmpi .OR. 
     &           isbound( mibcon(1),mmxp(1),mmyp(1),0,mmyp(1) ) ) then
                do 70 izcl=1,noz
                   if( conc(icl,noy,izcl,ispc) .LE. 0. ) goto 70
c
c   --- loop over the tracer classes ---
c
                   do icls=1,ntrcls
                       saconc(icl,noy,izcl,iptcls(icls)+ioff) = 
     &                    saconc(icl,noy,izcl,iptcls(icls)+ioff) + 
     &                         conc(icl,noy,izcl,ispc) * trspmap(ispc,icls)
                   enddo
   70           continue
             endif
   50     continue
c
c  --- next species --
c
   10 continue
c
c  --- return to the calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
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
      return
      end
