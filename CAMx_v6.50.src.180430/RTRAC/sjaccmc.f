c*** SJACCMC
c
      subroutine sjaccmc(neq, t, y, ml, mu, pd, nrpd)
      use rtcmcchm
c
c----CAMx v6.50 180430
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Calculate fast species Jacobian (pd = df/dy) for SLSODE in the
c     RTRAC CMC solver
c
c     LSODE initializes pd() to zero 
c
c     Copyright 1996 - 2018
c    Ramboll
c
c    Argument descriptions:
c     Inputs:
c      neq   I  number of fast species (second dimension of pd)
c      t     R  current time
c      y     R  species concentrations
c      ml    I  not used
c      mu    I  not used
c      nrpd  I  number of rows in pd
c     Outputs:
c      pd    R  Jacobian (df/dy)
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   neq, ml, mu, nrpd
      real      t
      real      y(MXTRSP+MXSPEC), pd(nrpd,neq)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   k, n
      real      tmp
      real      rr(1)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- update any equilibrium species but not rr
c
      if( neqmrtc .GT. 0) call seqmcmc(y,rr,.false.,.true.)
c
c --- fill in the Jacobian 
c
      do n = 1,njactrm
         tmp = scoefjac(n)*srkrtc(idrxjac(n))
         if( nspjac(n) .GT. 0 ) then
            do k = 1,nspjac(n)
               tmp = tmp*y(idspjac(n,k))
            enddo
         endif
            pd(ipd(n),jpd(n)) = pd(ipd(n),jpd(n))+tmp
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end
