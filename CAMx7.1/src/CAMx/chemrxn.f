      subroutine chemrxn(igrd,iproc_id)
      use filunit
      use grid
      use grid_nodes
      use chmstry
      use camxfld
      use camxcom
      use procan
c
      use master_mod
      use node_mod
c
c----CAMx v7.10 210105
c
c     CHEMRXN passes arrays from common blocks to CHEMDRIV
c
c     Copyright 1996 - 2021
c     Ramboll
c            
c     Modifications:  
c        10/13/17 -bkoo-     Added aerosol pH to CHEMDRIV call (APH)
c        10/16/17 -bkoo-     Added time-weighted NO2 photolysis rate to CHEMDRIV call (AJNO2)
c
c     Input arguments:
c        igrd                grid index
c
c     Output arguments:
c        none
c
c     Routines called:
c        CHEMDRIV
c        MASSUM
c
c     Called by:
c        CAMx
c        NESTING
c
      include 'camx.prm'
      include 'flags.inc'
c
c-----Entry point
c
      call massum(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &           igrd,nspec,ncol(igrd),nrow(igrd),nlay(igrd),
     &           deltax(1,igrd),deltay(igrd),depth(iptr3d(igrd)),
     &           mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),xmstmp(1,igrd))
c
c  --- call routine to do the chemistry ---
c
      if( lchem ) then
        if( iproc_id .LE. 1 ) then
          write(*,'(a20,$)') 'chemdriv ......'
        endif
        write(iout,'(a20,$)') 'chemdriv ......'
        call flush(6)
        call chemdriv(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,igrd,
     &                ncol(igrd),nrow(igrd),nlay(igrd),
     &                deltat(igrd),itzon,idfin(iptr2d(igrd)),
     &                deltax(1,igrd),deltay(igrd),cldtrns(iptr3d(igrd)),
     &                water(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &                tsurf(iptr2d(igrd)),press(iptr3d(igrd)),
     &                height(iptr3d(igrd)),windu(iptr3d(igrd)),
     &                windv(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &                pwr(iptr3d(igrd)),cph(iptr3d(igrd)),
     &                aph(iptr3d(igrd)),ajno2(iptr3d(igrd)),
     &                cigwtr(iptr3d(igrd)),
     &                conc(iptr4d(igrd)),cellat(iptr2d(igrd)),
     &                cellon(iptr2d(igrd)),snow(iptr2d(igrd)),
     &                snowrat(iptr2d(igrd)),snowfrc(iptr2d(igrd)),
     &                snowalb(iptr2d(igrd)),topo(iptr2d(igrd)),
     &                fsurf(iptrlu(igrd)),albedo(iptr2d(igrd)),
     &                lrdlai(igrd),lai(iptr2d(igrd)),
     &                ldark(iptr2d(igrd)),solmas(iptrsm(igrd)),
     &                vegmas(iptrsm(igrd)),reemis(iptrsm(igrd)),
     &                l3davg(igrd),iptr2d(igrd),ipsa3d(igrd),
     &                ipacl_3d(iptr3d_full(igrd)),iproc_id)
        if( iproc_id .LE. 1 ) then
           write(*,'(a)') '   Done'
        endif
        write(iout,'(a)') '   Done'
        call flush(6)
        call flush(iout)
      endif
c
      call massum(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &            igrd,nspec,ncol(igrd),nrow(igrd),nlay(igrd),
     &            deltax(1,igrd),deltay(igrd),depth(iptr3d(igrd)),
     &            mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),xmass(1,igrd))
c
      do l = 1,nspec 
        xmschem(l,igrd) = xmschem(l,igrd) + xmass(l,igrd) - 
     &                    xmstmp(l,igrd) 
      enddo
c
      return
      end
