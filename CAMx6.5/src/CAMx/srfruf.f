      subroutine srfruf(m1,m2,m3,ncol,nrow,nlay,date,cellat,cellon,
     &                 windu,windv,fsurf,snow,lrdlai,lai,sfcz0)
c
c----CAMx v6.50 180430
c 
c     SRFRUF calculates surface roughness for the specified grid, according
c     to landuse, season, snow, and wind speed (water surfaces only).
c 
c     Copyright 1996 - 2018
c     Ramboll
c           
c     Modifications:
c        8/10/09   Added Zhang (2003) LAI-based surface roughness calculation
c                  for 26 landuse categories
c        02/11/11  Removed optional roughness from AHO
c        04/02/12  Removed drought stress and snow flag; AHO
c                  file is now just ozone column
c        08/11/14  Snow cover changed to water equivalent depth
c
c     Input arguments:
c        m1                  number of columns (MPI slice)
c        m2                  number of rows (MPI slice)
c        m3                  number of layers (MPI slice)
c        ncol                number of columns 
c        nrow                number of rows 
c        nlay                number of layers 
c        date                model date
c        cellat              cell centroid latitude (deg)
c        cellon              cell centroid longitude (deg)
c        windu               layer U-component wind field (m/s)
c        windv               layer V-component wind field (m/s)
c        fsurf               fractional landuse cover field (fraction)
c        snow                snow cover water equivalent (m)
c             
c     Output arguments: 
c        sfcz0               surface roughness (m)
c             
c     Routines called: 
c        None
c             
c     Called by: 
c        EMISTRNS
c 
      implicit none
      include 'camx.prm'
      include 'deposit.inc'
      include 'flags.inc'
c
      integer :: m1,m2,m3,iday,iyear
      integer :: ncol,nrow,nlay,idate,date,month
      real, dimension(m1,m2) :: cellat, cellon, lai, sfcz0, snow
      integer, dimension(12) :: nday
      real, dimension(m1,m2,m3) :: windu, windv
      real, dimension(m1,m2,nlu) :: fsurf
      real lai_ref_intpl,rlai,ref_lai,lai_f
      logical lrdlai
c
      integer i,j,m,mbin,latbin,isesn
      real ucomp,vcomp,wind,totland,z0,z0sum
c
      data nday/31,28,31,30,31,30,31,31,30,31,30,31/
c
c-----Entry point
c
      idate = date
      call caldate(idate)
      iyear = idate/1000
      if (mod(iyear,4).eq.0) nday(2)=29
      month = (idate - 10000*int(idate/10000.))/100
      iday = idate - 100*int(idate/100.) !Julian day
c
c-----Loop over rows and columns
c
      do 30 j = 2,m2-1 
        do 20 i = 2,m1-1
c
c-----Determine season
c
          mbin = month
          if (cellat(i,j).lt.0.) then
            mbin = mod(month+6,12)
            if (mbin.eq.0) mbin = 12
          endif
          latbin = 1
          if (abs(cellat(i,j)).gt.20.) then
            latbin = 2
          elseif (abs(cellat(i,j)).gt.35.) then
            latbin = 3
          elseif (abs(cellat(i,j)).gt.50.) then
            latbin = 4
          elseif (abs(cellat(i,j)).gt.75.) then
            latbin = 5
          endif
          if ((cellat(i,j).gt.50. .and. cellat(i,j).lt.75.) .and.
     &        (cellon(i,j).gt.-15. .and. cellon(i,j).lt.15.)) latbin = 3
          isesn = iseason(latbin,mbin)
c
c-----Use input snow cover to set season, if specified
c
          if (snow(i,j).ge.0.001) isesn = 4   ! Snow cover check > 1 cm
c
c-----Determine cell-average relative LAI if it was read
c
          if (lrdlai .and. nlu.eq.26) then
            ref_lai = 0.
            do m = 1,nlu
              lai_ref_intpl = lai_ref(m,mbin) + iday/nday(mbin)*
     &                       (lai_ref(m,mbin+1) - lai_ref(m,mbin))
              ref_lai = ref_lai + fsurf(i,j,m)*lai_ref_intpl
            enddo
            rlai = lai(i,j)/(ref_lai + 1.e-10)
          endif
c
c-----Load local met variables
c
          ucomp = (windu(i,j,1) + windu(i-1,j,1))/2.
          vcomp = (windv(i,j,1) + windv(i,j-1,1))/2.
          wind = sqrt(ucomp**2 + vcomp**2)
          wind = amax1(0.1,wind)
c
c-----Loop over land use; determine roughness depending on choice of deposition
c     model
c
          totland = 0.
          z0sum = 0.
          do 10 m = 1,nlu
            if (fsurf(i,j,m).lt.0.01) goto 10
            totland = totland + fsurf(i,j,m)
c
c-----Wesely (1989) landuse cats; surface roughness for water is dependent on
c     wind speed
c
            if (nlu .eq. 11) then
              z0 = z0lu(m,isesn)
              if (m.eq.7) z0 = amax1(z0,2.0e-6*wind**2.5)
c
c-----Zhang (2003) landuse cats; roughnes scaled by LAI
c
            else
              lai_f = lai_ref(m,mbin) +
     &                float(min(nday(mbin),iday))/float(nday(mbin))*
     &                (lai_ref(m,mbin+1) - lai_ref(m,mbin))
              if (lrdlai) then
                lai_f = lai_f*rlai
                lai_f = amin1(lai_ref(m,15),lai_f)
                lai_f = amax1(lai_ref(m,14),lai_f)
              endif
              if (m.eq.1 .or. m.eq.3) then
                z0 = 2.0e-6*wind**2.5
              else
                if (z02(m).gt.z01(m)) then
                  z0 = z01(m) + (z02(m) - z01(m))*
     &                        (lai_f - lai_ref(m,14))/
     &                        (lai_ref(m,15) - lai_ref(m,14))
                else
                  z0 = z01(m)
                endif
              endif
            endif
c
            z0sum = z0sum + alog(z0)*fsurf(i,j,m)
 10       continue

          totland = amax1(totland, 0.01)
          sfcz0(i,j) = exp(z0sum)/totland

 20     continue
 30   continue
c
      return
      end
