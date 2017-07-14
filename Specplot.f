
      subroutine specplot
c******************************************************************************
c     This routine produces MONGO plots of the syntheses.
c******************************************************************************

      implicit real*8 (a-h,o-z)
      include 'Quants.com'
      include 'Factor.com'
      include 'Atmos.com'
      include 'Linex.com'
      include 'Pstuff.com'
      include 'Equivs.com'
      include 'Multistar.com'
      real*4 style(1)
      real*4 yup,ydown
      integer iflip


c*****for grid syntheses, dump out relevant information to a file
      if (choice .eq. 'g') then
         write (nf6out,3001) syncount
         write (nf6out,3002) obsitle, moditle, linitle, smitle
      endif
 

cc*****begin with a default window
c      call sm_location (3500,31000,4000,30000)
c      call sm_window (1,1,1,1,1,1)
c      call sm_limits (0.0,1.0,0.0,1.0)
c      call defcolor (1)
c
c
cc*****write smoothing information at the top of the plot
c      call sm_lweight (2.2)
c      call sm_expand (0.7)
c      call sm_relocate (-0.120,1.015)
c      call sm_label (smitle)
c
c
c      if (isoitle(1:10) .eq. '          ') then
c         isoitle(1:16) = 'no isotopic data'
c      endif
c      call sm_relocate (-0.120,1.075)
c      call sm_label (isoitle(1:120))
c      if (numiso .gt. 3) then
c         call sm_relocate (-0.120,1.045)
c         call sm_label (isoitle(121:240))
c      endif
c      if (control .eq. 'gridplo' .or.
c     .    control .eq. 'gridsyn' .or.
c     .    control .eq. 'gridend') then
c         write (nf6out,3002) isoitle(1:120)
c         write (nf6out,3002) isoitle(121:240)
c      endif
c 
c
cc*****define the real plot limits
c      if (xlo .lt. xhi) then
c         call sm_limits (xlo,xhi,ylo,yhi)
c         iflip = 0
c      else
c         call sm_limits (xhi,xlo,ylo,yhi)
c         iflip = 1
c      endif
c      call findtic (xlo,xhi,bigxtic,smlxtic)
c      call findtic (ylo,yhi,bigytic,smlytic)
c      call sm_ticksize (smlxtic,bigxtic,smlytic,bigytic)
c
c
cc*****draw and label the box for the spectra
c      call defcolor (1)
c      if (whichwin .eq. '1of1') then
c         idev = 1
c         call sm_window (1,1,1,1,1,1)
c      else
c         idev = 2
c         call sm_defvar ('y_gutter','0.0')
c         call sm_window (1,2,1,1,1,1)
c      endif
c      call sm_lweight (4.0)
c      call sm_expand (1.2)
c      call sm_box (0,0,0,0)
c      call sm_lweight (2.0)
c      call sm_expand (0.8)
c      call sm_box (1,2,4,4)
c      if (iflip .eq. 1) then
c         array = 'Wavenumber'
c      else
c         array = 'Wavelength'
c      endif
c      call sm_xlabel (array)
c      array = 'Rel  Flux'
c      call sm_ylabel (array)
c
c
cc*****plot the synthetic spectra
c      call sm_lweight (2.2)
c      call sm_expand (0.7)
c      do i=1,100
c         if (pec(i) .ne. 0) go to 111
c      enddo            
c111   do j=1,nsyn
c         if (choice.eq.'h' .or. choice.eq.'f' .or.
c     .       choice.eq.'g') then
c            call defcolor (8)
c            call sm_ltype (j-1)
c         else
c            if (smterm(1:3) .eq. 'x11') then
c               call defcolor (j+1)
c               call sm_ltype (0)
c            else
c               call defcolor (1)
c               call sm_ltype (j-1)
c            endif
c         endif
c         call sm_connect (xsyn,chunk(1,j),kount) 
c         if (iflip .eq. 1) then
c            call sm_relocate (xhi+0.045*(xlo-xhi),
c     .                     ylo+(0.12+0.06*j)*(yhi-ylo))
c            call sm_draw (xhi+0.005*(xlo-xhi),
c     .                 ylo+(0.12+0.06*j)*(yhi-ylo))
c            call sm_relocate (xhi+0.05*(xlo-xhi),
c     .                     ylo+(0.12+0.06*j)*(yhi-ylo))
c         else
c            call sm_relocate (xlo+0.045*(xhi-xlo),
c     .                     ylo+(0.12+0.06*j)*(yhi-ylo))
c            call sm_draw (xlo+0.005*(xlo-xhi),
c     .                 ylo+(0.12+0.06*j)*(yhi-ylo))
c            call sm_relocate (xlo+0.05*(xhi-xlo),
c     .                     ylo+(0.12+0.06*j)*(yhi-ylo))
c         endif
c         noff = 80*(j-1)
c         call sm_lweight (2.2)
c         call sm_label (abitle(noff+1:noff+80))
c         if ((control .eq. 'gridplo' .or.
c     .        control .eq. 'gridsyn' .or.
c     .        control .eq. 'gridend') .and. 
c     .        whichwin.eq.'1of1') then
c            write (nf6out,3002) abitle(noff+1:noff+80)
c         endif
c      enddo
c      call defcolor (1)
c      call sm_ltype (0)
c
c   
cc*****plot the observed spectrum
c      if (plotopt .eq. 2) then
c         call defcolor (1)
c         if (choice.eq.'h' .or. choice.eq.'f' .or.
c     .       choice.eq.'g') then
c            call sm_lweight (4.0)
c         else 
c            call sm_lweight (2.2)
c         endif
c         call sm_ltype (0)
c         call sm_expand (3.0)
c         style(1) = 43.5
c         call sm_ptype (style,1)
c         mount = lim2obs - lim1obs + 1
c         if (mount .lt. 500) then
c            call sm_points (xobs(lim1obs),yobs(lim1obs),mount)
c         else
c            if (histoyes .eq. 1) then
c               call sm_histogram (xobs(lim1obs),yobs(lim1obs),mount)
c            else
c               call sm_connect (xobs(lim1obs),yobs(lim1obs),mount)
c            endif
c         endif
c         call sm_lweight (2.2)
c         call sm_expand (0.7)
c         if (iflip .eq. 1) then
c            call sm_relocate (xhi+0.05*(xlo-xhi),ylo+0.12*(yhi-ylo))
c         else
c            call sm_relocate (xlo+0.05*(xhi-xlo),ylo+0.12*(yhi-ylo))
c         endif
c         call sm_label (obsitle)
c      endif
c      if (iflip .eq. 1) then
c         call sm_relocate (xhi+0.05*(xlo-xhi),ylo+0.06*(yhi-ylo))
c      else
c         call sm_relocate (xlo+0.05*(xhi-xlo),ylo+0.06*(yhi-ylo))
c      endif
c      call sm_label (moditle)
c      if (whichwin.eq.'1of1' .or. plotopt.ne.2) then
c         return
c      endif
c
c
cc*****this section of code is executed only if a deviations plot is desired;
cc     find the starting and stopping points in the arrays for the deviations
c      if (xsyn(kount) .le. xobs(lim1obs)) go to 1000
c      if (xsyn(1) .gt. xobs(lim2obs)) go to 1000
c      if (xsyn(1) .gt. xobs(lim1obs)) go to 150
c      lim3obs = lim1obs
c      do k=2,kount
c         if (xsyn(k) .gt. xobs(lim3obs)) then
c            lim1syn = k - 1
c            go to 155
c         endif
c      enddo
c150   lim1syn = 1
c      do l=lim1obs,lim2obs
c         if (xsyn(lim1syn) .le. xobs(l)) then
c            lim3obs = l 
c            go to 155
c         endif
c      enddo
c155   if (xsyn(kount) .lt. xobs(lim2obs)) go to 160
c      lim4obs = lim2obs
c      do k=lim1syn,kount
c         if (xsyn(k) .gt. xobs(lim4obs)) then
c            lim2syn = k
c            go to 165
c         endif
c      enddo
c160   lim2syn = kount
c      do l=lim3obs,lim2obs
c         if (xsyn(lim2syn) .lt. xobs(l)) then
c            lim4obs = l - 1
c            go to 165
c         endif
c      enddo
c
c
cc  compute the deviations; linear interpolation in the wavelength array
cc  of the synthetic spectra is considered sufficient
c165   do j=1,nsyn
c         lpoint = lim1syn
c         devsigma = 0.
c         do i=lim3obs,lim4obs
c170         if (xsyn(lpoint+1) .lt. xobs(i)) then
c               lpoint = lpoint + 1
c               go to 170
c            endif
c            syninterp = (chunk(lpoint+1,j)-chunk(lpoint,j))*
c     .         (xobs(i)-xsyn(lpoint))/(xsyn(lpoint+1)-xsyn(lpoint)) +
c     .         chunk(lpoint,j)
c            dev(i) = yobs(i) - syninterp
c            devsigma = devsigma + dev(i)**2
c         enddo
c         devsigma = dsqrt(devsigma/(lim4obs-lim3obs-1))
c         
c      
cc  from first set of deviations, define the plot limits, draw and label box
c         if (j .eq. 1) then
c            yup = -1000.
c            ydown = +1000.
c            do i=lim3obs,lim4obs
c               yup = amax1(yup,dev(i))
c               ydown = amin1(ydown,dev(i))
c            enddo
c            ydelta = amin1(0.3,amax1(0.05,1.5*(yup-ydown)/2.))
c            ydown = -ydelta
c            yup = ydelta
c            call sm_defvar ('y_gutter','0.0')
c            call sm_window (1,2,1,2,1,2)
c            call sm_limits (xlo,xhi,ydown,yup)
c            call findtic (ydown,yup,bigytic,smlytic)
c            call sm_ticksize (smlxtic,bigxtic,smlytic,bigytic)
c            call sm_lweight (4.0)
c            call sm_expand (1.2)
c            call defcolor (1)
c            call sm_box (0,0,0,0)
c            call sm_lweight (2.0)
c            call sm_expand (0.8)
c            call sm_box (4,2,4,4)
c            array = 'Obs - Comp'
c            call sm_ylabel (array)
c            call sm_relocate (xlo,0.0)
c            call sm_draw (xhi,0.0)
c         endif
c
c
cc  plot the array of deviations
c         if (choice.eq.'h' .or. choice.eq.'f' .or.
c     .       choice.eq.'g') then
c            call defcolor (8)
c            call sm_ltype (j-1)
c         else
c            if (smterm(1:3) .eq. 'x11') then
c               call defcolor (j+1)
c               call sm_ltype (0)
c            else
c               call defcolor (1)
c               call sm_ltype (j-1)
c            endif
c         endif
c         call defcolor (j+1)
c         call sm_lweight (2.2)
c         call sm_connect (xobs(lim1obs),dev(lim1obs),mount)
c         write (array,1021) devsigma
c         call sm_relocate(xhi-0.2*(xhi-xlo),
c     .                 ydown+(0.10+0.06*j)*(yup-ydown))
c         call sm_relocate(xhi-0.24*(xhi-xlo),
c     .                 ydown+(0.10+0.06*j)*(yup-ydown))
c         call sm_draw(xhi-0.215*(xhi-xlo),
c     .                 ydown+(0.10+0.06*j)*(yup-ydown))
c         call sm_label (array)
c         if (choice .eq. 'g') then
c            noff = 80*(j-1)
c            write (nf6out,3002) abitle(noff+1:noff+80)
c            write (nf6out,3003) devsigma, velsh
c         endif
c      enddo
c
c
cc  reset the spectrum plot boundaries before exiting
c      if(xlo .lt. xhi) then
c         call sm_limits (xlo,xhi,ylo,yhi)
c         iflip = 0
c      else
c         call sm_limits (xhi,xlo,ylo,yhi)
c         iflip = 1
c      endif
1000  return


c*****format statements
1021  format ('sigma =',f7.4)
3001  format (/'RUN NUMBER:', i4, 65('-'))
3002  format (a80)
3003  format ('sigma =',f8.5, '   lambda shift =',f7.3,' km/s')


      end



