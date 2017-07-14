
      subroutine abunplot
c******************************************************************************
c     This routine produces MONGO plots of line abundances versus
c     excitation potentials and equivalent widths
c******************************************************************************

      implicit real*8 (a-h,o-z)
      include 'Atmos.com'
      include 'Linex.com'
      include 'Pstuff.com'
      include 'Dummy.com'
      real*4 ep(2500),abb(2500),logrw(2500),wavepl(2500)
      equivalence (ep,dummy3(1)),(abb,dummy3(1251)),(logrw,dummy3(2501))
      real*4 style(1),ymed
      character ion*4


c*****dump the data into working arrays
      j = 0
      do l=lim1obs,lim2obs
         if (abundout(l) .ne. 999.99) then
            j = j + 1
            ep(j) = e(l,1)
            abb(j) = abundout(l)
c           logrw(j) = dlog10(wid1comp(l)/wave1(l))
            logrw(j) = dlog10(width(l)/wave1(l))
            wavepl(j) = wave1(l)
         endif
      enddo


c*****find the plot boundaries for the excitation potential plot
      xlo = 20.
      do j=1,kount
         xlo = amin1(xlo,ep(j))
      enddo
      xhi =  0.
      do j=1,kount
         xhi = amax1(xhi,ep(j))
      enddo
      if (xhi-xlo .lt. 5.) then
         xlo = amax1((xlo+xhi)/2.-2.5,-0.2)
         xhi = xlo + 5.0
      else
         xlo = amax1((xlo+xhi)/2.-5.0,-0.2)
         xhi = xlo + 10.
      endif
      ylo = 20.
      do j=1,kount
         ylo = amin1(ylo,abb(j))
      enddo
      yhi = -20.
      do j=1,kount
         yhi = amax1(yhi,abb(j))
      enddo
      if (yhi-ylo .lt. 0.5) then
         ylo = (ylo+yhi)/2. - 0.30
         yhi = ylo + 0.60
      elseif (yhi-ylo .lt. 1.0) then
         ylo = (ylo+yhi)/2. - 0.55
         yhi = ylo + 1.10
      else
         ylo = (ylo+yhi)/2. - 1.10
         yhi = ylo + 2.20
      endif


cc*****start the excitation potential plot via plot setup calls
c      call sm_defvar ('y_gutter','0.9')
c      call sm_window (1,3,1,3,1,3)
c      call sm_limits (xlo,xhi,ylo,yhi)
c      call findtic (xlo,xhi,bigxtic,smlxtic)
c      call findtic (ylo,yhi,bigytic,smlytic)
c      call sm_ticksize (smlxtic,bigxtic,smlytic,bigytic)
c
c
cc*****draw and label the box for the excitation potential plot
c      call defcolor (1)
c      call sm_lweight (4.0)
c      call sm_expand (1.2)
c      call sm_box (0,0,0,0)
c      call sm_lweight (2.0)
c      call sm_expand (0.8)
c      call sm_box (1,2,4,4)
c      array = 'E. P. (eV)'
c      call sm_relocate (0.5*(xlo+xhi),ylo-0.20*(yhi-ylo))
c      call sm_putlabel (5,array)
c      array = 'log eps'
c      call sm_relocate (xlo-0.10*(xhi-xlo),0.5*(yhi+ylo))
c      call sm_angle (90.)
c      call sm_putlabel (5,array)
c      call sm_angle (0.)
c      
c
cc*****make the excitation potential plot
c      call defcolor (2)
c      call sm_expand (2.2)
c      style(1) = 43.5
c      call sm_ptype (style,1) 
c      call sm_points (ep,abb,kount)
c      call defcolor (4)
c      ymed = average
c      call sm_lweight (4.0)
c      call sm_ltype (2)
c      call sm_relocate (xlo,ymed)
c      call sm_draw (xhi,ymed)
c      if (kount .gt. 2 .and. deltaep .gt. 1.5) then
c         call sm_ltype (3)
c         call defcolor (3)
c         call sm_relocate (xlo,real(xxm1*xlo+xxb1)) 
c         call sm_draw (xhi,real(xxm1*xhi+xxb1)) 
c      endif
c      call sm_ltype (0)
c      call sm_lweight (2.0)
c      call defcolor (1)
c      call sm_relocate (xlo+0.05*(xhi-xlo),ylo+0.15*(yhi-ylo))
c      call sm_expand (0.8)
c      ich = idint(charge(lim1obs) + 0.1)
c      if (ich .eq. 1) then 
c         ion = ' I  '
c      elseif (ich .eq. 2) then
c         ion = ' II '
c      elseif (ich .eq. 3) then
c         ion = ' III'
c      endif
c      iatom = idint(atom1(lim1obs))
c      write (array,1002) names(iatom),ion
c      call sm_relocate ((xhi+xlo)/2.,yhi-0.12*(yhi-ylo))
c      call sm_expand (0.8)
c      call sm_putlabel (5,array)
c   
c
cc*****define the plot limits for the equivalent width plot
c      xlo = 5000.
c      do j=1,kount
c         xlo = amin1(xlo,logrw(j))
c      enddo
c      xhi =  -5000.
c      do j=1,kount
c         xhi = amax1(xhi,logrw(j))
c      enddo
c      xlo = int((xlo-0.01)*2.-1.)/2.
c      xhi = int((xhi+0.01)*2.)/2.
c      call sm_defvar ('y_gutter','0.9')
c      call sm_window (1,3,1,2,1,2)
c      call sm_limits (xlo,xhi,ylo,yhi)
c      call findtic (xlo,xhi,bigxtic,smlxtic)
c      call findtic (ylo,yhi,bigytic,smlytic)
c      call sm_ticksize (smlxtic,bigxtic,smlytic,bigytic)
c
c
cc*****draw and label the box for the equivalent width plot
c      call defcolor (1)
c      call sm_lweight (4.0)
c      call sm_expand (1.2)
c      call sm_box (0,0,0,0)
c      call sm_lweight (2.0)
c      call sm_expand (0.8)
c      call sm_box (1,2,4,4)
c      array = 'log (EW/lambda)'
c      call sm_relocate (0.5*(xlo+xhi),ylo-0.20*(yhi-ylo))
c      call sm_putlabel (5,array)
c      array = 'log eps'
c      call sm_relocate (xlo-0.10*(xhi-xlo),0.5*(yhi+ylo))
c      call sm_angle (90.)
c      call sm_putlabel (5,array)
c      call sm_angle (0.)
c
c
cc*****make the equivalent width plot
c      call defcolor (2)
c      call sm_expand (2.2)
c      style(1) = 43.5
c      call sm_ptype (style,1)
c      call sm_points (logrw,abb,kount)
c      call defcolor (4)
c      call sm_lweight (4.0)
c      call sm_ltype (2)
c      call sm_relocate (xlo,ymed)
c      call sm_draw (xhi,ymed)
c      if (kount .gt. 2 .and. deltarw .gt. 0.5) then
c         call sm_ltype (3)
c         call defcolor (3)
c         call sm_relocate (xlo,real(xxm2*xlo+xxb2)) 
c         call sm_draw (xhi,real(xxm2*xhi+xxb2)) 
c      endif
c      call sm_ltype (0)
c      call sm_lweight (2.0)
c      call defcolor (1)
c      call sm_expand (0.8)
c      call sm_relocate ((xhi+xlo)/2.,yhi-0.12*(yhi-ylo))
c      call sm_putlabel (5,moditle(1:56))
c      call sm_relocate ((xhi+xlo)/2.,ylo+0.12*(yhi-ylo))
c      call sm_putlabel (5,moditle(57:80))
c
c
cc*****define the plot limits for the wavelength plot
c      xlo = 5000000.
c      do j=1,kount
c         xlo = amin1(xlo,wavepl(j))
c      enddo
c      xhi = 0.
c      do j=1,kount
c         xhi = amax1(xhi,wavepl(j))
c      enddo
c      xlo = int(xlo-50.)
c      xhi = int(xhi+50.)
c      call sm_defvar ('y_gutter','0.9')
c      call sm_window (1,3,1,1,1,1)
c      call sm_limits (xlo,xhi,ylo,yhi)
c      call findtic (xlo,xhi,bigxtic,smlxtic)
c      call findtic (ylo,yhi,bigytic,smlytic)
c      call sm_ticksize (smlxtic,bigxtic,smlytic,bigytic)
c
c
cc*****draw and label the box for the wavelength plot
c      call defcolor (1)
c      call sm_lweight (4.0)
c      call sm_expand (1.2)
c      call sm_box (0,0,0,0)
c      call sm_lweight (2.0)
c      call sm_expand (0.8)
c      call sm_box (1,2,4,4)
c      array = 'lambda (A)'
c      call sm_relocate (0.5*(xlo+xhi),ylo-0.20*(yhi-ylo))
c      call sm_putlabel (5,array)
c      array = 'log eps' 
c      call sm_relocate (xlo-0.10*(xhi-xlo),0.5*(yhi+ylo)) 
c      call sm_angle (90.) 
c      call sm_putlabel (5,array) 
c      call sm_angle (0.) 
c
c
cc*****make the wavelength plot, and exit normally
c      call defcolor (2)
c      call sm_expand (2.2)
c      style(1) = 43.5 
c      call sm_ptype (style,1)
c      call sm_points (wavepl,abb,kount)
c      call defcolor (4)
c      call sm_relocate (xlo,ymed)
c      call sm_lweight (4.0)
c      call sm_ltype (2)
c      call sm_draw (xhi,ymed)
c      if (kount .gt. 2 .and. deltawv .gt. 500.) then
c         call sm_ltype (3)
c         call defcolor (3)
c         call sm_relocate (xlo,real(xxm3*xlo+xxb3))
c         call sm_draw (xhi,real(xxm3*xhi+xxb3))
c      endif
c      call sm_ltype (0)  
c      call sm_lweight (2.0)
c      call defcolor (1)
c      call sm_relocate ((xhi+xlo)/2.,yhi-0.12*(yhi-ylo))
c      call sm_expand (0.8)
c      call sm_putlabel (5,linitle)
      return


c*****format statements
1002  format (a2,a4)


      end








