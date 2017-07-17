      subroutine sourcefunc_scat_line
c******************************************************************
c     Calculates the *LINE+CONT* source function which incorporates both a
c     scattering and an absorption component.  The relevant quantities are S_line (source function),
c     J_line (the mean intensity), and Flux_line (the total flux).  Note that these
c     quantities have a dependence on frequency.
c******************************************************************
      implicit real*8 (a-h,o-z)
      include 'Atmos.com'
      include 'Linex.com'
      include 'Source.com'
      data jentry /0/

c***  Local Arrays/Variables
      real*8  Thomson_line(100)
c     real*8  B_planck(100)
      real*8  ddm(100), alo(100), ood(100)
      real*8  eta(100), etat(100), Scat_opacity(100)
      real*8  ddmm(100), dtau1MI(100), Therm_opacity(100)
      real*8  dtau(100), WOP(100), W1(100), W0(100)
c     real*8  J_line(100) 
      real*8  exptau2(100)
      real*8  J_line_OLD(100)
      real*8  J_line_freq(100)
c     real*8  S_line(100)
      real*8  xfr(20), phi(20), wfr(20)
      real*8  pisqi
      integer IT
      integer ifr
      integer xmax
      real*8  CC_line(100)
      real*8  exptau3(100)

c***  Beginning of "future" frequency loop
      pisqi = 0.5641895835
      xmax  = 5.
      nfr   = 2.

      if(xmax.le.0. .or. nfr.le.1) then
         nfr    = 1.
         phi(1) = 1.
         wfr(1) = 1.
         return
      endif
      dx  = xmax / dble(nfr-1)
      sum = 0

c***  Absorption profile assumed to be given by a simple Doppler profile

      do ifr=1, nfr
         x       = (ifr-1.)*dx
         xfr(ifr)= x
         phi(ifr)= exp(-x*x)*pisqi
         wfr(ifr)= dx
         if(ifr.eq.1.or.ifr.eq.nfr) then 
            wfr(ifr)= dx * 0.5
         endif
         sum = sum + phi(ifr)*wfr(ifr)
      enddo

c***  Renormalization of frequency quadrature weights

      do ifr=1, nfr
         wfr(ifr) = wfr(ifr)/sum
      enddo


c*** Call the AngWeight.f subroutine.
c    During initial pass, calculate the Gaussian integration points and weights.
c    AngWeight.f functions ONLY with odd numbers of rays (mmu = 1, 3, 5,...).
c    Do I need to call this again?  Clean-up. 

      if (jentry .eq. 0) then
         mmu = 3
         aaa = 0.
         bbb = 1.
         call angweight (mmu,aaa,bbb,wtmu,mu)
               xxx = 0
               do i=1,mmu
                   xxx = xxx + wtmu(i)
               enddo
         jentry = 1     
      endif

c***  Criteria for convergence (note that the initial standards were relaxed)
c     Originally: Converg_iter = 1.e-5 and Max_iter = 25
      Converg_iter = 2.E-3
      Max_iter     = 65

c*** Prepare rhox for MODELS that do not contain these values.
c    An additional constant might be necessary as kappa does vary.
      do i=1, ntau
         if (modtype .eq. 'KURTYPE   ' .or.
     >       modtype .eq. 'KURUCZ    ' .or.
     >       modtype .eq. 'WEBMARCS  ') cycle
         rhox(i) = tauref(i)/(kapref(i)/rho(i))
      enddo   

c*** Set-up the delta(tau) variable (name: dtau1).
      do i=1, ntau
        ood(i)     = (kaplam(i) + kapnu(i)) / rho(i)
        if (i .eq. 1)  cycle
        ddm(i)     = 0.5 * abs((rhox(i) - rhox(i-1)))
        dtau1(i)   = ddm(i) * (ood(i-1) + ood(i))
        if (i .eq. ntau) cycle
        ddmm(i)    = 0.25 * (rhox(i+1) - rhox(i-1))
        dtau1MI(i) = 1. / (ddmm(i) * (ood(i-1) + ood(i+1)))
      enddo

c      do i=1, ntau
c         write (0,'(A,1X, I2, F12.5, 1X, 1p7e10.2)')
c     >         'LINE: tau ood dtau', i, ood(i), dtau1(i)
c      enddo
c      pause

c*** Prepare the THOMSON, Scat_opacity, and Therm_opacity terms.  Note now the
c    inclusion of the line-associated quantities.

      do i=1, ntau
         Scat_opacity(i)  = kaplamsca(i)
         Therm_opacity(i) = kaplamabs(i) + kapnu(i)
         Thomson_line(i)  = Scat_opacity(i)/(kaplam(i) + kapnu(i))
      enddo
 
c***  To accelerate the convergence of the solution of the RTE, it is necessary to employ the technique of 
c     accelerated lambda iteration (ALI). Below, the set-up of the approximate lambda operator occurs. 
c     Gamma controls the amount of acceleration.
c     Gamma = 0 : No Acceleration
c           = 1 : Full Acceleration
c           > 1 : Acceleration with dampening
 
      gamma = 1.
      if (gamma .eq. 0.) then
         do i=1, ntau
           alo(i) = 1.
         enddo
      else
        do i=1, ntau
          if (i .eq. 1) then
            dtau_alo = dtau1(2)
          else if (i .eq. ntau) then

c***  Apply the necessary extra dampening for the inner boundary
            dtau_alo = 0.5 * dtau1(ntau)
          else
            dtau_alo = 0.5 * (dtau1(i) + dtau1(i+1))
          endif

c***  Dampening by 2. * gamma
c     Sum (Integral) of 1/mu * dmu = Sum of mu * dmu = 0.5
c     Alternate expression for dtau_alo operator which may be used: dtau_alo = dtau_alo * 1. /

          dtau_alo = dtau_alo * Thomson_line(i) / (2. * gamma)

c***  Lambda Operator with THOMSON factor applied as shown above.
          exp_dtau_alo = exp(-dtau_alo)
          if (exp_dtau_alo .ne. 1.) then
            alo(i) = dtau_alo / (1. - exp(-dtau_alo))
          else
            alo(i) = 1.
          endif
        enddo
      endif

c***  Emissivity from planck function
      Const1       = 1.43878858E08
      Const2       = 3.972610376E08
      H_line       = Const1/wave
      H_line_core  = H_line/t(ntau)
      B_core       = Const2/((exp(H_line_core)-1.)*wave*wave*wave)
      H_line_core1 = H_line/t(ntau-1)
      B_core1      = Const2/((exp(H_line_core1)-1.)*wave*wave*wave)
      DB_core      = B_core-B_core1

c***  Critical definitions
      do i=1, ntau
        H_line_t       = H_line / t(i)
        B_planck(i)    = Const2/((exp(H_line_t)-1.)*wave*wave*wave)
        J_line_OLD(i)  = B_planck(i)
        eta(i)         = B_planck(i) * Therm_opacity(i)
        etat(i)        = Scat_opacity(i) * J_line_OLD(i)
        J_line_freq(i) = 0.
        Fluxarray_line(i) = 0.
        CC_line(i)     = 0.
      enddo
 
c     ---------------------------------
c***  START of the main ITERATION loop
c     ---------------------------------
      IT   = 0
      do
        IT = IT + 1

c     ----------------------------
c***  START of the FREQUENCY loop
c     ----------------------------
c        do ifr=1, nfr

           do i=1,ntau
              J_line_OLD(i)     = J_line(i)
              J_line(i)         = 0.
              S_line(i)         = (eta(i)+etat(i))/(kaplam(i)+kapnu(i))
           enddo

c     --------------------------
c***  START of the ANGLE loop
c     --------------------------
           Flux_line = 0.
           do j=1, mmu

c***  Employ the first-order short-characteristics methodology.
c     Note that there is no incident radiation.
              XI = 0.

c***  Second Depth Loop : Integration inwards
              do i=2, ntau
                 dtau(i)   = dtau1(i) / mu(j)
                 exptau    = dexp(-dtau(i))
                 WOP(i)    = (exptau - 1.) / dtau(i)
                 W0(i)     = 1. + WOP(i)
                 W1(i)     = -exptau - WOP(i)
                 XI        = XI*exptau+W0(i)*S_line(i)+W1(i)*S_line(i-1)
                 J_line(i) = J_line(i) + 0.5 * wtmu(j) * XI
c***  End of second Depth Loop
              enddo

c***  Prepare Inner Boundary, multiplied by 2/dtau
              XI = 0.5*wtmu(j)*(B_core+(DB_core*mu(j)/dtau1(ntau)))

c***  Inner Boundary Condition
              J_line(ntau) = J_line(ntau) + XI

c***  Third Depth Loop : Integration outwards
              do i=ntau-1, 1, -1
                 dtau(i)   = dtau1(i+1) / mu(j)
                 exptau    = dexp(-dtau(i))
                 WOP(i)    = (exptau - 1.) / dtau(i)
                 W0(i)     = 1. + WOP(i)
                 W1(i)     = -exptau - WOP(i)
                 XI        = XI*exptau+W0(i)*S_line(i)+W1(i)*S_line(i+1)
                 J_line(i) = J_line(i) + 0.5 * wtmu(j) * XI
c***  End of third Depth Loop
              enddo
              
c***  Flux with no frequency averaging
           Flux_line = Flux_line+XI*0.5*wtmu(j)*mu(j)

c     -----------------
c***  END of ANGLE loop
c     -----------------
           enddo
c           Flux_line_freq(ifr) = Flux_line           
           do i=1, ntau
              J_line_freq(i) = J_line_freq(i) + 
     >                         J_line(i)*wfr(ifr)*phi(ifr)
           enddo

c     ---------------------
c     END of FREQUENCY loop
c     ---------------------
c         enddo


c***  Calculate New etat and Exit Iteration Loop :: ALI applied!
        q_max = 0.
        i_max = 0.

        do i=1, ntau
c***  My feeling is that etat_new should be a function of J_line_freq
c     etat_new = Scat_opacity(i) * J_line_freq(i)
c     If the above holds true, then also a new lambda operator should
c     also be introduced.

c***  Original etat_new and DELTA(etat)
          etat_new = Scat_opacity(i) * J_line(i)
          DE       = etat_new - etat(i)

c***  Accelerated DELTA(etat) and improved etat_new
          DE       = DE * alo(i)
          etat_new = etat(i) + DE
          QNA      = (etat_new - etat(i)) / etat_new
          Q        = ABS(etat_new - etat(i)) / etat_new
          if (Q .gt. q_max) then
            q_max   = Q
            q_maxNA = QNA
            i_max   = i
          endif
          etat(i) = etat_new
        enddo

c***  Determines with the quality/efficacy of convergence. 
        if (q_max .le. Converg_iter .or. Max_iter .eq. 1) exit

        if (IT .ge. Max_iter) then
          write (0,'(A,F12.5,1X,1p7e10.2,1X,I3,1X,I3)')
     >      'Maximum number of iterations exceeded : wave, q_max= ',
     >      wave, q_max, i_max, ntau
          exit
        endif

c***  Initial attempt to keep track of finalized flux per atmospheric layer
           do i=1, ntau
              exptau2(i)        = dexp(-dtau(i))
              Fluxarray_line(i) = S_line(i)*exptau2(i)
           enddo 


         if (wave .eq. wave1(lim1)) then
            do i=1, ntau
               exptau3(i) = dexp(-tauref(i))
               CC_line(i) = tauref(i)*(kapnu(i)/kapref(i))*
     >                              S_line(i)*exptau3(i)
c               write (0,'(A,F12.1,1x,i2,1p2e12.4)') 'wave CC_LINE',
c     >               wave,i,CC_line(i)
            enddo   
          endif

c     ---------------------
c***  END of ITERATION loop
c     ---------------------
      enddo

      return
      end


