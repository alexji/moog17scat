      subroutine sourcefunc_scat_cont
c*************************************************************************
c     Calculates the *CONTINUUM* source function which incorporates both a
c     scattering and an absorption component.  The relevant quantities are S_cont (source function),
c     J_cont (the mean intensity), and Flux_cont (the total flux).
c*************************************************************************
      implicit real*8 (a-h,o-z)
      include 'Atmos.com'
      include 'Linex.com'
      include 'Source.com'
      data jentry /0/

c***  Local Arrays/Variables
      real*8  Thomson_cont(100)
c     real*8  B_planck(100) 
      real*8  ddm(100), alo(100), ood(100)
      real*8  eta(100), etat(100)
      real*8  Scat_opacity(100) 
      real*8  ddmm(100), dtau1MI(100) 
      real*8  Therm_opacity(100)
      real*8  dtau(100), WOP(100), W1(100), W0(100)
c     real*8  J_cont(100) 
      real*8  J_cont_OLD(100)
      real*8  J_cont_moog(100), S_cont_moog(100)
      real*8  exptau2(100)
c***  real*8  S_cont(100)
c     real*8  Fluxarray_cont(100)
      integer IT


c*** Call the AngWeight.f subroutine.
c    During initial pass, calculate the Gaussian integration points and weights.
c    AngWeight.f functions ONLY with odd numbers of rays (mmu = 1, 3, 5,...).

      if (jentry .eq. 0) then
         mmu = 3
         aaa = 0.
         bbb = 1.
         call AngWeight (mmu,aaa,bbb,wtmu,mu)
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
        ood(i)     = kaplam(i) / rho(i)
        if (i .eq. 1)  cycle
        ddm(i)     = 0.5 * abs((rhox(i) - rhox(i-1)))
        dtau1(i)   = ddm(i) * (ood(i-1) + ood(i))
        if (i .eq. ntau) cycle
        ddmm(i)    = 0.25 * (rhox(i+1) - rhox(i-1))
        dtau1MI(i) = 1. / (ddmm(i) * (ood(i-1) + ood(i+1)))
      enddo

c       do i=1, ntau
c         write (0,'(A,1X, I2, F12.5, 1X, 1p7e10.2)')
c     >         'CONT: tau ood dtau', i, ood(i), dtau1(i)
c      enddo
c      pause


c*** Prepare the THOMSON, Scat_opacity, and Therm_opacity terms.
c*** As in the standard formulation, Therm_opacity(i) = kaplam(i) - Scat_opacity(i).
      do i=1, ntau
         Scat_opacity(i)  = kaplamsca(i)
         Therm_opacity(i) = kaplamabs(i)
         Thomson_cont(i)  = Scat_opacity(i)/kaplam(i)
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

          dtau_alo = dtau_alo * Thomson_cont(i) /
     >                      (2. * gamma)

c***  Lambda Operator with THOMSON factor applied as above.
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
      H_cont       = Const1/wave
      H_cont_core  = H_cont/t(ntau)
      B_core       = Const2/((exp(H_cont_core)-1.)*wave*wave*wave)
      H_cont_core1 = H_cont/ t(ntau-1)
      B_core1      = Const2/((exp(H_cont_core1)-1.)*wave*wave*wave)
      DB_core      = B_core-B_core1

c***  Critical definitions
      do i=1, ntau
        H_cont_t      = H_cont / t(i)
        B_planck(i)   = Const2 / ((exp(H_cont_t)-1.)*wave*wave*wave)
        J_cont_OLD(i) = B_planck(i)
        eta(i)        = B_planck(i) * Therm_opacity(i)
        etat(i)       = Scat_opacity(i) * J_cont_OLD(i)
        Fluxarray_cont(i) = 0.
      enddo
 
c     ---------------------------------
c***  START of the main ITERATION loop
c     ---------------------------------
      IT   = 0
      do
        IT = IT + 1
        do i=1,ntau
           J_cont_OLD(i)     = J_cont(i)
           J_cont(i)         = 0.
           S_cont(i)         = (eta(i) + etat(i)) / kaplam(i)
        enddo

c     -------------------------
c***  START of the ANGLE loop
c     -------------------------
        Flux_cont    = 0.
        Flux_pureabs = 0.

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
            XI        = XI*exptau + W0(i)*S_cont(i) + W1(i)*S_cont(i-1)
            J_cont(i) = J_cont(i) + 0.5 * wtmu(j) * XI
c***  End of second Depth Loop
          enddo

c***  Prepare Inner Boundary, multiplied by 2/dtau
          XI = 0.5 * wtmu(j) * (B_core+(DB_core*mu(j)/dtau1(ntau)))

c***  Inner Boundary Condition
          J_cont(ntau) = J_cont(ntau) + XI

c***  Third Depth Loop : Integration outwards
          do i=ntau-1, 1, -1
            dtau(i)   = dtau1(i+1) / mu(j)
            exptau    = dexp(-dtau(i))
            WOP(i)    = (exptau - 1.) / dtau(i)
            W0(i)     = 1. + WOP(i)
            W1(i)     = -exptau - WOP(i)
            XI        = XI*exptau + W0(i)*S_cont(i) + W1(i)*S_cont(i+1)
            J_cont(i) = J_cont(i) + 0.5 * wtmu(j) * XI
c***  End of third Depth Loop
          enddo

c***  Emergent Flux (at depth of ntau = 1) 
          Flux_cont    = Flux_cont + XI * 0.5 * wtmu(j) * mu(j)

c     ------------------
c***  END of ANGLE Loop
c     ------------------
        enddo

c***  Calculate New etat and Exit Iteration Loop :: ALI (acceleration) applied!
        q_max = 0.
        i_max = 0.
        do i=1, ntau
c***  Original etat_new and DELTA-etat
          etat_new = Scat_opacity(i) * J_cont(i)
          DE       = etat_new - etat(i)
c***  Accelerated DELTA-etat and improved etat_new
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

c***  Deals with the quality/efficacy of convergence. 
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
              Fluxarray_cont(i) = S_cont(i)*exptau2(i)
c              write (0,'(A,1X,I2,1x,6(1p2d12.3))')
c     >              'i Flux_cont exptau xref',
c     >               i,Fluxarray_cont(i),exptau2(i),xref(i)
           enddo 

c     -----------------------
c***  END of ITERATION loop
c     -----------------------
      enddo
      

c***  Tabulation of the flux due to pure absorption (not requisite).
      Flux_pureabs = Flux_pureabs + B_planck(1)


c***  Conversion of variables to MOOG/Edmonds format/units (if desired).
       do i=1,ntau
          S_cont_moog(i) = 2.9977518E26*(1/(wave**2))
     >                *S_cont(i)
          J_cont_moog(i) = 2.9977518E26*(1/(wave**2))
     >                *J_cont(i)
       enddo

       Flux_cont_moog = 2.9977518E26*(1/(wave**2))
     >             *Flux_cont
       Flux_pureabs_moog =  2.9977518E26*(1/(wave**2))
     >             *Flux_pureabs
           
      return
      end

