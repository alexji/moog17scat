
c*****this common block has quantities utilized in the computation
c     of the source function (and associated subroutines Sourcefunc_*, Cdcalc, etc.).

      real*8          dtau1(100),  
     .                wtmu(5), mu(5), Flux_line, 
     .                Flux_cont, adepth,                 
     .                S_cont(100), S_line(100),
     .                B_planck(100),
     .                J_cont(100), J_line(100),
     .                S_cont_CF(100) 
      integer         mmu

      common/intense/ dtau1,
     .                wtmu, mu, Flux_line, 
     .                Flux_cont, adepth,
     .                S_cont, S_line,
     . 		      B_planck,
     .                J_cont, J_line,
     .                S_cont_CF,
     .                mmu

