      common/basis_info/ntot_alpha, ntot_pcoef, ntot_shells,  
     *                  Talpha, Tpcoeff, Tixalpha, Tixpcoeff, 
     *                  Tcoord 

      integer ntot_alpha, ntot_pcoef, ntot_shells 
      integer maxcoeff, maxalpha, maxshells    
      parameter (maxcoeff = 50000)
      parameter (maxalpha = 10000)
      parameter (maxshells = 10000)
      double precision Talpha(maxalpha)
      double precision Tpcoeff(maxcoeff)
      double precision Tcoord(3,maxshells)
      integer Tixalpha(maxshells)
      integer Tixpcoeff(maxshells)

