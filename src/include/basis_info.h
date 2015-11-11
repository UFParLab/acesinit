      common/basis_info/ntot_alpha, ntot_pcoef, ntot_shells,  
     *                  Talpha, Talpha_norm, Tpcoeff, Tixalpha,
     *                  Tixpcoeff, Tcoord 

      integer ntot_alpha, ntot_pcoef, ntot_shells 
      integer maxcoeff, maxalpha, maxshells, Max_prims 
      integer max_centers_dupl

      parameter (maxcoeff = 50000)
      parameter (maxalpha = 10000)
      parameter (maxshells = 5000)
      parameter (Max_prims = 1000)
      parameter (max_centers_dupl = 300)
      double precision Talpha(maxalpha)
      double precision Talpha_norm(maxalpha)
      double precision Tpcoeff(maxcoeff)
      double precision Tcoord(3,maxshells)
      integer Tixalpha(maxshells)
      integer Tixpcoeff(maxshells)

