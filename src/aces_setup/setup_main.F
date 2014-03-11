! ============================================================================
! Name        : setup_main.f
! Author      : 
! Version     :
! Copyright   : Your copyright notice
! Description : 
! ============================================================================


      subroutine setup_main(jobname,datname)
c      program setup_main
          use, intrinsic :: iso_c_binding
          implicit none
          character, dimension(*), intent(in) :: datname
          character, dimension(*), intent(in) :: jobname

      include 'setup_interface.f'
      include 'trace.h'
      include 'int_gen_parms.h'
      include 'frag_def.h'
      include 'sial_config_params.h'
      include 'basis_info.h'
      include 'mxatms.par'

      integer max_segments, mxcbf  
      parameter (max_segments = 1000000)
      parameter (mxcbf = 10000) 
c      parameter (max_segments = 10000)
c      parameter (mxcbf = 500) 
      integer (C_INT) nfocc 
      integer (C_INT) nfvirt
      integer (C_INT) nafocc 
      integer (C_INT) nafvirt
      integer (C_INT) nbfocc 
      integer (C_INT) nbfvirt 
      integer (C_INT) norb 
      integer (C_INT) norbs 
      integer (C_INT) nsegs 
      integer (C_INT) c_nshells  

c     integer (C_INT) maxmem 
      integer (C_INT) nsial 
      integer (C_INT) val0
c     integer (C_INT) nd
c     integer (C_INT) d(6)
c     integer (C_INT) d2(2)
      integer (C_INT) dd(max_segments)
      integer (C_INT) sDim2(2), sDim1(1), sDims(8)
c     real (C_DOUBLE) sVals(mxcbf,mxcbf)
      real (C_DOUBLE) cVals(3,mxatms)
      real (C_DOUBLE) sVals1(mxcbf)
      integer (C_INT) iVals1(mxcbf)

      real(C_DOUBLE) dval
c     real(C_DOUBLE) val(6)
c     real(C_DOUBLE) val2(9)

      integer (C_INT) sial_program_instance 
      character*80 sial_program 
      character*80 which_sial(100) ! No more than 100 sial programs in one run allowed
      integer (C_INT) sial_cparams(400) ! No more than 100 sial programs in one run allowed
      character*80 jobflow  
      real(C_DOUBLE) vnn  
c     integer (C_INT) indx_cc(max_shells) 
      integer (C_INT) ccbeg(max_shells)   
      integer (C_INT) ccend(max_shells)   
      integer (C_INT) intSpherical  

      character*5 lformat
      character*5 lstring

      integer i, j, kfrag 
      integer icc 
      integer segs(maxsegs)   
      integer saveocc 
      integer savevirt  
      integer saveao 
      integer auto_seg_switch 
      integer m_segs 
      integer n_a_occ, n_b_occ, n_a_virt, n_b_virt 

c------------------------------------------------------------------------------------------  
c                  Parameters read from ZMAT   
c------------------------------------------------------------------------------------------  

      integer (C_INT) mstr_is_wrkr
c     integer (C_INT) sip_mx_ao_segsize
c     integer (C_INT) sip_mx_occ_segsize
c     integer (C_INT) sip_mx_virt_segsize
      integer (C_INT) sheap_flag  
      integer (C_INT) ierr  
      character*80 package
c     character*80 molfile
c     character*80 local_path
      character*80 tracetype
      character*80 erd_fast_memcalc
      character*80 debug_print
c     integer (C_INT) nworkthread
      character*80 auto_seg_algorithm
      character*80 ctrip_str
      character*80 ihess_str
      character*80 jhess_str
      character*80 sub_str
      character*80 timer_param  
c     integer (C_INT) sip_sub_segsize
c     integer (C_INT) sip_sub_occ_segsize
c     integer (C_INT) sip_sub_virt_segsize
c     integer (C_INT) sip_sub_ao_segsize
c      character *80 tmp

      print *, 'calling initsetup_f from fortran'

      call init_machine_types() 

c Assume this can be set to zero. If we are on an Altix then fix it. 
      sheap_flag = 0

c This is effectively a dry run so set the dryrun flag to true. 
      dryrun     = .true. 

c      tmp = 'jobf10July'//C_NULL_CHAR
c      call init_setup('jobf10July'//C_NULL_CHAR)
      call init_setup(jobname)

      call aces_init_rte() 

      call init_params('ZMAT') 

      maxmem = 0 
      call igetparam('MAXMEM', 1, maxmem) 
      print *, 'Maxmem = ', maxmem  

      call mem_alloc_init(maxmem, sheap_flag, ierr) 
      print *, 'ierr out of mem_alloc_init = ', ierr  
      if (ierr .lt. 0) then 
          call abort_job()
      else 
          print *,'MEMORY ALLOCATION Sucessfull: maxmem = ',maxmem
      endif

      call scf_init(vnn) 

      call verify_aces2_params()
      call write_zmat_auto(jobflow)
      call aces_ja_fin() 
      call init_params('ZMAT.AUTO') 

c------------------------------------------------------------------------------------------  
c                          Create a list of sial_programs to run 
c                              Set the sial codes used  
c------------------------------------------------------------------------------------------  
  
      nsial                 = 0 
      sial_program_instance = 1 
10    continue 
      sial_program = ' ' 
      call cgetparam('SIAL_PROGRAM',sial_program_instance,sial_program)
      if (sial_program .ne. ' ') then 
         print *, 'sial_program = ', sial_program  
         call add_sial_program(sial_program//C_NULL_CHAR)
         nsial = nsial + 1 
         which_sial(nsial) = sial_program 
         sial_program_instance = sial_program_instance + 1 
         go to 10 
      endif 

      call reset_internal_system_data()   ! restore system flags

c------------------------------------------------------------------------------------------  
c                     Set default values for the following parameters  
c------------------------------------------------------------------------------------------  

      mstr_is_wrkr         = 1 
      package              = 'ERD'
      molfile              = 'MOL' 
      local_path           = '.' 
      tracetype            = ' ' 
      sial_program         = ' ' 
      erd_fast_memcalc     = 'NO' 
      debug_print          = 'NO' 
c     restart_program      = ' ' 
      auto_seg_algorithm   = 'SEGMENT_OPTIMIZED' 
      timer_param          = 'NO' 
      ctrip_str            = ' ' 
      ihess_str            = ' ' 
      jhess_str            = ' ' 
      sub_str              = ' ' 

      nworkthread          = 20 
      sip_mx_ao_segsize    = 0  
      sip_mx_occ_segsize   = 0  
      sip_mx_virt_segsize  = 0
      sip_sub_segsize      = 5  
      sip_sub_occ_segsize  = 5
      sip_sub_virt_segsize = 5
      sip_sub_ao_segsize   = 5

c------------------------------------------------------------------------------------------  
c                  Read vaules for these parameters from ZMAT   
c------------------------------------------------------------------------------------------  

      call igetparam('MASTER_IS_WORKER',     1, mstr_is_wrkr)
      call igetparam('SIP_MX_SEGSIZE',       1, sip_mx_ao_segsize)
      call igetparam('SIP_MX_OCC_SEGSIZE',   1, sip_mx_occ_segsize)
      call igetparam('SIP_MX_VIRT_SEGSIZE',  1, sip_mx_virt_segsize)
      call cgetparam('INTEGRAL_PACKAGE',     1, package)
      call cgetparam('MOLFILE',              1, molfile)
      call cgetparam('LOCAL_PATH',           1, local_path)
      call cgetparam('TRACE',                1, tracetype)
      call cgetparam('FAST_ERD_MEMCALC',     1, erd_fast_memcalc)
      call cgetparam('DEBUG_PRINT',          1, debug_print)
      call igetparam('NWORKTHREAD',          1, nworkthread)
      call cgetparam('AUTO_SEG_ALGORITHM',   1, auto_seg_algorithm)
      call cgetparam('ITRIP',                1, ctrip_str)
      call cgetparam('IHESS',                1, ihess_str)
      call cgetparam('JHESS',                1, jhess_str)
      call cgetparam('SUB',                  1, sub_str)
      call igetparam('SIP_SUB_SEGSIZE',      1, sip_sub_segsize)
      call igetparam('SIP_SUB_OCC_SEGSIZE',  1, sip_sub_occ_segsize)
      call igetparam('SIP_SUB_VIRT_SEGSIZE', 1, sip_sub_virt_segsize)
      call igetparam('SIP_SUB_AO_SEGSIZE',   1, sip_sub_ao_segsize)

c-------------------------------------------------------------------------
c   Strip any embedded nulls out of molfile name.
c-------------------------------------------------------------------------

      do i = 1, len(molfile)
         if (molfile(i:i) .eq. char(0)) molfile(i:i) = ' '
      enddo

c--------------------------------------------------------------------------
c   Read program-specific parameters from the config file. .
c 
c   1. NO_SERVERS            YES->All processors used as workers  
c   2. IGNORE_DROPMO         YES->Full basis used  
c   3. SECOND_DERIVATIVES    YES->Second derivative integrals used  
c   3. VVVI_STACK            YES->Stack of size vvvi(i=simple index) used  
c--------------------------------------------------------------------------

      do i = 1, nsial  
          sial_program = which_sial(i) 
          call get_sial_config_params(sial_program)
          write(6,*) 'SIAL PROGRAM:', sial_program 
          write(6,*) ' NO SERVERS:', no_servers 
          write(6,*) ' IGNORE DROPMO:', ignore_dropmo  
          write(6,*) 'use_2der:', use_2der_integrals  
          write(6,*) 'VVVI STACK:', vvvi_stack

          lformat = '(L1)'
          write (lstring, lformat) no_servers
          call set_config_info(trim(sial_program)//C_NULL_CHAR,
     *            "NO_SERVERS"//C_NULL_CHAR, trim(lstring)//C_NULL_CHAR)

          write (lstring, lformat) ignore_dropmo
          call set_config_info(trim(sial_program)//C_NULL_CHAR,
     *         "IGNORE_DROPMO"//C_NULL_CHAR, trim(lstring)//C_NULL_CHAR)
          write (lstring, lformat) use_2der_integrals
c           print *, lstring
          call set_config_info(trim(sial_program)//C_NULL_CHAR,
     *              "use_2der"//C_NULL_CHAR, trim(lstring)//C_NULL_CHAR)
          write (lstring, lformat) vvvi_stack
c           print *, lstring
          call set_config_info(trim(sial_program)//C_NULL_CHAR,
     *            "VVVI_STACK"//C_NULL_CHAR, trim(lstring)//C_NULL_CHAR)

c          do j = 1, 4
c             sial_cparams(4*(i-1) + j) = 0
c          enddo
c          if (no_servers)         sial_cparams(4*(i-1) + 1) = 1
c          if (ignore_dropmo)      sial_cparams(4*(i-1) + 2) = 1
c          if (use_2der_integrals) sial_cparams(4*(i-1) + 3) = 1
c          if (vvvi_stack)         sial_cparams(4*(i-1) + 4) = 1
      enddo 

c------------------------------------------------------------------------------------------  
c   Set sial_config_params in the parameter table  
c------------------------------------------------------------------------------------------  

c      norbs = 4*nsial
c      call set_config_info(norbs, sial_cparams) ! sial_config_params

c------------------------------------------------------------------------------------------  
c                  If OCC or VIRT max segsize = 0 find default  
c------------------------------------------------------------------------------------------  

      stack_algorithm_type = 2 ! Default so that extra blocks allocated base on usage 
      ignore_dropmo = .true.   ! Turns off dromo for now  
100   continue 
      if (ignore_dropmo) write(6,*) ' IGNORE DROPMO INITIALIZATION' 
      if (.not. ignore_dropmo) write(6,*) ' DROPMO INITIALIZATION' 

c   Initialize data: Read and store basis set information needed in the
c   computation of oed and erd integrels. 
      call init_data() 
      call joda_prep() 
      call int_gen_init

      if (sip_mx_occ_segsize  .lt. 1 .or.
     *    sip_mx_virt_segsize .lt. 1 .or.  
     *    sip_mx_ao_segsize   .lt. 1) then    

       saveocc  = sip_mx_occ_segsize
       savevirt = sip_mx_virt_segsize
       saveao   = sip_mx_ao_segsize

       auto_seg_switch = 2 ! VFL algorithm  
       call det_segsizes(auto_seg_switch, 1) ! The second argument is the number of procs which is unknown  
       if (saveocc  .gt. 0) sip_mx_occ_segsize  = saveocc
       if (savevirt .gt. 0) sip_mx_virt_segsize = savevirt
       if (saveao   .gt. 0) sip_mx_ao_segsize   = saveao
      end if

c------------------------------------------------------------------------------------------  
c                 Now you can build the segment tables.  
c------------------------------------------------------------------------------------------  

      if (.not. frag_calc) then  
      nfrags = 1 
      do i = 1, nshells 
         segs(i) = 0 
      enddo 
      write(6,*) ' Nalpha occ virt = ', nalpha_occupied, nalpha_virtual 
      call set_naocc_segs(nafocc, nalpha_occupied, sip_mx_occ_segsize) 
      call set_nbocc_segs(nbfocc, nbeta_occupied, sip_mx_occ_segsize) 
      call set_navirt_segs(nafvirt, nalpha_occupied, nalpha_virtual, 
     *                     sip_mx_virt_segsize) 
      call set_nbvirt_segs(nbfvirt, nbeta_occupied, nbeta_virtual, 
     *                     sip_mx_virt_segsize) 
      call shells_to_segments(sip_mx_ao_segsize, segs, nsegs, .false.)
      endif 


      if (frag_calc) then 
c---------------------------------------------------------------------------
c     If this is a fragment calculation set up the orbital ranges. 
c---------------------------------------------------------------------------

        call fragment_orbitals(i) 
        call fshells_to_segments(sip_mx_ao_segsize, segs, nsegs)


        call set_naocc_segs(nafocc, nalpha_occupied, sip_mx_occ_segsize) 
        call set_nbocc_segs(nbfocc, nbeta_occupied, sip_mx_occ_segsize) 
        call set_navirt_segs(nafvirt, nalpha_occupied, nalpha_virtual, 
     *                     sip_mx_virt_segsize) 
        call set_nbvirt_segs(nbfvirt, nbeta_occupied, nbeta_virtual, 
     *                     sip_mx_virt_segsize) 
        call set_frag_segs()  

      endif 

c------------------------------------------------------------------------------------------  
c                              Set the index segment ranges  
c------------------------------------------------------------------------------------------  

      norbs = 1 
      dd(norbs) = segs(1) 
      do i = 2, nsegs 
         norbs = norbs + 1 
         dd(norbs) = segs(i) - segs(i-1)  
      enddo 
c     do i = 1, norbs 
c        write(6,*) ' NORBS DD :', norbs, dd(i) 
c     enddo 
      call set_aoindex_info(norbs,dd)              ! Atomic orbital segments 
      sDim1(1) = norbs     
      do i = 1, sDim1(1)
         iVals1(i) = dd(i)  
         if (i .gt. 1) then 
            do j = 1, i-1 
               iVals1(i) = iVals1(i) + dd(j)
            enddo 
         endif 
      enddo
      call set_predefined_integer_array('ao_seg_ranges'//C_NULL_CHAR,1,
     *                                   sDim1, iVals1) 

      norbs = 0 
      do kfrag = 1, nfrags 
         do i = 1, n_aocc(kfrag) 
            norbs = norbs + 1 
            dd(norbs) = eaocc_frag(kfrag,i) - baocc_frag(kfrag,i) +1 
         enddo 
         do i = 1, n_avirt(kfrag) 
            norbs = norbs + 1 
            dd(norbs) = eavirt_frag(kfrag,i) - bavirt_frag(kfrag,i)+1  
         enddo 
      enddo 

c     do i = 1, norbs 
c        write(6,*) ' NORBS DD :', norbs, dd(i) 
c     enddo 

      call set_moindex_info(norbs,dd)              ! MO index ranges. BOTH occupied and virtual 
      sDim1(1) = norbs     
      do i = 1, sDim1(1)
         iVals1(i) = dd(i)
      enddo
      call set_predefined_integer_array('mo_seg_ranges'//C_NULL_CHAR,1,
     *                                   sDim1, iVals1) 

      call set_moaindex_info(norbs,dd)             ! MO alpha index ranges. BOTH occupied and virtual 
      sDim1(1) = norbs     
      do i = 1, sDim1(1)
         iVals1(i) = dd(i)
      enddo
      call set_predefined_integer_array('moa_seg_ranges'//C_NULL_CHAR,1,
     *                                   sDim1, iVals1) 

      norbs = 0 
      do kfrag = 1, nfrags 
         do i = 1, n_bocc(kfrag) 
            norbs = norbs + 1 
            dd(norbs) = ebocc_frag(kfrag,i) - bbocc_frag(kfrag,i) +1 
         enddo 
         do i = 1, n_avirt(kfrag) 
            norbs = norbs + 1 
            dd(norbs) = ebvirt_frag(kfrag,i) - bbvirt_frag(kfrag,i)+1  
         enddo 
      enddo 

c     do i = 1, norbs 
c        write(6,*) ' NORBS DD :', norbs, dd(i) 
c     enddo 

      call set_mobindex_info(norbs,dd)              ! MO beta index ranges. BOTH occupied and virtual 
      sDim1(1) = norbs     
      do i = 1, sDim1(1)
         iVals1(i) = dd(i)
      enddo
      call set_predefined_integer_array('mob_seg_ranges'//C_NULL_CHAR,1,
     *                                   sDim1, iVals1) 

      write(6,*) ' Done setting segment ranges ' 

c------------------------------------------------------------------------------------------  
c                              End Setting the index segment ranges  
c------------------------------------------------------------------------------------------  

c------------------------------------------------------------------------------------------  
c                              Set the integer constants 
c------------------------------------------------------------------------------------------  
  
      val0 = nsegs 
      call set_constant('norb'//C_NULL_CHAR, val0)     ! Number of atomic orbital segments 
      val0 = 1 ! start segments at 1 
      call set_constant('bocc'//C_NULL_CHAR, val0)     ! Beginning occupied segment  
      val0 = nafocc 
      call set_constant('eocc'//C_NULL_CHAR, val0)     ! Ending occupied  segment 
      val0 = 1 ! start segments at 1 
      call set_constant('baocc'//C_NULL_CHAR, val0)    ! Beginning alpha occupied segment  
      val0 = nafocc  
      call set_constant('eaocc'//C_NULL_CHAR, val0)    ! Ending alpha occupied  segment 
      val0 = 1 ! start segments at 1 
      call set_constant('bbocc'//C_NULL_CHAR, val0)    ! Beginning beta occupied segment  
      val0 = nafocc  
      call set_constant('ebocc'//C_NULL_CHAR, val0)    ! Ending beta occupied  segment 
      val0 = nafocc + 1 
      call set_constant('bavirt'//C_NULL_CHAR, val0)   ! Beginning alpha virtual segment  
      val0 = nafocc + nafvirt  
      call set_constant('eavirt'//C_NULL_CHAR, val0)   ! Ending alpha virtual segment  
      val0 = nbfocc + 1 
      call set_constant('bbvirt'//C_NULL_CHAR, val0)   ! Beginning beta virtual segment  
      val0 = nbfocc + nbfvirt  
      call set_constant('ebvirt'//C_NULL_CHAR, val0)   ! Ending beta virtual segment  


      val0 = nalpha_occupied + 1  
      call set_constant('bavirtorb'//C_NULL_CHAR, val0)  ! Beginning alpha virtual orbital   
      val0 = nalpha_occupied + nalpha_virtual   
      call set_constant('eavirtorb'//C_NULL_CHAR, val0)  ! Ending alpha virtual orbital   
      val0 = nbeta_occupied + 1  
      call set_constant('bbvirtorb'//C_NULL_CHAR, val0)  ! Beginning beta virtual orbital   
      val0 = nbeta_occupied + nbeta_virtual   
      call set_constant('ebvirtorb'//C_NULL_CHAR, val0)  ! Ending beta virtual orbital   
      val0 = nalpha_occupied 
      call set_constant('naocc'//C_NULL_CHAR, val0)      ! Number of alpha occupied orbital   
      val0 = nbeta_occupied 
      call set_constant('nbocc'//C_NULL_CHAR, val0)      ! Number of beta occupied orbital   

      val0 = scf_iter 
      call set_constant('scf_iter'//C_NULL_CHAR, val0)   ! Maximum number of scf iterations 
      val0 = cc_iter 
      call set_constant('cc_iter'//C_NULL_CHAR, val0)    ! Maximum number of cc/lambda iterations 
      val0 = scf_beg  
      call set_constant('scf_beg'//C_NULL_CHAR, val0)    ! Beginning iteration of SCF to employ DIIS  
      val0 = scf_hist  
      call set_constant('scf_hist'//C_NULL_CHAR, val0)   ! Number of histories to use in the SCF DIIS  
      call set_constant('fromvar'//C_NULL_CHAR, nd)      ! ????  

      write(6,*) ' Done setting integer constants ' 

c     if (ignore_dropmo) then 
c         ignore_dropmo = .false. 
c         go to 100  
c     endif 

c------------------------------------------------------------------------------------------  
c                              Set the predefined constants 
c------------------------------------------------------------------------------------------  
  
      dval = 33.456
      call set_scalar('scalar1'//C_NULL_CHAR, dval)
      dval = 0.0 
      call set_scalar('scfeneg'//C_NULL_CHAR, dval)     ! The scf energy 
      dval = 0.0 
      call set_scalar('totenerg'//C_NULL_CHAR, dval)   ! The total energy 
      dval = damp_init  
      call set_scalar('damp'//C_NULL_CHAR, dval)       ! The Davidson damping parameter used in the SCF  
      dval = scf_conv   
      call set_scalar('scf_conv'//C_NULL_CHAR, dval)   ! The SCF convergence threshold   
      dval = cc_conv   
      call set_scalar('cc_conv'//C_NULL_CHAR, dval)    ! The CC/lambda convergence threshold   
      write(6,*) ' The NN repulsion is :', scf_energy 
      call set_scalar('nn_repulsion'//C_NULL_CHAR, scf_energy) !  Nuclear-Nuclear repulsion energy 

      write(6,*) ' Done setting predefined constants constants ' 

c------------------------------------------------------------------------------------------  
c                              Set the 'static' predefined arrays  
c------------------------------------------------------------------------------------------  


      sDim1(1) = segs(nsegs)  
      do i = 1, sDim1(1) 
         sVals1(i) = 0.0 
      enddo 
      call set_predefined_scalar_array('e'//C_NULL_CHAR, 1,Sdim1,sVals1)
      call set_predefined_scalar_array('ea'//C_NULL_CHAR,1,Sdim1,sVals1)
      call set_predefined_scalar_array('eb'//C_NULL_CHAR,1,Sdim1,sVals1)

      sDim2(1) = segs(nsegs)  
      sDim2(2) = nalpha_occupied + nalpha_virtual  
      do i = 1, sDim1(1) 
      do j = 1, sDim2(2) 
         sVals(i,j) = 0.0 
      enddo 
      enddo 
      call set_predefined_scalar_array('c'//C_NULL_CHAR, 2,sDim2,sVals)
      call set_predefined_scalar_array('ca'//C_NULL_CHAR,2,sDim2,sVals)

      sDim2(1) = segs(nsegs)  
      sDim2(2) = nbeta_occupied + nbeta_virtual  
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         sVals(i,j) = 0.0 
      enddo 
      enddo 
      call set_predefined_scalar_array('cb'//C_NULL_CHAR,2,sDim2,sVals)


      sDim2(1) = nalpha_occupied + nalpha_virtual  
      sDim2(2) = nalpha_occupied + nalpha_virtual  
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         sVals(i,j) = 0.0 
      enddo 
      enddo 
      call set_predefined_scalar_array('fock_a'//C_NULL_CHAR, 
     *                                                  2, sDim2, sVals)


      sDim2(1) = nbeta_occupied + nbeta_virtual  
      sDim2(2) = nbeta_occupied + nbeta_virtual  
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         sVals(i,j) = 0.0 
      enddo 
      enddo 
      call set_predefined_scalar_array('fock_b'//C_NULL_CHAR, 
     *                                                   2, sDim2,sVals)


      sDim2(1) = segs(nsegs)  
      sDim2(2) = segs(nsegs)  
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         sVals(i,j) = 0.0 
      enddo 
      enddo 
      call set_predefined_scalar_array('oed_nai'//C_NULL_CHAR,2,
     *                                                   sDim2,sVals)
      call set_predefined_scalar_array('oed_kin'//C_NULL_CHAR,2,
     *                                                    sDim2,sVals)
      call set_predefined_scalar_array('oed_ovl'//C_NULL_CHAR,2,
     *                                                    sDim2,sVals)

      val0 = ntot_alpha
      call set_constant('nalphas'//C_NULL_CHAR, val0)  ! Number of coefficients in basis

      val0 = ntot_pcoef
      call set_constant('npcoeffs'//C_NULL_CHAR, val0)  ! Number of exponents in basis

      val0 = ntot_shells
      call set_constant('ntotshells'//C_NULL_CHAR, val0)  ! Number of shells in basis

      sDim1(1) = ntot_alpha
      write(6,*) ' ALPHAS into SVALS ' 
      do i = 1, sDim1(1)
         sVals1(i) = talpha(i)
         write(6,*) i, talpha(i), sVals1(i) 
      enddo
      call set_predefined_scalar_array('alphas'//C_NULL_CHAR,
     *                                          1,sDim1,sVals1)

      sDim1(1) = ntot_pcoef
      do i = 1, sDim1(1)
         sVals1(i) = tpcoeff(i)
      enddo
      call set_predefined_scalar_array('pcoeffs'//C_NULL_CHAR,
     *                                           1,sDim1,sVals1)

      sDim1(1) = ntot_shells
      do i = 1, sDim1(1)
         iVals1(i) = tixalpha(i)
      enddo
      call set_predefined_integer_array('ixalphas'//C_NULL_CHAR,
     *                                          1, sDim1, iVals1)

      sDim1(1) = ntot_shells
      do i = 1, sDim1(1)
         iVals1(i) = tixpcoeff(i)
      enddo
      call set_predefined_integer_array('ixpcoeffs'//C_NULL_CHAR,1,
     *                                                  sDim1, iVals1)

c Array containg atomic coordinates 

      sDim2(1) = 3 
      sDim2(2) = ncenters  
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         cVals(i,j) = acenter(j,i) 
      enddo 
      enddo 
      write(6,*) ' COORDS:' 
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         write(6,*) '  ', i, j, cVals(i,j) 
      enddo 
      enddo 
      call set_predefined_scalar_array('coords'//C_NULL_CHAR,
     *                                           2,sDim2,cVals)

      sDim2(1) = 3
      sDim2(2) = nshells  
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         cVals(i,j) = tcoord(i,j) 
      enddo 
      enddo 
      write(6,*) ' TCOORDS:' 
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         write(6,*) '  ', i, j, cVals(i,j) 
      enddo 
      enddo 
      call set_predefined_scalar_array('tcoords'//C_NULL_CHAR,
     *                                              2,sDim2,cVals)


c Form the arrays ccbeg and ccend and indx_cc  

      call setup_ccbeg(talpha, tixalpha, tpcoeff, tixpcoeff,
     *                       ncfps, npfps, nshells, ccbeg, ccend,
     *                       indx_cc, icc) 
      write(6,*) ' CC_BEG '
      sDim1(1) = icc  
      do i = 1, sDim1(1)
         iVals1(i) = ccbeg(i)
         write(6,*) i, iVals1(i) 
      enddo
      call set_predefined_integer_array('ccbeg'//C_NULL_CHAR,1,sDim1,
     *                                   iVals1)
      write(6,*) ' CC_END '
      do i = 1, sDim1(1)
         iVals1(i) = ccend(i)
         write(6,*) i, iVals1(i) 
      enddo
      call set_predefined_integer_array('ccend'//C_NULL_CHAR,1,sDim1,
     *                                   iVals1)
      write(6,*) ' INDX_CC '
      sDim1(1) = nshells   
      do i = 1, sDim1(1)
         iVals1(i) = indx_cc(i)
         write(6,*) i, iVals1(i) 
      enddo
      call set_predefined_integer_array('indx_cc'//C_NULL_CHAR,1,sDim1,
     *                                   iVals1)

c Form the arrays end_nfps, ncfps, npfps, ivangmom  

      sDim1(1) = nshells   
      do i = 1, sDim1(1)
         iVals1(i) = end_nfps(i)
      enddo
      call set_predefined_integer_array('end_nfps'//C_NULL_CHAR,1,sDim1,
     *                                   iVals1)
      do i = 1, sDim1(1)
         iVals1(i) = ncfps(i)
      enddo
      call set_predefined_integer_array('ncfps'//C_NULL_CHAR,1,sDim1, 
     *                                   iVals1)
      do i = 1, sDim1(1)
         iVals1(i) = npfps(i)
      enddo
      call set_predefined_integer_array('npfps'//C_NULL_CHAR,1,sDim1, 
     *                                   iVals1)
      do i = 1, sDim1(1)
         iVals1(i) = ivangmom(i)
      enddo
      call set_predefined_integer_array('ivangmom'//C_NULL_CHAR,1,sDim1,
     *                                   iVals1)

c Set end_nfps, ncfps, npfps  

c Set the number of occupied and virtual orbitals 

      call set_constant('nalpha_occupied'//C_NULL_CHAR,nalpha_occupied) 
      call set_constant('nbeta_occupied'//C_NULL_CHAR,nbeta_occupied) 
      call set_constant('nalpha_virtual'//C_NULL_CHAR,nalpha_virtual) 
      call set_constant('nbeta_virtual'//C_NULL_CHAR,nbeta_virtual) 

c Set the number of shells  
      call set_constant('nshells'//C_NULL_CHAR,nshells) 

c Set the spherical flag. This is a logical which has been converted to
c integer 
c     true --> 1 
c     false --> 0 
c     if (ispherical) intSpherical = 1 
c     if (.not. ispherical) intSpherical = 0 
      if (ispherical.eq.1) intSpherical = 1 
      if (ispherical.ne.1) intSpherical = 0 
      call set_constant('intSpherical'//C_NULL_CHAR,intSpherical) 
      call set_constant('ncenters'//C_NULL_CHAR,ncenters) 

      sDim1(1) = ncenters  
      write(6,*) ' Number of charges = ', sDim1(1) 
      do i = 1, sDim1(1)
         sVals1(i) = charge(i)
         write(6,*) i, sVals1(i) 
      enddo
      call set_predefined_scalar_array('charge'//C_NULL_CHAR,
     *                                              1,sDim1,sVals1)

c Set the maximum number of integral Shells  
      call set_constant('max_shells'//C_NULL_CHAR,max_shells) 

      write(6,*) ' Done setting predefined arrays ' 

c     call set_predefined_scalar_array('fockrohf_a'//C_NULL_CHAR, 2,
c    *                            fockrohf_a)  
c     call set_predefined_scalar_array('fockrohf_b'//C_NULL_CHAR, 2,
c    *                            fockrohf_b)  

c *************************************************************************
c Set the parameters defining the fragment calculation. 
c *************************************************************************
      call set_constant('nfrags'//C_NULL_CHAR,nfrags) 

      sDim1(1) = nfrags  
      call set_predefined_integer_array('natoms_frag'//C_NULL_CHAR,1,
     *                                  sDim1, natoms_frag)
      sDim2(1) = nfrags  
      sDim2(2) = 1  
      do i = 1, nfrags 
         if (natoms_frag(i) .gt. sDim2(2)) sDim2(2) = natoms_frag(i) 
      enddo   
      call set_predefined_integer_array('watom_frag'//C_NULL_CHAR,2,
     *                                  sDim2, watom_frag)
      call set_predefined_integer_array('fragAO'//C_NULL_CHAR,1,
     *                                  sDim1, fragAO)
      call set_predefined_integer_array('n_occ'//C_NULL_CHAR,1,
     *                                  sDim1, n_occ)
      call set_predefined_integer_array('n_aocc'//C_NULL_CHAR,1,
     *                                  sDim1, n_aocc)
      call set_predefined_integer_array('n_bocc'//C_NULL_CHAR,1,
     *                                  sDim1, n_bocc)
      call set_predefined_integer_array('n_virt'//C_NULL_CHAR,1,
     *                                  sDim1, n_virt)
      call set_predefined_integer_array('n_avirt'//C_NULL_CHAR,1,
     *                                  sDim1, n_avirt)
      call set_predefined_integer_array('n_bvirt'//C_NULL_CHAR,1,
     *                                  sDim1, n_bvirt)

      call set_predefined_integer_array('bocc_frag'//C_NULL_CHAR,2,
     *                                  sDim2, bocc_frag)
      call set_predefined_integer_array('baocc_frag'//C_NULL_CHAR,2,
     *                                  sDim2, baocc_frag)
      call set_predefined_integer_array('bbocc_frag'//C_NULL_CHAR,2,
     *                                  sDim2, bbocc_frag)
      call set_predefined_integer_array('eocc_frag'//C_NULL_CHAR,2,
     *                                  sDim2, eocc_frag)
      call set_predefined_integer_array('eaocc_frag'//C_NULL_CHAR,2,
     *                                  sDim2, eaocc_frag)
      call set_predefined_integer_array('ebocc_frag'//C_NULL_CHAR,2,
     *                                  sDim2, ebocc_frag)
      call set_predefined_integer_array('bvirt_frag'//C_NULL_CHAR,2,
     *                                  sDim2, bvirt_frag)
      call set_predefined_integer_array('bavirt_frag'//C_NULL_CHAR,2,
     *                                  sDim2, bavirt_frag)
      call set_predefined_integer_array('bbvirt_frag'//C_NULL_CHAR,2,
     *                                  sDim2, bbvirt_frag)
      call set_predefined_integer_array('evirt_frag'//C_NULL_CHAR,2,
     *                                  sDim2, evirt_frag)
      call set_predefined_integer_array('eavirt_frag'//C_NULL_CHAR,2,
     *                                  sDim2, eavirt_frag)
      call set_predefined_integer_array('ebvirt_frag'//C_NULL_CHAR,2,
     *                                  sDim2, ebvirt_frag)

      write(6,*) ' Done first set of fragment predefined arrays ' 

       n_a_occ = 0 
       n_b_occ = 0 
       n_a_virt = 0 
       n_b_virt = 0 
       do i = 1, nfrags 
          n_a_occ = n_a_occ + n_aocc(i) 
          n_b_occ = n_b_occ + n_bocc(i) 
          n_a_virt = n_a_virt + n_avirt(i) 
          n_b_virt = n_b_virt + n_bvirt(i) 
       enddo 

      sDim1(1) = nsegs     
      write(6,*) ' AO_SEGS = ', sDim1(1)  
      call set_predefined_integer_array('w_frag_ao'//C_NULL_CHAR,1,
     *                                  sDim1, w_frag_AO)

      sDim1(1) = n_a_occ   
      write(6,*) ' moocc_SEGS = ', sDim1(1)  
      call set_predefined_integer_array('w_frag_occ'//C_NULL_CHAR,1,
     *                                  sDim1, w_frag_occ)

      sDim1(1) = n_a_occ     
      write(6,*) ' naocc_SEGS = ', sDim1(1)  
      call set_predefined_integer_array('w_frag_aocc'//C_NULL_CHAR,1,
     *                                  sDim1, w_frag_aocc)

      sDim1(1) = n_b_occ     
      write(6,*) ' nbocc_SEGS = ', sDim1(1)  
      call set_predefined_integer_array('w_frag_bocc'//C_NULL_CHAR,1,
     *                                  sDim1, w_frag_bocc)

      sDim1(1) = n_a_virt     
      write(6,*) ' nvirt_SEGS = ', sDim1(1)  
      call set_predefined_integer_array('w_frag_virt'//C_NULL_CHAR,1,

     *                                  sDim1, w_frag_virt)
      sDim1(1) = n_a_virt  
      write(6,*) ' navirt_SEGS = ', sDim1(1)  

      call set_predefined_integer_array('w_frag_avirt'//C_NULL_CHAR,1,
     *                                  sDim1, w_frag_avirt)

      sDim1(1) = n_b_virt     
      write(6,*) ' nbvirt_SEGS = ', sDim1(1)  
      call set_predefined_integer_array('w_frag_bvirt'//C_NULL_CHAR,1,
     *                                  sDim1, w_frag_bvirt)

      write(6,*) ' Done second set of fragment predefined arrays ' 

c *************************************************************************
c Done Setting the parameters defining the fragment calculation. 
c *************************************************************************
c 

      call finalize_setup()
      call dump_file(datname)

      return
      end subroutine
c      end program setup_main

