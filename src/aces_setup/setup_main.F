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
      include 'ecp_info.h'
      include 'mxatms.par'
      include 'dropmo.h'

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
      integer (C_INT) niorbs 
      integer (C_INT) nsegs 
      integer (C_INT) c_nshells  

c     integer (C_INT) maxmem 
      integer (C_INT) nsial 
      integer (C_INT) val0
      integer (C_INT) junk
c     integer (C_INT) nd
c     integer (C_INT) d(6)
c     integer (C_INT) d2(2)
      integer (C_INT) dd(max_segments)
      integer (C_INT) sDim2(2), sDim1(1), sDims(8)
c     real (C_DOUBLE) sVals(mxcbf,mxcbf)
      real (C_DOUBLE) cVals(3,mxatms)
      real (C_DOUBLE) sVals1(mxcbf)
      integer (C_INT) iVals1(mxcbf)
      integer (C_INT) Three_natoms 

C C_DOUBLE arrays to handle ECP real variable arrays. 
C
      real (C_DOUBLE) Cdbl_Clp(Mxecpprim)
      real (C_DOUBLE) Cdbl_Zlp(Mxecpprim)

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
C
C C_INT arrays to handle ECP integer variable arrays. 
C
      integer (C_INT) Cint_nlp(Mxecpprim) 
      integer (C_INT) Cint_llmax(Mxatms) 
      integer (C_INT) Cint_nelecp(mxatms)
      integer (C_INT) Cint_ipseux(mxatms)
      integer (C_INT) Cint_klast(Maxang*mxatms)
      integer (C_INT) Cint_kfirst(Maxang*mxatms)

      integer (C_INT) Cint_iqmstr(mxatms)
      integer (C_INT) Cint_jcostr(mxatms*maxjco)
      integer (C_INT) Cint_nucstr(mxatms*max_shells*maxjco)
      integer (C_INT) Cint_nrcstr(mxatms*max_shells*maxjco)
      integer (C_INT) Cint_jstrt(max_shells)
      integer (C_INT) Cint_jrs(max_shells)

C Integer scalar used in ECP (perhaps unused but lets fill it)

      integer (C_INT) Cint_ipseud(1)
      integer (C_INT) Cint_max_ecp_angmom(1)
      integer (C_INT) Cint_max_ecp_prims(1)
      integer (C_INT) Cint_kfirst_last_len(1)
      integer (C_INT) Cint_necp_atoms(1)
      integer (C_INT) Cint_natoms(1)

      integer (C_INT) Cint_to_elements(1)
      integer (C_INT) Cint_jcostr_length(1)    
      integer (C_INT) Cint_nr_nu_str_length(1) 
      integer (C_INT) Cint_nshells(1) 
C
      integer max_ecp_angmom, max_num_ecp_prims, num_ecp_prims
      integer necp_atoms, tot_elements_1, tot_elements_2
      integer kfirst_last_length

      character*5 lformat
      character*5 lstring

      integer i, j, k, kfrag, jcount, kcount 
      integer icc 
      integer segs(maxsegs)   
      integer saveocc 
      integer savevirt  
      integer saveao 
      integer auto_seg_switch 
      integer m_segs 
      integer n_a_occ, n_b_occ, n_a_virt, n_b_virt 
      integer nTot, nover  

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

c This is kindof kludgy but we initialize som parameters here
      ndropmo_a = 0 
      ndropmo_b = 0 

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
100   continue 
C      if (ignore_dropmo) write(6,*) ' IGNORE DROPMO INITIALIZATION' 
C      if (.not. ignore_dropmo) write(6,*) ' DROPMO INITIALIZATION' 

c   Initialize data: Read and store basis set information needed in the
c   computation of oed and erd integrels. 
      ignore_dropmo = .true.   ! Turns off dromo for now  
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
         baocc_drop = 1
         bbocc_drop = 1
      write(6,*) ' Nalpha occ virt = ', nalpha_occupied, nalpha_virtual 

c     call set_naocc_segs(nafocc, nalpha_occupied, sip_mx_occ_segsize) 
c     call set_nbocc_segs(nbfocc, nbeta_occupied, sip_mx_occ_segsize) 
c     call set_navirt_segs(nafvirt, nalpha_occupied, nalpha_virtual, 
c    *                        sip_mx_virt_segsize) 
c     call set_nbvirt_segs(nbfvirt, nbeta_occupied, nbeta_virtual, 
c    *                        sip_mx_virt_segsize) 

c VFL test on 4/12/2018 
      nafocc = nalpha_occupied / sip_mx_occ_segsize 
      n_aocc(1) = nafocc  
      nbfocc = nbeta_occupied / sip_mx_occ_segsize 
      n_bocc(1) = nbfocc  
      nafvirt = nalpha_virtual / sip_mx_virt_segsize 
      n_avirt(1) = nafvirt   
      nbfvirt = nbeta_virtual / sip_mx_virt_segsize 
      n_bvirt(1) = nbfvirt   

      call shells_to_segments(sip_mx_ao_segsize, segs, nsegs, .false.)
         nfrags = 1 
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

c Determine the alpha moindex ranges 
      nafocc = (nalpha_occupied-ndropmo_a) / sip_mx_occ_segsize 
      norbs = 0 
      do kfrag = 1, nfrags 
         if (ndropmo_a .eq. 0) then 
            dd(1) = nalpha_occupied/ nafocc 
         else 
            dd(1) = ndropmo_a 
            baocc_drop = 2
         endif 
         nTot = dd(1)  
         norbs = 1 
         do i = 2, nafocc - 1    
            norbs = norbs + 1 
            dd(norbs) = (nalpha_occupied-ndropmo_a) / nafocc  
            nTot = nTot + dd(norbs) 
         enddo 
         norbs = norbs + 1 
         dd(norbs) = nalpha_occupied - nTot 
      enddo 
      niorbs = norbs 

      nafvirt = nalpha_virtual / sip_mx_virt_segsize 
      do kfrag = 1, nfrags 
         norbs = norbs + 1 
         dd(norbs) = nalpha_virtual / nafvirt  
         nTot = dd(norbs)  
         do i = 2, n_avirt(kfrag)-1  
            norbs = norbs + 1 
            dd(norbs) = nalpha_virtual / nafvirt   
            nTot = nTot + dd(norbs) 
         enddo 
         norbs = norbs + 1 
         dd(norbs) = nalpha_virtual - nTot 
c check if last seg is too large 
         nover = dd(norbs) - nalpha_virtual / nafvirt 
         if (nover .le. dd(norbs) - 1) then  
            do i = 1, nover 
               dd(niorbs+i) = dd(niorbs+i) + 1 
            enddo 
            dd(norbs) = dd(norbs) - nover  
         endif 
      enddo 

      do i = 1, norbs 
         write(6,*) ' NORBS DD alpha :', norbs, dd(i), ndropmo_a  
      enddo 

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
#ifdef _DEBUG_LVL0
         write(6,"(a,2I4)") ' moarange:', i, dd(i) 
#endif 
      enddo
      call set_predefined_integer_array('moa_seg_ranges'//C_NULL_CHAR,1,
     *                                   sDim1, iVals1) 

c Determine the beta moindex ranges 
      nbfocc = (nbeta_occupied-ndropmo_b) / sip_mx_occ_segsize 
      norbs = 0 
      do kfrag = 1, nfrags 
         if (ndropmo_b .eq. 0) then 
            dd(1) = nbeta_occupied/ nbfocc 
         else 
            dd(1) = ndropmo_b 
            bbocc_drop = 2
         endif 
         nTot = dd(1)  
         norbs = 1 
         do i = 2, nbfocc - 1   
            norbs = norbs + 1 
            dd(norbs) = (nbeta_occupied-ndropmo_b) / nbfocc  
            nTot = nTot + dd(norbs) 
         enddo 
         norbs = norbs + 1 
         dd(norbs) = nbeta_occupied - nTot 
      enddo 
      niorbs = norbs 

      nbfvirt = nbeta_virtual / sip_mx_virt_segsize 
      do kfrag = 1, nfrags 
         norbs = norbs + 1 
         dd(norbs) = nbeta_virtual / nbfvirt  
         nTot = dd(norbs)  
         do i = 2, n_bvirt(kfrag)-1  
            norbs = norbs + 1 
            dd(norbs) = nbeta_virtual / nbfvirt   
            nTot = nTot + dd(norbs) 
         enddo 
         norbs = norbs + 1 
         dd(norbs) = nbeta_virtual - nTot 
c check if last seg is too large 
         nover = dd(norbs) - nbeta_virtual / nbfvirt 
         if (nover .le. dd(norbs) - 1) then  
            do i = 1, nover 
               dd(niorbs+i) = dd(niorbs+i) + 1 
            enddo 
            dd(norbs) = dd(norbs) - nover  
         endif 
      enddo 

      do i = 1, norbs 
         write(6,*) ' NORBS DD beta :', norbs, dd(i), ndropmo_b  
      enddo 

      call set_mobindex_info(norbs,dd)              ! MO beta index ranges. BOTH occupied and virtual 
      sDim1(1) = norbs     
      do i = 1, sDim1(1)
         iVals1(i) = dd(i)
#ifdef _DEBUG_LVL0
         write(6,"(a,2I4)") ' mobarange:', i, dd(i) 
#endif 
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

      Write(*,*)
      write(6,*) ' NORB    =', nsegs  
      write(6,*) ' NAOOCC  =', nafocc  
      write(6,*) ' NBOOCC  =', nbfocc  
      write(6,*) ' NAFVIRT =', nafvirt    
      write(6,*) ' NBFVIRT =', nbfvirt    
  
      val0 = nsegs 
      call set_constant('norb'//C_NULL_CHAR, val0)     ! Number of atomic orbital segments 
      val0 = 1 ! start segments at 1 
      call set_constant('bocc'//C_NULL_CHAR, val0)     ! Beginning occupied segment  
      val0 = nafocc 
      call set_constant('eocc'//C_NULL_CHAR, val0)     ! Ending occupied  segment 
      val0 = baocc_drop ! start segments at 1 
      call set_constant('baocc'//C_NULL_CHAR, val0)    ! Beginning alpha occupied segment  
      val0 = nafocc  
      call set_constant('eaocc'//C_NULL_CHAR, val0)    ! Ending alpha occupied  segment 
      val0 = bbocc_drop ! start segments at 1 
      call set_constant('bbocc'//C_NULL_CHAR, val0)    ! Beginning beta occupied segment  
      val0 = nbfocc  
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

      call set_constant('lindep_tol'//C_NULL_CHAR, lindep_tol)
      call set_constant('intgrl_tol'//C_NULL_CHAR, intgrl_tol)

      dval = eom_tol
      call set_scalar('eom_tol'//C_NULL_CHAR, dval)     ! eom convergence threshold
      val0 = eom_roots
      call set_constant('eom_roots'//C_NULL_CHAR, val0)   ! number of EOM roots
      val0 = eom_roots*3
      call set_constant('eom_subspc'//C_NULL_CHAR, val0)  ! size of davidson subspace collapse 

C
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
c     call set_predefined_scalar_array('e'//C_NULL_CHAR,  1, Sdim1, sVals1)
c     call set_predefined_scalar_array('ea'//C_NULL_CHAR, 1, Sdim1, sVals1)
c     call set_predefined_scalar_array('eb'//C_NULL_CHAR, 1, Sdim1, sVals1)

c     sDim2(1) = segs(nsegs)  
c     sDim2(2) = nalpha_occupied + nalpha_virtual  
c     do i = 1, sDim1(1) 
c     do j = 1, sDim2(2) 
c        sVals(i,j) = 0.0 
c     enddo 
c     enddo 
c     call set_predefined_scalar_array('c'//C_NULL_CHAR,  2, sDim2, sVals)
c     call set_predefined_scalar_array('ca'//C_NULL_CHAR, 2, sDim2, sVals)

c     sDim2(1) = segs(nsegs)  
c     sDim2(2) = nbeta_occupied + nbeta_virtual  
c     do i = 1, sDim2(1) 
c     do j = 1, sDim2(2) 
c        sVals(i,j) = 0.0 
c     enddo 
c     enddo 
c     call set_predefined_scalar_array('cb'//C_NULL_CHAR, 2, sDim2, sVals)


c     sDim2(1) = nalpha_occupied + nalpha_virtual  
c     sDim2(2) = nalpha_occupied + nalpha_virtual  
c     do i = 1, sDim2(1) 
c     do j = 1, sDim2(2) 
c        sVals(i,j) = 0.0 
c     enddo 
c     enddo 
c     call set_predefined_scalar_array('fock_a'//C_NULL_CHAR, 2, sDim2, sVals)


c     sDim2(1) = nbeta_occupied + nbeta_virtual  
c     sDim2(2) = nbeta_occupied + nbeta_virtual  
c     do i = 1, sDim2(1) 
c     do j = 1, sDim2(2) 
c        sVals(i,j) = 0.0 
c     enddo 
c     enddo 
c     call set_predefined_scalar_array('fock_b'//C_NULL_CHAR, 2, sDim2, sVals)


c     sDim2(1) = segs(nsegs)  
c     sDim2(2) = segs(nsegs)  
c     do i = 1, sDim2(1) 
c     do j = 1, sDim2(2) 
c        sVals(i,j) = 0.0 
c     enddo 
c     enddo 
c     call set_predefined_scalar_array('oed_nai'//C_NULL_CHAR, 2, sDim2, sVals)
c     call set_predefined_scalar_array('oed_kin'//C_NULL_CHAR, 2, sDim2, sVals)
c     call set_predefined_scalar_array('oed_ovl'//C_NULL_CHAR, 2, sDim2, sVals)

      val0 = ntot_alpha
      call set_constant('nalphas'//C_NULL_CHAR, val0)  ! Number of coefficients in basis

      val0 = ntot_pcoef
      call set_constant('npcoeffs'//C_NULL_CHAR, val0)  ! Number of exponents in basis

      val0 = ntot_shells
      call set_constant('ntotshells'//C_NULL_CHAR, val0)  ! Number of shells in basis

      sDim1(1) = ntot_alpha

#ifdef _DEBUG_LVL0
      write(6,"(a)") ' ALPHAS into SVALS ' 
#endif 
      do i = 1, sDim1(1)
         sVals1(i) = talpha(i)
#ifdef _DEBUG_LVL0
         write(6,"(I4,2F15.7)") i, talpha(i), sVals1(i) 
#endif
      enddo
      call set_predefined_scalar_array('alphas'//C_NULL_CHAR,
     *                                           1,sDim1,sVals1)

#ifdef _DEBUG_LVL0
      write(6,"(a)") ' ALPHA NORMS into SVALS ' 
#endif 
      do i = 1, sDim1(1)
         sVals1(i) = talpha_norm(i)
#ifdef _DEBUG_LVL0
         write(6,"(I4,2F15.7)") i, talpha_norm(i), sVals1(i) 
#endif
      enddo

      call set_predefined_scalar_array('alpha_norms'//C_NULL_CHAR,
     *                                           1,sDim1,sVals1)
C
      sDim1(1) = ntot_pcoef
      do i = 1, sDim1(1)
         sVals1(i) = tpcoeff(i)
      enddo
      call set_predefined_scalar_array('pcoeffs'//C_NULL_CHAR,
     *                                            1,sDim1,sVals1)

      sDim1(1) = ntot_shells
#ifdef _DEBUG_LVL0
      write(6,"(a)") ' IXALPHS ' 
#endif 
      do i = 1, sDim1(1)
         iVals1(i) = tixalpha(i)
#ifdef _DEBUG_LVL0
         write(6,"(I4,F15.7)") i, tixalpha(i) 
#endif 
      enddo
      call set_predefined_integer_array('ixalphas'//C_NULL_CHAR,
     *                                  1,sDim1, iVals1)

      sDim1(1) = ntot_shells
#ifdef _DEBUG_LVL0
      write(6,"(a)") ' IXCOEFFS ' 
#endif 
      do i = 1, sDim1(1)
         iVals1(i) = tixpcoeff(i)
#ifdef _DEBUG_LVL0
         write(6,"(I4,F15.7)") i, iVals1(i) 
#endif 
      enddo
      call set_predefined_integer_array('ixpcoeffs'//C_NULL_CHAR,1,
     *                                   sDim1, iVals1)

c Array containg atomic coordinates 

      sDim2(1) = 3 
      sDim2(2) = ncenters  
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         cVals(i,j) = acenter(j,i) 
      enddo 
      enddo 
#ifdef _DEBUG_LVL0
      write(6,"(a)") ' COORDS:' 
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         write(6,"(2I4,F15.7)") i, j, cVals(i,j) 
      enddo 
      enddo 
#endif 
      call set_predefined_scalar_array('coords'//C_NULL_CHAR,
     *                                                2,sDim2,cVals)

      sDim2(1) = 3
      sDim2(2) = nshells  
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         cVals(i,j) = tcoord(i,j) 
      enddo 
      enddo 

#ifdef _DEBUG_LVL0
      write(6,*) ' TCOORDS:' 
      do i = 1, sDim2(1) 
      do j = 1, sDim2(2) 
         write(6,"(2I4,2F15.7)") i, j, tcoord(i,j), cVals(i,j) 
      enddo 
      enddo 
#endif 
      call set_predefined_scalar_array('tcoords'//C_NULL_CHAR,
     *                                               2,sDim2,cVals)
c Form the arrays ccbeg and ccend and indx_cc  

      call setup_ccbeg(talpha, tixalpha, tpcoeff, tixpcoeff,
     *                       ncfps, npfps, nshells, ccbeg, ccend,
     *                       indx_cc, icc) 
#ifdef _DEBUG_LVL0
      write(6,"(a)") ' CC_BEG '
#endif 

      sDim1(1) = icc  

      call set_constant('n_c_funcs'//C_NULL_CHAR,icc)

      do i = 1, sDim1(1)
         iVals1(i) = ccbeg(i)
#ifdef _DEBUG_LVL0
         write(6,"(I4,F15.7)") i, iVals1(i) 
#endif 
      enddo
      call set_predefined_integer_array('ccbeg'//C_NULL_CHAR,1,sDim1,
     *                                   iVals1)
#ifdef _DEBUG_LVL0
      write(6,"(a)") ' CC_END '
#endif 
      do i = 1, sDim1(1)
         iVals1(i) = ccend(i)

#ifdef _DEBUG_LVL0
         write(6,"(I4,F15.7)") i, iVals1(i) 
#endif 
      enddo

      call set_predefined_integer_array('ccend'//C_NULL_CHAR,1,sDim1,
     *                                   iVals1)
#ifdef _DEBUG_LVL0
      write(6,"(a)") ' INDX_CC '
#endif 
      sDim1(1) = nshells   
      do i = 1, sDim1(1)
         iVals1(i) = indx_cc(i)
#ifdef _DEBUG_LVL0
         write(6,"(I4,F15.7)") i, iVals1(i) 
#endif 
      enddo
      call set_predefined_integer_array('indx_cc'//C_NULL_CHAR,1,sDim1,
     *                                   iVals1)
C
C Process the information need to evaluate the ECP integrals and write
C the to the .dat file inorder to retrive them in ACESIV SIP. 
C
C#ifdef _DEBUG_LVL0
         Write(*,*)
         Write(*,"(a)") "The ECPs on a atom?"
         write(6,"(10I4)") (ipseux(i), i=1, natoms)
         Write(*,"(a)") "The maximum angular momentum"
         write(6,"(10I4)") (llmax(i), i=1, natoms)
         Write(*,"(a)") "The number of core electrons replaced by ECP"
         write(6,"(10I4)") (nelecp(i), i=1, natoms)
         Write(*,"(a)") "The kfirst and klast arrays"
         write(6,"(10I4)") ((kfirst(i,j),i=1,Maxang),j=1,natoms)
         write(6,"(10I4)") ((klast(i,j), i=1,Maxang),j=1,natoms)
         Write(*,"(a)") "The nlp (R^n) Clp (coefs.) Zlp (exps) arrays"
         Write(*,*) "The number atoms", natoms

         do j=1, natoms 
         if (ipseux(j) .gt.0) then
         do k=1, llmax(j)+1
            write(6,"(10I4)")   (nlp(i), i=kfirst(k,j), klast(k,j)) 
            write(6,"(5F15.8)") (Clp(i), i=kfirst(k,j), klast(k,j)) 
            write(6,"(5F15.8)") (zlp(i), i=kfirst(k,j), klast(k,j)) 
            write(6,*)
         enddo
         endif 
         enddo
C#endif
     
C Lets find the maximum angular momentum 
    
      max_ecp_angmom = 0
      num_ecp_prims  = 0
      necp_atoms     = 0
      jcount         = 0
      kcount         = 0

      Do i=1, natoms 
         max_ecp_angmom = Max(max_ecp_angmom, llmax(i))
         
         Cint_ipseux(i) = ipseux(i)
         Cint_llmax(i)  = llmax(i)
         Cint_nelecp(i) = nelecp(i)

         if (ipseux(i) .gt. 0) then
            necp_atoms = necp_atoms + 1
            do k= 1, llmax(i)+1
               kcount = kcount + 1
               num_ecp_prims = num_ecp_prims + 
     *                         (klast(k,i) - kfirst(k,i)) + 1

               Cint_kfirst(kcount) = kfirst(k,i)
               Cint_klast(kcount)  = klast(k,i)

               do j = kfirst(k,i), klast(k,i)
                  jcount = jcount + 1 
                  Cint_nlp(jcount) = nlp(j)
                  Cdbl_Clp(jcount) = clp(j)
                  Cdbl_zlp(jcount) = zlp(j)
               enddo 

            enddo
         endif 
      Enddo 
     
      max_num_ecp_prims = num_ecp_prims 
      kfirst_last_length= necp_atoms * (max_ecp_angmom+1)

      Cint_ipseud(1)         = ipseud
      Cint_natoms(1)         = natoms 
      Cint_necp_atoms(1)     = necp_atoms
      Cint_maX_ecp_angmom(1) = Max_ecp_angmom
      Cint_max_ecp_prims(1)  = max_num_ecp_prims
      Cint_kfirst_last_len(1)= kfirst_last_length 

C#ifdef _DEBUG_LVL0      
      Write(6,"(a,I4)") " The number of atoms with an ECP: ", 
     *                    Cint_necp_atoms
      Write(6,"(a,I4)") " The maximum ECP ang. momentum: ", 
     *                    Cint_maX_ecp_angmom
      Write(6,"(a,I4)") " The maximum ECP primitives: ", 
     *                    Cint_maX_ecp_prims 
C#endif

      tot_elements_1 = 0
      tot_elements_2 = 0
      Do i=1, natoms 
         Cint_iqmstr(i) = iqmstr(i)

         do k=1, iqmstr(i)
            tot_elements_1 = tot_elements_1 + 1
            Cint_jcostr(tot_elements_1) = jcostr(i,k)
         enddo 

         do j= 1, iqmstr(i)

            do k=1, jcostr(i,j)
               tot_elements_2  = tot_elements_2 + 1
               Cint_nucstr(tot_elements_2) = nucstr(i,j,k)
               Cint_nrcstr(tot_elements_2) = nrcstr(i,j,k)
            enddo 

         enddo 
      enddo

      do j=1, Nshells
         Cint_jstrt(j) = jstrt(j) 
         Cint_jrs(j)   = jrs(j)
      enddo 
      
      Cint_jcostr_length(1)     = tot_elements_1
      Cint_nr_nu_str_length(1)  = tot_elements_2
      Cint_nshells(1)           = Nshells 
C
C#ifdef _DEBUG_LVL0      
      write(6,*) " The Cint_iqmstr", tot_elements_1, tot_elements_2
      write(6,"(10I4)") (Cint_iqmstr(i),i=1,natoms)
C#endif
C Dump all the ECP data to the .Dat file.
C
      call set_constant('ipseud'//C_NULL_CHAR,Cint_ipseud(1)) 

      call set_constant('necp_atoms'//C_NULL_CHAR,Cint_necp_atoms(1)) 

      call set_constant('max_ecp_angmom'//C_NULL_CHAR,
     *                                    Cint_maX_ecp_angmom(1))

      call set_constant('max_ecp_prims'//C_NULL_CHAR,
     *                                     Cint_maX_ecp_prims(1))

      call set_predefined_integer_array('ipseux'//C_NULL_CHAR,1, 
     *                                   Cint_natoms, Cint_ipseux)

      call set_predefined_integer_array('llmax'//C_NULL_CHAR,1, 
     *                                   Cint_natoms, Cint_llmax)

      call set_predefined_integer_array('nelecp'//C_NULL_CHAR,1,
     *                                   Cint_natoms, Cint_nelecp)

      call set_predefined_integer_array('kfirst'//C_NULL_CHAR,1,
     *                                   Cint_kfirst_last_len, 
     *                                   Cint_kfirst)

      call set_predefined_integer_array('klast'//C_NULL_CHAR,1,
     *                                  Cint_kfirst_last_len, 
     *                                  Cint_klast)

      call set_predefined_integer_array('nlp'//C_NULL_CHAR,1,
     *                                 Cint_max_ecp_prims, Cint_nlp)

      call set_predefined_integer_array('iqmstr'//C_NULL_CHAR,1,
     *                                 Cint_natoms, Cint_iqmstr)

      call set_predefined_integer_array('jstrt'//C_NULL_CHAR,1,
     *                                 Cint_nshells, Cint_jstrt)

      call set_predefined_integer_array('jrs'//C_NULL_CHAR,1,
     *                                 Cint_nshells, Cint_jrs)

      call set_predefined_integer_array('jcostr'//C_NULL_CHAR,1,
     *                                  Cint_jcostr_length, 
     *                                  Cint_jcostr)

      call set_predefined_integer_array('nucstr'//C_NULL_CHAR,1,
     *                                  Cint_nr_nu_str_length, 
     *                                  Cint_nucstr)

      call set_predefined_integer_array('nrcstr'//C_NULL_CHAR,1,
     *                                  Cint_nr_nu_str_length, 
     *                                  Cint_nrcstr)

      call set_predefined_scalar_array('clp'//C_NULL_CHAR,1,
     *                                 Cint_max_ecp_prims, Cdbl_clp)

      call set_predefined_scalar_array('zlp'//C_NULL_CHAR,1,
     *                                Cint_max_ecp_prims, Cdbl_zlp)

c Form the arrays end_nfps, ncfps, npfps, ivangmom, atom[map of atoms to shells] 

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
      do i = 1, sDim1(1)
         iVals1(i) = atom(i)
      enddo
      call set_predefined_integer_array('atom'//C_NULL_CHAR,1,sDim1,
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
#ifdef _DEBUG_LVL0
      write(6,"(a)") ' Number of charges = ', sDim1(1) 
#endif 
      do i = 1, sDim1(1)
         sVals1(i) = charge(i)
      enddo
#ifdef _DEBUG_LVL0
      write(6,"(10I4)") (sVals1(i), i=1, sDim1(1))
#endif 
      call set_predefined_scalar_array('charge'//C_NULL_CHAR,
     *                                           1,sDim1,sVals1)

c Set the maximum number of integral Shells  
      call set_constant('max_shells'//C_NULL_CHAR,max_shells) 

C Write the number of basis functions (read and computed from the
C MOL file. Notice that this does not account for drop-orbitals (which
C in principle should be accounted for by nalpha_occupied,
C nbeta_occupied and so on.
     
      call set_constant('nbasis_funcs'//C_NULL_CHAR,nbasis) 

C The new key-tokens that deals with SCF convegence. These are all 
C read from the input file and consistent with ACES II/III and now IV.
      Write(*,"(a,a,a,a)")"lock_orbocc,lock_orbitals,damp_tol,damp_end"
      Write(*,"(4i4)") lock_orbocc, lock_orbitals, damp_tol, damp_end
      Write(*,"(a,a)")"diis_start,diss_order,iecp_on"
      Write(*,"(3i4)") diis_start, diis_order,iecp_on
      Write(*,*) 
C#endif 
      Three_natoms = 3 * Natoms
      call set_constant('guess'//C_NULL_CHAR,guess) 
      call set_constant('lshft_a1'//C_NULL_CHAR,lshft_a1) 
      call set_constant('lshft_b1'//C_NULL_CHAR,lshft_b1) 
      call set_constant('damp_type'//C_NULL_CHAR,damp_type) 
      call set_constant('lock_orbocc'//C_NULL_CHAR,lock_orbocc) 
      call set_constant('lock_orbitals'//C_NULL_CHAR,lock_orbitals) 
      call set_constant('damp_tol'//C_NULL_CHAR,damp_tol) 
      call set_constant('damp_end'//C_NULL_CHAR,damp_end) 
      call set_constant('diis_start'//C_NULL_CHAR,diis_start) 
      call set_constant('diis_order'//C_NULL_CHAR,diis_order) 
      call set_constant('natoms'//C_NULL_CHAR,natoms) 
      call set_constant('iecp_on'//C_NULL_CHAR,iecp_on) 
      call set_constant('Three_natoms'//C_NULL_CHAR,Three_natoms) 
      call set_constant('reference'//C_NULL_CHAR,iuhf) 
C
C Added on 04/2005, Ajith Perera.

      write(6,*) ' Done setting second set of predefined arrays ' 

c     call set_predefined_scalar_array('fockrohf_a'//C_NULL_CHAR, 2,
c    *                            fockrohf_a)  
c     call set_predefined_scalar_array('fockrohf_b'//C_NULL_CHAR, 2,
c    *                            fockrohf_b)  

c *************************************************************************
c Set the parameters defining the fragment calculation. 
c *************************************************************************
      nfrags = 0  
      call set_constant('nfrags'//C_NULL_CHAR,nfrags) 

c *************************************************************************
c Done Setting the parameters defining the fragment calculation. 
c *************************************************************************
c 

      call finalize_setup()
      call dump_file(datname)

      return
      end subroutine
c      end program setup_main

