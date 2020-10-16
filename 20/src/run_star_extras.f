! ***********************************************************************
!
!   Copyright (C) 2010  Bill Paxton
!
!   this file is part of mesa.
!
!   mesa is free software; you can redistribute it and/or modify
!   it under the terms of the gnu general library public license as published
!   by the free software foundation; either version 2 of the license, or
!   (at your option) any later version.
!
!   mesa is distributed in the hope that it will be useful,
!   but without any warranty; without even the implied warranty of
!   merchantability or fitness for a particular purpose.  see the
!   gnu library general public license for more details.
!
!   you should have received a copy of the gnu library general public license
!   along with this software; if not, write to the free software
!   foundation, inc., 59 temple place, suite 330, boston, ma 02111-1307 usa
!
! ***********************************************************************

      module run_star_extras

      use star_lib
      use star_def
      use const_def
      use crlibm_lib

      implicit none

      ! these routines are called by the standard run_star check_model
      contains

        subroutine extras_controls(id, ierr)
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return

           ! this is the place to set any procedure pointers you want to change
           ! e.g., other_wind, other_mixing, other_energy  (see star_data.inc)

           ! Uncomment these lines if you wish to use the functions in this file,
           ! otherwise we use a null_ version which does nothing.
           s% extras_startup => extras_startup
           s% extras_start_step => extras_start_step
           s% extras_check_model => extras_check_model
           s% extras_finish_step => extras_finish_step
           s% extras_after_evolve => extras_after_evolve
           s% how_many_extra_history_columns => how_many_extra_history_columns
           s% data_for_extra_history_columns => data_for_extra_history_columns
           s% how_many_extra_profile_columns => how_many_extra_profile_columns
           s% data_for_extra_profile_columns => data_for_extra_profile_columns

           s% how_many_extra_history_header_items => how_many_extra_history_header_items
           s% data_for_extra_history_header_items => data_for_extra_history_header_items
           s% how_many_extra_profile_header_items => how_many_extra_profile_header_items
           s% data_for_extra_profile_header_items => data_for_extra_profile_header_items

           ! Once you have set the function pointers you want,
           ! then uncomment this (or set it in your star_job inlist)
           ! to disable the printed warning message,
            s% job% warn_run_star_extras =.false.

        end subroutine extras_controls

        ! None of the following functions are called unless you set their
        ! function point in extras_control.


        integer function extras_startup(id, restart, ierr)
           integer, intent(in) :: id
           logical, intent(in) :: restart
           integer, intent(out) :: ierr
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           extras_startup = 0
           if (.not. restart) then
              call alloc_extra_info(s)
           else ! it is a restart
              call unpack_extra_info(s)
           end if
        end function extras_startup


        integer function extras_start_step(id, id_extra)
           integer, intent(in) :: id, id_extra
           integer :: ierr
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           extras_start_step = 0
        end function extras_start_step


        ! returns either keep_going, retry, backup, or terminate.
        integer function extras_check_model(id, id_extra)
           integer, intent(in) :: id, id_extra
           integer :: ierr
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           extras_check_model = keep_going
           if (.false. .and. s% star_mass_h1 < 0.35d0) then
              ! stop when star hydrogen mass drops to specified level
              extras_check_model = terminate
              write(*, *) 'have reached desired hydrogen mass'
              return
           end if


           ! if you want to check multiple conditions, it can be useful
           ! to set a different termination code depending on which
           ! condition was triggered.  MESA provides 9 customizeable
           ! termination codes, named t_xtra1 .. t_xtra9.  You can
           ! customize the messages that will be printed upon exit by
           ! setting the corresponding termination_code_str value.
           ! termination_code_str(t_xtra1) = 'my termination condition'

           ! by default, indicate where (in the code) MESA terminated
           if (extras_check_model == terminate) s% termination_code = t_extras_check_model
        end function extras_check_model


        integer function how_many_extra_history_columns(id, id_extra)
           integer, intent(in) :: id, id_extra
           integer :: ierr
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           how_many_extra_history_columns = 86
        end function how_many_extra_history_columns


        subroutine data_for_extra_history_columns(id, id_extra, n, names, vals, ierr)
           integer, intent(in) :: id, id_extra, n
           character (len=maxlen_history_column_name) :: names(n)
           real(dp) :: vals(n)

           real(dp) :: v_HI_max, v_HeI_max, v_HeII_max, v_FeCZ_max
           real(dp) :: v_HI_aver, v_HeI_aver, v_HeII_aver, v_FeCZ_aver
           real(dp) :: b_HI_max, b_HeI_max, b_HeII_max, b_FeCZ_max
           real(dp) :: b_HI_aver, b_HeI_aver, b_HeII_aver, b_FeCZ_aver
           real(dp) :: b_HI_surf, b_HeI_surf, b_HeII_surf, b_FeCZ_surf
           real(dp) :: b_HI_surf_max, b_HeI_surf_max, b_HeII_surf_max, b_FeCZ_surf_max
           real(dp) :: HI_hp_aver, HeI_hp_aver, HeII_hp_aver, FeCZ_hp_aver
           real(dp) :: mach_HI_top, mach_HeI_top, mach_HeII_top, mach_FeCZ_top
           real(dp) :: rho_HI_aver, rho_HeI_aver, rho_HeII_aver, rho_FeCZ_aver
           real(dp) :: turnover_HI, turnover_HeI, turnover_HeII, turnover_FeCZ
           real(dp) :: mach_HI_aver_ahp, mach_HeI_aver_ahp, mach_HeII_aver_ahp, mach_FeCZ_aver_ahp
           real(dp) :: v_HI_aver_ahp, v_HeI_aver_ahp, v_HeII_aver_ahp, v_FeCZ_aver_ahp
           real(dp) :: v_HI_surf, v_HeI_surf, v_HeII_surf, v_FeCZ_surf
           real(dp) :: HI_r_top, HI_r_bottom, HeI_r_top, HeI_r_bottom
           real(dp) :: HeII_r_top, HeII_r_bottom, FeCZ_r_top, FeCZ_r_bottom
           real(dp) :: HI_mass, HeI_mass, HeII_mass, FeCZ_mass
           real(dp) :: r_hp_1, r_hp_2, r_hp_3, r_hp_4, r_hp_5, r_hp_6, r_hp_7, r_hp_8
           real(dp) :: r_hp_10, r_hp_15, r_hp_20, r_hp_30, r_hp_50, r_hp_100
           real(dp) :: HI_fcmax, HeI_fcmax, HeII_fcmax, FeCZ_fcmax
           real(dp) :: HI_b_p_eq, HI_b_p_max, HeI_b_p_eq, HeI_b_p_max, HeII_b_p_eq, HeII_b_p_max
           real(dp) :: FeCZ_b_p_eq, FeCZ_b_p_max
           real(dp) :: mixing_length_alpha, rho_surf
           real(dp) :: v_max_core, v_aver_core,b_eq_core,b_max_core,rho_aver_core, hp_aver_core, turnover_core
           real(dp) :: hp_core_top, r_core, m_core, mach_top_cz_core, mach_aver_ahp_core, rho_aver_ahp_core, v_aver_ahp_core
           !real(dp), DIMENSION(4) :: b_eq, b_max, hp_aver, sc_turnover, mach_aver_ahp, rho_aver_ahp, b_surf_aver, b_surf_max, v_surf_aver
           integer, intent(out) :: ierr
           integer ::  i, j, k, m, num_conv_regions, sc1_top, sc2_top, sc3_top, sc4_top
           integer ::  sc1_bottom, sc2_bottom, sc3_bottom, sc4_bottom, col_count, sc_convective_core
           integer ::  hp_1, hp_2, hp_3, hp_4, hp_5, hp_6, hp_7
           integer ::  hp_8, hp_10, hp_15, hp_20, hp_30, hp_50, hp_100
           character (len=100) :: col_name
           character (len=10) :: str
           character (len=7) ::  sc1_type
           character (len=7), dimension(4) :: sc_type ! Fixed max number of cz. Can be improved
           integer, dimension(4) :: sc_top, sc_bottom
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return


           ! 1) Need to initialize to zero all the columns!!!

          b_HI_aver = 0d0
          b_HI_max= 0d0
          b_HI_surf= 0d0
          b_HI_surf_max= 0d0
          HI_hp_aver= 0d0
          mach_HI_aver_ahp= 0d0
          turnover_HI= 0d0
          v_HI_surf= 0d0
          HI_r_top = 0d0
          HI_r_bottom = 0d0
          HI_mass = 0d0
          HI_fcmax = 0d0
          HI_b_p_eq = 0d0
          HI_b_p_max = 0d0

          b_HeI_aver= 0d0
          b_HeI_max= 0d0
          b_HeI_surf= 0d0
          b_HeI_surf_max= 0d0
          HeI_hp_aver= 0d0
          mach_HeI_aver_ahp= 0d0
          turnover_HeI= 0d0
          v_HeI_surf= 0d0
          HeI_r_top = 0d0
          HeI_r_bottom = 0d0
          HeI_mass = 0d0
          HeI_fcmax = 0d0
          HeI_b_p_eq = 0d0
          HeI_b_p_max = 0d0

          b_HeII_aver= 0d0
          b_HeII_max= 0d0
          b_HeII_surf= 0d0
          b_HeII_surf_max= 0d0
          HeII_hp_aver= 0d0
          mach_HeII_aver_ahp= 0d0
          turnover_HeII= 0d0
          v_HeII_surf= 0d0
          HeII_r_top = 0d0
          HeII_r_bottom = 0d0
          HeII_mass = 0d0
          HeII_fcmax = 0d0
          HeII_b_p_eq = 0d0
          HeII_b_p_max = 0d0

          b_FeCZ_aver= 0d0
          b_FeCZ_max= 0d0
          b_FeCZ_surf= 0d0
          b_FeCZ_surf_max= 0d0
          FeCZ_hp_aver= 0d0
          mach_FeCZ_aver_ahp= 0d0
          turnover_FeCZ= 0d0
          v_FeCZ_surf= 0d0
          FeCZ_r_top = 0d0
          FeCZ_r_bottom = 0d0
          FeCZ_mass = 0d0
          FeCZ_fcmax = 0d0
          FeCZ_b_p_eq = 0d0
          FeCZ_b_p_max = 0d0

          v_max_core = 0d0
          v_aver_core = 0d0
          b_eq_core = 0d0
          b_max_core = 0d0
          rho_aver_core = 0d0
          hp_aver_core = 0d0
          hp_core_top = 0d0
          turnover_core = 0d0
          m_core = 0d0
          r_core = 0d0
          v_aver_ahp_core = 0d0
          mach_top_cz_core = 0d0
          mach_aver_ahp_core = 0d0
          rho_aver_ahp_core = 0d0



           mixing_length_alpha = s% mixing_length_alpha


           ! Identify top of convective core (center has singular values of e.g. density. Use s% nz -1 )
           call get_convective_core(id, sc_convective_core, ierr)
           if (sc_convective_core < s% nz) then
              !write(*,*) 'Mass Convective Core: ', s% m(sc_convective_core)/Msun
              !write(*,*) 'sc_convective_core, s nz', sc_convective_core, s% nz
              call get_conv_velocities(id, ierr, v_max_core, v_aver_core, sc_convective_core, &
                   s% nz - 1, b_eq_core,b_max_core,rho_aver_core)
              !write(*,*) 'CORE:', v_max_core/1e5, v_aver_core/1e5, b_eq_core, b_max_core,rho_aver_core
              call get_average_hp(id, ierr, sc_convective_core, s% nz - 1, hp_aver_core)
              !write(*,*) 'HP average, boundary:', hp_aver_core/Rsun, s% scale_height(sc_convective_core)/Rsun
              call get_turnover(mixing_length_alpha, v_aver_core, hp_aver_core, turnover_core)
              !write(*,*) 'Turnover V_aver+Hp_aver:', turnover_core/(3600*24)
              m_core = s% m(sc_convective_core)
              r_core = s% r(sc_convective_core)
              hp_core_top = s% scale_height(sc_convective_core)
              call get_conv_ahp(id, ierr, sc_convective_core, s% nz - 1, v_aver_ahp_core, &
                                mach_top_cz_core, mach_aver_ahp_core, rho_aver_ahp_core)
              !write(*,*) 'v_aver_ahp, mach_top_cz, mach_aver_ahp, rho_aver_ahp,rho_aver:', &
              !                     v_aver_ahp_core, mach_top_cz_core, mach_aver_ahp_core, &
              !                     rho_aver_ahp_core, rho_aver_core
              !call get_turnover(mixing_length_alpha, v_aver_core, s% scale_height(sc_convective_core), turnover_core)
              !write(*,*) 'Turnover core V_aver+Hp_top:', turnover_core/(3600*24)
              !call get_turnover(mixing_length_alpha, v_max_core, hp_aver_core, turnover_core)
              !write(*,*) 'Turnover core Vmax+Hp_aver:', turnover_core/(3600*24)
              !call get_turnover(mixing_length_alpha, v_max_core, s% scale_height(sc_convective_core), turnover_core)
              !write(*,*) 'Turnover core Vmax+Hp_top:', turnover_core/(3600*24)
           end if


           ! Identify number of convective regions above a certain temperature  (Max 4, HI, HeI, HeII, FeCZ)

           call get_conv_regions_above_T(id,1d6,ierr,num_conv_regions)
           names(1) = 'subsurface_convective_regions'
           vals(1)  = num_conv_regions

           rho_surf = s% rho(1)
           names(2) = 'rho_surf'
           vals(2)  = rho_surf

           ! Calculate relevant column values
           do k = 1, num_conv_regions ! num_conv_regions should always be >= 1
             sc_top(k) = s% mixing_region_top(k)
             sc_bottom(k) = s% mixing_region_bottom(k)
             if (sc_top(k) .NE. 0) then
               call classify_conv_region_above_T(id, ierr, sc_top(k), sc_bottom(k), sc_type(k))
               if ( sc_type(k) == 'HI' ) then
                  call get_conv_velocities(id, ierr, v_HI_max, v_HI_aver, sc_top(k), sc_bottom(k), b_HI_aver, b_HI_max, rho_HI_aver)
                  call get_average_hp(id, ierr, sc_top(k),  sc_bottom(k), HI_hp_aver)
                  call get_conv_ahp(id, ierr, sc_top(k),  sc_bottom(k), v_HI_aver_ahp, mach_HI_top, mach_HI_aver_ahp, rho_HI_aver)
                  call get_microturb(mach_HI_aver_ahp, rho_HI_aver, rho_surf,v_HI_aver_ahp, v_HI_surf)
                  call get_turnover(mixing_length_alpha, v_HI_aver, HI_hp_aver, turnover_HI)
                  call get_bsurf(rho_surf, rho_HI_aver, b_HI_aver, b_HI_max, b_HI_surf, b_HI_surf_max)
                  call get_conv_radii(id, ierr, sc_top(k), sc_bottom(k), HI_r_top, HI_r_bottom)
                  call get_conv_mass(id, ierr, sc_top(k), sc_bottom(k), HI_mass)
                  call get_max_fc(id, ierr, HI_fcmax, sc_top(k), sc_bottom(k))
                  call get_pressure_eq_field(id, ierr, sc_top(k), sc_bottom (k),HI_b_p_eq,HI_b_p_max)
                  !write(*,*) sc_top(k), sc_bottom(k), sc_type(k), v_HI_aver, rho_HI_aver, b_HI_max, b_HI_surf, v_HI_surf
               else if ( sc_type(k) == 'HeI' ) then
                  call get_conv_velocities(id, ierr, v_HeI_max, v_HeI_aver, sc_top(k), sc_bottom(k), &
                  b_HeI_aver, b_HeI_max, rho_HeI_aver)
                  call get_average_hp(id, ierr, sc_top(k),  sc_bottom(k), HeI_hp_aver)
                  call get_conv_ahp(id, ierr, sc_top(k),  sc_bottom(k), v_HeI_aver_ahp, mach_HeI_top, &
                  mach_HeI_aver_ahp, rho_HeI_aver)
                  call get_microturb(mach_HeI_aver_ahp, rho_HeI_aver, rho_surf,v_HeI_aver_ahp, v_HeI_surf)
                  call get_turnover(mixing_length_alpha, v_HeI_aver, HeI_hp_aver, turnover_HeI)
                  call get_bsurf(rho_surf, rho_HeI_aver, b_HeI_aver, b_HeI_max, b_HeI_surf, b_HeI_surf_max)
                  call get_conv_radii(id, ierr, sc_top(k), sc_bottom(k), HeI_r_top, HeI_r_bottom)
                  call get_conv_mass(id, ierr, sc_top(k), sc_bottom(k), HeI_mass)
                  call get_max_fc(id, ierr, HeI_fcmax, sc_top(k), sc_bottom(k))
                  call get_pressure_eq_field(id, ierr, sc_top(k), sc_bottom (k),HeI_b_p_eq,HeI_b_p_max)
                  !write(*,*) sc_top(k), sc_bottom(k), sc_type(k), v_HeI_aver, rho_HeI_aver, b_HeI_max, b_HeI_surf, v_HeI_surf
               else if ( sc_type(k) == 'HeII' ) then
                  call get_conv_velocities(id, ierr, v_HeII_max, v_HeII_aver, sc_top(k), sc_bottom(k), &
                  b_HeII_aver, b_HeII_max, rho_HeII_aver)
                  call get_average_hp(id, ierr, sc_top(k),  sc_bottom(k), HeII_hp_aver)
                  call get_conv_ahp(id, ierr, sc_top(k),  sc_bottom(k), v_HeII_aver_ahp, mach_HeII_top, &
                  mach_HeII_aver_ahp, rho_HeII_aver)
                  call get_microturb(mach_HeII_aver_ahp, rho_HeII_aver, rho_surf,v_HeII_aver_ahp, v_HeII_surf)
                  call get_turnover(mixing_length_alpha, v_HeII_aver, HeII_hp_aver, turnover_HeII)
                  call get_bsurf(rho_surf, rho_HeII_aver, b_HeII_aver, b_HeII_max, b_HeII_surf, b_HeII_surf_max)
                  call get_conv_radii(id, ierr, sc_top(k), sc_bottom(k), HeII_r_top, HeII_r_bottom)
                  call get_conv_mass(id, ierr, sc_top(k), sc_bottom(k), HeII_mass)
                  call get_max_fc(id, ierr, HeII_fcmax, sc_top(k), sc_bottom(k))
                  call get_pressure_eq_field(id, ierr, sc_top(k), sc_bottom (k),HeII_b_p_eq,HeII_b_p_max)
                  !write(*,*) sc_top(k), sc_bottom(k), sc_type(k), v_HeII_aver, rho_HeII_aver, b_HeII_max, b_HeII_surf, v_HeII_surf
               else if ( sc_type(k) == 'FeCZ' ) then
               	  call get_conv_velocities(id, ierr, v_FeCZ_max, v_FeCZ_aver, sc_top(k), sc_bottom(k), &
                  b_FeCZ_aver, b_FeCZ_max, rho_FeCZ_aver)
                  call get_average_hp(id, ierr, sc_top(k),  sc_bottom(k), FeCZ_hp_aver)
                  call get_conv_ahp(id, ierr, sc_top(k),  sc_bottom(k), v_FeCZ_aver_ahp, mach_FeCZ_top, &
                  mach_FeCZ_aver_ahp, rho_FeCZ_aver)
                  call get_microturb(mach_FeCZ_aver_ahp, rho_FeCZ_aver, rho_surf,v_FeCZ_aver_ahp, v_FeCZ_surf)
                  call get_turnover(mixing_length_alpha, v_FeCZ_aver, FeCZ_hp_aver, turnover_FeCZ)
                  call get_bsurf(rho_surf, rho_FeCZ_aver, b_FeCZ_aver, b_FeCZ_max, b_FeCZ_surf, b_FeCZ_surf_max)
                  call get_conv_radii(id, ierr, sc_top(k), sc_bottom(k), FeCZ_r_top, FeCZ_r_bottom)
                  call get_conv_mass(id, ierr, sc_top(k), sc_bottom(k), FeCZ_mass)
                  call get_max_fc(id, ierr, FeCZ_fcmax, sc_top(k), sc_bottom(k))
                  call get_pressure_eq_field(id, ierr, sc_top(k), sc_bottom (k),FeCZ_b_p_eq,FeCZ_b_p_max)
                  !write(*,*) sc_top(k), sc_bottom(k), sc_type(k), v_FeCZ_aver, rho_FeCZ_aver, b_FeCZ_max, b_FeCZ_surf, v_FeCZ_surf
               end if
            end if
           end do
           ! Store relevant column values (8x4) = 32 columns

           names(3) = 'v_HI_surf'
           vals(3)  = v_HI_surf
           names(4) = 'b_HI_surf'
           vals(4)  = b_HI_surf
           names(5) = 'b_HI_surf_max'
           vals(5)  = b_HI_surf_max
           names(6) = 'b_HI_aver'
           vals(6)  = b_HI_aver
           names(7) = 'b_HI_max'
           vals(7)  = b_HI_max
           names(8) = 'HI_hp_aver'
           vals(8)  = HI_hp_aver
           names(9) = 'mach_HI_aver_ahp'
           vals(9)  = mach_HI_aver_ahp
           names(10) = 'turnover_HI'
           vals(10)  = turnover_HI


           names(11) = 'v_HeI_surf'
           vals(11)  = v_HeI_surf
           names(12) = 'b_HeI_surf'
           vals(12)  = b_HeI_surf
           names(13) = 'b_HeI_surf_max'
           vals(13)  = b_HeI_surf_max
           names(14) = 'b_HeI_aver'
           vals(14)  = b_HeI_aver
           names(15) = 'b_HeI_max'
           vals(15)  = b_HeI_max
           names(16) = 'HeI_hp_aver'
           vals(16)  = HeI_hp_aver
           names(17) = 'mach_HeI_aver_ahp'
           vals(17)  = mach_HeI_aver_ahp
           names(18) = 'turnover_HeI'
           vals(18)  = turnover_HeI

           names(19) = 'v_HeII_surf'
           vals(19)  = v_HeII_surf
           names(20) = 'b_HeII_surf'
           vals(20)  = b_HeII_surf
           names(21) = 'b_HeII_surf_max'
           vals(21)  = b_HeII_surf_max
           names(22) = 'b_HeII_aver'
           vals(22)  = b_HeII_aver
           names(23) = 'b_HeII_max'
           vals(23)  = b_HeII_max
           names(24) = 'HeII_hp_aver'
           vals(24)  = HeII_hp_aver
           names(25) = 'mach_HeII_aver_ahp'
           vals(25)  = mach_HeII_aver_ahp
           names(26) = 'turnover_HeII'
           vals(26)  = turnover_HeII

           names(27) = 'v_FeCZ_surf'
           vals(27)  = v_FeCZ_surf
           names(28) = 'b_FeCZ_surf'
           vals(28)  = b_FeCZ_surf
           names(29) = 'b_FeCZ_surf_max'
           vals(29)  = b_FeCZ_surf_max
           names(30) = 'b_FeCZ_aver'
           vals(30)  = b_FeCZ_aver
           names(31) = 'b_FeCZ_max'
           vals(31)  = b_FeCZ_max
           names(32) = 'FeCZ_hp_aver'
           vals(32)  = FeCZ_hp_aver
           names(33) = 'mach_FeCZ_aver_ahp'
           vals(33)  = mach_FeCZ_aver_ahp
           names(34) = 'turnover_FeCZ'
           vals(34)  = turnover_FeCZ

           names(35) = 'HI_r_top'
           vals(35)  = HI_r_top
           names(36) = 'HI_r_bottom'
           vals(36)  = HI_r_bottom

           names(37) = 'HeI_r_top'
           vals(37)  = HeI_r_top
           names(38) = 'HeI_r_bottom'
           vals(38)  = HeI_r_bottom

           names(39) = 'HeII_r_top'
           vals(39)  = HeII_r_top
           names(40) = 'HeII_r_bottom'
           vals(40)  = HeII_r_bottom

           names(41) = 'FeCZ_r_top'
           vals(41)  = FeCZ_r_top
           names(42) = 'FeCZ_r_bottom'
           vals(42)  = FeCZ_r_bottom

           names(43) = 'HI_mass'
           vals(43)  = HI_mass
           names(44) = 'HeI_mass'
           vals(44)  = HeI_mass
           names(45) = 'HeII_mass'
           vals(45)  = HeII_mass
           names(46) = 'FeCZ_mass'
           vals(46)  = FeCZ_mass

           names(47) = 'HI_Fc_max'
           vals(47)  = HI_fcmax
           names(48) = 'HeI_Fc_max'
           vals(48)  = HeI_fcmax
           names(49) = 'HeII_Fc_max'
           vals(49)  = HeII_fcmax
           names(50) = 'FeCZ_Fc_max'
           vals(50)  = FeCZ_fcmax



  !        Pressure scale Heigths (0,1,2,3,4,5,6,7,8)
           call get_hp_radii(id, ierr, 1d0, hp_1)
           call get_hp_radii(id, ierr, 2d0, hp_2)
           call get_hp_radii(id, ierr, 3d0, hp_3)
           call get_hp_radii(id, ierr, 4d0, hp_4)
           call get_hp_radii(id, ierr, 5d0, hp_5)
           call get_hp_radii(id, ierr, 6d0, hp_6)
           call get_hp_radii(id, ierr, 7d0, hp_7)
           call get_hp_radii(id, ierr, 8d0, hp_8)
           call get_hp_radii(id, ierr, 10d0, hp_10)
           call get_hp_radii(id, ierr, 15d0, hp_15)
           call get_hp_radii(id, ierr, 20d0, hp_20)
           call get_hp_radii(id, ierr, 30d0, hp_30)
           call get_hp_radii(id, ierr, 50d0, hp_50)
           call get_hp_radii(id, ierr, 100d0, hp_100)

           r_hp_1 = s% r(hp_1)
           r_hp_2 = s% r(hp_2)
           r_hp_3 = s% r(hp_3)
           r_hp_4 = s% r(hp_4)
           r_hp_5 = s% r(hp_5)
           r_hp_6 = s% r(hp_6)
           r_hp_7 = s% r(hp_7)
           r_hp_8 = s% r(hp_8)
           r_hp_10 = s% r(hp_10)
           r_hp_15 = s% r(hp_15)
           r_hp_20 = s% r(hp_20)
           r_hp_30 = s% r(hp_30)
           r_hp_50 = s% r(hp_50)
           r_hp_100 = s% r(hp_100)

           names(51) = 'r_hp_1'
           vals(51)  = r_hp_1
           names(52) = 'r_hp_2'
           vals(52)  = r_hp_2
           names(53) = 'r_hp_3'
           vals(53)  = r_hp_3
           names(54) = 'r_hp_4'
           vals(54)  = r_hp_4
           names(55) = 'r_hp_5'
           vals(55)  = r_hp_5
           names(56) = 'r_hp_6'
           vals(56)  = r_hp_6
           names(57) = 'r_hp_7'
           vals(57)  = r_hp_7
           names(58) = 'r_hp_8'
           vals(58)  = r_hp_8
           names(59) = 'r_hp_10'
           vals(59)  = r_hp_10
           names(60) = 'r_hp_15'
           vals(60)  = r_hp_15
           names(61) = 'r_hp_20'
           vals(61)  = r_hp_20
           names(62) = 'r_hp_30'
           vals(62)  = r_hp_30
           names(63) = 'r_hp_50'
           vals(63)  = r_hp_50
           names(64) = 'r_hp_100'
           vals(64)  = r_hp_100

           names(65) = 'HI_b_p_eq'
           vals(65) = HI_b_p_eq
           names(66) = 'HI_b_p_max'
           vals(66) = HI_b_p_max

           names(67) = 'HeI_b_p_eq'
           vals(67) = HeI_b_p_eq
           names(68) = 'HeI_b_p_max'
           vals(68) = HeI_b_p_max

           names(69) = 'HeII_b_p_eq'
           vals(69) = HeII_b_p_eq
           names(70) = 'HeII_b_p_max'
           vals(70) = HeII_b_p_max

           names(71) = 'FeCZ_b_p_eq'
           vals(71) = FeCZ_b_p_eq
           names(72) = 'FeCZ_b_p_max'
           vals(72) = FeCZ_b_p_max


           names(73) = 'v_max_core'
           names(74) = 'v_aver_core'
           names(75) = 'b_eq_core'
           names(76) = 'b_max_core'
           names(77) = 'rho_aver_core'
           names(78) = 'hp_aver_core'
           names(79) = 'hp_core_top'
           names(80) = 'turnover_core'
           names(81) = 'm_core'
           names(82) = 'r_core'

           vals(73) = v_max_core
           vals(74) = v_aver_core
           vals(75) = b_eq_core
           vals(76) = b_max_core
           vals(77) = rho_aver_core
           vals(78) = hp_aver_core
           vals(79) = hp_core_top
           vals(80) = turnover_core
           vals(81) = m_core
           vals(82) = r_core



           names(83) = 'v_aver_ahp_core'
           names(84) = 'mach_top_cz_core'
           names(85) = 'mach_aver_ahp_core'
           names(86) = 'rho_aver_ahp_core'

           vals(83) = v_aver_ahp_core
           vals(84) = mach_top_cz_core
           vals(85) = mach_aver_ahp_core
           vals(86) = rho_aver_ahp_core



        end subroutine data_for_extra_history_columns



        integer function how_many_extra_profile_columns(id, id_extra)
           use star_def, only: star_info
           integer, intent(in) :: id, id_extra
           integer :: ierr
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           how_many_extra_profile_columns = 3
        end function how_many_extra_profile_columns

        subroutine get_conv_regions_above_T(id, T_limit, ierr, num_conv_regions)
           ! use mlt_def, only: convective_mixing
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           real(dp) :: T_limit
           integer :: prev_type, cur_type, cur_top, n, k, num_conv_regions, max_num_conv_regions, n_limit
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return

           ierr = 0
           cur_type = s% mixing_type(1)
           cur_top = 1
           n = 0
           n_limit = 0
           max_num_conv_regions = 4 ! Max number of convective regions allowed


           ! Find gridpoint corresponding to max temperature (select only outer layers)
           do k = 1, s% nz
              if (s% T(k) < T_limit) then
                    n_limit = k
              end if
           end do

           ! Find all convective regions in the outer layers down to T_limit
           do k = 2, n_limit
              prev_type = cur_type
              cur_type = s% mixing_type(k)
              if (cur_type == prev_type .and. k < n_limit) cycle
              ! change of type from k-1 to k
              if (prev_type == convective_mixing) then
                 n = n + 1
                 s% mixing_region_type(n) = prev_type
                 s% mixing_region_top(n) = cur_top
                 if (k == n_limit) then
                    s% mixing_region_bottom(n) = k
                 else
                    s% mixing_region_bottom(n) = k-1
                 end if
                 if (n == max_num_conv_regions) exit
              end if
              cur_top = k
           end do

           num_conv_regions = n

        end subroutine get_conv_regions_above_T


      subroutine get_convective_core(id, sc_convective_core,ierr)
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr, sc_convective_core
           integer :: cur_type, k
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return

           k = s% nz
           cur_type = s% mixing_type(k)
           write(*,*) 'Convective type', cur_type
           do while (cur_type == 1 .and. k > 2)
             cur_type = s% mixing_type(k)
             k = k - 1
           end do
           sc_convective_core = k
      end subroutine get_convective_core


  	  subroutine classify_conv_region_above_T(id, ierr, sc_top, sc_bottom, sc_type)

           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr

           character (len=7) ::  sc_type
           real(dp), DIMENSION(2) :: T_HI, T_HeI, T_HeII, T_FeCZ
           integer :: n, k, sc_top, sc_bottom
           include 'formats'

           ! Pass upper and lower gridpoints of convective regions, check temperature and classify

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return


           T_HI    = (/ 3000,11000 /)     ! Rough T range for H Conv Region
           T_HeI   = (/ 11000,35000 /)    ! Rough T range for HeI Conv Region
           T_HeII  = (/ 35000,100000 /)   ! Rough T range for HeII Conv Region
           T_FeCZ  = (/ 100000,500000 /)  ! Rough T range for FeCZ Conv Region

           !write(*,*)   T_HI(1), T_HI(2), MAXVAL(s% T(sc_top:sc_bottom))

           ! Find gridpoint corresponding to max temperature (select only outer layers)

           sc_type = 'UNKNOWN'
           if ( sc_top > 0 ) then
             if (s% T(sc_top) < T_HI(2)) then
               sc_type = 'HI'
             else
               do k = sc_top, sc_bottom
                  if  (s% T(k) > T_HeI(1) .AND. s% T(k) < T_HeI(2)) then
                    sc_type = 'HeI'
              	else if (s% T(k) > T_HeII(1) .AND. s% T(k) < T_HeII(2)) then
                    sc_type = 'HeII'
              	else if (s% T(k) > T_FeCZ(1) .AND. s% T(k) < T_FeCZ(2)) then
                    sc_type = 'FeCZ'
              	else
                    sc_type = 'UNKNOWN'
              	end if
           	 end do
             end if
           end if
           write(*,*) 'Type: ', s% T(sc_top), s% T(sc_bottom), sc_type
        end subroutine classify_conv_region_above_T

  	  subroutine get_conv_radii(id, ierr, sc_top, sc_bottom, r_top, r_bottom)
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           real(dp) :: r_top, r_bottom
           integer :: n, k, sc_top, sc_bottom
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           r_top = 0d0
           r_bottom = 0d0

           if ( sc_top > 0 ) then
           	r_top = s% r(sc_top)
           	r_bottom = s% r(sc_bottom)
           end if
        end subroutine get_conv_radii


        subroutine get_hp_radii(id, ierr, hp, k_hp)
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           integer :: k_hp
           real(dp) :: hp
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           k_hp = 1
  !        write(*,*) s% lnP
  !         do while (((s% lnP(k_hp) - s% lnP(1)) < LOG10(hp)) .and. (k_hp < s% nz))
           do while ((LOG(EXP(s% lnP(k_hp))/EXP(s% lnP(1))) < hp).and.(k_hp < s% nz))
           !           write(*,*)  s% lnP(k_hp),  s% lnP(1), LOG10(hp)
           	k_hp = k_hp + 1
           end do
        end subroutine get_hp_radii

        subroutine get_max_fc(id, ierr, fcmax, sc_top, sc_bottom)
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           real(dp) :: fcmax
           integer :: n, k, sc_top, sc_bottom
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           fcmax = 0
           ! fcmax = MAXVAL((s% conv_vel(sc_top:sc_bottom)**3.0) * s% rho(sc_top:sc_bottom))
           fcmax = MAXVAL(s% L_conv(sc_top:sc_bottom)/s% L(sc_top:sc_bottom))
        end subroutine get_max_fc

        subroutine get_conv_mass(id, ierr, sc_top, sc_bottom, total_mass)
        type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           real(dp) :: total_mass
           integer :: n, k, sc_top, sc_bottom
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return

           total_mass = 0d0
           if ( sc_top > 0 ) then
           	total_mass = SUM(s% dm(sc_top:sc_bottom))
           end if

        end subroutine  get_conv_mass

        subroutine get_conv_velocities(id, ierr, v_max, v_aver, sc_top, sc_bottom,b_eq,b_max,rho_aver)
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           real(dp) :: v_max, v_aver, b_eq, b_max, rho_aver, rho_v_max
           integer :: n, k, sc_top, sc_bottom, i_v_max
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return

           v_max = 0d0
           v_aver = 0d0
           rho_aver = 0d0
           b_eq = 0d0
           b_max = 0d0
           rho_aver = 0d0
           i_v_max = 0
           rho_v_max = 0d0
           ! Calculate Max and average V_conv in Conv region (average in dr, see Eq. 6 in Cantiello et al. 2009)


           if ( sc_top > 0 ) then
           	do k = sc_top, sc_bottom
              	v_aver = v_aver + (s% r(k) - s% r(k+1)) * s% conv_vel(k)
              	rho_aver = rho_aver + (s% r(k) - s% r(k+1)) * s% rho(k)
              !  write(*,*) 'DRHO: ', (s% r(k) - s% r(k+1)) * s% rho(k), s% rho(k)
            !    write(*,*) 'DV: ',(s% r(k) - s% r(k+1))*s% conv_vel(k),s% conv_vel(k)
           	end do
           	v_max = MAXVAL(s% conv_vel(sc_top:sc_bottom))
            i_v_max = MAXLOC (s% conv_vel(sc_top:sc_bottom), DIM=1)
            rho_v_max = s% rho(i_v_max)
           	v_aver = v_aver /( s% r(sc_top) - s% r(sc_bottom) )
           	rho_aver = rho_aver /( s% r(sc_top) - s% r(sc_bottom) )


           end if
           ! Calculate B_equipartition and B_max

           b_eq = (v_aver)*(4.0*pi*rho_aver)**(0.5)
           b_max = (v_max)*(4.0*pi*rho_v_max)**(0.5)
           !b_max = (v_max)*(4.0*pi*rho_aver)**(0.5) ! For convective core this would work better

           !write(*,*) v_aver, v_max, rho_aver, b_eq, b_max

        end subroutine get_conv_velocities

        subroutine get_pressure_eq_field(id, ierr, sc_top, sc_bottom,b_p_eq,b_p_max)
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           real(dp) :: p_max, p_aver, b_p_eq, b_p_max
           integer :: n, k, sc_top, sc_bottom
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return


           b_p_eq = 0d0
           b_p_max = 0d0
           p_aver = 0d0

           if ( sc_top > 0 ) then
           	do k = sc_top, sc_bottom
              	p_aver = p_aver + (s% r(k) - s% r(k+1)) * EXP(s% lnP(k))
           	end do
           	p_max = EXP(MAXVAL(s% lnP(sc_top:sc_bottom)))
           	p_aver = p_aver /( s% r(sc_top) - s% r(sc_bottom) )
           end if
           ! Calculate B_Pressure_equipartition and B_max

           b_p_eq = (p_aver*8.0*pi)**(0.5)
           b_p_max = (p_max*8*pi)**(0.5)

  !         write(*,*) b_p_eq, b_p_max

        end subroutine get_pressure_eq_field

        subroutine get_average_hp(id, ierr, sc_top, sc_bottom, hp_aver)
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           real(dp) :: hp_aver
           integer :: n, k, sc_top, sc_bottom
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return

           hp_aver = 0d0
           ! Calculate average hp in convective region (dr weighted)

           if ( sc_top > 0 ) then
           	do k = sc_top, sc_bottom
              	hp_aver = hp_aver + (s% r(k) - s% r(k+1)) *s% scale_height(k)
           	end do
              hp_aver = hp_aver /( s% r(sc_top) - s% r(sc_bottom) )
           end if

        end subroutine get_average_hp

        subroutine get_microturb(mach1_aver_ahp, rho1_aver, rho_surf,v1_aver_ahp, v1_surf_aver)
           real(dp) :: v1_surf_aver, v1_aver_ahp, mach1_aver_ahp, rho1_aver, rho_surf

           v1_surf_aver = 0d0
           if (rho_surf /= 0) then
  	         v1_surf_aver = v1_aver_ahp * (mach1_aver_ahp * rho1_aver/rho_surf)**(1./2.)
           end if

        end subroutine get_microturb

  	  subroutine get_turnover(mixing_length_alpha,v_HI_aver, HI_hp_aver, turnover_HI)
           real(dp) :: v_HI_aver, HI_hp_aver, turnover_HI, mixing_length_alpha


            turnover_HI = 0d0
            if (v_HI_aver /= 0) then
              turnover_HI = mixing_length_alpha*HI_hp_aver/v_HI_aver
           endif


        end subroutine get_turnover

        subroutine get_bsurf(rho_surf, rho_HI_aver, b_HI_aver, b_HI_max, b_HI_surf, b_HI_surf_max)
           real(dp) :: rho_surf, rho_HI_aver, b_HI_aver, b_HI_max, b_HI_surf, b_HI_surf_max
           b_HI_surf = 0d0
           b_HI_surf_max = 0d0
            if (rho_HI_aver /= 0) then
              b_HI_surf = b_HI_aver * (rho_surf/rho_HI_aver)**(2./3.)
              b_HI_surf_max = b_HI_max * (rho_surf/rho_HI_aver)**(2./3.)
           endif


        end subroutine get_bsurf



  	  subroutine get_conv_ahp(id, ierr, sc_top, sc_bottom, v_aver_ahp, mach_top_cz, mach_aver_ahp, rho_aver_ahp)
           type (star_info), pointer :: s
           integer, intent(in) :: id
           integer, intent(out) :: ierr
           real(dp) ::  v_aver_ahp, mach_aver_ahp, rho_aver_ahp, cs_aver_ahp, delta_r, mach_top_cz
           integer :: n, k, sc_top, sc_bottom, kk
           include 'formats'

           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return

           v_aver_ahp = 0d0
           rho_aver_ahp = 0d0
           cs_aver_ahp = 0d0
           mach_aver_ahp = 0d0
           mach_top_cz = 0d0
           kk = 0

           ! Calculate rho_aver and v_aver in top alpha*Hp of convective zone ! Follows Eq.6 in Cantiello et al. 2009

           if ( sc_top > 0 ) then
           	do k = sc_top, sc_bottom
              	if (s% r(k) > s% r(sc_top) - s% mixing_length_alpha * s% scale_height(sc_top)) then
                 		rho_aver_ahp = rho_aver_ahp + (s% r(k) - s% r(k+1)) * s% rho(k)
                 		v_aver_ahp =  v_aver_ahp + (s% r(k) - s% r(k+1)) * s%  conv_vel(k)
                 		cs_aver_ahp = cs_aver_ahp + (s% r(k) - s% r(k+1)) * s% csound(k)
                 		kk = k
              	end if
           	end do
           end if
           rho_aver_ahp = rho_aver_ahp / ( s% r(sc_top) - s% r(kk) )
           v_aver_ahp = v_aver_ahp / ( s% r(sc_top) - s% r(kk) )
           cs_aver_ahp = cs_aver_ahp / ( s% r(sc_top) - s% r(kk) )

           if (cs_aver_ahp /=0) then
           	mach_aver_ahp = v_aver_ahp/cs_aver_ahp
           end if

           if (s% csound(sc_top) /=0) then
           	mach_top_cz = s%  conv_vel(sc_top) / s% csound(sc_top)
           end if


        end subroutine get_conv_ahp

        subroutine data_for_extra_profile_columns(id, id_extra, n, nz, names, vals, ierr)
           use star_def, only: star_info, maxlen_profile_column_name
           use const_def, only: dp
           use crlibm_lib, only: safe_log10_cr
           integer, intent(in) :: id, id_extra, n, nz
           character (len=maxlen_profile_column_name) :: names(n)
           real(dp) :: vals(nz,n), rsun , pi, clight
           integer, intent(out) :: ierr
           type (star_info), pointer :: s
           integer :: k
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return

           pi = 3.1415
           rsun = 6.96e10
           clight = 2.99792458e10

           !!!

           if (n /= 3) stop 'data_for_extra_profile_columns'
           names(1) = 'log_f_div_rhocs3'

           do k = 1, nz
              vals(k,1) = (s% L(k)/(4*pi*(s% r(k)**2)))/(s% Rho(k) * s% csound(k)**3)
              vals(k,1) = safe_log10_cr(vals(k,1))
           end do

           names(2) = 'log_fconv_div_rhocs3'

           do k = 1, nz
              vals(k,2) = (s% conv_vel(k) / s% csound(k))**3
              vals(k,2) = safe_log10_cr(vals(k,2))
           end do

           names(3) = 'log_Fedd_div_rhocs3'

           do k = 1, nz
             vals(k,3) = 4*pi*clight*s% cgrav(k)*s% m_grav(k)/s% opacity(k) ! L_edd
             vals(k,3) = vals(k,3) / (4*pi*(s% r(k)**2))                    ! F_edd
             vals(k,3) = vals(k,3) /(s% Rho(k) * s% csound(k)**3)           ! F_edd/F_max
             vals(k,3) = safe_log10_cr(vals(k,3))
           end do

           !!!!


        end subroutine data_for_extra_profile_columns



        subroutine how_many_extra_history_header_items(id, id_extra, num_cols)
        integer, intent(in) :: id, id_extra
        integer, intent(out) :: num_cols
        num_cols=0
        end subroutine how_many_extra_history_header_items

        subroutine data_for_extra_history_header_items( &
                    id, id_extra, num_extra_header_items, &
                    extra_header_item_names, extra_header_item_vals, ierr)
        integer, intent(in) :: id, id_extra, num_extra_header_items
        character (len=*), pointer :: extra_header_item_names(:)
        real(dp), pointer :: extra_header_item_vals(:)
        type(star_info), pointer :: s
        integer, intent(out) :: ierr
        ierr = 0
        call star_ptr(id,s,ierr)
        if(ierr/=0) return

        !here is an example for adding an extra history header item
        !set num_cols=1 in how_many_extra_history_header_items and then unccomment these lines
        !extra_header_item_names(1) = 'mixing_length_alpha'
        !extra_header_item_vals(1) = s% mixing_length_alpha
        end subroutine data_for_extra_history_header_items


        subroutine how_many_extra_profile_header_items(id, id_extra, num_cols)
        integer, intent(in) :: id, id_extra
        integer, intent(out) :: num_cols
        num_cols = 0
        end subroutine how_many_extra_profile_header_items

        subroutine data_for_extra_profile_header_items( &
                    id, id_extra, num_extra_header_items, &
                    extra_header_item_names, extra_header_item_vals, ierr)
        integer, intent(in) :: id, id_extra, num_extra_header_items
        character (len=*), pointer :: extra_header_item_names(:)
        real(dp), pointer :: extra_header_item_vals(:)
        type(star_info), pointer :: s
        integer, intent(out) :: ierr
        ierr = 0
        call star_ptr(id,s,ierr)
        if(ierr/=0) return

        !here is an example for adding an extra profile header item
        !set num_cols=1 in how_many_extra_profile_header_items and then unccomment these lines
        !extra_header_item_names(1) = 'mixing_length_alpha'
        !extra_header_item_vals(1) = s% mixing_length_alpha
        end subroutine data_for_extra_profile_header_items


        ! returns either keep_going or terminate.
        ! note: cannot request retry or backup; extras_check_model can do that.
        integer function extras_finish_step(id, id_extra)
           integer, intent(in) :: id, id_extra
           integer :: ierr
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
           extras_finish_step = keep_going
           call store_extra_info(s)

           ! to save a profile,
              ! s% need_to_save_profiles_now = .true.
           ! to update the star log,
              ! s% need_to_update_history_now = .true.

           ! see extras_check_model for information about custom termination codes
           ! by default, indicate where (in the code) MESA terminated
           if (extras_finish_step == terminate) s% termination_code = t_extras_finish_step
        end function extras_finish_step


        subroutine extras_after_evolve(id, id_extra, ierr)
           integer, intent(in) :: id, id_extra
           integer, intent(out) :: ierr
           type (star_info), pointer :: s
           ierr = 0
           call star_ptr(id, s, ierr)
           if (ierr /= 0) return
        end subroutine extras_after_evolve


        ! routines for saving and restoring extra data so can do restarts

           ! put these defs at the top and delete from the following routines
           !integer, parameter :: extra_info_alloc = 1
           !integer, parameter :: extra_info_get = 2
           !integer, parameter :: extra_info_put = 3


        subroutine alloc_extra_info(s)
           integer, parameter :: extra_info_alloc = 1
           type (star_info), pointer :: s
           call move_extra_info(s,extra_info_alloc)
        end subroutine alloc_extra_info


        subroutine unpack_extra_info(s)
           integer, parameter :: extra_info_get = 2
           type (star_info), pointer :: s
           call move_extra_info(s,extra_info_get)
        end subroutine unpack_extra_info


        subroutine store_extra_info(s)
           integer, parameter :: extra_info_put = 3
           type (star_info), pointer :: s
           call move_extra_info(s,extra_info_put)
        end subroutine store_extra_info


        subroutine move_extra_info(s,op)
           integer, parameter :: extra_info_alloc = 1
           integer, parameter :: extra_info_get = 2
           integer, parameter :: extra_info_put = 3
           type (star_info), pointer :: s
           integer, intent(in) :: op

           integer :: i, j, num_ints, num_dbls, ierr

           i = 0
           ! call move_int or move_flg
           num_ints = i

           i = 0
           ! call move_dbl

           num_dbls = i

           if (op /= extra_info_alloc) return
           if (num_ints == 0 .and. num_dbls == 0) return

           ierr = 0
           call star_alloc_extras(s% id, num_ints, num_dbls, ierr)
           if (ierr /= 0) then
              write(*,*) 'failed in star_alloc_extras'
              write(*,*) 'alloc_extras num_ints', num_ints
              write(*,*) 'alloc_extras num_dbls', num_dbls
              stop 1
           end if

           contains

           subroutine move_dbl(dbl)
              real(dp) :: dbl
              i = i+1
              select case (op)
              case (extra_info_get)
                 dbl = s% extra_work(i)
              case (extra_info_put)
                 s% extra_work(i) = dbl
              end select
           end subroutine move_dbl

           subroutine move_int(int)
              integer :: int
              i = i+1
              select case (op)
              case (extra_info_get)
                 int = s% extra_iwork(i)
              case (extra_info_put)
                 s% extra_iwork(i) = int
              end select
           end subroutine move_int

           subroutine move_flg(flg)
              logical :: flg
              i = i+1
              select case (op)
              case (extra_info_get)
                 flg = (s% extra_iwork(i) /= 0)
              case (extra_info_put)
                 if (flg) then
                    s% extra_iwork(i) = 1
                 else
                    s% extra_iwork(i) = 0
                 end if
              end select
           end subroutine move_flg

        end subroutine move_extra_info


      end module run_star_extras
