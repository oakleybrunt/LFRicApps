!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief     Initialises the lfric2lfric miniapp infrastructure.
!>
!> @details   Initialises the required infrastructure for the lfric2lfric
!!            miniapp. This includes meshes and extrusions, XIOS contexts,
!!            field collections and the fields for source and destination
!!            meshes.
!>
module lfric2lfric_infrastructure_mod

  use add_mesh_map_mod,           only : assign_mesh_maps
  use constants_mod,              only : str_def, r_def, i_def, l_def
  use create_mesh_mod,            only : create_mesh
  use driver_modeldb_mod,         only : modeldb_type
  use driver_fem_mod,             only : init_fem
  use driver_io_mod,              only : init_io, &
                                         filelist_populator
  use extrusion_mod,              only : extrusion_type,         &
                                         uniform_extrusion_type, &
                                         TWOD
  use field_mod,                  only : field_type
  use gungho_extrusion_mod,       only : create_extrusion
  use inventory_by_mesh_mod,      only : inventory_by_mesh_type
  use io_context_mod,             only : callback_clock_arg
  use linked_list_mod,            only : linked_list_type
  use lfric_xios_context_mod,     only : lfric_xios_context_type
  use lfric_xios_action_mod,      only : advance
  use log_mod,                    only : log_event,       &
                                         log_level_error, &
                                         log_level_debug
  use mesh_mod,                   only : mesh_type
  use mesh_collection_mod,        only : mesh_collection
  use namelist_mod,               only : namelist_type

  !------------------------------------
  ! lfric2lfric modules
  !------------------------------------
  use lfric2lfric_init_mesh_mod,  only : init_mesh
  use lfric2lfric_check_conf_mod, only : lfric2lfric_check_configuration
  use lfric2lfric_file_init_mod,  only : init_lfric2lfric_dst_files, &
                                         init_lfric2lfric_src_files
  use lfric2lfric_init_mod,       only : init_lfric2lfric

  !------------------------------------
  ! Configuration modules
  !------------------------------------
  use lfric2lfric_config_mod,     only : regrid_method_map,         &
                                         source_geometry_spherical, &
                                         source_geometry_planar
  use base_mesh_config_mod,       only : geometry_planar, &
                                         geometry_spherical

  implicit none

  private
  public :: initialise_infrastructure

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief          Sets up required state in preparation for lfric2lfric run.
  !> @details        Calls the `initialise_infrastructure` subroutine that
  !!                 checks the configuration namelist, initialises meshes,
  !!                 extrusions, XIOS contexts and files, field collections and
  !!                 fields.
  !> @param [in,out] modeldb                 The structure holding model state
  !> @param [in]     xios_ctx_src            The name of the XIOS context that
  !!                                         will hold the source file
  !> @param [in]     xios_ctx_dst            The name of the XIOS context that
  !!                                         will hold the file to be written
  !> @param [in]     source_collection_name  The name of the field collection
  !!                                         that will store the source fields
  !> @param [in]     target_collection_name  The name of the field collection
  !!                                         that will store the target fields
  subroutine initialise_infrastructure( modeldb,                    &
                                        xios_ctx_src, xios_ctx_dst, &
                                        source_collection_name,     &
                                        target_collection_name      )

    implicit none

    type(modeldb_type),     intent(inout) :: modeldb
    character(len=*),       intent(in)    :: xios_ctx_src
    character(len=*),       intent(in)    :: xios_ctx_dst
    character(len=*),       intent(in)    :: source_collection_name
    character(len=*),       intent(in)    :: target_collection_name

    ! Coordinate field
    type(field_type),             pointer :: chi(:)
    type(field_type),             pointer :: panel_id
    type(inventory_by_mesh_type)          :: chi_inventory
    type(inventory_by_mesh_type)          :: panel_id_inventory

    !-----------------------
    ! Mesh Pointers
    !-----------------------
    type(mesh_type),     pointer :: mesh_src
    type(mesh_type),     pointer :: twod_mesh_src
    type(mesh_type),     pointer :: mesh_dst
    type(mesh_type),     pointer :: twod_mesh_dst

    class(extrusion_type),        allocatable :: extrusion
    type(uniform_extrusion_type), allocatable :: extrusion_2d

    ! Pointers for namelists
    type(namelist_type), pointer :: planet_nml
    type(namelist_type), pointer :: extrusion_nml
    type(namelist_type), pointer :: lfric2lfric_nml
    type(namelist_type), pointer :: files_nml

    ! Namelist parameters
    character(len=str_def)                 :: mesh_names(2)
    character(len=str_def),    allocatable :: twod_names(:)
    character(len=str_def)                 :: start_dump_filename

    ! lfric2lfric namelist parameters
    integer(kind=i_def)     :: origin_domain
    integer(kind=i_def)     :: target_domain

    integer(kind=i_def)     :: stencil_depth
    integer(kind=i_def)     :: source_geometry
    integer(i_def)          :: regrid_method
    real(kind=r_def)        :: domain_bottom
    real(kind=r_def)        :: scaled_radius

    integer :: geometry
    integer :: extrusion_method

    integer(i_def) :: number_of_layers
    real(r_def)    :: domain_height

    integer(kind=i_def)            :: i
    integer(kind=i_def), parameter :: one_layer = 1_i_def

    integer(kind=i_def), parameter :: dst = 1
    integer(kind=i_def), parameter :: src = 2


    !------------------------
    ! XIOS contexts
    !------------------------
    ! Pointer for subroutines used in init_io
    procedure(filelist_populator), pointer  :: files_init_ptr
    procedure(callback_clock_arg), pointer  :: before_close => null()

    ! Source context pointer and temporary context for setup
    type(lfric_xios_context_type)          :: tmp_io_context_src
    type(lfric_xios_context_type), pointer :: io_context_src
    type(lfric_xios_context_type), pointer :: io_context_dst
    type(linked_list_type),        pointer :: file_list

    ! -------------------------------
    ! 0.0 Extract namelist variables
    ! -------------------------------
    planet_nml      => modeldb%configuration%get_namelist('planet')
    extrusion_nml   => modeldb%configuration%get_namelist('extrusion')
    lfric2lfric_nml => modeldb%configuration%get_namelist('lfric2lfric')
    files_nml       => modeldb%configuration%get_namelist('files')

    call planet_nml%get_value( 'scaled_radius', scaled_radius )
    call extrusion_nml%get_value( 'method', extrusion_method )
    call extrusion_nml%get_value( 'number_of_layers', number_of_layers )
    call extrusion_nml%get_value( 'domain_height', domain_height )

    ! Check lfric2lfric configuration settings are allowed
    call lfric2lfric_check_configuration( lfric2lfric_nml )

    call lfric2lfric_nml%get_value( 'origin_domain', origin_domain )
    call lfric2lfric_nml%get_value( 'regrid_method', regrid_method )
    call lfric2lfric_nml%get_value( 'destination_mesh_name', &
                                             mesh_names(dst) )
    call lfric2lfric_nml%get_value( 'source_mesh_name', &
                                             mesh_names(src) )
    call lfric2lfric_nml%get_value( 'target_domain', target_domain )
    call lfric2lfric_nml%get_value( 'source_geometry', source_geometry )
    call files_nml%get_value( 'start_dump_filename', start_dump_filename )

    !=======================================================================
    ! 1.0 Mesh
    !=======================================================================
    !-----------------------------------------------------------------------
    ! 1.1 Create the required extrusions
    !-----------------------------------------------------------------------
    select case (source_geometry)
      case (source_geometry_planar)
        domain_bottom = 0.0_r_def
        geometry      = geometry_planar

      case (source_geometry_spherical)
        domain_bottom = scaled_radius
        geometry      = geometry_spherical

      case default
        call log_event("Invalid geometry for mesh initialisation", &
                       log_level_error)
    end select

    allocate( extrusion, source=create_extrusion( extrusion_method, &
                                                  geometry,         &
                                                  number_of_layers, &
                                                  domain_height,    &
                                                  scaled_radius ) )

    extrusion_2d = uniform_extrusion_type( domain_bottom, &
                                           domain_bottom, &
                                           one_layer, TWOD )

    !-----------------------------------------------------------------------
    ! 1.2 Create the required meshes
    !-----------------------------------------------------------------------
    stencil_depth = 1_i_def
    call init_mesh( modeldb%configuration,       &
                    modeldb%mpi%get_comm_rank(), &
                    modeldb%mpi%get_comm_size(), &
                    mesh_names, extrusion,       &
                    stencil_depth, regrid_method )

    allocate( twod_names, source=mesh_names )
    do i=1, size(twod_names)
      twod_names(i) = trim(twod_names(i))//'_2d'
    end do
    call create_mesh( mesh_names, extrusion_2d, &
                      alt_name=twod_names )
    if (regrid_method == regrid_method_map) then
      call assign_mesh_maps(twod_names)
    end if

    !=======================================================================
    ! 2.0 Build the FEM function spaces and coordinate fields
    !=======================================================================
    !-----------------------------------------------------------------------
    ! 2.1 Create the FEM function spaces
    !-----------------------------------------------------------------------
    ! Create FEM specifics (function spaces and chi field)
    call init_fem( mesh_collection, chi_inventory, panel_id_inventory )

    !-----------------------------------------------------------------------
    ! 2.2 Create the coordinates fields
    !-----------------------------------------------------------------------
    ! Assign pointers to the correct meshes
    mesh_src      => mesh_collection%get_mesh(trim(mesh_names(src)))
    twod_mesh_src => mesh_collection%get_mesh(trim(twod_names(src)))
    mesh_dst      => mesh_collection%get_mesh(trim(mesh_names(dst)))
    twod_mesh_dst => mesh_collection%get_mesh(trim(twod_names(dst)))

    deallocate(twod_names)

    ! Log this change
    call log_event('Source mesh set to: '           &
                   //mesh_src%get_mesh_name(),      &
                   log_level_debug)
    call log_event('Source 2D mesh set to: '        &
                   //twod_mesh_src%get_mesh_name(), &
                   log_level_debug)
    call log_event('Destination mesh set to: '      &
                   //mesh_dst%get_mesh_name(),      &
                   log_level_debug)
    call log_event('Destination 2D mesh set to: '   &
                   //twod_mesh_dst%get_mesh_name(), &
                   log_level_debug)

    !=======================================================================
    ! 3.0 Setup I/O system
    !=======================================================================
    !-----------------------------------------------------------------------
    ! 3.1 Create the IO context for destination files
    !-----------------------------------------------------------------------
    ! Set a pointer to the method for setting files in the destination
    ! IO context
    files_init_ptr => init_lfric2lfric_dst_files

    ! Initialise the IO context with all the required info
    call init_io( xios_ctx_dst,                     &
                  mesh_names(dst),                  &
                  modeldb,                          &
                  chi_inventory,                    &
                  panel_id_inventory,               &
                  populate_filelist=files_init_ptr  )

    call modeldb%io_contexts%get_io_context(xios_ctx_dst, io_context_dst)

    ! Must call advance to align IO context clock with iodef and file
    ! output frequency
    call advance(io_context_dst, modeldb%clock)

    !-----------------------------------------------------------------------
    ! 3.1 Create the IO context for source files
    !-----------------------------------------------------------------------
    ! Because the source files are not the same as the 'prime mesh', we have
    ! to initialise the source context manually with the desired mesh

    ! Add the source context to modeldb and return a pointer to it
    call tmp_io_context_src%initialise(xios_ctx_src)
    call modeldb%io_contexts%add_context(tmp_io_context_src)
    call modeldb%io_contexts%get_io_context(xios_ctx_src, io_context_src)

    ! Get the file list of context and populate
    file_list => io_context_src%get_filelist()
    call init_lfric2lfric_src_files( file_list, modeldb )

    ! Get panel_id and chi from correct mesh
    call chi_inventory%get_field_array(mesh_src, chi)
    call panel_id_inventory%get_field(mesh_src, panel_id)

    ! Using correct chi and panel_id, initialise xios context for source mesh
    call io_context_src%initialise_xios_context( modeldb%mpi%get_comm(), &
                                                 chi,                    &
                                                 panel_id,               &
                                                 modeldb%clock,          &
                                                 modeldb%calendar,       &
                                                 before_close            )

    ! Must call advance to align IO context clock with iodef and file
    ! output frequency
    call advance(io_context_dst, modeldb%clock)

    !=======================================================================
    ! 4.0 Create and initialise prognostic fields
    !=======================================================================
    call init_lfric2lfric( modeldb, start_dump_filename,                   &
                           source_collection_name, mesh_src, twod_mesh_src, &
                           target_collection_name, mesh_dst, twod_mesh_dst )

  end subroutine initialise_infrastructure

end module lfric2lfric_infrastructure_mod
