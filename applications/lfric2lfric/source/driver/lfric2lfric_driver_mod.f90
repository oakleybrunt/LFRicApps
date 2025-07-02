!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Drives the execution of the lfric2lfric miniapp. This is a
!!        temporary solution until we have a proper driver layer.
!>
module lfric2lfric_driver_mod

  use constants_mod,        only : str_def, i_def
  use driver_modeldb_mod,   only : modeldb_type
  use driver_fem_mod,       only : final_fem
  use driver_io_mod,        only : final_io
  use field_collection_mod, only : field_collection_type
  use lfric_xios_read_mod,  only : read_checkpoint
  use lfric_xios_write_mod, only : write_checkpoint
  use log_mod,              only : log_event, &
                                   log_level_info
  use namelist_mod,         only : namelist_type
  use xios,                 only : xios_context,    &
                                   xios_get_handle, &
                                   xios_set_current_context

  !------------------------------------
  ! lfric2lfric modules
  !------------------------------------
  use lfric2lfric_infrastructure_mod, only : initialise_infrastructure

  !------------------------------------
  ! Configuration modules
  !------------------------------------
  use io_config_mod, only : write_diag

  implicit none

  private
  public initialise, run, finalise

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief          Sets up required state in preparation for run.
  !> @details        Calls the `initialise_infrastructure` subroutine that
  !!                 checks the configuration namelist, initialises meshes,
  !!                 extrusions, XIOS contexts and files, field collections
  !!                 and fields.
  !> @param [in,out] modeldb                 The structure holding model state
  !> @param [in]     xios_ctx_src            The name of the XIOS context that
  !!                                         will hold the source file
  !> @param [in]     xios_ctx_dst            The name of the XIOS context that
  !!                                         will hold the file to write to
  !> @param [in]     source_collection_name  The name of the field collection
  !!                                         that will store the source fields
  !> @param [in]     target_collection_name  The name of the field collection
  !!                                         that will store the target fields
  subroutine initialise( modeldb,                                        &
                         xios_ctx_src, xios_ctx_dst,                     &
                         source_collection_name, target_collection_name  )

    implicit none

    type(modeldb_type), intent(inout) :: modeldb
    character(len=*),   intent(in)    :: xios_ctx_src
    character(len=*),   intent(in)    :: xios_ctx_dst
    character(len=*),   intent(in)    :: source_collection_name
    character(len=*),   intent(in)    :: target_collection_name

    call initialise_infrastructure( modeldb,                    &
                                    xios_ctx_src, xios_ctx_dst, &
                                    source_collection_name,     &
                                    target_collection_name      )

  end subroutine initialise

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief    Performs a time step.
  !> @details  Currently, populates fields in the source field collection with
  !!           data read from the source XIOS context file, switches to the
  !!           destination XIOS context and writes to a checkpoint file.
  !!           TODO: #262 Porting algorithms and kernels
  !!           Over one time step, all regridding is performed by algorithm
  !!           modules specific to the regridding method defined in the
  !!           configuration.
  !!           Fields to be regridded are extracted from the source field
  !!           collection and located in the source dump file using
  !!           `read_checkpoint`. Corresponding source and target field pairs
  !!           are passed to the regridding algorithm, written to fields
  !!           in the destination field collection and then written to
  !!           an outfile.
  !> @param [in,out] modeldb                 The structure that holds model
  !!                                         state
  !> @param [in]     xios_ctx_src            The name of the XIOS context that
  !!                                         will hold the source file
  !> @param [in]     xios_ctx_dst            The name of the XIOS context that
  !!                                         will hold the file to be written
  !> @param [in]     source_collection_name  The name of the field collection
  !!                                         that will store the source fields
  !> @param [in]     target_collection_name  The name of the field collection
  !!                                         that will store the target fields
  subroutine run( modeldb,                                        &
                  xios_ctx_src, xios_ctx_dst,                     &
                  source_collection_name, target_collection_name  )

    implicit none

    type(modeldb_type), intent(inout) :: modeldb
    character(len=*),   intent(in)    :: xios_ctx_src
    character(len=*),   intent(in)    :: xios_ctx_dst
    character(len=*),   intent(in)    :: source_collection_name
    character(len=*),   intent(in)    :: target_collection_name

    type(namelist_type), pointer :: files_nml
    type(namelist_type), pointer :: lfric2lfric_nml
    type(xios_context)           :: handle

    type( field_collection_type ), pointer :: source_fields
    type( field_collection_type ), pointer :: target_fields

    ! LFRic-XIOS constants
    integer(kind=i_def) :: start_timestep = 1_i_def

    ! Namelist variables
    character(len=str_def) :: start_dump_filename
    character(len=str_def) :: checkpoint_stem_name
    integer(kind=i_def)    :: regrid_method

    ! Namelist pointers
    files_nml       => modeldb%configuration%get_namelist('files')
    lfric2lfric_nml => modeldb%configuration%get_namelist('lfric2lfric')

    ! Extract configuration variables
    call files_nml%get_value( 'start_dump_filename', start_dump_filename )
    call files_nml%get_value( 'checkpoint_stem_name', checkpoint_stem_name )
    call lfric2lfric_nml%get_value( 'regrid_method', regrid_method )

    ! Point to source and target field collections
    source_fields => modeldb%fields%get_field_collection(source_collection_name)
    target_fields => modeldb%fields%get_field_collection(target_collection_name)

    call read_checkpoint(source_fields,      &
                         start_timestep,     &
                         start_dump_filename )

    ! TODO: #262 Porting algorithms and kernels to lfric2lfric

    ! Write output
    call xios_get_handle(xios_ctx_dst, handle)
    call xios_set_current_context(handle)

    call write_checkpoint(target_fields, modeldb%clock, checkpoint_stem_name)

  end subroutine run

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Tidies up after a run.
  !>
  !> @param [in]     program_name An identifier given to the model being run
  !> @param [in,out] modeldb      The structure that holds model state
  subroutine finalise( program_name, modeldb )

    implicit none

    character(len=*),   intent(in)     :: program_name
    type(modeldb_type), intent(inout)  :: modeldb


    !--------------------------------------------------------------------------
    ! Model finalise
    !--------------------------------------------------------------------------

    call log_event( program_name//': Miniapp completed', log_level_info )

    !-------------------------------------------------------------------------
    ! Driver layer finalise
    !-------------------------------------------------------------------------

    ! Finalise IO
    call final_io( modeldb )

    call final_fem()

  end subroutine finalise

end module lfric2lfric_driver_mod
