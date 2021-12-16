MODULE write_netcdf
    USE iso_fortran_env
    USE netcdf
    IMPLICIT NONE

    ! This type contains all the parameters input by the user when they run the 
    ! game of life program. It also contains the name of the program so that the
    ! netcdf file can be traced to a source. 
    TYPE game_info
        CHARACTER(20) :: program_name
    END TYPE

    CONTAINS

        ! This is the subroutine to write to the netcdf file "filename".
        ! Large amounts of this subroutine were taken from the example given in Workshop 7.
        SUBROUTINE write(phi, rho, r, v, a, filename, ierr, input_game_info)
            ! The final frame of the world array
            REAL(REAL64), INTENT(IN), DIMENSION(:,:) :: phi, rho
            REAL(REAL64), INTENT(IN), DIMENSION(:) :: r, v, a
            ! The game_info metadata
            TYPE(game_info), INTENT(IN) :: input_game_info

            INTEGER, DIMENSION(2) :: phi_shape
            INTEGER :: r_shape
            ! The number of dimensions to include in the netcdf file
            INTEGER, PARAMETER :: ndims = 11
            ! Arrays to store the sizes and ids of the dimensions.
            INTEGER, DIMENSION(ndims) :: sizes, dim_ids
            ! The names of the dimensions themselves
            CHARACTER(1), DIMENSION(ndims) :: dims = ['phi_x', 'phi_y', 'rho_x', 'rho_y', 'E_x_x', 'E_x_y', 'E_y_x', 'E_y_y', 'r', 'v', 'a']
            ! The filename for the netcdf
            CHARACTER(*), INTENT(IN) :: filename
            ! Error code varaibles and ids
            INTEGER :: ierr, file_id, phi_var_id, rho_var_id, E_x_var_id, E_y_var_id, r_var_id, v_var_id, a_var_id, i

            ! Get the sizes of the dimensions in the netcdf file
            phi_shape = SHAPE(phi)
            r_shape = SHAPE(r)
            sizes = [phi_shape(1), phi_shape(2), r_shape]


            ! NOW ENTERING THE CODE FROM WORKSHOP 7 (MY COMMENTS FROM NOW ON WILL BE IN CAPS):

            ! Create the file, overwriting if it exists
            ierr = nf90_create(filename, NF90_CLOBBER, file_id)

            ! I don't want to bomb if there is an error, rather return to caller
            ! This is tricky to do from another sub. so I choose this instead
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ! Now I am going to do several actions, checking and printing each one
            ! I am using a loop here, to save a bit of duplication. In higher
            ! dimensions, this would really help!

            DO i = 1, ndims
                ierr = nf90_def_dim(file_id, dims(i), sizes(i), dim_ids(i))
                IF (ierr /= nf90_noerr) THEN
                    PRINT*, TRIM(nf90_strerror(ierr))
                    RETURN
                END IF
            END DO

            ! DEFINE THE 2D GRID DATA VARAIBLE
            ierr = nf90_def_var(file_id, "phi_grid_data", NF90_REAL, dim_ids(1:2), phi_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_def_var(file_id, "rho_grid_data", NF90_REAL, dim_ids(3:4), rho_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_def_var(file_id, "E_x_grid_data", NF90_REAL, dim_ids(5:6), E_x_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_def_var(file_id, "E_y_grid_data", NF90_REAL, dim_ids(7:8), E_y_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ! DEFINE THE 1D TIME HISTORY VARIABLE
            ierr = nf90_def_var(file_id, "r_hist", NF90_REAL, dim_ids(9), r_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_def_var(file_id, "v_hist", NF90_REAL, dim_ids(10), v_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_def_var(file_id, "a_hist", NF90_REAL, dim_ids(11), a_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ! DEFINE THE USER INPUT METADATA FROM THE INPUT_GAME_INFO GAME_INFO TYPE DUMMY VARAIBLE
            ierr = nf90_put_att(file_id, NF90_GLOBAL, "program_name", input_game_info%program_name)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ! Finish defining metadata
            ierr = nf90_enddef(file_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF


            ! Actually write the variable
            ierr = nf90_put_var(file_id, inarr_var_id, inarr)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_put_var(file_id, time_hist_var_id, time_hist)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ! Close the file
            ierr = nf90_close(file_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF
        
        END SUBROUTINE write
END MODULE write_netcdf