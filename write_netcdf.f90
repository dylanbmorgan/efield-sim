!For usage of this module look at test_nc
MODULE write_netcdf
    USE iso_fortran_env
    USE netcdf
    IMPLICIT NONE

    CONTAINS

        ! This is the subroutine to write to the netcdf file "filename".
        ! Large amounts of this subroutine were taken from the example given in Workshop 7.
        SUBROUTINE write_array(phi, rho, E_x, E_y, r, v, a, filename)
            ! The final frame of the world array
            REAL(REAL64), INTENT(IN), DIMENSION(:,:) :: phi, rho, E_x, E_y,r, v, a

            ! The number of dimensions to include in the netcdf file
            INTEGER, PARAMETER :: ndims = 14
            ! Arrays to store the sizes and ids of the dimensions.
            INTEGER, DIMENSION(ndims) :: sizes, dim_ids
            ! The names of the dimensions themselves
            CHARACTER(5), DIMENSION(ndims) :: dims = ['phi_x', 'phi_y', 'rho_x', 'rho_y', 'E_x_x', &
             'E_x_y', 'E_y_x', 'E_y_y', 'r_x  ', 'r_y  ', 'v_x  ', 'v_y  ', 'a_x  ', 'a_y  ']
            ! The filename for the netcdf
            CHARACTER(*), INTENT(IN) :: filename
            ! Error code varaibles and ids
            INTEGER :: ierr, file_id, phi_var_id, rho_var_id, E_x_var_id, E_y_var_id, r_var_id, &
            v_var_id, a_var_id, i

            ! Get the sizes of the dimensions in the netcdf file
            sizes(1:2) = SHAPE(phi)
            sizes(3:4) = SHAPE(rho)
            sizes(5:6) = SHAPE(E_x)
            sizes(7:8) = SHAPE(E_y)
            sizes(9:10) = SIZE(r)
            sizes(11:12) = SIZE(v)
            sizes(13:14) = SIZE(a)

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
                ierr = nf90_def_dim(file_id, TRIM(dims(i)), sizes(i), dim_ids(i))
                IF (ierr /= nf90_noerr) THEN
                    PRINT*, TRIM(nf90_strerror(ierr))
                    RETURN
                END IF
            END DO

            ! define the varaibles
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

            ierr = nf90_def_var(file_id, "r_hist", NF90_REAL, dim_ids(9:10), r_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_def_var(file_id, "v_hist", NF90_REAL, dim_ids(11:12), v_var_id)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_def_var(file_id, "a_hist", NF90_REAL, dim_ids(13:14), a_var_id)
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
            ierr = nf90_put_var(file_id, phi_var_id, phi)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_put_var(file_id, rho_var_id, rho)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_put_var(file_id, E_x_var_id, E_x)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_put_var(file_id, E_y_var_id, E_y)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_put_var(file_id, r_var_id, r)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_put_var(file_id, v_var_id, v)
            IF (ierr /= nf90_noerr) THEN
                PRINT*, TRIM(nf90_strerror(ierr))
                RETURN
            END IF

            ierr = nf90_put_var(file_id, a_var_id, a)
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

        END SUBROUTINE write_array
END MODULE write_netcdf
