!for usage of this module look at test_nc
module write_netcdf
    use iso_fortran_env, dp => real64
    use netcdf
    implicit none

    contains

        ! this is the subroutine to write to the netcdf file "filename".
        ! large amounts of this subroutine were taken from the example given in workshop 7.
        subroutine write_array(phi, rho, e_x, e_y, r, v, a, filename)
            ! the final frame of the world array
            real(dp), intent(in), dimension(:,:) :: phi, rho, e_x, e_y,r, v, a

            ! the number of dimensions to include in the netcdf file
            integer, parameter :: ndims = 14
            ! arrays to store the sizes and ids of the dimensions.
            integer, dimension(ndims) :: sizes, dim_ids
            ! the names of the dimensions themselves
            character(5), dimension(ndims) :: dims = ['phi_x', 'phi_y', 'rho_x', 'rho_y', 'e_x_x', &
             'e_x_y', 'e_y_x', 'e_y_y', 'r_x  ', 'r_y  ', 'v_x  ', 'v_y  ', 'a_x  ', 'a_y  ']
            ! the filename for the netcdf
            character(*), intent(in) :: filename
            ! error code varaibles and ids
            integer :: ierr, file_id, phi_var_id, rho_var_id, e_x_var_id, e_y_var_id, r_var_id, &
            v_var_id, a_var_id, i

            ! get the sizes of the dimensions in the netcdf file
            sizes(1:2) = shape(phi)
            sizes(3:4) = shape(rho)
            sizes(5:6) = shape(e_x)
            sizes(7:8) = shape(e_y)
            sizes(9:10) = shape(r)
            sizes(11:12) = shape(v)
            sizes(13:14) = shape(a)

            print*,sizes

            ! create the file, overwriting if it exists
            ierr = nf90_create(filename, nf90_clobber, file_id)

            ! i don't want to bomb if there is an error, rather return to caller
            ! this is tricky to do from another sub. so i choose this instead
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ! now i am going to do several actions, checking and printing each one
            ! i am using a loop here, to save a bit of duplication. in higher
            ! dimensions, this would really help!

            do i = 1, ndims
                ierr = nf90_def_dim(file_id, trim(dims(i)), sizes(i), dim_ids(i))
                if (ierr /= nf90_noerr) then
                    print*, trim(nf90_strerror(ierr))
                    return
                end if
            end do

            ! define the varaibles
            ierr = nf90_def_var(file_id, "phi_grid_data", nf90_real, dim_ids(1:2), phi_var_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_def_var(file_id, "rho_grid_data", nf90_real, dim_ids(3:4), rho_var_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_def_var(file_id, "e_x_grid_data", nf90_real, dim_ids(5:6), e_x_var_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_def_var(file_id, "e_y_grid_data", nf90_real, dim_ids(7:8), e_y_var_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_def_var(file_id, "r_hist", nf90_real, dim_ids(9:10), r_var_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_def_var(file_id, "v_hist", nf90_real, dim_ids(11:12), v_var_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_def_var(file_id, "a_hist", nf90_real, dim_ids(13:14), a_var_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ! finish defining metadata
            ierr = nf90_enddef(file_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if


            ! actually write the variable
            ierr = nf90_put_var(file_id, phi_var_id, phi)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_put_var(file_id, rho_var_id, rho)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_put_var(file_id, e_x_var_id, e_x)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_put_var(file_id, e_y_var_id, e_y)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_put_var(file_id, r_var_id, r)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_put_var(file_id, v_var_id, v)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

            ierr = nf90_put_var(file_id, a_var_id, a)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if


            ! close the file
            ierr = nf90_close(file_id)
            if (ierr /= nf90_noerr) then
                print*, trim(nf90_strerror(ierr))
                return
            end if

        end subroutine write_array
end module write_netcdf
