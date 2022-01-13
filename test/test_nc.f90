program gs_test
    use iso_fortran_env, dp => real64
    use write_netcdf
    implicit none

    real(dp), dimension(:,:), allocatable :: phi, rho, e_x, e_y, a, r, v
    real(dp) :: out_etot, out_d_rms
    real(dp) :: dx = 0.4_dp, dy = 0.4_dp
    real(dp) :: errors
    allocate(phi(5, 5))
    allocate(rho(5, 5))
    allocate(e_x(10, 10))
    allocate(e_y(12, 22))
    allocate(a(13, 2))
    allocate(r(13, 2))
    allocate(v(13, 2))
    ! create dummy variables to write
    phi = 10
    rho = 2
    e_x = 1.4_real64
    e_y = 0.01_real64
    a = 1
    r = 23
    v = 1

    !call the write function as follows
    call write_array(phi, rho, e_x, e_y, r, v, a, "test_nc")

end program gs_test
