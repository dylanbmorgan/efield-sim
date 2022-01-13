PROGRAM gs_test
    USE ISO_FORTRAN_ENV
    USE write_netcdf
    IMPLICIT NONE

    REAL(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: phi, rho, E_x, E_y, a, r, v
    REAL(KIND=REAL64) :: out_etot, out_d_rms
    REAL(KIND=REAL64) :: dx = 0.4_REAL64,dy = 0.4_REAL64
    REAL(KIND=REAL64) :: errors
    ALLOCATE(phi(5,5))
    ALLOCATE(rho(5,5))
    ALLOCATE(E_x(10,10))
    ALLOCATE(E_y(12,22))
    ALLOCATE(a(13,2))
    ALLOCATE(r(13,2))
    ALLOCATE(v(13,2))
    ! Create dummy variables to write
    phi = 10
    rho = 2
    E_x = 1.4_REAL64
    E_y = 0.01_REAL64
    a = 1
    r = 23
    v = 1

    !Call the write function as follows
    call write_array(phi, rho, E_x, E_y, r, v, a, "Test_Nc")

END PROGRAM gs_test
