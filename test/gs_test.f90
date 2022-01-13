! This is some code to test gauss_seidel works correctly
PROGRAM gs_test
    USE ISO_FORTRAN_ENV
    USE gauss_seidel
    USE gauss_seidel_errors
    IMPLICIT NONE

    REAL(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: phi,rho
    REAL(KIND=REAL64) :: out_etot, out_d_rms
    REAL(KIND=REAL64) :: dx = 0.4_REAL64,dy = 0.4_REAL64
    REAL(KIND=REAL64) :: errors
    ALLOCATE(phi(5,5))
    ALLOCATE(rho(5,5))
    ! Create a dummy rho and phi with ghost cells
    rho(:,1)=0
    rho(1,:)=0
    rho(:,5)=0
    rho(5,:)=0
    phi(:,1)=0
    phi(1,:)=0
    phi(:,5)=0
    phi(5,:)=0

    phi(2:4,2:4) = 12
    rho(2:4,2:4) = 1



    errors = 1
    DO WHILE(errors > 0.0001)
      ! Run an interation of gauss_seidel
      CALL run_gs(phi,rho,dx,dy)
      ! Run functions form gauss_seidel_errors
      out_etot = e_tot(phi,rho,dx,dy)
      out_d_rms = d_rms(phi,dx,dy)
      ! Calculate the error
      errors = out_etot/out_d_rms
      ! Continue interation until error below desired tolerance
      PRINT *, errors
    END DO
    PRINT *, "Successfully converged"

END PROGRAM gs_test
