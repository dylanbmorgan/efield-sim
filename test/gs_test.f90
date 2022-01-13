! this is some code to test gauss_seidel works correctly
program gs_test
    use iso_fortran_env, dp => real64
    use gauss_seidel
    use gauss_seidel_errors
    implicit none

    real(dp), dimension(:,:), allocatable :: phi,rho
    real(dp) :: out_etot, out_d_rms
    real(dp) :: dx = 0.4_dp, dy = 0.4_dp
    real(dp) :: errors
    allocate(phi(5, 5))
    allocate(rho(5, 5))
    ! create a dummy rho and phi with ghost cells
    rho(:, 1) = 0
    rho(1, :) = 0
    rho(:, 5) = 0
    rho(5, :) = 0
    phi(:, 1) = 0
    phi(1, :) = 0
    phi(:, 5) = 0
    phi(5, :) = 0

    phi(2:4, 2:4) = 0
    rho(2:4, 2:4) = 1



    errors = 1_dp
    do while(errors > 0.0001_dp)
      ! run an interation of gauss_seidel
      call run_gs(phi,rho,dx,dy)
      ! run functions form gauss_seidel_errors
      out_etot = e_tot(phi,rho,dx,dy)
      out_d_rms = d_rms(phi,dx,dy)
      ! calculate the error
      errors = out_etot/out_d_rms
      ! continue interation until error below desired tolerance
      print *, errors
    end do
    print *, "successfully converged"

end program gs_test
