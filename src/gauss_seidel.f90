module gauss_seidel

  use iso_fortran_env, dp => real64
  implicit none

contains
  ! runs a single iteration of the gauss_seidel method (the information for which is in the brief)
  ! for usage of this ini conjunction with gauss_seidel_errors look at gs_test
  subroutine run_gs(phi, rho, dx, dy)
    real(dp), dimension(:,:), intent(out) :: phi
    real(dp), dimension(:,:), intent(in) :: rho
    real(dp), intent(in) :: dx,dy
    integer, dimension(2) ::  dims
    integer :: i,j
    dims = shape(phi)
    do i = 2, dims(1) - 1
      do j = 2, dims(2) - 1
          phi(i,j) = -(rho(i,j) - (phi(i + 1, j)+phi(i - 1,j))/(dx*dx) -  &
          (phi(i, j + 1)+phi(i,j - 1))/(dy*dy))/(2.0_dp/(dx*dx)+2.0_dp/(dy*dy))
      end do
    end do

  end subroutine run_gs

end module gauss_seidel
