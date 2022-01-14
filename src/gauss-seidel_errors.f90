module gauss_seidel_errors
    use iso_fortran_env, dp => real64
    implicit none

! These functions are used to calculate the error on a given gauss seidel iteration.

contains
    ! This function calculates the convergence error of the current gauss seidel
    ! iteration. It is proportional to the magnitude of the gradients. To remove
    ! this dependency, e_tot should always be divided by d_rms (see below).
    function e_tot(phi, rho, dx, dy)
        real(dp), intent(in), dimension(:, :) :: phi, rho
        real(dp), intent(in) :: dx, dy
        real(dp) :: e_tot
        integer, dimension(2) :: shape_phi
        integer :: i, j

        shape_phi = shape(phi)

        do i = 2, shape_phi(1)-1
            do j = 2, shape_phi(2)-1
                e_tot = e_tot + abs((phi(i - 1, j) - 2.0_dp * phi(i, j) + phi(i + 1, j)) / (dx * dx)&
                +(phi(i, j - 1) - 2.0_dp * phi(i, j) + phi(i, j + 1)) / (dy * dy) - rho(i, j))
            end do
        end do

    end function e_tot

    ! This function calculates the root-mean-square of the second derivative of
    ! the potential.
    function d_rms(phi, dx, dy)
        real(dp), intent(in), dimension(:, :) :: phi
        real(dp), intent(in) :: dx, dy
        real(dp) :: d_rms, n
        integer, dimension(2) :: shape_phi
        integer :: i, j

        shape_phi = shape(phi)
        
        n = shape_phi(1)*shape_phi(2)

        do i = 2, shape_phi(1)-1
            do j = 2, shape_phi(2)-1
                d_rms = d_rms + ((phi(i - 1, j) - 2.0_dp * phi(i, j) + phi(i + 1, j)) / (dx * dx) + &
                (phi(i, j - 1) - 2.0_dp * phi(i, j) + phi(i, j + 1)) / (dy * dy)) * ((phi(i - 1, j) - &
                2.0_dp * phi(i, j) + phi(i + 1, j)) / (dx * dx) + &
                (phi(i, j - 1) - 2.0_dp * phi(i, j) + phi(i, j + 1)) / (dy * dy))
            end do
        end do

        d_rms = sqrt(1.0_dp / n * d_rms)

    end function d_rms

end module gauss_seidel_errors
