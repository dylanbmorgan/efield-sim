module particle_mover

  use iso_fortran_env, dp => real64
  implicit none

contains

  subroutine calc_forces(E_x, E_y, phi, nx, ny, dx, dy)
    ! Calculate forces on the particle from electric scalar potential

    real(dp), dimension(:,:), intent(out) :: E_x, E_y
    real(dp), dimension(:,:), intent(in) :: phi
    real(dp), intent(in) :: dx, dy
    integer :: i, j, nx, ny

    do i = 1, nx
      do j = 1, ny
        E_x(i, j) = (phi(i+2, j+1) - phi(i, j+1)) / (2.0_dp * dx)
        E_y(i, j) = (phi(i+1, j+2) - phi(i+1, j)) / (2.0_dp * dy)
      end do
    end do

  end subroutine calc_forces

  subroutine velocity_verlet(E_x, E_y, dx, dy, v_x, v_y, p_x, p_y, pos_hist, vel_hist, acc_hist)
    ! Move the particle using the velocity verlet algorithm

    real(dp), dimension(:,:), allocatable, intent(out) :: pos_hist, vel_hist, acc_hist
    real(dp), dimension(:,:), intent(in) :: E_x, E_y
    real(dp), intent(inout) :: v_x, v_y, p_x, p_y
    real(dp) :: dt, q, a_x0, a_y0, a_x, a_y, a_xn, a_yn
    real(dp), intent(in) :: dx, dy
    integer :: iters, cell_pos_x, cell_pos_y, time_step

    iters = 1000  ! Number of iterations
    dt = 0.01_dp  ! Time step
    q = -1.0_dp  ! Charge

    ! Find initial cell position
    cell_pos_x = floor((p_x + 1.0_dp) / dx) + 1
    cell_pos_y = floor((p_y + 1.0_dp) / dy) + 1


    ! Lorentz force equation
    ! The field is taken at the location of the particle
    ! Find initial acceleration
    a_x0 = q * E_x(cell_pos_x, cell_pos_y)
    a_y0 = q * E_y(cell_pos_x, cell_pos_y)


    a_x = a_x0
    a_y = a_y0

    allocate(pos_hist(iters, 2))
    allocate(vel_hist(iters, 2))
    allocate(acc_hist(iters, 2))

    do time_step = 1, iters
      ! Find cell position
      cell_pos_x = floor((p_x + 1.0_dp) / dx) + 1
      cell_pos_y = floor((p_y + 1.0_dp) / dy) + 1

      if(cell_pos_y > size(E_x(1,:)) .or. cell_pos_x > size(E_x(:,1))) then
        exit
      end if
      ! Position
      p_x = p_x + v_x*dt + ((a_x / 2.0_dp) * dt**2)
      p_y = p_y + v_y*dt + ((a_y / 2.0_dp) * dt**2)

      ! Acceleration
      a_xn = a_x
      a_yn = a_y
      a_x = q * E_x(cell_pos_x, cell_pos_y)
      a_y = q * E_y(cell_pos_x, cell_pos_y)

      ! Velocity
      v_x = v_x + dt*((a_x + a_xn) / 2.0_dp)
      v_y = v_y + dt*((a_y + a_yn) / 2.0_dp)

      ! Save histories
      pos_hist(time_step, 1) = p_x
      pos_hist(time_step, 2) = p_y
      vel_hist(time_step, 1) = a_x
      vel_hist(time_step, 2) = a_y
      acc_hist(time_step, 1) = v_x
      acc_hist(time_step, 2) = v_y
    end do

  end subroutine velocity_verlet

end module particle_mover
