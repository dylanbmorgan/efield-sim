module particle_mover

  use iso_fortran_env, dp => real64
  implicit none

contains

  subroutine calc_forces(phi, nx, ny, dx, dy)
    !> Calculate forces on the particle from electric scalar potential
    !>@param phi Electric scalar potential
    !>@param nx Number of grid cells in x-direction
    !>@param ny Number of grid cells in y-direction
    !>@param dx x length of cell
    !>@param dy y length of cell

    real(dp), dimension(:,:), intent(in) :: phi
    real(dp), dimension(:,:) :: E_x, E_y
    real(dp), intent(in) :: dx, dy
    integer :: i, j, nx, ny

    do i = 1, nx
      do j = 1, ny
        E_x(i, j) = (phi(i+2, j+1) - phi(i, j+1)) / 2 * dx
        E_y(i, j) = (phi(i+1, j+2) - phi(i+1, j)) / 2 * dy
      end do
    end do

  end subroutine calc_forces

  subroutine velocity_verlet(E_x, E_y, dx, dy, v, nx, ny)
    !> Move the particle using the velocity verlet algorithm

    real(dp) :: dt, q, a_x0, a_y0, a_x, a_y, a_xn, a_yn, v_x, v_y
    real(dp), dimension(:,:) :: pos_hist, vel_hist, acc_hist
    integer :: iters, cell_pos_x, cell_pos_y, time_step
    real(dp), dimension(:,:), intent(in) :: E_x, E_y
    integer, intent(in) :: dx, dy, nx, ny
    real(dp), intent(in) :: v

    iters = 1000  ! Number of iterations
    dt = 0.01_dp  ! Time step
    q = -1.0_dp  ! Charge

    ! Find cell position
    cell_pos_x = floor((part_x - 1.0_dp) / dx) + 1
    cell_pos_y = floor((part_y - 1.0_dp) / dy) + 1
    ! TODO What is part_x and part_y???

    ! Lorentz force equation
    ! The field is taken at the location of the particle
    ! Find initial acceleration
    a_x0 = q * E_x(cell_pos_x, cell_pos_y)
    a_y0 = q * E_y(cell_pos_y, cell_pos_y)

    a_x = a_x0
    a_y = a_y0
    v_x = v
    v_y = v

    allocate(pos_hist(iters, 2))
    allocate(vel_hist(iters, 2))
    allocate(acc_hist(iters, 2))

    do time_step = 1, iters
      ! Position
      cell_pos_x = cell_pos_x + v_x*dt + ((a_x / 2.0_dp) * dt**2)
      cell_pos_y = cell_pos_y + v_y*dt + ((a_y / 2.0_dp) * dt**2)
      ! TODO Check if squared values should be ints or dp

      ! Acceleration
      a_xn = a_x
      a_yn = a_y
      a_x = q * E_x(cell_pos_x, cell_pos_y)
      a_y = q * E_y(cell_pos_x, cell_pos_y)

      ! Velocity
      v_x = v_x + dt*((a_x + a_xn) / 2.0_dp)
      v_y = v_y + dt*((a_y + a_yn) / 2.0_dp)

      ! Save histories
      pos_hist(time_step, 1) = cell_pos_x
      pos_hist(time_step, 2) = cell_pos_y
      vel_hist(time_step, 1) = a_x
      vel_hist(time_step, 2) = a_y
      acc_hist(time_step, 1) = v_x
      acc_hist(time_step, 2) = v_y
    end do

    ! TODO Check if any double precisions should be returned to ints

  end subroutine velocity_verlet

end module particle_mover
