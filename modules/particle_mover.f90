module particle_mover

  use iso_fortran_env
  implicit none

contains

  subroutine calc_forces(phi, nx, ny, dx, dy)
    !> Calculate forces on the particle from electric scalar potential
    !>@param phi Electric scalar potential
    !>@param nx Number of grid cells in x-direction
    !>@param ny Number of grid cells in y-direction
    !>@param dx x length of cell
    !>@param dy y length of cell

    real(kind=real64), dimension(:,:), intent(in) :: phi
    real(kind=real64), dimension(:,:) :: E_x, E_y
    real(kind=real64), intent(in) :: dx, dy
    integer :: i, j, nx, ny

    do i = 1, nx
      do j = 1, ny
        E_x(i, j) = (phi(i+2, j+1) - phi(i, j+1)) / 2 * dx
        E_y(i, j) = (phi(i+1, j+2) - phi(i+1, j)) / 2 * dy
      end do
    end do

  end subroutine calc_forces

  subroutine velocity_verlet(E_x, E_y)
    !> Move the particle using the velocity verlet algorithm

    real(kind=real64), dimension(:,:) :: E_x, E_y
    integer :: cell_pos_x, cell_pos_y,

  end subroutine velocity_verlet


end module particle_mover
