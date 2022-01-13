program main
  use iso_fortran_env
  use gauss_seidel
  use gauss_seidel_errors
  use write_netcdf
  use useful_funcs
  use command_line
  use particle_mover
  implicit none

  ! defining variables (not all yet)
  logical :: success
  integer :: nx,ny,i,j
  character(10) :: problem
  real(kind=real64), dimension(:,:),allocatable :: rho
  real(kind=real64) :: v_x, v_y, p_x, p_y

  call parse_args

  ! getting command line args and dealing with missing or
  ! invalid inputs
  success = get_arg("nx", nx)

  if(.not. success) then
    print*, "failed to read nx"
    return
  end if


  success = get_arg("ny", ny)

  if(.not. success) then
    print*, "failed to read ny"
    return
  end if

  success = get_arg("problem", problem)

  if(.not. success) then
    print*, "failed to read problem"
    return
  end if

  allocate(rho(nx + 2,ny + 2))
  ! initialise based on "problem"
  if(problem == "null") then
    rho = 0_real64
    v_x = 0.1_real64
    v_y = 0.1_real64
    p_x = 0_real64
    p_y = 0_real64
  else if(problem == "single") then
    do i=2,nx-1
      do j=2,ny-1
        rho(i,j)=exp(-(i_to_x(i,nx)/0.1)*(i_to_x(i,nx)/0.1)&
        -(j_to_y(j,ny)/0.1)*(j_to_y(j,ny)/0.1))
      end do
    end do
    v_x = 0.0_real64
    v_y = 0.0_real64
    p_x = 0.1_real64
    p_y = 0_real64

  else if(problem == "double") then
    do i=2,nx-1
      do j=2,ny-1
        rho(i,j)=exp(-((i_to_x(i,nx)+0.25)/0.1)*((i_to_x(i,nx)+0.25)/0.1)&
        -((j_to_y(j,ny)+0.25)/0.1)*((j_to_y(j,ny)+0.25)/0.1))+exp(-((i_to_x(i,nx)&
        -0.75)/0.2)*((i_to_x(i,nx)-0.75)/0.2)-((j_to_y(j,ny)-0.75)/0.2)*((j_to_y(j,ny)-0.75)/0.2))
      end do
    end do
    v_x = 0.0_real64
    v_y = 0.0_real64
    p_x = 0_real64
    p_y = 0.5_real64
  else
    print*, "problem was not recognised"
    return
  end if


  !need put the program together from modules
  call write_array(rho,rho,rho,rho,rho,rho,rho,"rho_test")

  call calc_forces(phi, nx, ny, dx, dy)

  call velocity_verlet(E_x, E_y, dx, dy, v_x, v_y, p_x, p_y)

  ! TODO Write to netcdf
  ! TODO Helper module

end program main
