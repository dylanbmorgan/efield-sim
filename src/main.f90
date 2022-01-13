program main
  use iso_fortran_env, dp => real64
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
  real(dp), dimension(:,:),allocatable :: rho, E_x, E_y, phi, part_hist, &
   vel_hist, acc_hist
  real(dp) :: v_x, v_y, p_x, p_y, dx, dy, errors, out_etot, out_d_rms

  call parse_args

  ! getting command line args and dealing with missing or
  ! invalid inputs
  success = get_arg("nx", nx)

  if(.not. success) then
    print*, "failed to read nx"
    stop
  end if


  success = get_arg("ny", ny)

  if(.not. success) then
    print*, "failed to read ny"
    stop
  end if

  success = get_arg("problem", problem)

  if(.not. success) then
    print*, "failed to read problem"
    stop
  end if

  !allocate and initialise
  allocate(rho(nx + 2,ny + 2))
  allocate(phi(nx + 2,ny + 2))
  allocate(E_x(nx,ny))
  allocate(E_y(nx,ny))

  phi = 0.0
  E_x = 0.0
  E_y = 0.0

  dx = 2.0_dp / nx
  dy = 2.0_dp / ny

  ! initialise based on "problem"
  if(problem == "null") then
    rho = 0.0_dp
    v_x = 0.1_dp
    v_y = 0.1_dp
    p_x = 0.0_dp
    p_y = 0.0_dp
  else if(problem == "single") then
    do i=2,nx+1
      do j=2,ny+1
        rho(i,j)=exp(-(i_to_x(i,nx)/0.1)*(i_to_x(i,nx)/0.1)&
        -(j_to_y(j,ny)/0.1)*(j_to_y(j,ny)/0.1))
      end do
    end do
    v_x = 0.0_dp
    v_y = 0.0_dp
    p_x = 0.1_dp
    p_y = 0.0_dp

  else if(problem == "double") then
    do i=2,nx+1
      do j=2,ny+1
        rho(i,j)=exp(-((i_to_x(i,nx)+0.25)/0.1)*((i_to_x(i,nx)+0.25)/0.1)&
        -((j_to_y(j,ny)+0.25)/0.1)*((j_to_y(j,ny)+0.25)/0.1))+exp(-((i_to_x(i,nx)&
        -0.75)/0.2)*((i_to_x(i,nx)-0.75)/0.2)-((j_to_y(j,ny)-0.75)/0.2)*((j_to_y(j,ny)-0.75)/0.2))
      end do
    end do
    v_x = 0.0_dp
    v_y = 0.0_dp
    p_x = 0.0_dp
    p_y = 0.5_dp
  else
    print*, "problem was not recognised"
    stop
  end if



  !Finding Phi as in gs_test
  errors = 1.0_dp
  do while(errors > 0.0001_dp)
    call run_gs(phi,rho,dx,dy)
    out_etot = e_tot(phi,rho,dx,dy)
    out_d_rms = d_rms(phi,dx,dy)
    errors = out_etot / out_d_rms
    !print *, errors
  end do
  print *, "Successfully converged"

  !calculating paricle paths
  !As we do not have damping forces whenever we would have harmonic motion in reality
  !we instead have an increasing amplitude. This is as the calculation is not perfect and
  !introduces small errors. Additionally when we wish to start at y = 0 and ny is even
  ! y=0 will fall between cells exactly and so it is placed at y=dy instead. This
  !introduces and occilation which then increases in aplitude, so the path become invalid
  !for larger time. A larger grid size will reduce the effect of this.
  call calc_forces(E_x, E_y, phi, nx, ny, dx, dy)
  call velocity_verlet(E_x, E_y, dx, dy, v_x, v_y, p_x, p_y, part_hist, vel_hist, acc_hist)

  !writing to a file
  call write_array(phi(2:nx + 1, 2:ny + 1), rho(2:nx + 1, 2:ny + 1), E_x, E_Y, part_hist &
  , vel_hist, acc_hist, "rho_test")


end program main
