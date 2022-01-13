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
  real(kind=real64), dimension(:,:),allocatable :: rho, E_x, E_y, phi, part_hist,&
   vel_hist, acc_hist
  real(kind=real64) :: v_x, v_y, p_x, p_y, dx, dy, errors, out_etot,out_d_rms

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
    rho = 0_real64
    v_x = 0.1_real64
    v_y = 0.1_real64
    p_x = 0_real64
    p_y = 0_real64
  else if(problem == "single") then
    do i=2,nx+1
      do j=2,ny+1
        rho(i,j)=exp(-(i_to_x(i,nx)/0.1)*(i_to_x(i,nx)/0.1)&
        -(j_to_y(j,ny)/0.1)*(j_to_y(j,ny)/0.1))
      end do
    end do
    v_x = 0.0_real64
    v_y = 0.0_real64
    p_x = 0.1_real64
    p_y = 0_real64

  else if(problem == "double") then
    do i=2,nx+1
      do j=2,ny+1
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
    stop
  end if




  errors = 1
  do while(errors > 0.0001)
    call run_gs(phi,rho,dx,dy)
    out_etot = e_tot(phi,rho,dx,dy)
    out_d_rms = d_rms(phi,dx,dy)
    errors = out_etot/out_d_rms
    !print *, errors
  end do
  print *, "Successfully converged"

  call calc_forces(E_x, E_y, phi, nx, ny, dx, dy)
  call velocity_verlet(E_x, E_y, dx, dy, v_x, v_y, p_x, p_y, part_hist, vel_hist, acc_hist)


  call write_array(phi(2:nx+1,2:ny+1),rho(2:nx+1,2:ny+1),E_x,E_Y,part_hist,vel_hist,acc_hist,"rho_test")



  ! TODO Write to netcdf
  ! TODO Helper module

end program main
