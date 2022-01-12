PROGRAM main
  USE ISO_FORTRAN_ENV
  USE gauss_seidel
  USE gauss_seidel_errors
  USE write_netcdf
  USE useful_funcs
  USE command_line
  IMPLICIT NONE

  ! Defining variables (not all yet)
  LOGICAL :: success
  INTEGER :: nx,ny,i,j
  CHARACTER(10) :: problem
  REAL(KIND=REAL64), DIMENSION(:,:),ALLOCATABLE :: rho
  REAL(KIND=REAL64) :: v_x, v_y, p_x, p_y

  CALL parse_args

  ! Getting command line args and dealing with missing or
  ! invalid inputs
  success = get_arg("nx", nx)

  IF(.not. success) THEN
    PRINT*, "Failed to read nx"
    RETURN
  END IF


  success = get_arg("ny", ny)

  IF(.not. success) THEN
    PRINT*, "Failed to read ny"
    RETURN
  END IF

  success = get_arg("problem", problem)

  IF(.not. success) THEN
    PRINT*, "Failed to read problem"
    RETURN
  END IF

  ALLOCATE(rho(nx + 2,ny + 2))
  ! Initialise based on "problem"
  IF(problem == "null") THEN
    rho = 0_REAL64
    v_x = 0.1_REAL64
    v_y = 0.1_REAL64
    p_x = 0_REAL64
    p_y = 0_REAL64
  ELSE IF(problem == "single") THEN
    DO i=2,nx-1
      DO j=2,ny-1
        rho(i,j)=EXP(-(i_to_x(i,nx)/0.1)*(i_to_x(i,nx)/0.1)&
        -(j_to_y(j,ny)/0.1)*(j_to_y(j,ny)/0.1))
      END DO
    END DO
    v_x = 0.0_REAL64
    v_y = 0.0_REAL64
    p_x = 0.1_REAL64
    p_y = 0_REAL64

  ELSE IF(problem == "double") THEN
    DO i=2,nx-1
      DO j=2,ny-1
        rho(i,j)=EXP(-((i_to_x(i,nx)+0.25)/0.1)*((i_to_x(i,nx)+0.25)/0.1)&
        -((j_to_y(j,ny)+0.25)/0.1)*((j_to_y(j,ny)+0.25)/0.1))+EXP(-((i_to_x(i,nx)&
        -0.75)/0.2)*((i_to_x(i,nx)-0.75)/0.2)-((j_to_y(j,ny)-0.75)/0.2)*((j_to_y(j,ny)-0.75)/0.2))
      END DO
    END DO
    v_x = 0.0_REAL64
    v_y = 0.0_REAL64
    p_x = 0_REAL64
    p_y = 0.5_REAL64
  ELSE
    PRINT*, "problem was not recognised"
    RETURN
  END IF


  !Need put the programm together from modules
  call write_array(rho,rho,rho,rho,rho,rho,rho,"RHO_TEST")





END PROGRAM main
