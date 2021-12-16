PROGRAM main
  USE ISO_FORTRAN_ENV
  USE gauss_seidel
  USE gauss_seidel_errors
  USE write_netcdf
  USE command_line
  IMPLICIT NONE

  LOGICAL :: success
  INTEGER :: nx,ny
  CHARACTER(10) :: problem

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

END PROGRAM main
