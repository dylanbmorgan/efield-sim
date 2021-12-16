MODULE gauss_seidel

  USE ISO_FORTRAN_ENV
  IMPLICIT NONE

CONTAINS
  ! Runs a single iteration of the gauss_seidel method
  ! For usage of this ini conjunction with gauss_seidel_errors look at gs_test 
  SUBROUTINE run_gs(phi, rho, dx, dy)
    REAL(KIND=REAL64), DIMENSION(:,:), INTENT(INOUT) :: phi
    REAL(KIND=REAL64), DIMENSION(:,:), INTENT(IN) :: rho
    REAL(KIND=REAL64), INTENT(IN) :: dx,dy
    INTEGER, DIMENSION(2) ::  dims
    INTEGER :: i,j
    dims = SHAPE(phi)
    DO i = 2, dims(1) - 1
      DO j = 2, dims(2) - 1
          phi(i,j) = -(rho(i,j) - (phi(i + 1, j)+phi(i - 1,j))/(dx*dx) -  &
          (phi(i, j + 1)+phi(i,j - 1))/(dy*dy))/(2/(dx*dx)+2/(dy*dy))
      END DO
    END DO

  END SUBROUTINE run_gs

END MODULE gauss_seidel
