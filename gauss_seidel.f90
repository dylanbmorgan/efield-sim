MODULE gauss_seidel

  USE ISO_FORTRAN_ENV
  IMPLICIT NONE

CONTAINS

  SUBROUTINE run_gs(phi, rho, dx, dy)
    REAL(KIND=REAL64), DIMENSION(:,:), INTENT(INOUT) :: phi
    REAL(KIND=REAL64), DIMENSION(:,:), INTENT(IN) :: rho
    !REAL(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: run_gs
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
