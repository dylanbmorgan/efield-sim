MODULE gauss_seidel
    USE ISO_FORTRAN_ENV
    IMPLICIT NONE
    
    
CONTAINS
    FUNCTION e_tot(phi, rho, dx, dy) 
        REAL(REAL64), INTENT(IN), DIMENSION(:,:) :: phi, rho
        REAL(REAL64), INTENT(IN) :: dx, dy
        REAL(REAL64) :: e_tot
        INTEGER, DIMENSION(2) :: shape_phi
        INTEGER :: i, j

        shape_phi = SHAPE(phi)

        DO i = 2, shape_phi(1)-1
            DO j = 2, shape_phi(2)-1
                e_tot = e_tot + ABS((phi(i-1,j)-2*phi(i,j)+phi(i+1,j))/(dx*dx) + &
                (phi(i,j-1)-2*phi(i,j)+phi(i,j+1))/(dy*dy) - rho(i,j))
            END DO
        END DO
    
    END FUNCTION e_tot

    FUNCTION d_rms(phi, dx, dy) 
        REAL(REAL64), INTENT(IN), DIMENSION(:,:) :: phi
        REAL(REAL64), INTENT(IN) :: dx, dy
        REAL(REAL64) :: d_rms, N
        INTEGER, DIMENSION(2) :: shape_phi
        INTEGER :: i, j

        shape_phi = SHAPE(phi)

        N = shape_phi(1)*shape_phi(2)

        DO i = 2, shape_phi(1)-1
            DO j = 2, shape_phi(2)-1
                d_rms = d_rms + ((phi(i-1,j)-2*phi(i,j)+phi(i+1,j))/(dx*dx) + &
                (phi(i,j-1)-2*phi(i,j)+phi(i,j+1))/(dy*dy))*((phi(i-1,j)-2*phi(i,j)+phi(i+1,j))/(dx*dx) + &
                (phi(i,j-1)-2*phi(i,j)+phi(i,j+1))/(dy*dy))
            END DO
        END DO

        d_rms = SQRT(1/N * d_rms)
    
    END FUNCTION d_rms

END MODULE gauss_seidel