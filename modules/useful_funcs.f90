MODULE useful_funcs
    USE ISO_FORTRAN_ENV
    IMPLICIT NONE

CONTAINS
    FUNCTION i_to_x(i, nx) RESULT(x)
        INTEGER, INTENT(IN) :: i, nx
        REAL(REAL64) :: x
        x = 2.0*((i-2.0)/(nx-1.0))-1.0
    END FUNCTION i_to_x

    FUNCTION j_to_y(j, ny) RESULT(y)
        INTEGER, INTENT(IN) :: j, ny
        REAL(REAL64) :: y
        y = -(2.0*((j-2.0)/(ny-1.0))-1.0)
    END FUNCTION j_to_y
END MODULE useful_funcs
