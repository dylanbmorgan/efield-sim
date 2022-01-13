module useful_funcs
    use iso_fortran_env, dp => real64
    implicit none

contains
    function i_to_x(i, nx) result(x)
        integer, intent(in) :: i, nx
        real(dp) :: x
        x = 2.0_dp*((i-2.0_dp)/(nx-1.0_dp))-1.0_dp
    end function i_to_x

    function j_to_y(j, ny) result(y)
        integer, intent(in) :: j, ny
        real(dp) :: y
        y = -(2.0_dp*((j-2.0_dp)/(ny-1.0_dp))-1.0_dp)
    end function j_to_y
end module useful_funcs
