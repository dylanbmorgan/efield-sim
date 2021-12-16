make:
	ls
test_gs:
	gfortran gauss-seidel_errors.f90 gauss_seidel.f90 gs_test.f90 -o test_gs
	./test_gs
clean :
	 -rm test_gs
