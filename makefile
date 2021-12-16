make:
	ls
test_gs:
	gfortran jeret_est.f90 gauss_seidel.f90 gs_test.f90 -o test_gs
	./test_gs
clean :
	 -rm test_gs
