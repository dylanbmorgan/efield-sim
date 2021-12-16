make:
	ls
test_gs:
	gfortran gauss-seidel_errors.f90 gauss_seidel.f90 gs_test.f90 -o test_gs
	./test_gs
test_nc:
	gfortran write_netcdf.f90 test_nc.f90 -o test_nc `nf-config --fflags --flibs`
	./test_nc
clean :
	 -rm test_gs
	 -rm test_nc
