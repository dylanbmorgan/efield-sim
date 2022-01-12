make:
	gfortran gauss-seidel_errors.f90 gauss_seidel.f90 write_netcdf.f90 command_line.f90 useful_funcs.f90 main.f90 -o e_sim `nf-config --fflags --flibs`
	./e_sim nx=1000 ny=1000 problem="double"
	python visualise.py
test_gs:
	gfortran gauss-seidel_errors.f90 gauss_seidel.f90 gs_test.f90 -o test_gs
	./test_gs
test_nc:
	gfortran write_netcdf.f90 test_nc.f90 -o test_nc `nf-config --fflags --flibs`
	./test_nc
clean :
	 -rm test_gs
	 -rm test_nc
