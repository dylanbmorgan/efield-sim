make:
	gfortran -Wall -Wextra -pedantic src/gauss-seidel_errors.f90 src/particle_mover.f90 src/gauss_seidel.f90 src/write_netcdf.f90 src/command_line.f90 src/useful_funcs.f90 src/main.f90 -o bin/e_sim `nf-config --fflags --flibs`
	./bin/e_sim nx=100 ny=100 problem="double"
	python bin/visualise.py
test_gs:
	gfortran src/gauss-seidel_errors.f90 src/gauss_seidel.f90 test/gs_test.f90 -o bin/test_gs
	./bin/test_gs
test_nc:
	gfortran src/write_netcdf.f90 test/test_nc.f90 -o bin/test_nc `nf-config --fflags --flibs`
	./bin/test_nc
clean :
	 -rm test_gs
	 -rm test_nc
