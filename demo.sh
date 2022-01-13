#!/usr/bin/bash

# Exit if a command fails
set -e

# Delete current netcdf file if present 
# If the program detects an issue with the usr input, it will exit before 
# writing the netcdf file. To prevent invoking visualise.py, this script will
#exit if the netcdf file isn't detected after running the fortran binary 
rm output.nc

compile() {
  # Compile the program

  comp_file="./bin/esim"

  program_files="./src/gauss-seidel_errors.f90 ./src/gauss_seidel.f90
  ./src/write_netcdf.f90 ./src/command_line.f90 ./src/useful_funcs.f90
  ./src/particle_mover.f90 ./src/main.f90"

  flibs=`nf-config --flibs`
  fflags=`nf-config --fflags`
  src=`realpath src/`
  bin=`realpath bin/`

  # Save .mod files to src and binaries to bin
  $cf $fflags -J$src $program_files $flibs -o $comp_file -I$bin
}

run() {
  $comp_file $init_state $nx $ny 

  # Check if netcdf file is present 
  if [[ -f "./output.nc" ]]; then 
    $bin/visualise.py
  else 
    echo "There was an error with outputting the netcdf file"
  fi
}

# Default args if no args are given
if [ $# -eq 0 ]; then  
  # Default args
  cf="gfortran -g -std=f2008"
  init_state="problem=single"
  nx="nx=1000"
  ny="ny=1000"

  # TODO Need to check if esim exists in bin

  compile()
  run()
else
  init_state=$1
  nx=$2
  ny=$3
  if [ $4 == "--debug" ]; then
    # Only for debugging
    cf="gfortran -std=f2008 -Wall -g -fimplicit-none -fcheck=all -Wextra 
    -pedantic -fbacktrace" 
  else
    cf="gfortran -g -std=f2008"
  fi
fi

