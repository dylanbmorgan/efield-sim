#!/usr/bin/bash

# Exit if a command fails
set -e

# Can also use make:
# make

# Parse args
if [ -z $1 ]; then
  cf="gfortran -g"
  comp_file=./bin/e_sim
elif [ $1 == "-d" ]; then
  # Used for debugging
  cf="gfortran -std=f2008 -Wall -g -fimplicit-none -fcheck=all -Wextra -pedantic
  -fbacktrace"
  comp_file=./bin/e_sim
else
  cf="gfortran -g"
  comp_file=./bin/$1
fi

# Compile
program_files="./src/gauss-seidel_errors.f90 ./src/gauss_seidel.f90
./src/write_netcdf.f90 ./src/command_line.f90 ./src/useful_funcs.f90
./src/particle_mover.f90 ./src/main.f90"

flibs=`nf-config --flibs`
fflags=`nf-config --fflags`
src=`realpath src/`
bin=`realpath bin/`

# Save .mod files to src and binaries to bin
$cf $fflags -J$src $program_files $flibs -o $comp_file -I$bin

# Run
$comp_file nx=100 ny=100 problem="single"
$bin/visualise.py
