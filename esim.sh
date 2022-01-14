#!/usr/bin/bash

# Exit if a command fails
set -e

# Delete current netcdf file.
# If the program detects an issue with the usr input, it will exit before 
# writing the netcdf file. To prevent invoking visualise.py, this script will
# exit if the netcdf file isn't detected after running the fortran binary
if [[ -e "./output.nc" ]]; then
  rm output.nc
fi

compile() {
  # Compile the program

  echo "Compiling..."

  comp_file="./bin/esim"  # Compile location and file name

  program_files="./src/gauss-seidel_errors.f90 ./src/gauss_seidel.f90 \
     ./src/write_netcdf.f90 ./src/command_line.f90 ./src/useful_funcs.f90 \
     ./src/particle_mover.f90 ./src/main.f90"

  flibs=`nf-config --flibs`
  fflags=`nf-config --fflags`
  src=`realpath src/`
  bin=`realpath bin/`

  # Save .mod files to src and binaries to bin
  $cf $fflags -J$src $program_files $flibs -o $comp_file -I$bin

  echo "Compilation finished!"
}

run() {
  # Run the program

  echo "Running..."

  $comp_file $problem $nx $ny

  # Check if netcdf file is present 
  if [[ -e "./output.nc" ]]; then 
    ./bin/visualise.py
  else 
    echo "An issue has occurred. Please check that the program has compiled"\
      "and that appropriate arguments have been used. See the README for more"\
      "details."
    exit 0
  fi
}

if [[ $# -eq 0 ]]; then
  # Default args if none are given
  cf="gfortran -g -std=f2008"
  problem="problem=single"
  nx="nx=100"
  ny="ny=100"

  # Automatically compile if it hasn't already been compiled
  if [[ ! -e "./bin/esim" ]] && [[ ! -e "./src/*.mod" ]]; then
    compile
  fi

  comp_file="./bin/esim"  # This has to be defined again
  run

elif [[ $# -eq 1 ]]; then

  if [[ $1 == "--compile" ]] || [[ $1 == "-c" ]]; then
    # Compile without running
    cf="gfortran -g -std=f2008"
    compile
  else
    echo "Incorrect arguments were used."
    echo "Please check the README for how to use this script."
    exit 0
  fi

elif [[ $# -eq 3 ]] || [[ $# -eq 4 ]]; then
  problem=$1
  nx=$2
  ny=$3

  if [[ $4 == "--debug" ]] || [[ $4 == "-d" ]]; then
    # Only for debugging use
    cf="gfortran -std=f2008 -Wall -g -fimplicit-none -fcheck=all -Wextra \
      -pedantic -fbacktrace"
  elif [[ $4 == "--run" ]] || [[ $4 == "-r" ]]; then
    # Run without compiling

    if [[ ! -e "./bin/esim" ]] || [[ ! -e "./src/*.mod" ]]; then
      # Catch if program hasn't been compiled
      echo "The program hasn't yet been compiled!"
      read -p "Would you like to compile and run it now? (y/n): " usr_inp

      if [[ $usr_inp == "y" ]]; then
        cf="gfortran -g -std=f2008"
        compile
        run
        exit 0
      elif [[ $usr_inp == "n" ]]; then
        echo "Exiting now..."
        exit 0
      else
        echo "Invalid input. Exiting now..."
        exit 0
      fi

    else
      # Run if program has already been compiled
      cf="gfortran -g -std=f2008"
      comp_file="./bin/esim"  # This has to be defined again
      run
      exit 0
    fi

  else
    # Run as normal with usr defined parameters
    cf="gfortran -g -std=f2008"
  fi

  compile
  run

else
  # Catcher if incorrect args used
  echo "Incorrect arguments were used."
  echo "Please check the README for how to use this script."
  exit 0
fi
