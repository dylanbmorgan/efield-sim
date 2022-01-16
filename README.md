# EField-Sim
Miniproject assignment for PX913 as part of HetSys CDT at the University of Warwick. This program simulates the movement of a particle through an electric field 

## Dependencies
The following dependencies are required to install/compile the program. These are:

- [GNU Fortran](https://gcc.gnu.org/fortran/)
- [Python 3](https://www.python.downloads/org/)
- [NetCDF4](https://www.unidata.ucar.edu/software/netcdf/) (as well as the [python library](https://pypi.org/project/netCDF4/) for it)
- [matplotlib](https://matplotlib.org/)

## Install and Compile 
This can be installed on any system with git installed using the command:

`git clone https://github.com/dylanbmorgan/efield-sim` 

Compilation of EField-Sim is handled by the bash shell script `esim.sh`. To compile the program: 

`esim.sh -c`

## Usage
This program should not be interacted with through any other means than the `esim.sh`, which handles everything for the end user. A number of flags can be supplied to the shell script when it is invoked to alter the output. In all cases, 1000 time steps are used and the time step (dt) = 0.01. 

Firstly, ensure you are in the cloned repository; `cd ./efield-sim`

The possible ways to invoke the script are as follows: 
- `esim.sh` (no flags): runs the program with the following 'default' parameters:
  - `problem=single nx=100 ny=100`

- `esim.sh --compile` (or `-c`): compile EField-Sim without running it. 

- `esim.sh problem=<problem> nx=<grid x-length> ny=<grid y-length>`: parameters which can be customised:
  - `problem` = 'null', 'single', or 'double'
  - `nx` and `ny` = any positive integer. Note that beyond 1000, the calculation rapidly becomes prohibitively slow!
  
- `esim.sh problem=<problem> nx=<grid x-length> ny=<grid y-length> --run` (or `-r`): run the program without compiling it

Note that the flags have to be specified in this order and cannot be rearranged.

## Contributors 
Dylan Morgan, Matyas Parrag, and Jeremy Thorn

Institution: University of Warwick 

Copyright © 2021-2022, Dylan Morgan, Matyas Parrag, and Jeremy Thorn
