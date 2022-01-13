# EField-Sim
Miniproject assignment for PX913 as part of HetSys CDT at the University of Warwick. This program simulates the movement of a particle through an electric field that the particle doesn't influence.

## Install and Compile 
This can be installed on any system with git installed using the command `git clone https://github.com/dylanbmorgan/efield-sim` 

Compilation and running the program is handled by the bash shell script `esim.sh`. To compile the program: 

`esim.sh -c`

## Usage
When running the program, there are 3 different initialisation states that can be run. These are 'null', 'single', and 'double'. These represent...
Additionally, the size of the domain for the movement of the particle can be changed by setting the parameters 'nx' and 'ny'. If `esim.sh` is run without any parameters, the default behaviour of these are both set as 1000 with the 'single' initialisation state.

The syntax for setting these parameters is outlined as follows: 

- To set the initialisation state:
    - `esim.sh -[state]`, where [state] can be 'null', 'single', or 'double'


## Contributors 
Dylan Morgan, Matyas Parrag, and Jeremey Thorn

Institution: University of Warwick 

Copyright © 2021, Dylan Morgan, Matyas Parrag, and Jeremy Thorn
