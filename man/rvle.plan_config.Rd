
\name{rvle.plan_config}
\docType{methods}
\alias{rvle.plan_config}

\title{Configure the simulation of experiment plans.}

\description{
The VLE software provides the simulation of experiment plans based on 
parallelization techniques. This function configures the options 
for simulation of experiment plans.
}

\usage{
 rvle.plan_config(vleObj, parallel_option, nb_slots, simulation_spawn,
                  rm_MPI_files, generate_MPI_host, working_dir)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{parallel_option}{type of parallelization either "single", "threads" or 
                      "cvle"}
 \item{nb_slots}{number of slots used for simulation of the experiment 
                          plan} 
 \item{simulation_spawn}{logical that specifies if simulation are spawn or 
                       if there are laucnhed in the current process.},  
 \item{rm_MPI_files}{logical that specifies simulation if temporary files 
                         are removed or not after simulation}, 
 \item{generate_MPI_host}{logical that specifies if the hostfile for cvle 
                      is generated previously to the call of mpirun.},
 \item{working_dir}{path of the temporary directory where the temporary files
                    are generated}
}

\seealso{
 rvle.plan_run
}




