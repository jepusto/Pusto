#!/bin/bash
#SBATCH -J future		        	# Job name
#SBATCH -o future.o%j 	    	# Name of stdout output file (%j expands to jobId)
#SBATCH -e future.o%j 	    	# Name of stderr output file(%j expands to jobId)
#SBATCH -p development	    	# Submit to the 'normal' or 'development' queue
#SBATCH -N 1					      	# Total number of nodes
#SBATCH -n 5	 			   	    	# Total number of mpi tasks requested
#SBATCH -t 0:30:00  		    	# Run time (hh:mm:ss)
#SBATCH --mail-user=jepusto@gmail.com
#SBATCH --mail-type=begin
#SBATCH --mail-type=end

# load R module
module load Rstats        

# call R code from RMPISNOW
ibrun RMPISNOW < ./TACC-script.R
