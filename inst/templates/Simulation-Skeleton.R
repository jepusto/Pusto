rm(list = ls())

#------------------------------------------------------
# Set development values for simulation parameters
#------------------------------------------------------

# What are your model parameters?
# What are your design parameters?

#------------------------------------------------------
# Data Generating Model
#------------------------------------------------------

dgm <- function(model_params) {

  return(dat)
}

# Test the data-generating model - How can you verify that it is correct?


#------------------------------------------------------
# Model-fitting/estimation/testing functions
#------------------------------------------------------


estimate <- function(dat, design_params) {

  return(result)
}

# Test the estimation function

#------------------------------------------------------
# Calculate performance measures
# (For some simulations, it may make more sense
# to do this as part of the simulation driver.)
#------------------------------------------------------

performance <- function(results, model_params) {

  return(performance_measures)
}

# Check performance calculations

#-----------------------------------------------------------
# Simulation Driver - should return a data.frame or tibble
#-----------------------------------------------------------

runSim <- function(iterations, model_params, design_params, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  results <- replicate(iterations, {
                dat <- dgm(model_params)
                estimate(dat, design_params)
              })

  performance(results, model_params)
}

# demonstrate the simulation driver


#-------------------------------------
# Experimental Design
#-------------------------------------
source_obj <- ls()

set.seed(20150316) # change this seed value!

# now express the simulation parameters as vectors/lists

design_factors <- list(factor1 = , factor2 = , ...) # combine into a design set
params <- expand.grid(design_factors)
params$iterations <- 5
params$seed <- round(runif(1) * 2^30) + 1:nrow(params)

# All look right?
lengths(design_factors)
nrow(params)
head(params)



#--------------------------------------------------------
# run simulations in serial - mdply workflow
#--------------------------------------------------------

system.time(results <- plyr::mdply(params, .fun = runSim))

#--------------------------------------------------------
# run simulations in serial - purrr workflow
#--------------------------------------------------------
library(purrr)

system.time(
  results <- 
    params %>% 
    invoke_rows(.f = runSim, .to = "res")
)

#--------------------------------------------------------
# run simulations in parallel - mdply workflow
#--------------------------------------------------------

library(Pusto)
cluster <- start_parallel(source_obj = source_obj)

system.time(results <- plyr::mdply(params, .fun = runSim, .parallel = TRUE))

stopCluster(cluster)

#--------------------------------------------------------
# run simulations in parallel - multidplyr workflow
#--------------------------------------------------------

library(multidplyr)
library(Pusto)
cluster <- start_parallel(source_obj = source_obj)

system.time(
  results <- 
    params %>%
    partition(cluster = cluster) %>%
    do(invoke_rows(.d = ., .f = runSim, .to = "res")) %>%
    collect() %>% ungroup() %>%
    select(-PARTITION_ID)
)

#--------------------------------------------------------
# Save results and details
#--------------------------------------------------------

session_info <- sessionInfo()
run_date <- date()

save(params, results, session_info, run_date, file = "Simulation Results.Rdata")
