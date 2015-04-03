rm(list = ls())

#------------------------------------------------------
# Data Generating Model
#------------------------------------------------------

dgm <- function(model_params) {

  return(dat)
}

#------------------------------------------------------
# Model-fitting/estimation/testing functions
#------------------------------------------------------

estimate <- function(dat, design_params) {

  return(result)
}

#------------------------------------------------------
# Calculate performance measures
# (For some simulations, it may make more sense
# to do this as part of the simulation driver.)
#------------------------------------------------------

performance <- function(results, model_params) {

  return(performance_measures)
}

#------------------------------------------------------
# Simulation Driver
#------------------------------------------------------

runSim <- function(iterations, model_params, design_params, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  results <- replicate(iterations, {
                dat <- dgm(model_params)
                estimate(dat, design_params)
              })

  performance(results, model_params)
}

source_obj <- ls()

#-------------------------------------
# Experimental Design
#-------------------------------------

set.seed(20150316)

design_factors <- list(factor1 = , factor2 = , ...)
params <- expand.grid(design_factors)
params$iterations <- 5
params$seed <- round(runif(nrow(params)) * 2^30)

sapply(design_factors, length)
nrow(params)


#--------------------------------------------------------
# run simulations in parallel
#--------------------------------------------------------

library(Pusto)
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = TRUE))

stopCluster(cluster)


save(results, file = "Simulation Results.Rdata")


