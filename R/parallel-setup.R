#' @title Initializes a cluster for parallel computing
#'
#' @description
#' On Windows, this function initializes a socket cluster, registers the cluster with the DoParallel package,
#' exports a list of objects to each node of the cluster, and returns the cluster object. On Mac
#' and other operating systems that can take advantage of multicore, the function simply registers a cluster
#' of the specified number of nodes with DoParallel.
#'
#' @param source_obj list of objects to be exported to each node of the cluster. Defaults to NULL.
#' @param cores desired number of cores. Defaults to one less than the number of available cores.
#'
#' @export
#'
#' @return On Windows, returns the cluster object. On Mac, returns NULL.
#'
#' @examples
#' \dontrun{
#' cluster <- start_parallel()
#' stopCluster(cluster)
#' }
#' @import parallel


start_parallel <- function(source_obj = NULL, cores) {
  if (missing(cores)) cores <- detectCores() - 1

  if (!is.na(pmatch("Windows", Sys.getenv("OS")))) {
    cluster <- makePSOCKcluster(cores)
    doParallel::registerDoParallel(cluster)
    clusterExport(cluster, source_obj)
    cat("Don't forget to use stopCluster() to close the cluster.")
    return(cluster)
  } else {
    doParallel::registerDoParallel(cores=cores)
    return(NULL)
  }
}
