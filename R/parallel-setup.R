#' @title Initializes a cluster for parallel computing
#'   
#' @description On Windows, this function initializes a socket cluster and then 
#'   (optionally) registers the cluster with the \pkg{DoParallel} package, loads
#'   a list of packages on each node of the cluster, exports a list of objects
#'   to each node, and returns the cluster object. On Mac and other operating 
#'   systems that can take advantage of multicore, the function does nothing but
#'   (optionally) register a cluster of the specified number of nodes with 
#'   \pkg{DoParallel}. On TACC, the function detects an existing MPI cluster
#'   using \code{\link[snow]{getMPIcluster}}, (optionally) registers the cluster
#'   with \pkg{doSNOW}, loads a list of packages on each node, exports a list of
#'   objects to each node, and returns the cluster object.
#'   
#'   
#' @param cores desired number of cores. Defaults to one less than the number of
#'   available cores.
#' @param source_obj list of objects to be exported to each node of the cluster.
#'   Defaults to NULL.
#' @param packages list of package names to be exported to each node of the 
#'   cluster. Defaults to NULL.
#' @param register logical value indicating whether or not to register the 
#'   cluster using doParallel or doSNOW. This is necessary for using the 
#'   \code{.parallel} option in \pkg{plyr} functions. Defaults to FALSE.
#'   
#' @export
#' 
#' @return On TACC or Windows, returns the cluster object. On Mac, returns NULL.
#'   
#' @examples
#' \dontrun{
#' cluster <- start_parallel()
#' stopCluster(cluster)
#' }
#'


start_parallel <- function (cores, source_obj = NULL, packages = NULL, 
                            register = FALSE) {
  
  if (requireNamespace("snow", quietly = TRUE)) {
    
    if (!is.null(snow::getMPIcluster())) {
      
      # TACC setup
      
      cluster <- snow::getMPIcluster()
      if (register) {
        if (!requireNamespace("doSNOW", quietly = TRUE)) {
          stop("The doSNOW package is required for registering the cluster. Please install it.", call. = FALSE)
        }     
        doSNOW::registerDoSNOW(cluster)
      } 
      if (!is.null(source_obj)) {
        snow::clusterExport(cluster, source_obj)
      }
      if (!is.null(packages)) {
        library_calls <- lapply(packages, function(lib) call("library", lib))
        snow::clusterExport(cluster, "library_calls", envir = environment())
        snow::clusterEvalQ(cluster, lapply(library_calls, eval))
      }
      return(cluster)
    }
  }     
  
  if (missing(cores)) cores <- parallel::detectCores() - 1
  
  if (!is.na(pmatch("Windows", Sys.getenv("OS")))) {
    
    # Windows setup
    if (requireNamespace("multidplyr", quietly = TRUE)) {
      cluster <- multidplyr::create_cluster(cores = cores)
    } else {
      cluster <- parallel::makePSOCKcluster(cores)
      cat("Don't forget to use stop_parallel() to close the cluster.")
    }
    
    if (register) {
      if (!requireNamespace("doParallel", quietly = TRUE)) {
        stop("The doParallel package is required for registering the cluster. Please install it.", call. = FALSE)
      }     
      doParallel::registerDoParallel(cluster)
    }
    
    if (!is.null(source_obj)) {
      parallel::clusterExport(cluster, source_obj)
    }
    if (!is.null(packages)) {
      library_calls <- lapply(packages, function(lib) call("library", lib))
      parallel::clusterExport(cluster, "library_calls", envir = environment())
      parallel::clusterEvalQ(cluster, lapply(library_calls, eval))
    }
    return(cluster)
  } else {
    
    # Mac setup
    
    if (register) {
      if (!requireNamespace("doParallel", quietly = TRUE)) {
        stop("The doParallel package is required for registering the cluster. Please install it.", call. = FALSE)
      }
      doParallel::registerDoParallel(cores = cores)
    }
    return(NULL)
  }
}

#' @title Shut down worker nodes on a cluster
#'   
#' @description Explicitly shut down the worker nodes in a cluster created by \code{start_parallel}
#'   
#'   
#' @param cluster object of class \code{"cluster"}.
#'   
#' @export
#'   
#' @examples
#' \dontrun{
#' cluster <- start_parallel()
#' stop_parallel(cluster)
#' }
#'
#'
#'

stop_parallel <- function(cluster = NULL) parallel::stopCluster(cluster)

