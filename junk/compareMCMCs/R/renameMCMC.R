#' Rename an MCMC method throughout a list of `MCMCresult` objects
#' 
#' This is useful because an MCMC method name appears in multiple
#' places
#' 
#' @param MCMCresult One or a named list of `MCMCresult` objects, such
#'   as returned by \code{\link{compareMCMCs}}.
#' 
#' @param newName A new (replacement) name for one of the MCMC method
#'   names
#' 
#' @param oldName An old (existing) name for one of the MCMC method
#'   names
#' 
#' @details This replaces the MCMC label `oldName` with `newName`
#'   anywhere they appear in the `MCMCresult` list.  This includes
#'   various places in the `metrics` elements of the `MCMCresult`
#'   objects.
#' 
#' If `oldName` is omitted, `MCMCresult` must be a single `MCMCresult`
#' object, in which the existing MCMC method name will be replaced by
#' `newName`.  Hence `oldName` is only necessary if `MCMCresult` is a
#' list of `MCMCresult` objects.
#' 
#' @export
renameMCMC <- function(MCMCresult, newName, oldName) {
  if(inherits(MCMCresult, "MCMCresult")) {
    MCMCresult$rename(newName, oldName)
    return(invisible(NULL))
  }
  if(!is.list(MCMCresult))
    stop(paste0("MCMCresult must be an MCMCresult object or list",
                " of MCMCresult objects, such as returned by compareMCMCs."))
  if(missing(oldName))
    stop("MCMCresult is a list, so oldName argument must be provided")
  MCMCresult <- lapply(
    MCMCresult,
    function(oneRes) {
      if(inherits(oneRes, "MCMCresult")) {
        oneRes$rename(newName, oldName)
        oneRes
      } else {
        warning(paste0("Skipping an element of MCMCresult that is",
                       " not an MCMCresult object."))
      }
    })
  names(MCMCresult)[names(MCMCresult) == oldName] <- newName
  MCMCresult
}
