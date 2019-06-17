#' @export
MCMCdef_dummy <- function(runInfo, MCMCinfo, otherInfo) {
  MCMCdef_dummy_impl(runInfo, MCMCinfo, otherInfo)
}

#' @export
MCMCdef_dummy_impl <- function(runInfo, MCMCinfo, otherInfo) {
  dummy_results <- matrix(
    rnorm(length(otherInfo$monitorVars) * MCMCinfo$niter),
    nrow = MCMCinfo$niter,
    dimnames = list(NULL, otherInfo$monitorVars)
  )
  result <- MCMCresult$new(samples = dummy_results,
                           times = list(sample = 60))
  return(result)
}
