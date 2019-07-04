#' @export
MCMCdef_dummy <- function(MCMCinfo, 
                          MCMCcontrol, 
                          monitorInfo, 
                          modelInfo) {
  MCMCdef_dummy_impl(MCMCinfo,
                     MCMCcontrol, 
                     monitorInfo, 
                     modelInfo)
}

#' @export
MCMCdef_dummy_impl <- function(MCMCinfo,
                               MCMCcontrol, 
                               monitorInfo, 
                               modelInfo) {
  dummy_results <- matrix(
    rnorm(length(monitorInfo$monitorVars) * MCMCcontrol$niter),
    nrow = MCMCcontrol$niter,
    dimnames = list(NULL, monitorInfo$monitorVars)
  )
  result <- MCMCresult$new(samples = dummy_results,
                           times = list(sample = 60))
  return(result)
}
