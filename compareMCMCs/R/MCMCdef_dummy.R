#' @rdname builtin_MCMCs
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

MCMCdef_dummy_impl <- function(MCMCinfo,
                               MCMCcontrol, 
                               monitorInfo, 
                               modelInfo) {
  if(length(monitorInfo$monitorVars) == 0) {
    warning("No monitorVars provided to dummy MCMCdef.  Using 'param'.")
    monitorInfo$monitorVars <- 'param'
  }
  dummy_results <- matrix(
    stats::rnorm(length(monitorInfo$monitors) * MCMCcontrol$niter),
    nrow = MCMCcontrol$niter,
    dimnames = list(NULL, monitorInfo$monitors)
  )
  dummytime <- 0.1
  dummytime[1:5] <- c(60, 60, 60, 0, 0)
  result <- MCMCresult$new(samples = dummy_results,
                           times = list(setup = 60,
                                        sampling = 120,
                                        burnin = 60, 
                                        postburnin = 60))
  return(result)
}
