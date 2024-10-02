test_that("MCMC engine registration works", {
  myMCMCengine <- function(MCMCinfo, MCMCcontrol, monitorInfo, modelInfo) {
    # This is a dummy MCMC engine that simply returns columns of rnorm draws and fills in some dummy timing values
    nCols <- length(monitorInfo$monitors)
    nRows <- floor((MCMCcontrol$niter - MCMCcontrol$burnin) / MCMCcontrol$thin)
    samples <- matrix(rnorm(nRows * nCols), ncol = nCols)
    colnames(samples) <- monitorInfo$monitors
    result <- MCMCresult$new(samples = samples,
                             times = list(
                               setup = 0.1, # assume the setup time is 
                               sampling = (MCMCcontrol$niter - MCMCcontrol$nburnin) * 1e-4, # assume an iteration takes 1e-4 seconds
                               burnin = MCMCcontrol$nburnin * 1e-4,
                               postburnin = MCMCcontrol$niter * 1e-4))
    result
  }
  registerMCMCengine("myMCMCengine", myMCMCengine)
  results <- compareMCMCs(
    MCMCcontrol = list(niter = 100000, nburnin = 10000, thin = 2),
    MCMCs = "myMCMCengine",
    monitors = letters[1:5]
  )
  expect_true(inherits(results[["myMCMCengine"]], "MCMCresult"))
})
