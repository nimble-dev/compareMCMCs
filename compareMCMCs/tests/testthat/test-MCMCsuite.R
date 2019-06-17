context("Testing MCMCsuite")

test_that("MCMCsuite works") {
  mc <- nimbleCode({a ~ dnorm(0,1)})
  res <- MCMCsuite(code = mc,
                   MCMCs = c("nimble"))
}


res <- MCMCsuite(code = mc,
                 MCMCs = c("jags"),
                 inits = list(a = 1))

debug(MCMCdef_jags_impl)
res <- runMCMCs(list(code = mc),
                MCMCs = c('jags', 'nimble'),
                MCMCinfo = list(inits = list(a = 1),
                                niter = 2000,
                                burnin = 100))
