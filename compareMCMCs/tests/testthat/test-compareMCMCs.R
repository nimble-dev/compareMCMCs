context("Testing compareMCMCs")

test_that("compareMCMCs works", {
  mc <- nimbleCode({a ~ dnorm(0,1)})
  res <- compareMCMCs::compareMCMCs(list(code = mc),
                                    MCMCs = c('jags', 'nimble'),
                                    MCMCcontrol = list(inits = list(a = 1),
                                                       niter = 2000,
                                                       burnin = 100))
  expect_true(is.list(res))
  expect_identical(names(res), c("jags", "nimble"))
  expect_true(inherits(res[['nimble']], "MCMCresult"))
  expect_true(inherits(res[['jags']], "MCMCresult"))
}
)