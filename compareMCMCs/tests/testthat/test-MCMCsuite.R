context("Testing MCMCsuite")

test_that("MCMCsuite works") {
  mc <- nimbleCode({a ~ dnorm(0,1)})
  res <- compareMCMCs::MCMCsuite(code = mc,
                                 inits = list(a = 1),
                                 MCMCs = c("nimble", "jags"))
  expect_true(is.list(res))
  expect_identical(names(res), c("jags", "nimble"))
  expect_true(inherits(res[['nimble']], "MCMCresult"))
  expect_true(inherits(res[['jags']], "MCMCresult"))
}
