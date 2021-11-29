context("Testing dummy MCMCdef")

test_that("compareMCMCs works", {
  res <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                    MCMCs = c('dummy'),
                                    monitors = c("param1", "param2"),
                                    MCMCcontrol = list(niter = 2000,
                                                       burnin = 100))
  expect_true(is.list(res))
  expect_identical(names(res), c("dummy"))
  expect_true(inherits(res[['dummy']], "MCMCresult"))
  expect_true(inherits(res$dummy$metrics$byParameter, "data.frame"))
  expect_true(
    identical(as.character(unique(res$dummy$metrics$byParameter$MCMC)),
              "dummy"))
}
)
