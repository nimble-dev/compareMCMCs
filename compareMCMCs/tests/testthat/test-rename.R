context("Testing dummy MCMCdef")

test_that("compareMCMCs works", {
  res1 <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                     MCMCs = c('dummy'),
                                     monitors = c("param1", "param2"),
                                     MCMCcontrol = list(niter = 2000,
                                                        burnin = 100))
  res2 <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                     MCMCs = c('dummy'),
                                     monitors = c("param1", "param2"),
                                     MCMCcontrol = list(niter = 2000,
                                                        burnin = 100))
  compareMCMCs:::renameMCMC(res2$dummy, "dummy2")
  
  expect_true(is.list(res2))
  expect_identical(names(res2), c("dummy"))
  expect_true(inherits(res2[['dummy']], "MCMCresult"))
  expect_true(inherits(res2$dummy$metrics$byParameter, "data.frame"))
  expect_true(identical(as.character(unique(res2$dummy$metrics$byParameter$MCMC)), "dummy2"))
  
  combo <- c(res1, res2)

  res3 <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                     MCMCs = c('dummy'),
                                     monitors = c("param1", "param2"),
                                     MCMCcontrol = list(niter = 2000,
                                                        burnin = 100))

  res3 <- compareMCMCs:::renameMCMC(res3, "dummy3", "dummy")
  expect_identical(names(res3), c("dummy3"))
  expect_true(identical(as.character(unique(res3$dummy3$metrics$byParameter$MCMC)), "dummy3"))
  
  combo2 <- c(res1, res3)
  combo2b <- compareMCMCs:::renameMCMC(combo2, "dummy3b", "dummy3")
  expect_identical(names(combo2b), c("dummy", "dummy3b"))
  expect_true(identical(as.character(unique(combo2b$dummy3b$metrics$byParameter$MCMC)), "dummy3b"))
  }
)