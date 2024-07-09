
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

##   res <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
##                                     MCMCs = c('dummy'),
##                                     monitors = paste0("x[", 1:20, "]"),
##                                     MCMCcontrol = list(niter = 2000))
## make_MCMC_comparison_pages(res, modelName = "dummy")
## browseURL(file.path(tempdir(), "dummy.html"))
