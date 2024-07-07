test_that("various metrics and comparison pages work", {

  ## with one variable
  results <- list(
    zippy = MCMCresult$new(MCMC = 'zippy'),
    jumpy = MCMCresult$new(MCMC = 'jumpy')
  )

  # set up results object as it would be returned by compareMCMCs
  results$zippy$setSamples(matrix(rnorm(10),
                                  ncol = 1,
                                  dimnames = list(NULL, c('alpha'))))
  results$jumpy$setSamples(matrix(rnorm(20),
                                  ncol = 1,
                                  dimnames = list(NULL, c('alpha'))))

  results$zippy$times$sampling <- 2
  results$jumpy$times$sampling <- 3

  # test individual metric function
  test1 <- MCMCmetric_mean(results$zippy)
  expect_true(is.list(test1))
  expect_identical(names(test1), "byParameter")
  expect_identical(names(test1[[1]]), "mean")
  
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter"))
  # test adding individual metric to results
  addMetrics(results,
             MCMCmetric_mean)
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "mean"))
  
  addMetrics(results,
             MCMCmetric_median)
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "mean", "median"))
  
  addMetrics(results,
             MCMCmetric_CI95)
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "mean", "median",
                     "CI95_low", "CI95_upp"))
  
  # test clearing metrics
  clearMetrics(results)
  expect_identical(names(results$zippy$metrics$byParameter), 
                   c("MCMC", "Parameter"))
  
  # re-add the metrics to keep going below
  addMetrics(results,
             MCMCmetric_median)
  addMetrics(results,
             MCMCmetric_CI95)
  
  ## Check that things work
  ## with two variables
  # Make another results object as would be returned by compareMCMCs
  results <- list(
    zippy = MCMCresult$new(MCMC = 'zippy'),
    jumpy = MCMCresult$new(MCMC = 'jumpy')
  )

  results$zippy$setSamples(matrix(rnorm(20),
                                  ncol = 2,
                                  dimnames = list(NULL, c('alpha', 'beta'))))
  results$jumpy$setSamples(matrix(rnorm(40),
                                  ncol = 2,
                                  dimnames = list(NULL, c('alpha', 'beta'))))

  results$zippy$times$sampling <- 2
  results$jumpy$times$sampling <- 3

  # test individual metric
  test1 <- MCMCmetric_mean(results$zippy)
  expect_true(is.list(test1))
  expect_identical(names(test1), "byParameter")
  expect_identical(names(test1[[1]]), "mean")
  
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter"))
  # test adding metrics to results
  addMetrics(results,
             MCMCmetric_mean)
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "mean"))
  
  addMetrics(results,
             MCMCmetric_median)
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "mean", "median"))
  
  addMetrics(results,
             MCMCmetric_CI95)
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "mean", "median",
                     "CI95_low", "CI95_upp"))
  
  # test clearing metrics
  clearMetrics(results)
  expect_identical(names(results$zippy$metrics$byParameter), 
                   c("MCMC", "Parameter"))
  
  # test registering and unregistering new metrics
  all_metrics <- getMetrics()
  expect_true(is.environment(all_metrics))
  expect_true(all(c("CI95", "ESS", "median") %in% ls(all_metrics))) # A few representative names to check
  # from vignette
  MCMCmetric_quartiles <- function(result, options) {
    p25 <- apply(result$samples, 2, quantile, probs = 0.25)
    p75 <- apply(result$samples, 2, quantile, probs = 0.75)
    ## q25 and q75 are named vectors with names matching model parameters
    ## i.e. column names of result$samples
    maxDiff <- max(p75-p25)
    list(byParameter = list(p25 = p25,
                            p75 = p75),
         byMCMC = list(maxQuartileDiff = maxDiff))
  }
  addMetrics(results, list(MCMCmetric_quartiles))
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "p25", "p75"))
  clearMetrics(results)
  registerMetrics(
    list(quartiles = MCMCmetric_quartiles)
  )
  expect_true("quartiles" %in% ls(getMetrics()))
  addMetrics(results, "quartiles")
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "p25", "p75"))
  clearMetrics(results)
  unregisterMetric("quartiles")
  expect_false("quartiles" %in% ls(getMetrics()))
  
  # add metrics again to keep going below
  addMetrics(results,
             MCMCmetric_mean)
  addMetrics(results,
             MCMCmetric_median)
  addMetrics(results,
             MCMCmetric_CI95)

  # test efficiency metric
  test2 <- MCMCmetric_efficiency(results$zippy)
  expect_identical(names(test2$byMCMC), c("min_efficiency", "mean_efficiency"))
  addMetrics(results,
             MCMCmetric_efficiency)
  expect_identical(names(results$zippy$metrics$byParameter),
                   c("MCMC", "Parameter", "mean", "median",
                     "CI95_low", "CI95_upp", "ESS", "efficiency"))
  expect_identical(names(results$zippy$metrics$byMCMC),
                   c("MCMC", "min_efficiency", "mean_efficiency"))
  # test efficiency metric called after ESS has been added (which was done by efficiency metric just added)
  # When efficiency metric is called in this way, the existing ESS result should be used, so this exercised another code branch.
  test2b <- MCMCmetric_efficiency(results$zippy)
  expect_identical(test2$byParameter$efficiency, test2b$byParameter$efficiency)
  expect_identical(test2$byMCMC, test2b$byMCMC)

  # test combining metrics
  combo <- combineMetrics(results)
  expect_identical(names(combo$byParameter),
                   c("MCMC", "Parameter", "mean", "median",
                     "CI95_low", "CI95_upp", "ESS", "efficiency"))
  expect_identical(names(combo$byMCMC), c("MCMC",
                                          "min_efficiency",
                                          "mean_efficiency"))
  
  # test including times when combining metrics
  combo_with_times <- combineMetrics(results, include_times = TRUE)
  expect_identical(names(combo_with_times$byParameter),
                   c("MCMC", "Parameter", "mean", "median",
                     "CI95_low", "CI95_upp", "ESS", "efficiency"))
  expect_identical(names(combo_with_times$byMCMC), c("MCMC",
                                                     "min_efficiency",
                                                     "mean_efficiency"))
  expect_identical(as.numeric(combo_with_times$times[,3]), c(2, 3))
  
  # test comparison components and comparison pages
  junk <- compareMCMCs:::posteriorSummaryComparisonComponent(combo)
  expect_true(inherits(junk$plottable, "ggplot"))
  # burnin and post-burnin times are absent and this also tests that
  # those are correctly handled as NAs, leaving empty cells in the
  # output table.
  make_MCMC_comparison_pages(results,
                             dir = tempdir(),
                             pageComponents = list(
                               timing = TRUE, posteriorSummary = TRUE),
                             modelName = 'test model')
  expect_true("test model.html" %in% list.files(tempdir()))
  expect_true("test model_posteriorSummary.jpg" %in% list.files(tempdir()))

  make_MCMC_comparison_pages(results,
                             dir = tempdir(),
                             modelName = 'test model all')
  expect_true("test model all.html" %in% list.files(tempdir()))
  expect_true("test model all_posteriorSummary.jpg" %in% list.files(tempdir()))
  
  
  results_rep <- list(
    zippy2 = MCMCresult$new(MCMC = 'zippy'),
    jumpy2 = MCMCresult$new(MCMC = 'jumpy')
  )
  
  results_rep$zippy2$setSamples(matrix(rnorm(20),
                                  ncol = 2,
                                  dimnames = list(NULL, c('alpha', 'beta'))))
  results_rep$jumpy2$setSamples(matrix(rnorm(40),
                                  ncol = 2,
                                  dimnames = list(NULL, c('alpha', 'beta'))))
  results_rep$zippy2$times$sampling <- 2.5
  results_rep$jumpy2$times$sampling <- 3.5
  addMetrics(results_rep,
             MCMCmetric_mean)
  addMetrics(results_rep,
             MCMCmetric_median)
  addMetrics(results_rep,
             MCMCmetric_CI95)
  addMetrics(results_rep,
             MCMCmetric_efficiency)
  results_both <- c(results, results_rep)
  make_MCMC_comparison_pages(results_both,
                             dir = tempdir(),
                             modelName = 'test model all rep')
  expect_true("test model all rep.html" %in% list.files(tempdir()))
  expect_true("test model all rep_posteriorSummary.jpg" %in% list.files(tempdir()))
  
  junk <- compareMCMCs:::minMeanComparisonComponent(combo)
  expect_true(inherits(junk$plottable, "ggplot"))
  
  make_MCMC_comparison_pages(results,
                             dir = tempdir(),
                             pageComponents = list(efficiencySummary = TRUE),
                             modelName = 'test model2')
  expect_true("test model2.html" %in% list.files(tempdir()))
  expect_true("test model2_efficiencySummary.jpg" %in% list.files(tempdir()))
  
  make_MCMC_comparison_pages(results,
                             dir = tempdir(),
                             pageComponents = list(efficiencySummary = TRUE,
                                                   posteriorSummary = TRUE),
                             modelName = 'test model3')
  expect_true("test model3.html" %in% list.files(tempdir()))
  expect_true("test model3_posteriorSummary.jpg" %in% list.files(tempdir()))
  expect_true("test model3_efficiencySummary.jpg" %in% list.files(tempdir()))
  
  junk <- compareMCMCs:::allParamEfficiencyComparisonComponent(combo)
  expect_true(inherits(junk$plottable, "ggplot"))
  
  junk <-  compareMCMCs:::efficiencyDetailsComparisonComponent(combo)
  expect_true(inherits(junk$plottable, "ggplot"))
  
  junk <- compareMCMCs:::minMeanAllComparisonComponent(combo)
  expect_true(inherits(junk$plottable$minMean, "ggplot"))
  expect_true(inherits(junk$plottable$allParams, "ggplot"))
  
  make_MCMC_comparison_pages(results,
                             dir = tempdir(),
                             modelName = 'test model4')
  expect_true("test model4.html" %in% list.files(tempdir()))
  expect_true("test model4_posteriorSummary.jpg" %in% list.files(tempdir()))
  expect_true("test model4_efficiencySummaryAll.jpg" %in% list.files(tempdir()))
  expect_true("test model4_efficiencyDetails.jpg" %in% list.files(tempdir()))
  expect_true("test model4_paceSummaryAll.jpg" %in% list.files(tempdir()))
}
)

test_that("byParameter sort order is natural", {
  paramNames <- paste0("x[", 1:20, "]")
  res <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                    MCMCs = c('dummy'),
                                    monitors = paramNames,
                                    MCMCcontrol = list(niter = 2000))
  expect_identical(as.character(res$dummy$metrics$byParameter$Parameter),
                   paramNames)
})

test_that("combineMetrics control over params and MCMCs works", {
  paramNames <- paste0("x[", 1:20, "]")
  res <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                    MCMCs = c('dummy'),
                                    monitors = paramNames,
                                    MCMCcontrol = list(niter = 2000))

  res2 <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                    MCMCs = c('dummy'),
                                    monitors = paramNames,
                                    MCMCcontrol = list(niter = 2000))
  res2 <- renameMCMC(res2, "dummy2", "dummy")

  res3 <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                    MCMCs = c('dummy'),
                                    monitors = paramNames,
                                    MCMCcontrol = list(niter = 2000))
  res3 <- renameMCMC(res3, "dummy3", "dummy")

  # The "plot='xyzzy'" is a hidden egg to support this testing by
  # returning the result of combineMetrics inside make_MCMC_comparison_pages.
  # The reason to test this is that the Filter arguments are hand-propagated
  # as expressions.

  both <- c(res, res2, res3)
  f1 <- combineMetrics(both, params = c("x[1]", "x[13]"), include_times=TRUE)
  expect_true(all(as.character(f1$byParameter$Parameter) %in% c("x[1]", "x[13]")))
  f1b <- make_MCMC_comparison_pages(both, params = c("x[1]", "x[13]"), plot="xyzzy")
  expect_identical(f1, f1b)

  f2 <- combineMetrics(both, paramFilter = Parameter %in% c("x[1]", "x[13]"),
                       include_times=TRUE)
  expect_true(all(as.character(f2$byParameter$Parameter) %in% c("x[1]", "x[13]")))
  f2b <- make_MCMC_comparison_pages(both,
                                    paramFilter = Parameter %in% c("x[1]", "x[13]"),
                                    plot = "xyzzy")
  expect_identical(f2, f2b)

  f3 <- combineMetrics(both,
                       params = paramNames[1:10],
                       paramFilter = grepl("1", Parameter),
                       include_times=TRUE)
  expect_true(all(as.character(f3$byParameter$Parameter) %in% c("x[1]", "x[10]")))
  f3b <- make_MCMC_comparison_pages(both,
                                    params = paramNames[1:10],
                                    paramFilter = grepl("1", Parameter),
                                    plot = "xyzzy")
  expect_identical(f3, f3b)

  f4 <- combineMetrics(both, MCMCs = c("dummy2", "dummy"), include_times=TRUE)
  expect_true(all(rownames(f4$times) %in% c("dummy", "dummy2")))
  expect_true(all(as.character(f4$byMCMC$MCMC) %in% c("dummy", "dummy2")))
  expect_true(all(as.character(f4$byParameter$MCMC) %in% c("dummy", "dummy2")))
  f4b <- make_MCMC_comparison_pages(both,
                                    MCMCs = c("dummy2", "dummy"),
                                    plot = "xyzzy")
  expect_identical(f4, f4b)

  f5 <- combineMetrics(both, MCMCFilter = MCMC %in% c("dummy2", "dummy"),
                       include_times=TRUE)
  expect_true(all(rownames(f5$times) %in% c("dummy", "dummy2")))
  expect_true(all(as.character(f5$byMCMC$MCMC) %in% c("dummy", "dummy2")))
  expect_true(all(as.character(f5$byParameter$MCMC) %in% c("dummy", "dummy2")))
  f5b <- make_MCMC_comparison_pages(both,
                                    MCMCFilter = MCMC %in% c("dummy2", "dummy"),
                                    plot = "xyzzy")
  expect_identical(f5, f5b)

  f6 <- combineMetrics(both, MCMCs = c("dummy", "dummy3"),
                       MCMCFilter = MCMC %in% c("dummy"),
                       include_times=TRUE)
  expect_true(all(rownames(f6$times) %in% c("dummy")))
  expect_true(all(as.character(f6$byMCMC$MCMC) %in% c("dummy")))
  expect_true(all(as.character(f6$byParameter$MCMC) %in% c("dummy")))
  f6b <- make_MCMC_comparison_pages(both,
                                    MCMCs = c("dummy2", "dummy"),
                                    MCMCFilter = MCMC %in% c("dummy"),
                                    plot = "xyzzy")
  expect_identical(f6, f6b)
})
