context("Testing metrics")

test_that("various metrics and comparison pages work", {

  ## with one variable
  results <- list(
    zippy = MCMCresult$new(MCMC = 'zippy'),
    jumpy = MCMCresult$new(MCMC = 'jumpy')
  )

  results$zippy$setSamples(matrix(rnorm(10),
                                  ncol = 1,
                                  dimnames = list(NULL, c('alpha'))))
  results$jumpy$setSamples(matrix(rnorm(20),
                                  ncol = 1,
                                  dimnames = list(NULL, c('alpha'))))

  results$zippy$times$sampling <- 2
  results$jumpy$times$sampling <- 3

  test1 <- MCMCmetric_mean(results$zippy)
  expect_true(is.list(test1))
  expect_identical(names(test1), "byParameter")
  expect_identical(names(test1[[1]]), "mean")
  
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter"))
  addMetrics(results,
             MCMCmetric_mean)
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter", "mean"))
  
  addMetrics(results,
             MCMCmetric_median)
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter", "mean", "median"))
  
  addMetrics(results,
             MCMCmetric_CI95)
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter", "mean", "median", "CI95_low", "CI95_upp"))
  
  ## with two variables
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

  test1 <- MCMCmetric_mean(results$zippy)
  expect_true(is.list(test1))
  expect_identical(names(test1), "byParameter")
  expect_identical(names(test1[[1]]), "mean")
  
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter"))
  addMetrics(results,
             MCMCmetric_mean)
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter", "mean"))
  
  addMetrics(results,
             MCMCmetric_median)
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter", "mean", "median"))
  
  addMetrics(results,
             MCMCmetric_CI95)
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter", "mean", "median", "CI95_low", "CI95_upp"))
  
  test2 <- MCMCmetric_efficiency(results$zippy)
  expect_identical(names(test2$byMCMC), c("min_efficiency", "mean_efficiency"))
  addMetrics(results,
             MCMCmetric_efficiency)
  expect_identical(names(results$zippy$metrics$byParameter), c("MCMC", "Parameter", "mean", "median", "CI95_low", "CI95_upp", "ESS", "efficiency"))
  expect_identical(names(results$zippy$metrics$byMCMC), c("MCMC", "min_efficiency", "mean_efficiency"))
  
  combo <- combineMetrics(results)
  expect_identical(names(combo$byParameter), c("MCMC", "Parameter", "mean", "median", "CI95_low", "CI95_upp", "ESS", "efficiency"))
  expect_identical(names(combo$byMCMC), c("MCMC", "min_efficiency", "mean_efficiency"))
  
  junk <- compareMCMCs:::posteriorSummaryComparisonComponent(combo)
  expect_true(inherits(junk$plottable, "ggplot"))
  # burnin and post-burnin times are absent and this also 
  # tests that those are correctly handled as NAs, leaving empty cells in the output table.
  make_MCMC_comparison_pages(results,
                             pageComponents = list(timing = TRUE, posteriorSummary = TRUE),
                             modelName = 'test model')
  expect_true("test model.html" %in% list.files())
  expect_true("test model_posteriorSummary.jpg" %in% list.files()) 
  
  junk <- compareMCMCs:::minMeanComparisonComponent(combo)
  expect_true(inherits(junk$plottable, "ggplot"))
  
  make_MCMC_comparison_pages(results,
                             pageComponents = list(efficiencySummary = TRUE),
                             modelName = 'test model2')
  expect_true("test model2.html" %in% list.files())
  expect_true("test model2_efficiencySummary.jpg" %in% list.files()) ## and others, not checked
  
  
  make_MCMC_comparison_pages(results,
                             pageComponents = list(efficiencySummary = TRUE,
                                                   posteriorSummary = TRUE),
                             modelName = 'test model3')
  expect_true("test model3.html" %in% list.files())
  expect_true("test model3_posteriorSummary.jpg" %in% list.files()) 
  expect_true("test model3_efficiencySummary.jpg" %in% list.files()) ## and others, not checked
  
  junk <- compareMCMCs:::allParamEfficiencyComparisonComponent(combo)
  expect_true(inherits(junk$plottable, "ggplot"))
  
  junk <-  compareMCMCs:::efficiencyDetailsComparisonComponent(combo)
  expect_true(inherits(junk$plottable, "ggplot"))
  
  junk <- compareMCMCs:::minMeanAllComparisonComponent(combo)
  expect_true(inherits(junk$plottable$minMean, "ggplot"))
  expect_true(inherits(junk$plottable$allParams, "ggplot"))
  
  make_MCMC_comparison_pages(results,
                             modelName = 'test model4')
  expect_true("test model4.html" %in% list.files())
  expect_true("test model4_posteriorSummary.jpg" %in% list.files()) 
  expect_true("test model4_efficiencySummaryAll.jpg" %in% list.files()) 
  expect_true("test model4_efficiencyDetails.jpg" %in% list.files()) 
  expect_true("test model4_paceSummaryAll.jpg" %in% list.files()) ## and others, not checked
}
)
