context("Testing metrics")

test_that("mean metric works") {

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

  results$zippy$times$samples <- 2
  results$jumpy$times$samples <- 3

  MCMCmetric_mean(results$zippy)

  ##undebug(addMetric)
  debugonce(results[[1]]$addMetricResult)
  compareMCMCs:::addMetrics(results,
                            MCMCmetric_mean)
  addMetric(results,
            MCMCmetric_median)
  addMetric(results,
            MCMCmetric_CI95)


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

  results$zippy$times$samples <- 2
  results$jumpy$times$samples <- 3

  MCMCmetric_mean(results$zippy)

  ##undebug(addMetric)
  debugonce(results[[1]]$addMetricResult)
  compareMCMCs:::addMetrics(results,
            MCMCmetric_mean)
  addMetric(results,
            MCMCmetric_median)
  addMetric(results,
            MCMCmetric_CI95)

  MCMCmetric_efficiency_coda(results$zippy)
  addMetric(results,
            MCMCmetric_efficiency_coda)

  combo <- combineMetrics(results)
  ##debug(posteriorSummaryComparisonComponent)
  junk <- posteriorSummaryComparisonComponent(combo)
  make_MCMC_comparison_pages(results,
                             pageComponents = list(posteriorSummary = TRUE),
                             modelName = 'test model')

  ##debug(minMeanComparisonComponent)
  junk <- minMeanComparisonComponent(combo)

  make_MCMC_comparison_pages(results,
                             pageComponents = list(efficiencySummary = TRUE),
                             modelName = 'test model')

  make_MCMC_comparison_pages(results,
                             pageComponents = list(efficiencySummary = TRUE,
                                                   posteriorSummary = TRUE),
                             modelName = 'test model')

  debug(allParamEfficiencyComparisonComponent)
  junk <- allParamEfficiencyComparisonComponent(combo)

  junk <- efficiencyDetailsComparisonComponent(combo)
  junk <- minMeanAllComparisonComponent(combo)
  make_MCMC_comparison_pages(results,
                             modelName = 'test model')
}
