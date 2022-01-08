.onLoad <- function(libname, pkgname) {
  registerMCMCengine('jags', MCMCdef_jags)
  registerMCMCengine('dummy', MCMCdef_dummy)
  registerMCMCengine('stan', MCMCdef_stan)
  
  registerMetrics(
    list(mean = MCMCmetric_mean,
         median = MCMCmetric_median,
         sd = MCMCmetric_sd,
         CI95_low = MCMCmetric_CI95low,
         CI95_upp = MCMCmetric_CI95upp,
         CI95 = MCMCmetric_CI95,
         ESS = MCMCmetric_ESS,
         ESS_coda = MCMCmetric_ESS,
         efficiency = MCMCmetric_efficiency,
         efficiency_coda = MCMCmetric_efficiency
         )
  )
  ## Set up library for building page components
  registerPageComponents(
    list(
      timing = list(
        make = 'timeComparisonComponent',
        linkText = "MCMC sampling time"),
      efficiencySummary = list(
        make = 'minMeanComparisonComponent',
        fileSuffix = "_efficiencySummary",
        linkText = "MCMC efficiency summary",
        control = list(
          invert = FALSE,  # default, but provided explicitly for clarity
          min_efficiency_name = "min_efficiency", 
          mean_efficiency_name = "mean_efficiency",
          suffix = "")),
      efficiencySummaryAllParams = list(
        make = 'minMeanAllComparisonComponent',
        fileSuffix = "_efficiencySummaryAll",
        linkText = "MCMC efficiency summary (with all parameters)",
        plot = 'plotMinMeanAll',
        control = list(
          invert = FALSE,  # default, but provided explicitly for clarity
          min_efficiency_name = "min_efficiency", 
          mean_efficiency_name = "mean_efficiency",
          efficiency_name = "efficiency",
          suffix = "")),
      paceSummaryAllParams = list(
        make = 'minMeanAllComparisonComponent',
        fileSuffix = "_paceSummaryAll",
        linkText = "MCMC pace summary (with all parameters)",
        plot = 'plotMinMeanAll',
        control = list(invert = TRUE)),
      efficiencyDetails = list
      (make = 'efficiencyDetailsComparisonComponent',
        fileSuffix = "_efficiencyDetails",
        linkText = "MCMC efficiency details",
        control = list(
          ncol = 4,
          efficiencyName = "efficiency",
          suffix = "")),
      posteriorSummary = list(
        make = 'posteriorSummaryComparisonComponent',
        fileSuffix = "_posteriorSummary",
        linkText = "Posterior summaries",
        control = list(ncol = 4)))
  )
}
