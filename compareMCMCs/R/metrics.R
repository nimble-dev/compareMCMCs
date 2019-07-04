## calculators for basic MCMC metrics
#' @export
MCMCmetric_mean <- function(result) {
  ## Results can be a single MCMCresult or a list of them.
  means <- apply(result$samples, 2, mean)
  list(byParameter = list(mean = means))
}

#' @export
MCMCmetric_median <- function(result) {
  res <- apply(result$samples, 2, median)
  list(byParameter = list(median = res))
}

#' @export
MCMCmetric_sd <- function(result) {
  res <- apply(result$samples, 2, sd)
  list(byParameter = list(sd = res))
}

#' @export
MCMCmetric_CI95 <- function(result) {
  c(MCMCmetric_CI95low(result),
    MCMCmetric_CI95upp(result)
  )
}

#' @export
MCMCmetric_CI95low <- function(result) {
  low <- apply(result$samples, 2, quantile, probs = 0.025)
  list(byParameter = list(CI95_low = low))
}

#' @export
MCMCmetric_CI95upp <- function(result) {
  upp <- apply(result$samples, 2, quantile, probs = 0.975)
  list(byParameter = list(CI95_upp = upp))
}


#' @export
MCMCmetric_efficiency_coda <- function(result) {
  if(!requireNamespace('coda', quietly = TRUE))
    stop('MCMCmetric_ESScoda requires coda package, but it is not installed.')
  ess <- coda::effectiveSize(result$samples)
  efficiency <- ess / result$times$sample[3]
  list(byParameter = list(ESS_coda = ess,
                          efficiency_coda = efficiency),
       byMCMC = list(min_efficiency_coda = efficiency[which.min(efficiency)],  ## This way to get the min preserves the Parameter name
                       mean_efficiency_coda = mean(efficiency)))
}

compareMCMCs_registered_metrics <- new.env()

#' @export
registerMetrics <- function(metrics) {
  list2env(metrics, compareMCMCs_registered_metrics)
}

#' @export
unregisterMetric <- function(name) {
  rm(list = name, envir = compareMCMCs_registered_metrics)
}

#' @export
getMetrics <- function() {
  compareMCMCs_registered_metrics
}

registerMetrics(
  list(mean = MCMCmetric_mean,
       median = MCMCmetric_median,
       sd = MCMCmetric_sd,
       CI95_low = MCMCmetric_CI95low,
       CI95_upp = MCMCmetric_CI95upp,
       CI95 = MCMCmetric_CI95,
       efficiency = MCMCmetric_efficiency_coda,
       efficiency_coda = MCMCmetric_efficiency_coda
  )
)
