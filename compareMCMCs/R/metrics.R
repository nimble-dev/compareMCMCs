## calculators for basic MCMC metrics
#' @export
MCMCmetric_mean <- function(result, ...) {
  ## Results can be a single MCMCresult or a list of them.
  means <- apply(result$samples, 2, mean)
  list(byParameter = list(mean = means))
}

#' @export
MCMCmetric_median <- function(result, ...) {
  res <- apply(result$samples, 2, median)
  list(byParameter = list(median = res))
}

#' @export
MCMCmetric_sd <- function(result, ...) {
  res <- apply(result$samples, 2, sd)
  list(byParameter = list(sd = res))
}

#' @export
MCMCmetric_CI95 <- function(result, ...) {
  list(byParameter = c(MCMCmetric_CI95low(result, ...)$byParameter,
                       MCMCmetric_CI95upp(result, ...)$byParameter)
  )
}

#' @export
MCMCmetric_CI95low <- function(result, ...) {
  low <- apply(result$samples, 2, quantile, probs = 0.025)
  list(byParameter = list(CI95_low = low))
}

#' @export
MCMCmetric_CI95upp <- function(result, ...) {
  upp <- apply(result$samples, 2, quantile, probs = 0.975)
  list(byParameter = list(CI95_upp = upp))
}

#' @export
MCMCmetric_ESS_coda <- function(result, ...) {
  if("ESS_coda" %in% colnames(result$byParameter))
    return(list())
  if(!requireNamespace('coda', quietly = TRUE))
    stop('MCMCmetric_ESScoda requires coda package, but it is not installed.')
  ess <- coda::effectiveSize(result$samples)
  list(byParameter = list(ESS_coda = ess))
}

#' @export
MCMCmetric_efficiency_coda <- function(result, options = NULL) {
  if(is.null(options))
    options <- list(time = "sampling")
  ESS_needed <- TRUE
  if("ESS_coda" %in% colnames(result$byParameter)) {
    # already calculated
    ess <- result$byParameter[, "ESS_coda"]
    names(ess) <- as.character(result$byParameter$Parameter)
    ESS_needed <- FALSE
  } else {
    ESSmetric <- MCMCmetric_ESS_coda(result, options = options)
    ess <- ESSmetric$byParameter$ESS_coda
  }
  if(options$time == "sampling")
    times <- result$times$sampling
  if(options$time == "total")
    times <- result$times$setup + result$times$sampling
  efficiency <- ess / times
  byParameter <- if(ESS_needed)
    list(ESS_coda = ess,
         efficiency_coda = efficiency)
  else
    list(efficiency_coda = efficiency)
  
  list(byParameter = byParameter,
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
       ESS = MCMCmetric_ESS_coda,
       ESS_coda = MCMCmetric_ESS_coda,
       efficiency = MCMCmetric_efficiency_coda,
       efficiency_coda = MCMCmetric_efficiency_coda
  )
)
