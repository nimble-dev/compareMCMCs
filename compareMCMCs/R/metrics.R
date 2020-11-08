# calculators for basic MCMC metrics

#' Built-in metrics for `MCMCresult` objects
#' 
#' These functions are normally called via compareMCMCs or addMetric.
#' 
#' @param result An MCMCresult object, normally a list element returned by \code{\link{compareMCMCs}}
#' 
#' @details 
#' 
#' A metric is a summary of MCMC output.  The summary may include results for each parameter, 
#' for each MCMC sample (across all parameters), and/or by arbitrary list. The last option is 
#' not used by any built-in metrics.
#' 
#' The built-in metrics include:
#' 
#' - mean : mean for each parameter
#' - median : median for each parameter
#' - sd : standard deviation for each parameter
#' - CI95 : both ends of 95% credible interval, a combination of CI95low and CI95upp
#' - CI95low : lower end of 95% credible interval
#' - CI95upp : upper end of 95% credible interval
#' - efficiency or (synonomously) efficiency_coda : effective sample size (ESS) and efficiency (ESS / computation time)
#' 
#' @return A list that may contain elements named:
#' 
#' - `byParameter`: A named list of vectors.  In each vector, 
#' the elements correspond to parameters.  The list names
#' will become names in any outputs.
#' - `byMCMC`: A named list of numbers.
#' 
#' It is also valid to return a list of such lists.
#' 
#' @name metrics
NULL

#' @rdname metrics
#' @export
MCMCmetric_mean <- function(result, ...) {
  ## Results can be a single MCMCresult or a list of them.
  means <- apply(result$samples, 2, mean)
  list(byParameter = list(mean = means))
}

#' @rdname metrics
#' @export
MCMCmetric_median <- function(result, ...) {
  res <- apply(result$samples, 2, median)
  list(byParameter = list(median = res))
}

#' @rdname metrics
#' @export
MCMCmetric_sd <- function(result, ...) {
  res <- apply(result$samples, 2, sd)
  list(byParameter = list(sd = res))
}

#' @rdname metrics
#' @export
MCMCmetric_CI95 <- function(result, ...) {
  list(byParameter = c(MCMCmetric_CI95low(result, ...)$byParameter,
                       MCMCmetric_CI95upp(result, ...)$byParameter)
  )
}

#' @rdname metrics
#' @export
MCMCmetric_CI95low <- function(result, ...) {
  low <- apply(result$samples, 2, quantile, probs = 0.025)
  list(byParameter = list(CI95_low = low))
}

#' @rdname metrics
#' @export
MCMCmetric_CI95upp <- function(result, ...) {
  upp <- apply(result$samples, 2, quantile, probs = 0.975)
  list(byParameter = list(CI95_upp = upp))
}

MCMCmetric_efficiency_internal <- function(result,
                                           effectiveSizeFun,
                                           timeName,
                                           suffix) {
  
  ess <- effectiveSizeFun(result$samples)
  efficiency <- ess / result$times[[timeName]]
  byParamNames <- paste0(c("ESS", "efficiency"), suffix)
  byMCMCNames <- paste0(c("min_efficiency", "mean_efficiency"), suffix)
  list(byParameter = structure(list(ess,
                                    efficiency),
                               names = byParamNames),
       byMCMC = structure(list(efficiency[which.min(efficiency)],  ## This way to get the min preserves the Parameter name
                               mean(efficiency)),
                          names = byMCMCNames)
  )
}

#' @rdname metrics
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

MCMCmetric_efficiency_coda_sample_total <- function(result) {
  if(!requireNamespace('coda', quietly = TRUE))
    stop('MCMCmetric_ESScoda requires coda package, but it is not installed.')
  MCMCmetric_efficiency_internal(result,
                                 coda::effectiveSize,
                                 'sample_total',
                                 '_coda_total')
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
