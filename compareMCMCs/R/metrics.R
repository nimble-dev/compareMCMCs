# calculators for basic MCMC metrics

#' Built-in metrics for `MCMCresult` objects
#' 
#' These functions are normally called via compareMCMCs or addMetric.
#' 
#' @param result An MCMCresult object, normally a list element returned by \code{\link{compareMCMCs}}
#' @param options A (metric-specific) list of named control options accepted by some metrics.
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
#' - ESS : effective sample size (ESS).  Control options
#'  include `ESSfun` (a function to estimate ESS, with default = `coda::effectiveSize`),
#'  and `suffix` (a character string to be appended to "ESS" to form a label,
#'  with default = "").
#' - efficiency or (synonomously) efficiency_coda : effective sample size (ESS) and 
#' efficiency (ESS / computation time).  If `ESS` was already calculated, it will not
#' be re-calculated.  Control options include `ESSfun` (passed to `ESS`), `suffix`
#' (a character string to be appended to "efficiency" to form a label,
#'  with default = ""), and `time` (a character string to be used as an expression
#'  to calculate the computation time from elements of the `times` element of the 
#'  `result` object, with default = "sampling" for burning+postburnin times). 
#' 
#' @return A list that may contain elements named:
#' 
#' - `byParameter`: A named list of vectors.  In each vector, 
#' the elements correspond to parameters.  The list names
#' will become parameter names in the `byParameter` element of
#' `metrics` elements in `MCMCresult` objects.
#' - `byMCMC`: A named list of numbers.
#' 
#' It is also valid to return a list of such lists.
#' 
#' In normal use, metrics are called by `addMetrics` (possibly from `compareMCMCs`)
#' and the results are collated in the `metrics` field of `MCMCresult` objects.
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

#' @rdname metrics
#' @export
MCMCmetric_ESS <- function(result, options = NULL) {
  defaults <- list(suffix = "") ## Treat ESSfun separately to avoid 
  ## invoking coda namespace unless it is needed
  ESSfun <- options[["ESSfun"]]
  if(is.null(options)) options <- defaults
  else options <- updateDefaults(defaults, options)
  if(is.null(ESSfun)) {
    if(!requireNamespace('coda', quietly = TRUE))
      stop(paste0('MCMCmetric_ESS requires coda package as default, but it is not installed.\n',
                  'Greater control can be achieved by providing options (or metricOptions).'))
    ESSfun <- coda::effectiveSize
  }
  
  ESSsuff <- paste0("ESS", options[['suffix']])
  if(ESSsuff %in% colnames(result$byParameter)) # It already exists so don't add it again
    return(list())
  ess <- ESSfun(result$samples)
  list(byParameter = structure(list(ess), names = ESSsuff))
}

#' @export
MCMCmetric_efficiency <- function(result, options = NULL) {
  defaults <- list(time = "sampling", suffix = "")
  ESSfun <- options[["ESSfun"]]
  if(is.null(options)) options <- defaults
  else {
    options <- updateDefaults(defaults, options)
    options$ESSfun <- ESSfun
  }
  ESS_needed <- TRUE
  ESSsuff <- paste0("ESS", options$suffix)
  if(ESSsuff %in% colnames(result$byParameter)) {
    # already calculated
    ess <- result$byParameter[, ESSsuff]
    names(ess) <- as.character(result$byParameter$Parameter)
    ESS_needed <- FALSE
  } else {
    ESSmetric <- MCMCmetric_ESS(result, options = options)
    ess <- ESSmetric$byParameter[[ESSsuff]]
  }
  time_denominator <- with(result$times, eval(parse(text = options$time, keep.source = FALSE))) ## This allows options$time to be a string of an expression, like "x+y"
  efficiency <- ess / time_denominator
  efficiencysuff <- paste0("efficiency", options$suffix)
  byParameter <- if(ESS_needed)
                   structure(list(ess, efficiency),
                             names = c(ESSsuff, efficiencysuff))
  else
    structure(list(efficiency), names = efficiencysuff)

  min_efficiencysuff <- paste0("min_efficiency", options$suffix)
  mean_efficiencysuff <- paste0("mean_efficiency", options$suffix)
  list(byParameter = byParameter,
       byMCMC = structure(list(efficiency[which.min(efficiency)],  ## This way to get the min preserves the Parameter name
                               mean(efficiency)),
                          names = c(min_efficiencysuff, mean_efficiencysuff) ) )
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
       ESS = MCMCmetric_ESS,
       ESS_coda = MCMCmetric_ESS,
       efficiency = MCMCmetric_efficiency,
       efficiency_coda = MCMCmetric_efficiency
  )
)
