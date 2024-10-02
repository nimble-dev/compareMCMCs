# calculators for basic MCMC metrics

#' Built-in metrics for `MCMCresult` objects
#' 
#' These functions are normally called via compareMCMCs or addMetric.
#' 
#' @param result An MCMCresult object, normally a list element
#'   returned by \code{\link{compareMCMCs}}
#' @param options A (metric-specific) list of named control options
#'   accepted by some metrics.
#' @param ... Possible additional arguments to metric functions.
#' 
#' @details 
#' 
#' A metric is a summary of MCMC output.  The summary may include
#' results for each parameter, for each MCMC sample (across all
#' parameters), and/or by arbitrary list. The last option is not used
#' by any built-in metrics.
#' 
#' The built-in metrics include:
#' 
#' - mean : mean for each parameter
#' - median : median for each parameter
#' - sd : standard deviation for each parameter
#'
#' - CI95 : both ends of 95% credible interval, a combination of
#' CI95low and CI95upp
#' 
#' - CI95low : lower end of 95% credible interval
#' - CI95upp : upper end of 95% credible interval
#'
#' - ESS : effective sample size (ESS).  Control options include
#' `ESSfun` (a function to estimate ESS, with default =
#' `coda::effectiveSize`), and `suffix` (a character string to be
#' appended to "ESS" to form a label, with default = "").
#' 
#' - efficiency or (synonomously) efficiency_coda : effective sample
#' size (ESS) and efficiency (ESS / computation time).  If `ESS` was
#' already calculated, it will not be re-calculated.  Control options
#' include `ESSfun` (passed to `ESS`), `suffix` (a character string to
#' be appended to "efficiency" to form a label, with default = ""),
#' and `time` (a character string to be used as an expression to
#' calculate the computation time from elements of the `times` element
#' of the `result` object, with default = "sampling" for
#' burning+postburnin times).
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
#' In normal use, metrics are called by `addMetrics` (possibly from
#' `compareMCMCs`) and the results are collated in the `metrics` field
#' of `MCMCresult` objects.
#' 
#' @name metrics
#'
#' @aliases MCMCmetric_mean MCMCmetric_median MCMCmetric_sd
#'   MCMCmetric_CI95 MCMCmetric_CI95low MCMCmetric_CI95upp
#'   MCMCmetric_ESS MCMCmetric_efficiency
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
  res <- apply(result$samples, 2, stats::median)
  list(byParameter = list(median = res))
}

#' @rdname metrics
#' @export
MCMCmetric_sd <- function(result, ...) {
  res <- apply(result$samples, 2, stats::sd)
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
  low <- apply(result$samples, 2, stats::quantile, probs = 0.025)
  list(byParameter = list(CI95_low = low))
}

#' @rdname metrics
#' @export
MCMCmetric_CI95upp <- function(result, ...) {
  upp <- apply(result$samples, 2, stats::quantile, probs = 0.975)
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
    ESSfun <- coda::effectiveSize
  }
  
  ESSsuff <- paste0("ESS", options[['suffix']])
  if(ESSsuff %in% colnames(result$byParameter))
    # It already exists so don't add it again
    return(list())
  ess <- ESSfun(result$samples)
  list(byParameter = structure(list(ess), names = ESSsuff))
}

#' @rdname metrics
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
  if(ESSsuff %in% colnames(result$metrics$byParameter)) {
    # already calculated
    ess <- result$metrics$byParameter[, ESSsuff]
    names(ess) <- as.character(result$metrics$byParameter$Parameter)
    ESS_needed <- FALSE
  } else {
    ESSmetric <- MCMCmetric_ESS(result, options = options)
    ess <- ESSmetric$byParameter[[ESSsuff]]
  }
  time_denominator <- with(result$times,
                           eval(parse(text = options$time,
                                      keep.source = FALSE)))
  ## Above line allows options$time to be a string of an expression, like "x+y"
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
       byMCMC = structure(list(efficiency[which.min(efficiency)],
                               mean(efficiency)),
                          names = c(min_efficiencysuff, mean_efficiencysuff)))
  ## This way to get the min preserves the Parameter name
}

compareMCMCs_registered_metrics <- new.env()

#' Register, unregister, or access registered MCMC metric functions
#' for use by `compareMCMCs` or `addMetrics`
#' @name registerMetrics
#'
#' @aliases unregisterMetric getMetrics
#'
#' @param metrics A named list of new metric functions to register
#' @param name Character name of a metric function to unregister
#'
#' @details These functions are called for their "side effects" of
#'     modifying the list metric functions for MCMC results that will
#'     be recognized by name from the `compareMCMCs` or `addMetrics`
#'     functions.  Those functions take a `metrics` argument that can
#'     be a character vector or a list.  Names in the character vector
#'     will be looked up from the registered metric functions.
#'
#' `registerMetrics` takes a named list and adds its elements to the list
#' of recognized metrics with the corresponding names.
#'
#' `unregisterMetric` removes one metric from the list at a time.
#'
#' `getMetrics` returns the list of registered metrics.
#'
#' @return
#'
#' `registerMetrics` and `getMetrics` return the environment of
#' registered metrics.
#'
#' `unregisterMetric` returns the result (which should be NULL) of a
#' call to `rm` that attempts to remove a metric.
#' 
#' @export
registerMetrics <- function(metrics) {
  list2env(metrics, compareMCMCs_registered_metrics)
}

#' @rdname registerMetrics
#' @export
unregisterMetric <- function(name) {
  rm(list = name, envir = compareMCMCs_registered_metrics)
}

#' @rdname registerMetrics
#' @export
getMetrics <- function() {
  compareMCMCs_registered_metrics
}
