#' Manipulate metrics in one or more `MCMCresult` object(s)
#' 
#' Clear metrics or add metrics to MCMC results.
#' 
#' @param results an `MCMCresult` object or list of `MCMCresult`
#'     objects.
#' @param byParameter `TRUE` or `FALSE`: whether to clear
#'     `byParameter` metrics
#' @param byMCMC `TRUE` or `FALSE`: whether to clear `byMCMC` metrics
#' @param metrics character vector of metric names to add.  See
#'     \code{\link{metrics}}.
#' @param options named list of options.  When calling a metric
#'     function (e.g. `mean`), if there is a named element with that
#'     name (e.g. "mean"), it will be passed as the second argument to
#'     the metric function.
#' 
#' @details These functions provide ways to manipulate the collection
#'   of metrics inside one or more `MCMCresult` objects.
#' 
#' The \code{\link{MCMCresult}} class is fairly simple.  One can also
#' modify contents of an `MCMCresult` object using class methods or
#' direct manipulation of contents.
#' 
#' Metrics are organized as "byParameter", when there is one result
#' for each parameter (column) of MCMC output, and "byMCMC", when
#' there is one result for an entire MCMC sample (across all
#' parameters).
#' 
#' `clearMetrics` clears all metrics by parameter, by MCMC, or both.
#' 
#' `addMetrics` populates a set of metrics.  See package vignette for
#' more information.
#' 
#' @name modifyMetrics
#'
#' @aliases clearMetrics addMetrics
#'
#' @seealso \link{combineMetrics}

#' @rdname modifyMetrics
#' @export
clearMetrics <- function(results, 
                         byParameter = TRUE,
                         byMCMC = TRUE) {
  if(!is.list(results))
    results <- list(results) #lacks test coverage
  for(iR in seq_along(results)) {
  results[[iR]]$clearMetrics(byParameter = byParameter,
                             byMCMC = byMCMC)
  }
  invisible(NULL)
}

#' @rdname modifyMetrics
#' @export
addMetrics <- function(results,
                       metrics = c('mean',
                                   'median',
                                   'sd',
                                   'CI95_low',
                                   'CI95_upp',
                                   'ESS',
                                   'efficiency'),
                       options = list()) {
  if(!is.list(results))
    results <- list(results) #lacks test coverage
  if(is.character(metrics))
    metrics <- as.list(metrics)
  if(!is.list(metrics))
    metrics <- list(metrics)
  for(iM in seq_along(metrics)) {
    if(is.character(metrics[[iM]])) {
      thisMetricName <- metrics[[iM]]
      thisMetric <- compareMCMCs_registered_metrics[[ thisMetricName ]]
      if(is.null(thisMetric))
        warning( #lacks test coverage
          paste0('No metric ',
                 'named \"',
                 thisMetricName,
                 '\" is registered.  Use registerMetrics to add new metrics.'))
    } else {
      thisMetric <- metrics[[iM]]
      thisMetricName <- paste0("#", iM)
    }
    for(iR in seq_along(results)) {
      metric <- try(thisMetric(results[[iR]],
                    options[[thisMetricName]]))
      if(inherits(metric, "try-error")) {
        warning(paste0("Problem applying metric ", #lacks test coverage
                       thisMetricName,
                       " to result #", iR, " (", names(results)[iR], ")"))
      }
      add_ok <- results[[iR]]$addMetricResult(metric)
      if(inherits(add_ok, 'try-error')) {
        warning(paste0("The result from metric ", #lacks test coverage
                       thisMetricName,
                       " for result #", iR, " (", names(results)[iR], ")",
                       "has a problem."))
      }
    }
  }
  invisible(NULL)
}

#' Combine all metrics from a list of `MCMCresult` objects.
#' 
#' This is useful for seeing results from multiple MCMC engines compactly.
#' 
#' @param results a list of `MCMCresult` objects
#' @param include_times if `TRUE`, attempt to include timing elements
#'   in the combination.
#'
#' @param params Character vector of parameter names to include. If \code{NULL},
#'   all available parameter results will be included.
#'
#' @param paramFilter Expression suitable for use in `dplyr::filter` to subset
#'   the parameters to include. The relevant column name of the data frame (to
#'   be passed to `filter`) is "Parameter". For example, `paramFilter=Parameter
#'   %in% c("alpha", "beta")` will include only `alpha` and `beta`. Subsetting
#'   parameters by the coarser `params` argument will be done before subsetting
#'   by `paramFilter`.
#'
#' @param MCMCs Character vector of MCMC names to include. If \code{NULL},
#'   all available MCMCs will be included.
#'
#' @param MCMCFilter Expression suitable for use in `dplyr::filter` to subset
#'   the MCMCs to include. The relevant column name is "MCMC". For
#'   example,`MCMCFilter=MCMC %in% c("MCMC1", "MCMC2")` Subsetting parameters by
#'   the coarser \code{MCMCs} argument will be done before subsetting by
#'   \code{MCMCFilter}.
#'
#' @return A list with elements `byParameter`, `byMCMC` and, if
#'   `include_times=TRUE`, `times`. Each element combines the corresponding
#'   elements for each `MCMCresult` object in the `results` argument.
#'
#' @seealso \link{modifyMetrics}
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export
combineMetrics <- function(results, include_times = FALSE,
                           params=NULL, paramFilter=NULL,
                           MCMCs=NULL, MCMCFilter=NULL) {
  byParameter <-  do.call('rbind',
                          c(lapply(results,
                                   function(x)
                                     x$metrics$byParameter),
                            list(make.row.names = FALSE))
                          )
  paramFilter <- substitute(paramFilter)
  MCMCFilter <- substitute(MCMCFilter)
  if(!is.null(params))
    byParameter <- byParameter |> dplyr::filter(.data$Parameter %in% params)
  if(!is.null(paramFilter))
    eval(substitute(byParameter <- byParameter |> dplyr::filter(PF), list(PF=paramFilter)))
  if(!is.null(MCMCs))
    byParameter <- byParameter |> dplyr::filter(.data$MCMC %in% MCMCs)
  if(!is.null(MCMCFilter))
    eval(substitute(byParameter <- byParameter |> dplyr::filter(MF), list(MF=MCMCFilter)))

  byMCMC <- do.call('rbind',
                      c(lapply(results,
                               function(x)
                                 x$metrics$byMCMC),
                        list(make.row.names = FALSE))
                    )
  if(!is.null(MCMCs))
    byMCMC <- byMCMC |> dplyr::filter(.data$MCMC %in% MCMCs)
  if(!is.null(MCMCFilter))
    eval(substitute(byMCMC <- byMCMC |> dplyr::filter(MF), list(MF=MCMCFilter)))

  if(include_times) {
    Null2NA <- function(x) if(is.null(x)) NA else x
    times <- do.call('rbind',
                     c(lapply(results,
                              function(x) {
                                  ans <- c(Null2NA(x$times[['burnin']]),
                                           Null2NA(x$times[['postburnin']]),
                                           Null2NA(x$times[['sampling']]))
                              }))
                     )
    colnames(times) <- c("burnin", "post-burnin", "total sampling")
    if(!is.null(MCMCs) || !is.null(MCMCFilter)) {
      timesdf <- as.data.frame(times)
      timesdf$MCMC <- row.names(times)
      if(!is.null(MCMCs))
        timesdf <- timesdf |> dplyr::filter(.data$MCMC %in% MCMCs)
      if(!is.null(MCMCFilter))
        eval(substitute(timesdf <- timesdf |> dplyr::filter(MF), list(MF=MCMCFilter)))
      timesdf$MCMC <- NULL
      times <- as.matrix(timesdf)
    }
  }
  ans <- list(byParameter = byParameter,
              byMCMC = byMCMC)
  if(include_times)
    ans$times <- times
  ans
}

