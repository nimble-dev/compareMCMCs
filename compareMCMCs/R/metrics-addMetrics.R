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
#' @return A list with elements `byParameter` and `byMCMC`.  Each
#'   element combines the corresponding elements for each `MCMCresult`
#'   object in the `results` argument.
#'
#' if `include_times` is `TRUE`, an element `times` will also be in
#' the returned list.
#'
#' @seealso \link{modifyMetrics}
#'
#' @export
combineMetrics <- function(results, include_times = FALSE) {
  byParameter <-  do.call('rbind',
                          c(lapply(results,
                                   function(x)
                                     x$metrics$byParameter),
                            list(make.row.names = FALSE))
  )
  byMCMC <- do.call('rbind',
                      c(lapply(results,
                               function(x)
                                 x$metrics$byMCMC),
                        list(make.row.names = FALSE))
                      )
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
  }
  ans <- list(byParameter = byParameter,
              byMCMC = byMCMC)
  if(include_times)
    ans$times <- times
  ans
}

