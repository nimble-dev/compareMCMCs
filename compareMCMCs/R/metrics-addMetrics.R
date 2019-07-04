#' @export
clearMetrics <- function(results, 
                         byParameter = TRUE,
                         byMCMC = TRUE) {
  for(iR in seq_along(results)) {
  results[[iR]]$clearMetrics(byParameter = byParameter,
                             byMCMC = byMCMC)
  }
  invisible(NULL)
}

#' @export
addMetrics <- function(results,
                       metrics = c('mean',
                                   'median',
                                   'sd',
                                   'CI95_low',
                                   'CI95_upp',
                                   'efficiency_coda')) {
  if(!require(reshape2)) stop(paste0('Package reshape2 is required to add metrics. ',
                                     'Please install reshape2.'))
  if(!is.list(results))
    results <- list(results)
  if(is.character(metrics))
    metrics <- as.list(metrics)
  if(!is.list(metrics))
    metrics <- list(metrics)
  for(iM in seq_along(metrics)) {
    if(is.character(metrics[[iM]])) {
      thisMetricName <- metrics[[iM]]
      thisMetric <- compareMCMCs_registered_metrics[[ thisMetricName ]]
      if(is.null(thisMetric))
        warning(paste0('No metric ',
                       'named \"',
                       thisMetricName,
                       '\" is registered.  Use registerMetrics to add new metrics.'))
    } else {
      thisMetric <- metrics[[iM]]
      thisMetricName <- paste0("#", iM)
    }
    for(iR in seq_along(results)) {
      metric <- try(thisMetric(results[[iR]]))
      if(inherits(metric, "try-error")) {
        warning(paste0("Problem applying metric ",
                       thisMetricName,
                       " to result #", iR, " (", names(results)[iR], ")"))
        
      }
      add_ok <- results[[iR]]$addMetricResult(metric)
      if(inherits(add_ok, 'try-error')) {
        warning(paste0("The result from metric ",
                       thisMetricName,
                       " for result #", iR, " (", names(results)[iR], ")",
                       "has a problem."))
      }
    }
  }
  invisible(NULL)
}

#' @export
combineMetrics <- function(results) {
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
  list(byParameter = byParameter,
       byMCMC = byMCMC)
}
