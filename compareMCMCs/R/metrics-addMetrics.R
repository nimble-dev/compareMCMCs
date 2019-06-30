#' @export
addMetrics <- function(results,
                       metrics) {
  if(!require(reshape2)) stop('Package reshape2 is required to add metrics. Please install reshape2.')
  if(!is.list(results))
    results <- list(results)
  if(is.character(metrics))
    metrics <- as.list(metrics)
  if(!is.list(metrics))
    metrics <- list(metrics)
  for(iM in seq_along(metrics)) {
    if(is.character(metrics[[iM]])) {
      thisMetric <- compareMCMCs_registered_metrics[[metrics[[iM]] ]]
      if(is.null(thisMetric))
        warning(paste0('No metric ',
                       named, ' ',
                       metrics[[iM]],
                       'is registered.  Use registerMetrics to add new metrics.'))
    } else {
      thisMetric <- metrics[[iM]]
    }
    for(iR in seq_along(results)) {
      metric <- thisMetric(results[[iR]])
      results[[iR]]$addMetricResult(metric)
    }
  }
}

#' @export
combineMetrics <- function(results) {
  byParameter <-  do.call('rbind',
                          c(lapply(results,
                                   function(x)
                                     x$metrics$byParameter),
                            list(make.row.names = FALSE))
  )
  bySample <- do.call('rbind',
                      c(lapply(results,
                               function(x)
                                 x$metrics$bySample),
                        list(make.row.names = FALSE))
                      )
  list(byParameter = byParameter,
       bySample = bySample)
}

