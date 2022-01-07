#' R6 class to hold MCMC samples, timing results, and metrics
#' 
#' @seealso \code{\link{renameMCMC}} to change the name of an MCMC
#'     method throughout the structure of a list of `MCMCresult`
#'     objects.
#'
#' @importFrom R6 R6Class
#' @export
MCMCresult <- R6::R6Class(
  classname = "MCMCresult",
  portable = TRUE,
  public = list(
    #' @field MCMC Optional name for the MCMC method.
    MCMC = character(),
    #' @field samples Matrix of MCMC samples. Rows are for MCMC
    #' iterations.  Columns are for parameters.  Columns must be
    #' named.
    samples = NULL,
    #' @field times A list of times including elements for `setup`,
    #' `burnin`,  `postburnin` (sampling for recorded samples), and
    #' `sampling` (normally  `burnin` + `postburnin`).  Each list
    #' element should be a single numeric value.
    times = list(),
    #' @field metrics A list of MCMC performance metrics such as
    #' effective sample size (ESS), efficiency, mean, median, and
    #' credible interval boundaries. `metrics` ' is organized as a list
    #' with three elements: `byMCMC`, `byParameter`, and `other` '
    #' (currently unused). 
    #'
    #' `byMCMC` is for metrics with one number for an entire
    #' MCMC sample (as opposed to one number for each parameter).
    #' `byMCMC` is a data frame with one row and columns for MCMC name
    #' each metric.  These would be metrics where there is a single
    #scalar for the ' entire MCMC, such as min(efficiency). 
    #'
    #' `byParameter` is for metrics with one number for each parameter in each
    #' MCMC sample.  `byParameter` is a `data.frame` with one row for each 
    #' MCMC-x-parameter combination and columns for MCMC method,
    #' parameter name, and
    #' each metric.  There will only be one MCMC method name
    #' (all entries in the
    #'  MCMC column will be the same).  
    #'
    #'  The MCMC columns in `byMCMC` and `byParameter`
    #'  are useful for combining
    #'  `metrics` from a list of `MCMCresult` objects,
    #'   such as done by \code{\link{combineMetrics}}, and for retaining
    #'  MCMC method labels if these `data.frames` are copied and used
    #'  outside of
    #'  an `MCMCresult` object.
    #'
    #'  `other` is simply an arbitrary list. This allows arbitrarily structured
    #'   metrics to be saved.
    #'
    #'   Elements of `metrics` are normally populated by `addMetrics` or
    #'   `compareMCMCs`
    #'   (which calls `addMetrics`).
    metrics = list(byMCMC = NULL,
                   byParameter = NULL,
                   other = list()),
    #' @field sessionInfo Result of running `sessionInfo()` prior to calling
    #' an MCMC engine, if requested.
    sessionInfo = NULL,
    #' @description
    #' Create a new `MCMCresult` object.
    #' @param ... Arbitrary initialization.  If a matrix is passed, it
    #' will be used to initialize `samples` and the `metrics` elements.
    #' If a list with a matrix element named `samples` is passed, this element
    #' will be used as if the matrix itself was passed.  Any other named
    #' elements of a list that correspond to fields of an `MCMCresult` object
    #' will be initialized from them.
    initialize = function(...) {
      dotsArg <- list(...)
      for (i in names(dotsArg)) {
        if(i == 'samples')
          self$setSamples(dotsArg[[i]])
        else
          self[[i]] <- dotsArg[[i]]
      }
    },
    #' @description 
    #' Populate the samples and initialize the metrics
    #' @param samples A `data.frame` with MCMC output.
    #' @return NULL
    setSamples = function(samples) {
      self$samples <<- samples
      self$metrics <<- list(byMCMC = NULL,
                            byParameter = NULL,
                            other = list())
      self$initializeMetrics(silent = TRUE)
      invisible(NULL)
   },
   #' @description
   #' Change the MCMC method name from oldName to newName
   #' @param newName New name for MCMC method in `metrics`
   #' @param oldName Old name for MCMC method in `metrics`
   #' @details
   #' This change the `MCMC` field and the corresponding columns
   #' of `metrics$byParameter` and `metrics$byMCMC`.
   #'  
   #' If `oldName` is not the MCMC method name, this function does nothing.
   #' @return NULL
   rename = function(newName, oldName) {
     if(!missing(oldName))
       if(self$MCMC != oldName)
         return(invisible(NULL))
     self$MCMC <- newName
     if(!is.null(self$metrics$byParameter)) {
       if(nrow(self$metrics$byParameter) > 0) {
         self$metrics$byParameter$MCMC <- newName
       }
     }
     if(!is.null(self$metrics$byMCMC)) {
       if(nrow(self$metrics$byMCMC) > 0)
         self$metrics$byMCMC$MCMC <- newName
     }
     invisible(NULL)
   },
   #' @description
   #' Initialize metrics if necessary
   #' @param silent `logical` indicating whether to emit warnings
   #' @details This function does nothing if metrics are already initialized.
   #' It does not clear metrics.  See `clearMetrics` for information on 
   #' how metrics are initialized.
   #' @return `logical` indicating whether `metrics` is well-formed or not.
   initializeMetrics = function(silent = FALSE) {
      if(is.null(self$metrics$byParameter) | is.null(self$metrics$byMCMC)) {
        if(length(self$MCMC)==0) {
          if(!silent)
            warning("Trying to initializeMetrics with no MCMC name set.\n") #lacks test coverage
          return(FALSE)
        }
        if(is.null(self$samples)) {
          if(!silent) #lacks test coverage
            warning("Trying to initializeMetrics with no samples set.\n")
          return(FALSE)
        }
        if(is.null(self$metrics$byParameter)) {
          self$clearMetrics(byParameter = TRUE, byMCMC = FALSE)
        }
        if(is.null(self$metrics$byMCMC)) {
          self$clearMetrics(byParameter = FALSE, byMCMC = TRUE)
        }
      }
      TRUE
    },
   #' @description 
   #' Clear (reset) `byParameter` and/or `byMCMC` metrics
   #' @param byParameter `logical` indicating whether to clear `byParameter`
   #' metrics
   #' @param byMCMC `logical` indicating whether to clear `byMCMC` metrics
   #' @details 
   #' `byParameter` metrics are initialized to a `data.frame` with columns for 
   #' `MCMC` (all the same entry, the `MCMC` field) and `Parameter`
   #' (taken from column
   #' names of the `samples`).
   #' 
   #' `byMCMC` metrics are initialized to a `data.frame`
   #' with a column for `MCMC`.
   clearMetrics = function(byParameter = TRUE, byMCMC = TRUE) {
     if(byParameter) {
       params <- colnames(self$samples)
       self$metrics$byParameter <-
           data.frame(MCMC = rep(self$MCMC, length(params)),
                      Parameter = params)
     }
     if(byMCMC)
       self$metrics$byMCMC <- data.frame(MCMC = self$MCMC)
   },
   #' @description 
   #' Add one set of metric results
   #' 
   #' @param metricResult A list with possible elements `byParameter`,
   #' `byMCMC`, and
   #' `other`.  These are typically returned from a metric function
   #' called via
   #' `addMetric`. Each is combined with previous metrics already in the
   #' corresponding
   #' elements of `metrics`.
    addMetricResult = function(metricResult) {
      if(!self$initializeMetrics()) {
        stop( #lacks test coverage
          paste0("Can't add metric results until metrics can be initialized.",
                 " This requires samples and a MCMC name."))
      }
      ## metric may be a list with elements named byMCMC,
      ## byParameter, and/or other
      validNames <- names(metricResult) %in% c("byMCMC", "byParameter", "other")
      if(!all(validNames)) {
          iInvalidNames <- which(!validNames) #lacks test coverage
          warning(paste0('metric input with names ',
                         paste(names(metricResult)[iInvalidNames],
                               collapse = ', '), ' will be ignored.'))
          metricResult <- metricResult[validNames]
      }
      if(!is.null(metricResult$byMCMC)) {
        for(i in seq_along(metricResult$byMCMC)) {
          thisMetric <- metricResult$byMCMC[[i]]
          thisMetricName <- names(metricResult$byMCMC)[i]
          if(is.vector(thisMetric)) {
            thisMetricList <- structure(list(thisMetric),
                                        names = thisMetricName)
          }
          self$metrics$byMCMC <- merge(self$metrics$byMCMC, thisMetricList)
        }
      }
      if(!is.null(metricResult$byParameter)) {
        for(i in seq_along(metricResult$byParameter)) {
          thisMetric <- metricResult$byParameter[[i]]
          thisMetricName <- names(metricResult$byParameter)[i]
          if(is.vector(thisMetric)) {
            thisMetricList <- structure(list(thisMetric), names = self$MCMC)
          }
          thisTidyMetric <- reshape2::melt(do.call('rbind', thisMetricList),
                                           varnames = c('MCMC', 'Parameter'),
                                           value.name = thisMetricName)
          self$metrics$byParameter <- merge(self$metrics$byParameter,
                                            thisTidyMetric)
        }
      }
      if(!is.null(metricResult$other)) {
        if(!is.list(metricResult$other)) #lacks test coverage
          stop('metricResult must be a list if provided with other=TRUE.')
        self$metrics$other <- c(self$metrics$other, metricResult)
      }
      self
    }
  )
)
