#' @export
MCMCresult <- R6Class(
  classname = "MCMCresult",
  portable = TRUE,
  ## TO-DO: possibly add a modelName field for optional use
  public = list(
    ## Name of MCMC method for which results are recorded in this object.
    MCMC = character(),
    ## Matrix of samples
    samples = NULL,
    ## Properties such as information about the MCMC runs
    properties = list(),
    ## Times related to setup (e.g. compilation) and running of an MCMC.
    ## An MCMC engine should typically provide the following times entries:
    ## setup:         Time to get ready to run, such as nimble or stan compile times
    ## sample_warmup: Time on burnin or warmpup samples
    ## sample:        Time on samples to be saved
    ## sample_total:  Total sampling time, typically sample_warumup + sample
    ## total:         Total time for everything, typically setup + sample_total
    times = list(),
    ## Metrics such as ESS, efficiency, mean, median.
    ## These are organized as nested lists with three options.
    ## Metrics in "byMCMC" will be organized in tidy format
    ## with a row for each MCMC sample. This will have a column for MCMC name
    ## and a column for each metric.  There will only be one row,
    ## but it will be easy to rbind with the same format from other MCMCs.
    ## These would be metrics where there is a single scalar for the
    ## entire MCMC, such as min(efficiency).
    ## Metrics in "byParameter" will be organized in tidy format
    ## with a row for each MCMC-x-parameter combination.  This will have a
    ## column for MCMC name, for parameter, and for each metric.  There will
    ## only be one MCMC, but it will be easy to rbind with the same format
    ## from other MCMCs.
    ## Metrics in "other" will simply be stored as an arbitrary list
    ## named by the metric.  This allows arbitrarily structured
    ## metrics to be saved.
    metrics = list(byMCMC = NULL,
                   byParameter = NULL,
                   other = list()),
    initialize = function(...) {
      dotsArg <- list(...)
      for (i in names(dotsArg)) {
        if(i == 'samples')
          self$setSamples(dotsArg[[i]])
        else
          self[[i]] <- dotsArg[[i]]
      }
    },
    setSamples = function(samples) {
      self$samples <<- samples
      self$metrics <<- list(byMCMC = NULL,
                                 byParameter = NULL,
                                 other = list())
      self$initializeMetrics(silent = TRUE)
   },
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
   },
    initializeMetrics = function(silent = FALSE) {
      if(is.null(self$metrics$byParameter) | is.null(self$metrics$byMCMC)) {
        if(length(self$MCMC)==0) {
          if(!silent)
            warning("Trying to initializeMetrics with no MCMC name set.\n")
          return(FALSE)
        }
        if(is.null(self$samples)) {
          if(!silent)
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
   clearMetrics = function(byParameter = TRUE, byMCMC = TRUE) {
     if(byParameter) {
       params <- colnames(self$samples)
       self$metrics$byParameter <- data.frame(MCMC = rep(self$MCMC, length(params)),
                                              Parameter = params)
     }
     if(byMCMC)
       self$metrics$byMCMC <- data.frame(MCMC = self$MCMC)
   },
    addMetricResult = function(metricResult) {
      if(!self$initializeMetrics()) {
        stop("Can't add metric results until metrics can be initialized.  This requires samples and a MCMC name.")
      }
      ## metric may be a list with elements named byMCMC, byParameter, and/or other
      validNames <- names(metricResult) %in% c("byMCMC", "byParameter", "other")
      if(!all(validNames)) {
          iInvalidNames <- which(!validNames)
          warning(paste0('metric input with names ', paste(names(metricResult)[iInvalidNames], collapse = ', '), ' will be ignored.'))
          metricResult <- metricResult[validNames]
      }
      if(!is.null(metricResult$byMCMC)) {
        ## To do: add checks that metricResult is a data frame with appropriate structure
        for(i in seq_along(metricResult$byMCMC)) {
          thisMetric <- metricResult$byMCMC[[i]]
          thisMetricName <- names(metricResult$byMCMC)[i]
          if(is.vector(thisMetric)) {
            thisMetricList <- structure(list(thisMetric),
                                        names = thisMetricName)
          }
          ## To do: add checks that metricResult is a data frame with appropriate structure
          ## or additional checks on valid metricResult input
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
        ## To do: add checks that metricResult is a data frame with appropriate structure
        ## or additional checks on valid metricResult input
          thisTidyMetric <- melt(do.call('rbind', thisMetricList),
                                 varnames = c('MCMC', 'Parameter'),
                                 value.name = thisMetricName)
          self$metrics$byParameter <- merge(self$metrics$byParameter, thisTidyMetric)
        }
      }
      if(!is.null(metricResult$other)) {
        if(!is.list(metricResult$other)) stop('metricResult must be a list if provided with other=TRUE.')
        self$metrics$other <- c(self$metrics$other, metricResult)
      }
      self
    }
  )
)
