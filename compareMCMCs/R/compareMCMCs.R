#' run a set of MCMCs for performance comparison
#'
#' run one or more MCMC engines for one model specification, with 
#' timing and performance metrics calculated.
#'
#' @param modelInfo A list of nimble model-specification information,
#'     which may be relevant for JAGS, WinBUGS and/or OpenBUGS as
#'     well. To provide information for a different MCMC engine, see argument
#'     \code{externalMCMCinfo}.  Named elements in \code{modelInfo} can include `code` (model
#'     code as returned from `nimbleCode`), `data` (a list with data),
#'     `constants` (a list with data and/or constants), and `inits` (a
#'     list of initial values).  See `nimbleModel` in package nimble
#'     for for information on these arguments.  For JAGS, WinBUGS and
#'     OpenBUGS, many models can be run from the same
#'     specification since they use nearly the same model language.
#' 
#' @param MCMCcontrol A list with fields `niter` (number of
#'     iterations), `thin` (thinning interval), and `burnin` (number
#'     of iterations to discard from the beginning of the MCMC
#'     sample).
#'
#' @param MCMCs A character vector of MCMC cases to run.  This can
#'     include "nimble" (default nimble samplers), "jags", "stan",
#'      one of several nimble special cases
#'     (see details below), custom nimble
#'     sampler configurations provided via argument
#'     \code{nimbleMCMCdefs}, and external MCMC engines registered via
#'     \code{\link{registerMCMCengine}}.  See \link{builtin_MCMCs} for information on
#'     "jags" and "stan".  Support for OpenBUGS and WinBUGS is pending.
#'
#' @param nimbleMCMCdefs A list of information for custom sampler configurations in nimble.  See Details below.
#'
#' @param externalMCMCinfo A list of arbitrary information for external MCMC engines, 
#' named by engine names.  If there is an external MCMC engine 
#'  named "myMCMC", then a list element `myMCMC` of
#'   \code{externalMCMCinfo} will be passed to the engine as its \code{MCMCinfo}
#'    argument.
#'
#' @param metrics Either a character vector of registered metric names
#'     to apply to each sample, or a list of elements with either
#'     metric names or metric functions to apply to each sample.  See
#'     \code{\link{addMetrics}} for more information.  A useful set of
#'     default metrics is provided.
#'
#' @param seed an (arbitrary) numeric value passed to `set.seed` to set the
#'     random-number generator seed before calling each MCMC engine.
#'     If NULL, no seed is set.  To obtain identical results from one call of `compareMCMCs`
#'     to the next, use identical `seed` values.
#'
#' @param monitors A vector of names of parameters to record in MCMC samples.
#' 
#' @details The special cases provided for the `MCMCs` argument
#'     include: 
#'     
#' - "nimble_noConj": use adaptive random-walk
#'     Metropolis-Hastings (ARWMH) samplers in place of Gibbs
#'     (conjugate) samplers.
#' - "nimble_RW": use all adaptive random-walk Metropolis-Hastings samplers.
#' - "nimble_slice": use all slice samplers.
#'
#' See package vignette for examples.
#'
#' @return A list of `MCMCresult` objects.
#' 
#' @export
compareMCMCs <- function(modelInfo = list(),
                         MCMCcontrol = list(niter = 10000,
                                            thin = 1,
                                            burnin = 2000), ##  niter, thin, burnin
                         MCMCs = 'nimble',
                         monitors = character(),
                         nimbleMCMCdefs = list(),
                         externalMCMCinfo = list(),
                         metrics = c('mean',
                                     'median',
                                     'sd',
                                     'CI95_low',
                                     'CI95_upp',
                                     'efficiency_coda'),
                         metricOptions = list(),
                         conversions = list(),
                         seed = NULL,
                         needRmodel,
                         verbose = TRUE
                         ) {
  if(length(MCMCs) == 0) 
    stop("No MCMCs requested.")
  MCMCs_with_labels <- NULL
  if(!is.null(names(MCMCs)))
    MCMCs_with_labels <- MCMCs ## for use later to relabel results

  # Check for valid (niter, thin, burnin) triplet.
  niter <- if(is.null(MCMCcontrol$niter)) 10000 else MCMCcontrol$niter
  thin  <- if(is.null(MCMCcontrol$thin))  1     else MCMCcontrol$thin
  burnin <- if(is.null(MCMCcontrol$burnin)) 2000 else MCMCcontrol$burnin
  
  ## SP: burnin is discarder pre thinning form nimble 0-6-11
  # nkeep <- floor(niter/thin) - burnin    ## wrong
  # if(nkeep < 0)
  #   stop(paste0('niter/thin - burnin is negative.\n',
  #               'This would not retain any samples.\n',
  #               'Try increasing niter, or decreasing burnin.'))
  # burninFraction <- burnin / (nkeep + burnin)
  
  ## Build the R model to use at least as a reference for model parameters.
  ## Later we could modify this so that the model is only built if it is really needed.
  ## Or we could at least set calculate = FALSE and then do a calculate
  ## later if a nimble MCMC will be run.
  
  if(missing(needRmodel))
    needRmodel <- `|`(
    any(c('nimble', 'jags', 'openbugs', 'winbugs') %in% MCMCs),
    (length(nimbleMCMCdefs) > 0)
    )
  
  if(needRmodel) {
    if(!("Rmodel" %in% names(modelInfo))) {
      if(verbose)
        message("building nimble model...")
        ## The following should work:
        ## Rmodel <- do.call('nimbleModel', modelInfo, quote = TRUE)
        ## but momentarily that gives a bug in nimble 0.8.0, so instead we use:
        Rmodel <- try(
          nimbleModel(code = modelInfo$code,
                      data = if(!is.null(modelInfo$data)) 
                        modelInfo$data else list(),
                      constants = if(!is.null(modelInfo$constants)) 
                        modelInfo$constants else list(),
                      inits = if(!is.null(modelInfo$inits))
                        modelInfo$inits else list())
        )
      if(inherits(Rmodel, 'try-error'))
        stop("Problem building Rmodel.")
    } else {
      Rmodel <- modelInfo$model
      if(!inherits(Rmodel, "RmodelBaseClass"))
        stop(paste0("modelInfo contains an element called Rmodel, ",
                    "but it is not a valid nimbleModel."))
    }

    if(length(monitors) == 0) {
      newMonitors <- Rmodel$getNodeNames(topOnly = TRUE, stochOnly = TRUE)
      newMonitors <- Rmodel$expandNodeNames(newMonitors,
                                            returnScalarComponents = TRUE)
      dataFlags <- unlist(lapply(newMonitors,
                                 function(mon) 
                                   eval(parse(text=mon, keep.source=FALSE)[[1]],
                                        envir=Rmodel$isDataEnv)))
      newMonitors <- newMonitors[!dataFlags]
      monitors <- newMonitors
    } else {
      newMonitors <- Rmodel$expandNodeNames(monitors,
                                            returnScalarComponents = TRUE)
      monitors <- newMonitors
    }
  } else {
    Rmodel <- NULL
  }
  monitorVars <- unique(nimble:::removeIndexing(monitors))
  ## set summary stats:
  ## This step may not be necessary.  If necessary, it should be handled 
  ## by MCMCmetric functions.

  # 1. Collect nimbleMCMCs and externalMCMCs
  nimbleMCMCs <- character()
  externalMCMCs <- character()
  availableExternalMCMCs <- ls(MCMCdefs_env)
  for(mcmc in MCMCs) {
    if(mcmc %in% availableExternalMCMCs)
      externalMCMCs <- c(externalMCMCs, mcmc)
    else if(mcmc %in% names(MCMCdefs_nimble_builtin))
      nimbleMCMCs <- c(nimbleMCMCs, mcmc)
    else if(mcmc %in% names(nimbleMCMCdefs))
      nimbleMCMCs <- c(nimbleMCMCs, mcmc)
    else warning(paste('No MCMC definition is available for requested MCMC "',
                       mcmc,'".'))
  }

  ## The old system did a step here to setMCMCdefs.
  ## It's not clear if a step like this is needed.

  ## Collect two sets of information for calling MCMC plugins.
  ## 1. MCMCcontrol contains specifications of the run.
  ## 2. otherInfo contains specifications of the model, data, constants, 
  ## inits, & monitors.
  MCMCcontrol <- list(niter = niter,
                      thin = thin,
                      burnin = burnin,
                      seed = seed)
  monitorInfo <- list(monitorVars = monitorVars,
                      monitors = monitors)
  modelInfo$model <- Rmodel
  ## The original version of MCMCsuite used MCMCs in a particular order,
  ## regardless of the order of the MCMCs argument.  To preserve behavior,
  ## we do the same.
  ## In the future we may choose to order the MCMCs according to the MCMCs argument,
  ## so the user can manage it in case they want to "watch" one go first.

  ## If present, use the order 'winbugs', 'openbugs', 'jags', 'stan'
  ## simply to imitate original behavior precisely, although it
  ## shouldn't matter.
  for(possibleMCMC in rev(c('winbugs', 'openbugs', 'jags', 'stan'))) {
    iMCMC <- which(externalMCMCs == possibleMCMC)
    if(length(iMCMC) > 0)
      externalMCMCs <- c(possibleMCMC, externalMCMCs[-iMCMC])
  }
  results <- list()
  ## run through plugin MCMCs
  for(mcmc in externalMCMCs) {
    if(!is.null(seed)) set.seed(as.numeric(seed))
    thisMCMCinfo <- externalMCMCinfo[[mcmc]]
    results[[mcmc]] <- try(MCMCdefs_env[[mcmc]](MCMCinfo = thisMCMCinfo,
                                                MCMCcontrol = MCMCcontrol,
                                                monitorInfo = monitorInfo,
                                                modelInfo = modelInfo))
    if(inherits(results[[mcmc]], 'try-error')) {
      warning(paste("MCMC ", mcmc, " failed.\n"))
      results[[mcmc]] <- NULL
    } else {
      ## set field MCMC, which is used as a name, if it was not
      ## set by the MCMC interface (MCMCdefs_env[[mcmc]])
      if(length(results[[mcmc]]$MCMC)==0)
        results[[mcmc]]$MCMC <- mcmc
    }
  }
  ## run any nimble MCMCs
  nimbleResults <- NULL
  if(length(nimbleMCMCs) > 0) {
    nimbleResults <- try(runNIMBLE(nimbleMCMCs,
                                   c(nimbleMCMCdefs, MCMCdefs_nimble_builtin),
                                   modelInfo,
                                   MCMCcontrol,
                                   monitorInfo,
                                   seed,
                                   parent.frame = parent.frame()))
  }
  if(!inherits(nimbleResults, 'try-error'))
    results <- c(results, nimbleResults)
  else
    warning("There was a problem compiling or running nimble MCMCs.")

  ## Standardize column names of samples
  ## Make this optional, and wrap it in try()
  for(i in seq_along(results)) {
    catcher <- try({
      if(!is.null(results[[i]]$samples)) {
        orig.col.names <- colnames(results[[i]]$samples)
        new.col.names <- unlist(lapply(orig.col.names,
                                       function(x) deparse(parse(text = x, keep.source = FALSE)[[1]])
                                       ))
        colnames(results[[i]]$samples) <- new.col.names
      }
    }) ## There is no meaningful handling of errors, but at least the function will continue and the MCMCs will not be lost.
  }
  
  catcher <- try({
    if(length(conversions)) {
      for(i in seq_along(results)) {
        thisName <- names(results)[i]
        thisConversion <- conversions[[thisName]]
        if(!is.null(thisConversion)) {
          applyConversions(results[[thisName]], thisConversion)
        }
      }
    }
  })
  
  catcher <- try({
    if(!is.null(metrics)) {
      not_used <- addMetrics(results, metrics, metricOptions)
      if(inherits(not_used, 'try-error'))
        warning("There was a problem calculating metrics.")
    }
  })

  ## relabel results
  ## To be written
  
  results
}
