#' @export
runMCMCs <- function(
  modelInfo,
  MCMCinfo, ##  niter, thin, burnin, setSeed, monitors
  MCMCs = 'nimble',
  nimbleMCMCdefs = list(),
  externalMCMCinfo = list(),
  metrics = c('mean', 'median', 'sd', 'CI95_low', 'CI95_upp','efficiency_coda')
) {
  # Check for valid (niter, thin, burnin) triplet.
  niter <- if(is.null(MCMCinfo$niter)) 10000 else MCMCinfo$niter
  thin  <- if(is.null(MCMCinfo$thin))  1     else MCMCinfo$thin
  burnin <- if(is.null(MCMCinfo$burnin)) 2000 else MCMCinfo$burnin
  nkeep <- floor(niter/thin) - burnin
  if(nkeep < 0)
    stop(paste0('niter/thin - burnin is negative.\n',
                'This would not retain any samples.\n',
                'Try increasing niter, or decreasing burnin.'))
  burninFraction <- burnin / (nkeep + burnin)
  ## Build the R model to use at least as a reference for model parameters.
  ## Later we could modify this so that the model is only built if it is really needed.
  ## Or we could at least set calculate = FALSE and then do a calculate
  ## later if a nimble MCMC will be run.
  Rmodel <- do.call('nimbleModel', modelInfo, quote = TRUE)
  monitors <- MCMCinfo$monitors
  if(length(monitors) == 0) {
    newMonitors <- Rmodel$getNodeNames(topOnly = TRUE, stochOnly = TRUE)
  }
  newMonitors <- Rmodel$expandNodeNames(newMonitors, returnScalarComponents = TRUE)
  dataFlags <- unlist(lapply(newMonitors, function(mon) eval(parse(text=mon, keep.source=FALSE)[[1]], envir=Rmodel$isDataEnv)))
  newMonitors <- newMonitors[!dataFlags]
  monitors <- newMonitors
  monitorVars <- unique(nimble:::removeIndexing(monitors))
  monitorNodesNIMBLE <- monitors
  #monitor name conversions should be handled by plugin
  #monitorNodesBUGS <<- gsub(' ', '', monitorNodesNIMBLE)
  nMonitorNodes <- length(monitorNodesNIMBLE)

  ## set summary stats:
  ## This step may not be necessary.  If necessary, it should be handled by MCMCmetric functions.

  ## setMCMCs: can instead look for MCMCplugins by name
  # 1. Collect nimbleMCMCs and externalMCMCs
  nimbleMCMCs <- character()
  externalMCMCs <- character()
  availableExternalMCMCs <- ls(MCMCdefs_env)
  for(mcmc in MCMCs) {
    if(mcmc %in% availableExternalMCMCs)
      externalMCMCs <- c(externalMCMCs, mcmc)
    else if(mcmc %in% names(MCMCdefs_nimble))
      nimbleMCMCs <- c(nimbleMCMCs, mcmc)
    else if(mcmc %in% names(nimbleMCMCdefs))
      nimbleMCMCs <- c(nimbleMCMCs, mcmc)
    else warning(paste('No MCMC definition is available for requested MCMC "',mcmc,'".'))
  }

  ## The old system did a step here to setMCMCdefs.
  ## It's not clear if a step like this is needed.

  ## Collect two sets of information for calling MCMC plugins.
  ## 1. MCMCinfo contains specifications of the run.
  ## 2. otherInfo contains specifications of the model, data, constants, inits, & monitors.
  MCMCinfo <- list(niter = niter,
                   thin = thin,
                   burnin = burnin)
  otherInfo <- list(Rmodel = Rmodel,
                    data = modelInfo$data,
                    constants = modelInfo$constants,
                    inits = modelInfo$inits,
                    monitorVars = monitorVars,
                    monitorNodesNIMBLE = monitorNodesNIMBLE)

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
  setSeed <- MCMCinfo$setSeed
  for(mcmc in externalMCMCs) {
    if(isTRUE(setSeed)) set.seed(0)
    results[[mcmc]] <- try(MCMCdefs_env[[mcmc]](runInfo = MCMCrunInfo[[mcmc]],
                                                MCMCinfo = MCMCinfo,
                                                otherInfo = otherInfo))
    if(inherits(results[[mcmc]], 'try-error')) {
      warning(paste("MCMC ", mcmc, " failed."))
    }
    if(length(results[[mcmc]]$MCMC)==0)
      results[[mcmc]]$MCMC <- mcmc
  }
  ## run any nimble MCMCs
  nimbleResults <- NULL
  if(length(nimbleMCMCs) > 0)
    nimbleResults <- try(runNIMBLE(nimbleMCMCs,
                                   c(nimbleMCMCdefs, MCMCdefs_nimble),
                                   MCMCinfo,
                                   otherInfo))
  if(!inherits(nimbleResults, 'try-error'))
    results <- c(results, nimbleResults)
  else
    warning("There was a problem compiling or running nimble MCMCs.")

  if(!is.null(metrics)) {
    dummy <- addMetrics(results, metrics)
    if(inherits(dummy, 'try-error'))
      warning("There was a problem calculating metrics.")
  }

  # Convert to old-style output if needed.
  results
}
