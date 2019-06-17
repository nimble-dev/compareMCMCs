runNIMBLE <- function(nimbleMCMCs,
                      MCMCdefs,
                      MCMCinfo,
                      otherInfo){
  require(nimble)
  RmcmcFunctionList <- list()
  CmcmcFunctionList <- list()
  nNimbleMCMCs <- length(nimbleMCMCs)
  for(iMCMC in seq_along(nimbleMCMCs)) {
    mcmcTag <- nimbleMCMCs[iMCMC]
    mcmcDef <- MCMCdefs[[mcmcTag]]
    if(is.function(mcmcDef)) {
      mcmcConf <- eval(call("mcmcDef", otherInfo$Rmodel))
    } else if(is.character(mcmcDef)) {
      mcmcConf <- eval(call(mcmcDef, otherInfo$Rmodel))
    } else {
      RmodelEnv <- new.env()
      RmodelEnv$Rmodel <- otherInfo$Rmodel
      mcmcConf <- eval(mcmcDef, envir = RmodelEnv)
    }
    mcmcConf$addMonitors(otherInfo$monitorVars, print = FALSE)
    mcmcConf$setThin(MCMCinfo$thin, print = FALSE)
    RmcmcFunctionList[[mcmcTag]] <- buildMCMC(mcmcConf)
  }
  compile_time <- system.time({
    Cmodel <- try(compileNimble(otherInfo$Rmodel))
    if(inherits(Cmodel, 'try-error')) {
      stop("There was a problem compiling the nimble model.")
    }
    CmcmcFunctionList_temp <- try(compileNimble(RmcmcFunctionList,
                                            project = otherInfo$Rmodel))
    if(inherits(CmcmcFunctionList_temp, 'try-error')) {
      stop("There was a problem compiling one or more nimble MCMCs.")
    }
    ## Arguably the following if-then-else should be outside of the system.time,
    ## but it was inside originally so it will stay there for now.
    if(nNimbleMCMCs == 1)
      CmcmcFunctionList[[nimbleMCMCs[1]]] <- CmcmcFunctionList_temp
    else
      CmcmcFunctionList <- CmcmcFunctionList_temp
  })

  ## Record full set of model states
  allInitialModelStates <- list()
  allModelVars <- Cmodel$getVarNames(includeLogProb = TRUE)
  for(var in allModelVars)
    allInitialModelStates[[var]] <- Cmodel[[var]]

  results <- list()
  for(iMCMC in seq_along(nimbleMCMCs)) {
    for(var in allModelVars)
      Cmodel[[var]] <- allInitialModelStates[[var]]
    mcmcTag <- nimbleMCMCs[iMCMC]
    Cmcmc <- CmcmcFunctionList[[mcmcTag]]
    if(isTRUE(as.logical(MCMCinfo$setSeed))) {
      if(isTRUE(MCMCinfo$setSeed)) set.seed(0)
      else set.seed(as.numeric(MCMCinfo$setSeed))
    }
    timeResult <- try(system.time({ Cmcmc$run(MCMCinfo$niter) }))
    if(!inherits(timeResult, 'try-error')) {
      CmvSamples <- Cmcmc$mvSamples
      samplesArray <- as.matrix(CmvSamples, varNames = otherInfo$monitorVars)
      samplesArray <- samplesArray[(MCMCinfo$burnin+1):floor(MCMCinfo$niter/MCMCinfo$thin),
                                   otherInfo$monitorNodesNIMBLE,
                                   drop=FALSE]
      ## addToOutput(mcmcTag, samplesArray, timeResult)
      results[[mcmcTag]] <- MCMCresult$new(samples = samplesArray,
                                           times = list(sample = timeResult),
                                           MCMC = mcmcTag)
    } else {
      warning(paste0("There was a problem running ", mcmcTag,"."))
    }
    ## For compile_time, we can provide error message if there are multiple nimble MCMCs,
    ## since their compile_time is bundled together.
  }
  results
}
