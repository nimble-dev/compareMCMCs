#' @importFrom nimble buildMCMC compileNimble
runNIMBLE <- function(nimbleMCMCs,
                      MCMCdefs,
                      modelInfo,
                      MCMCcontrol,
                      monitorInfo,
                      seed,
                      parent.frame,
                      sessionInfo = TRUE){
  if(missing(parent.frame))
    parent.frame <- parent.frame() # lacks test coverage
  RmcmcFunctionList <- list()
  CmcmcFunctionList <- list()
  nNimbleMCMCs <- length(nimbleMCMCs)
  for(iMCMC in seq_along(nimbleMCMCs)) {
    mcmcTag <- nimbleMCMCs[iMCMC]
    mcmcDef <- MCMCdefs[[mcmcTag]]
    if(is.function(mcmcDef)) {
      mcmcConf <- eval(call("mcmcDef", modelInfo$model))
    } else if(is.character(mcmcDef)) {
      mcmcConf <- eval(call(mcmcDef, modelInfo$model),
                       envir = parent.frame)
    } else {
      RmodelEnv <- new.env()
      RmodelEnv$model <- modelInfo$model
      mcmcConf <- eval(mcmcDef, envir = RmodelEnv)
    }
    mcmcConf$addMonitors(monitorInfo$monitorVars, print = FALSE)
    RmcmcFunctionList[[mcmcTag]] <- nimble::buildMCMC(mcmcConf)
  }
  compile_time <- system.time({
    Cmodel <- try(nimble::compileNimble(modelInfo$model))
    if(inherits(Cmodel, 'try-error')) {
      stop("There was a problem compiling the nimble model.") # lacks test coverage
    }
    CmcmcFunctionList_temp <- try(
      nimble::compileNimble(RmcmcFunctionList,
                            project = modelInfo$model))
    if(inherits(CmcmcFunctionList_temp, 'try-error')) {
      stop("There was a problem compiling one or more nimble MCMCs.") # lacks test coverage
    }
    ## Arguably the following if-then-else should be outside of the system.time,
    ## but it was inside originally so it will stay there for now.
    if(nNimbleMCMCs == 1)
      CmcmcFunctionList[[nimbleMCMCs[1]]] <- CmcmcFunctionList_temp
    else
      CmcmcFunctionList <- CmcmcFunctionList_temp # lacks test coverage
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
    if(!is.null(seed)) set.seed(as.numeric(seed)) # lacks test coverage
    if(sessionInfo) sessionInfo_result <- sessionInfo()
    timeResult <- try(system.time({ Cmcmc$run(MCMCcontrol$niter, 
                                              nburnin = MCMCcontrol$burnin,
                                              thin = MCMCcontrol$thin) }))
    if(!inherits(timeResult, 'try-error')) {
      CmvSamples <- Cmcmc$mvSamples
      samplesArray <- as.matrix(CmvSamples, varNames = monitorInfo$monitorVars)
      samplesArray <- samplesArray[, monitorInfo$monitors, drop=FALSE]
      samplingTime <- timeResult[3]
      burninTime <- samplingTime * MCMCcontrol$burnin / MCMCcontrol$niter
      postburninTime <- samplingTime - burninTime
      results[[mcmcTag]] <-
        MCMCresult$new(
          samples = samplesArray,
          times = list(setup_system.time = compile_time,
                       sampling_system.time = timeResult,
                       sampling = samplingTime,
                       burnin = burninTime,
                       postburnin = postburninTime,
                       setup = compile_time[3]),
          MCMC = mcmcTag)
      if(sessionInfo) results[[mcmcTag]]$sessionInfo <- sessionInfo_result
    } else {
      warning(paste0("There was a problem running ", mcmcTag,".")) # lacks test coverage
    }
    ## For compile_time, we could give a message if there are multiple
    ## nimble MCMCs,
    ## since their compile_time is bundled together.
  }
  results
}
