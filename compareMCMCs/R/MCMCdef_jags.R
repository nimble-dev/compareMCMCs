MCMCdef_jags <- function(runInfo, MCMCinfo, otherInfo) {
  MCMCdef_jags_impl(runInfo, MCMCinfo, otherInfo)
}

MCMCdef_jags_impl <- function(runInfo, MCMCinfo, otherInfo) {
  if(requireNamespace('rjags', quietly = TRUE)) {
    modelFileName <- 'model.txt'
    code <- otherInfo$Rmodel$modelDef$BUGScode
    constantsAndData <- c(otherInfo$constants, otherInfo$data)
    writeLines(paste0('model\n', paste0(deparse(code), collapse='\n')),
               con=modelFileName)
    jags_mod <- rjags::jags.model(file=modelFileName,
                                  data=constantsAndData,
                                  inits=otherInfo$inits,
                                  n.chains=1,
                                  quiet=FALSE)
    timeResult <- system.time({
      jags_out <- rjags::coda.samples(model=jags_mod,
                                      variable.names=otherInfo$monitorVars,
                                      n.iter=MCMCinfo$niter,
                                      thin=MCMCinfo$thin)
    })
    monitorNodesBUGS <<- gsub(' ', '', otherInfo$monitorNodesNIMBLE)
    samplesArray <- jags_out[[1]][(MCMCinfo$burnin+1):floor(MCMCinfo$niter/MCMCinfo$thin),
                                  monitorNodesBUGS,
                                  drop=FALSE]
    result <- MCMCresult$new(samples = samplesArray,
                             times = list(sample = timeResult))
    unlink(modelFileName)
    return(result)
  } else {
    stop("run_jags: rjags package is required for 'jags' option.  jags will not be run.\n")
    return(NULL)
  }
}
