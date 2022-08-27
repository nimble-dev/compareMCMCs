#' @rdname builtin_MCMCs
#' @importFrom stats update
#' @export
MCMCdef_jags <- function(MCMCinfo, 
                         MCMCcontrol, 
                         monitorInfo, 
                         modelInfo) {
  MCMCdef_jags_impl(MCMCinfo,
                    MCMCcontrol, 
                    monitorInfo, 
                    modelInfo)
}

MCMCdef_jags_impl <- function(MCMCinfo,
                              MCMCcontrol, 
                              monitorInfo, 
                              modelInfo) {
  if(requireNamespace('rjags', quietly = TRUE)) {
    modelFileName <- file.path(tempdir(), 'model.txt')
    code <- modelInfo$code
    constantsAndData <- c(modelInfo$constants, modelInfo$data)
    writeLines(paste0('model\n', paste0(deparse(code), collapse='\n')),
               con=modelFileName)
    setupTimeResult <- system.time({
      jags_mod <- rjags::jags.model(file=modelFileName,
                                  data=constantsAndData,
                                  inits=modelInfo$inits,
                                  n.chains=1,
                                  quiet=FALSE)
    })
    setupTime <- setupTimeResult[3]
    
    burninTimeResult <- system.time(0)   ## needs to be defined even when burnin = 0
    burninTime <- 0
    if(MCMCcontrol$burnin > 0) {
        burninTimeResult <- system.time(
            update(object = jags_mod,
                   n.iter = MCMCcontrol$burnin))
        burninTime <- burninTimeResult[3]
    }

    postburninTimeResult <- system.time({
        jags_out <- rjags::coda.samples(
                               model=jags_mod,
                               variable.names=monitorInfo$monitorVars,
                               n.iter=MCMCcontrol$niter - MCMCcontrol$burnin,
                               thin=MCMCcontrol$thin)
    })
    postburninTime <- postburninTimeResult[3]
    
    monitorNodesBUGS <- gsub(' ', '', monitorInfo$monitors)
    samplesArray <- jags_out[[1]][, monitorNodesBUGS, drop=FALSE]
    result <- MCMCresult$new(samples = samplesArray,
                             times = list(
                                 setup_system.time = setupTimeResult,
                                 burnin_system.time = burninTimeResult,
                                 postburnin_system.time = postburninTime,
                                 setup = setupTime,
                                 sampling = postburninTime + burninTime,
                                 burnin = burninTime,
                                 postburnin = postburninTime))
    unlink(modelFileName)
    return(result)
  } else {
      stop(paste0("run_jags: rjags package is required for 'jags' option. ",
                  "jags will not be run.\n"))
    return(NULL)
  }
}
