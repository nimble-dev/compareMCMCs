MCMCdef_stan <- function(MCMCinfo, 
                        MCMCcontrol, 
                        monitorInfo, 
                        modelInfo) {
  MCMCdef_stan_impl(MCMCinfo,
                    MCMCcontrol, 
                    monitorInfo, 
                    modelInfo)
}

MCMCdef_stan_impl <- function(MCMCinfo,
                              MCMCcontrol, 
                              monitorInfo, 
                              modelInfo) {
  ## SP: modelInfo is list containing
  ## model: name of .stan file containing the modelcode 
  ## data: should be already in long format?
  ## init: optional initial values 

  stanInfo <- modelInfo[['stan']]
  if(is.null(stanInfo))
    stop("stan MCMC was requested but there is no stan entry in modelInfo.")
  if(requireNamespace('rstan', quietly = TRUE)) {
    stop("stan MCMC was requested but but the rstan package is not installed.")
  }
  stan_model <- stanInfo$model
  if(is.null(stan_model) | stan_model == '')
    stop('must provide \'model\' argument to run Stan MCMC')
  dataFile <- stanInfo$data
  if(is.null(dataFile))
    stop("stan entry in modelInfo is missing a data entry.")
  if(!is.list(dataFile))
    constantsAndDataStan <- fileToList(dataFile)
  else
    constantsAndDataStan <- dataFile

  initFile <- stanInfo$init
  if(is.null(initFile))
    stop("stan entry in modelInfo is missing an init entry.")
  if(!is.list(initFile)) {
    if(file.exists(initFile))
      initsStan <- fileToList(initFile)
    else
      initsStan <- NULL
  } else
    initsStan <- initFile

  compileTime <- system.time(stan_mod <- rstan::stan_model(file = stan_model))

  if(is.null(initsStan)) {
    ## missing model.init.R file (stan inits file)
    runTime <- system.time(
      stan_out <- rstan::sampling(stan_mod,
                                  data=constantsAndDataStan,
                                  chains=1,
                                  iter=MCMCcontrols$niter,
                                  thin=MCMCcontrols$thin))
  } else {
    ## we have the model.init.R file
    ## this one includes inits = ...
    runTime <- system.time(
      stan_out <- rstan::sampling(stan_mod,
                                  data=constantsAndDataStan,
                                  chains=1,
                                  iter=MCMCcontrols$niter,
                                  thin=MCMCcontrols$thin,
                                  init=list(initsStan)))
  }


  tempArray <- rstan::extract(stan_out,
                              permuted = FALSE,
                              inc_warmup = FALSE)[, 1, ]
  
  ## sampling time, discarding warmup
  ## SP: rstan::get_elapsed_time() returns time in a different format from system.type, so I am just putting some 0 before to maintain the format across different types of MCMCresults

  timeResult <- c(0, 0, rstan::get_elapsed_time(stan_out)[2]) 

  monitors <- MCMCcontrols$monitors
  if(!all(monitors %in% dimnames(tempArray)[[2]])) {
    missingNames <- setdiff(monitors, dimnames(tempArray)[[2]])
    warning(paste0('Stan output is missing values for: ',
                   paste0(missingNames,collapse=', ')))
  }
  samplesArray <- array(0, dim = c(nkeep, length(monitors)))
  dimnames(samplesArray)[[2]] <- monitors
  monitorsWeHave <- intersect(monitors, dimnames(tempArray)[[2]])
  samplesArray[, monitorsWeHave] <- tempArray[1:floor(niter/thin),
                                              monitorsWeHave,
                                              drop=FALSE]
  
  ## return MCMCresult object, with samples and time populated                                         
  result <- MCMCresult$new(samples = samplesArray,
                             times = list(sample = timeResult))
  # unlink(modelFileName)
  return(result)
}
