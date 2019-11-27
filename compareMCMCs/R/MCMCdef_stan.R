## SP: This can support running stan via compareMCMCs function 'standalone'
## I don't think that this would work with multiple MCMCs engines by now

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
  ## - stan_model: name of .stan file containing the modelcode  ## SP: was only model, but then it is rewritten form compareMCMCs function
  ## - data: should be already in long format?
  ## - init: name of a file with initial values in the long format required by Stan

  stanInfo <- modelInfo
  
  if(is.null(stanInfo))
    stop("stan MCMC was requested but there is no stan entry in modelInfo.")
  if(requireNamespace('rstan', quietly = TRUE)) {
    
    stan_model <- stanInfo$stan_model
    if(is.null(stan_model) | stan_model == '')
      stop('must provide \'model\' argument to run Stan MCMC')
    
    dataFile <- stanInfo$data
    
    if(is.null(dataFile))
      stop("stan entry in modelInfo is missing a data entry.")
    
    if(!is.list(dataFile)){
      
      stop("need to pass data in stan supported format")
      ## SP: fileToList not found
      # constantsAndDataStan <- fileToList(dataFile)
   
    } else {
      constantsAndDataStan <- dataFile
    }

    initFile <- stanInfo$init
    if(is.null(initFile))
      stop("stan entry in modelInfo is missing an init entry.")
    if(!is.list(initFile)) {
      if(file.exists(initFile)){
        initsStan <- fileToList(initFile)
      } else {
        initsStan <- NULL
      }
    } else {
      initsStan <- initFile
    }

    compileTime <- system.time(stan_mod <- rstan::stan_model(file = stan_model))

    ## SP: doubling up niter since stan by default uses 
    if(is.null(initsStan)) {
      ## missing model.init.R file (stan inits file)
      runTime <- system.time(
        stan_out <- rstan::sampling(stan_mod,
                                    data=constantsAndDataStan,
                                    chains=1,
                                    iter= 2*MCMCcontrol$niter,
                                    thin=MCMCcontrol$thin))
    } else {
      ## we have the model.init.R file
      ## this one includes inits = ...
      runTime <- system.time(
        stan_out <- rstan::sampling(stan_mod,
                                    data=constantsAndDataStan,
                                    chains=1,
                                    iter= 2*MCMCcontrol$niter,
                                    thin=MCMCcontrol$thin,
                                    init=list(initsStan)))
    }


    tempArray <- rstan::extract(stan_out,
                                permuted = FALSE,
                                inc_warmup = TRUE)[, 1, ]
    
    ### ---- to discuss ---- #
    ## SP: rstan::get_elapsed_time() returns time in a different format from system.type, so I am just putting some zeros to maintain the format across different types of MCMCresults
    ## sampling time, discarding warmup

    timeResult <- c(0, 0, rstan::get_elapsed_time(stan_out)[2]) 
    ### ---- end to discuss ---- #

    monitors <- MCMCcontrol$monitors
    if(!all(monitors %in% dimnames(tempArray)[[2]])) {
      missingNames <- setdiff(monitors, dimnames(tempArray)[[2]])
      warning(paste0('Stan output is missing values for: ',
                     paste0(missingNames,collapse=', ')))
    }
    # samplesArray <- array(0, dim = c(nkeep, length(monitors)))
    # dimnames(samplesArray)[[2]] <- monitors
    
    monitorsWeHave <- intersect(monitorInfo$monitors, dimnames(tempArray)[[2]])
    
    ### ---- to discuss ---- #
    ## SP: doubling up niter since stan by default uses 
    samplesArray <- tempArray[(MCMCcontrol$niter+1):floor((2*MCMCcontrol$niter)/MCMCcontrol$thin),
                               monitorsWeHave,
                               drop=FALSE]
    ### ---- end to discuss ---- #

    ## return MCMCresult object, with samples and time populated                                         
    result <- MCMCresult$new(samples = samplesArray,
                               times = list(sample = timeResult))
    # unlink(modelFileName)
    return(result)
  } else {
    stop("stan MCMC was requested but the rstan package is not installed.")
    return(NULL)
  }
}
