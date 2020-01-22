MCMCdef_stan <- function(MCMCinfo, 
                        MCMCcontrol, 
                        monitorInfo, 
                        modelInfo) {
  MCMCdef_stan_impl(MCMCinfo,
                    MCMCcontrol, 
                    monitorInfo, 
                    modelInfo)
}
## externalMCMCinfo in compareMCMCs becomes MCMCinfo here
## A list of information, named by external MCMC engine names, to provided to each engine.
## If there is an external MCMC engine named "myMCMC", then a list element "myMCMC" of \code{externalMCMCinfo} will be passed to the engine as its \code{MCMCinfo} argument.

MCMCdef_stan_impl <- function(MCMCinfo,
                              MCMCcontrol, 
                              monitorInfo, 
                              modelInfo) {
  ## SP: modelInfo is a list containing
  ## - stan_model: name of .stan file containing the modelcode  ## SP: was only model, but then it is rewritten form compareMCMCs function
  ## - data: should be already in long format?
  ## - init: name of a file with initial values in the long format required by Stan

  ## If the user provides externalMCMCinfo = list(stan = list(file = "stan_code.stan")),
  ## then in this function, MCMCinfo will be list(file = "stan_code.stan")
  
  ## Plan:
  ## Allow two options for elements in MCMCinfo:
  ## Simple mode:
  ##    1. file (to be passed as argument file to rstan::stan_model)
  ##    2. data (to be passed as argument data to rstan::sampling)  
  ##    3. init (to be passed as argument init to rstan::sampling)
  ## General mode:
  ##    1. stan_model_args (to be passed as argument list to rstan::stan_model)
  ##    2. sampling_args (to be modified and passed as argument list to rstan::sampling)
  ##
  ##  Note: stan has a seed argument, which we will get from MCMCcontrol$seed,
  ##    which will have the value of the seed argument to compareMCMCs
  ##  Note: in rstan::sampling function the `iter` argument comprises also the number of warmup iterations
  ##  
  
  	if(is.null(MCMCinfo))
   		stop("stan MCMC was requested but there is no stan entry in externalMCMCinfo")
  	if(requireNamespace('rstan', quietly = TRUE)) {
    
    ## extract base elements from MCMCInfo
    fileStan <- MCMCinfo$file
    dataStan <- MCMCinfo$data
    initStan <- MCMCinfo$init

    if(is.null(fileStan) | fileStan == '') stop('must provide \'file\' argument to run Stan MCMC')
    
    if(is.null(dataStan)) stop("stan entry in externalMCMCinfo is missing a data entry.")
    
    if(!is.list(dataStan)) stop("need to pass data in stan supported format")   
      
    ## extract general elements from MCMCInfo
  	stan_model_args <- if(!is.null(MCMCinfo$stan_model_args)) MCMCinfo$stan_model_args else list() 
  	sampling_args   <- if(!is.null(MCMCinfo$sampling_args))   MCMCinfo$sampling_args   else list()

  	## modify stan_model_args
	stan_model_args$file <- fileStan
	## Create stan_model object
	compileTime <- system.time(stan_mod <- do.call(rstan::stan_model, stan_model_args))

  	## modify sampling args
	sampling_args$object <- stan_mod ## object of class stanmodel
    sampling_args$data   <- dataStan
    sampling_args$chains <- 1

    ##  Note: in rstan::sampling function the `iter` argument comprises also the number of warmup iterations
    sampling_args$iter   <- MCMCcontrol$niter*2  ## SP: using default stan warmup (half of niter)
    sampling_args$warmup <- floor(MCMCcontrol$niter/2)
    sampling_args$thin   <- MCMCcontrol$thin
    sampling_args$seed   <- MCMCcontrol$seed
    
    ## SP: we could have the monitors passed to rstan:::sampling function
    # sampling_args$pars <- MCMCcontrol$monitors

    ## Should 
    # sampling_args$pars   <- MCMCcontrol$monitors
    ## SP: stan has a default random initialization:
    ## 1) should we allow for that? 
  	## 2) if yes, should we give a warning message?
    
    if(is.null(initStan)) {
      ## missing init (uses stan random initialization)
      runTime <- system.time(
        stan_out <- do.call(rstan::sampling, sampling_args))
    } else {
    	sampling_args$init   <- initStan

      ## we have the model.init.R file
      ## this one includes inits = ...
      runTime <- system.time(
        stan_out <- do.call(rstan::sampling, sampling_args))
    }

    ## SP: should we include warmup samples?  
    tempArray <- rstan::extract(stan_out, 
    							pars = monitorInfo$monitors, 
                                permuted = FALSE,
                                inc_warmup = FALSE)[, 1, ]
    
    ## SP: rstan::get_elapsed_time() returns time in a different format from system.time,
    ## I am just adding some zeros to maintain the format across different types of MCMCresults
    ## sampling time, discarding warmup
 
    sampleTime <- c(0, 0, rstan::get_elapsed_time(stan_out)[2]) 
    totalTime  <- runTime

    ##---------------------------------------##
    ## SP: choose how to deal with monitors; to make this chunk of code working
    # monitors <- monitorInfo$monitors
    
    # if(!all(monitors %in% dimnames(tempArray)[[2]])) {
    #   missingNames <- setdiff(monitors, dimnames(tempArray)[[2]])
    #   warning(paste0('Stan output is missing values for: ',
    #                  paste0(missingNames,collapse=', ')))
    # }
    # dimnames(samplesArray)[[2]] <- monitors
    
    # monitorsWeHave <- intersect(monitorInfo$monitors, dimnames(tempArray)[[2]])
    
    # samplesArray <- tempArray[, monitorsWeHave,
    #                            drop=FALSE]
    ##---------------------------------------##

    samplesArray <- tempArray

    ## return MCMCresult object, with samples and time populated
    ## SP 'sample' is the default tilme                                         
    result <- MCMCresult$new(samples = samplesArray,
                               times = list(sample = totalTime, sampling = sampleTime, compile = compileTime))
    return(result)
  } else {
    stop("stan MCMC was requested but the rstan package is not installed.")
    return(NULL)
  }
}
