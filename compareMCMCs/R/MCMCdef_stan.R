#' @rdname builtin_MCMCs
#' @export
MCMCdef_stan <- function(MCMCinfo, 
                        MCMCcontrol, 
                        monitorInfo, 
                        modelInfo) {
  MCMCdef_stan_impl(MCMCinfo,
                    MCMCcontrol, 
                    monitorInfo, 
                    modelInfo)
}
## externalMCMCinfo in compareMCMCs becomes MCMCinfo here If there is
## an external MCMC engine named "myMCMC", then a list element
## "myMCMC" of \code{externalMCMCinfo} will be passed to the engine as
## its \code{MCMCinfo} argument.

MCMCdef_stan_impl <- function(MCMCinfo,
                              MCMCcontrol, 
                              monitorInfo, 
                              modelInfo) {
  ## For this plugin, MCMCinfo is a list containing
  ## - file: name of .stan file containing the model code
  ## - data: should be already in long format?
  ## - init: name of a file with initial values in the long format
  ## required by Stan
  ## - stan_model_args
  ## - stan_sampling_args
  
  ## If the user provides externalMCMCinfo = list(stan = list(file =
  ## "stan_code.stan")), then in this function, MCMCinfo will be
  ## list(file = "stan_code.stan")

  ##
  ##  Note: stan has a seed argument, which we will get from
  ##    MCMCcontrol$seed, which will have the value of the seed
  ##    argument to compareMCMCs
  ##
  ##  Note: in rstan::sampling function the `iter` argument comprises
  ##  also the number of warmup iterations
  ##  
  
  if(is.null(MCMCinfo))
      stop(paste0("stan MCMC was requested but there  is no ", #lacks test coverage
                  "stan entry in externalMCMCinfo."))
  if(!requireNamespace('rstan', quietly = TRUE))
    stop("stan MCMC was requested but the rstan package is not installed.") #lacks test coverage

  # Check for version of rstan that doesn't work on Windows
  # See https://blog.mc-stan.org/2022/04/26/stan-r-4-2-on-windows/
  if(!isTRUE(MCMCinfo[["skip_rstan_version_check"]])) {
    if(.Platform$OS.type == "windows") {
      if(as.numeric(R.Version()$major) >= 4) {
        if(as.numeric(R.Version()$minor) >= 2.0) {
          if(utils::packageVersion("rstan") <= "2.21.5") {
            stop("This version of rstan on Windows does not work for R version >= 4.2.0.\n",
                 "See https://blog.mc-stan.org/2022/04/26/stan-r-4-2-on-windows/.\n",
                 "Try the instructions for installing a newer version from the Stan repositories.\n",
                 "If you want to disable this error and run rstan anyway,\n",
                 " include 'skip_rstan_version_check = TRUE' in the externalMCMCinfo element for stan.\n",
                 "Specifically, in the call to compareMCMCs, use something like\n",
                 "'externalMCMCinfo=list(stan=list(skip_rstan_version_check=TRUE, <your other stan info>))'.")
          }
        }
      }
    }
  }
  
  ## extract base elements from MCMCInfo
  fileStan <- MCMCinfo$file
  dataStan <- MCMCinfo$data
  initStan <- MCMCinfo$init
  
  ## extract general elements from MCMCInfo
  stan_model_args <- if(!is.null(MCMCinfo$stan_model_args)) 
    MCMCinfo$stan_model_args else list() 
  sampling_args   <- if(!is.null(MCMCinfo$sampling_args)) 
    MCMCinfo$sampling_args   else list()
  
  if((is.null(fileStan) | isTRUE(fileStan == '')) & 
     is.null(stan_model_args$file) & 
     is.null(stan_model_args$model_code))
    stop(paste('You must provide model information to run Stan MCMC either via',
                '(i) a \'file\' element in the externalMCMCinfo list for \'stan\' ',
                'or (ii) a \'file\' or (iii) a \'model_code\' element in the',
                'stan_model_args (list) element in the externalMCMCinfo for',
                '\'stan\'. See help(\'builtin_MCMCs\') for more information.'))
  
  # It may be valid (unusual, but valid) to run without data, as it is
  # for nimble.
  
  ## explicit file argument, if provided, takes precedence
  if(!is.null(fileStan)) stan_model_args$file <- fileStan
  
  ## Create stan_model object
    compileTime <- system.time(stan_mod <- do.call(
                                   rstan::stan_model, stan_model_args))
  
  ## modify sampling args
  sampling_args$object <- stan_mod ## object of class stanmodel
  # If a user provided init, data, explicitly, they take precedence:
  if(!is.null(initStan))  sampling_args$init  <- initStan #lacks test coverage
  if(!is.null(dataStan)) sampling_args$data   <- dataStan #lacks test coverage
  if(!is.null(sampling_args$chains))
    if(sampling_args$chains != 1) #lacks test coverage
      warning("Stan chains value will be over-ridden and set to 1")
  sampling_args$chains <- 1
  
  ##  Note: in rstan::sampling function the `iter` argument includes
  ##  warmup iterations
  # Explcitly provided arguments take precedence over MCMCcontrol entries
  if(is.null(sampling_args$warmup))
    sampling_args$warmup <- floor(MCMCcontrol$niter/2) #lacks test coverage
  # This warmup setting matches Stan's default
  if(is.null(sampling_args$iter))
    sampling_args$iter   <- MCMCcontrol$niter #lacks test coverage
  if(is.null(sampling_args$thin))
    sampling_args$thin   <- MCMCcontrol$thin
  
  # Stan accepts a seed argument.
  # compareMCMCs also accepts a seed argument.  If a seed is provided,
  #    set.seed is called before calling each MCMC engine, such as this one.
  # Therefore Stan's seed argument may not be necessary, but nevertheless
  #    a user might for some reason want to provide one.
  # Therefore we do not do the following.  Instead a user must provide
  #   sampling_args$seed if they want it.
  # 
  #  What we don't do:
  # if(is.null(sampling_args$seed))
  #   sampling_args$seed   <- MCMCcontrol$seed
  
  if(!is.null(sampling_args$pars)) {
    message(paste0("Over-riding monitors with sampling_args$pars", #lacks test coverage
                   "(since it was provided) for Stan."))
  } else {
    sampling_args$pars <- monitorInfo$monitorVars
  }
  
  runTime <- system.time(
    stan_out <- do.call(rstan::sampling, sampling_args))
  
  ## Warmup samples should not be included.
  samplesArray <- rstan::extract(stan_out, 
                                 # pars = monitorInfo$monitorVars, # this can make type of returned object case-dependent, so use [] below
                                 permuted = FALSE,
                                 inc_warmup = FALSE)[, 1, ]
  
  ## Stan provides its own timings, but there is a need
  ## to have them be comparable to those for other MCMCs.
  ## What we do is use the system.time result for total sampling time
  ## so that it is recorded in a similar way as for other MCMCs.
  ## But we use Stan's internal timings, from get_elapsed_time,
  ## to separate the warmup (burnin) time.
  
  stan_times <- rstan::get_elapsed_time(stan_out)
  burninTime <- stan_times[1]
  samplingTime <- runTime[3]
  postburninTime <- samplingTime - burninTime
    
  ## return MCMCresult object, with samples and time populated
  ## SP 'sample' is the default tilme                                         

  # burnin_system.time not available as a system.time because burnin
  # comes from rstan::get_elapsed_time.
  # postburnin_system.time ditto
  result <- MCMCresult$new(samples = samplesArray[, colnames(samplesArray) != "lp__", drop=FALSE],
                           times = list(setup_system.time = compileTime,
                                        sampling_system.time = runTime,
                                        setup = compileTime[3],
                                        burnin = burninTime,
                                        postburnin = postburninTime,
                                        sampling = samplingTime))
  result
}
