#' @export
MCMCsuite <- function(
  code,
  constants           = list(),
  data                = list(),
  inits               = list(),
  monitors            = character(),
  niter               = 10000,
  burnin              = 2000,
  thin                = 1,
  summaryStats        = c('mean', 'median', 'sd', 'CI95_low', 'CI95_upp'),
  calculateEfficiency = FALSE,
  MCMCs               = 'nimble',
  MCMCdefs            = list(),
  winbugs_directory   = 'C:/WinBUGS14',    #to be deprecated
  winbugs_program     = 'WinBUGS',         #to be deprecated
  openbugs_directory  = 'C:/OpenBUGS323',  #to be deprecated
  openbugs_program    = 'OpenBUGS',        #to be deprecated
  stan_model          = '',                #to be deprecated
  stan_inits          = NULL,              #to be deprecated
  stan_data           = NULL,              #to be deprecated
  stanNameMaps        = list(),            #to be deprecated
  makePlot            = TRUE,
  savePlot            = TRUE,
  plotName            = 'MCMCsuite',
  setSeed             = TRUE,
  check               = getNimbleOption('checkModel'),
  debug               = FALSE
) {
  # This function is designed to maintain backward compatibility, at least at first.
  # We may want to go through a planned deprecation process.
  externalMCMCinfo <- list()
  # Collect openbugs, winbugs, and stan information and issue future-deprecation messages
  if(!missing(winbugs_directory) | !missing(winbugs_program)) {
    # message(paste0('Arguments winbugs_directory and winbugs_program may be\n',
    #                'deprecated in the future.  It is now recommended to provide\n',
    #                'this information via externalMCMCinfo.'))
    if('winbugs' %in% names(externalMCMCinfo))
      stop(paste('If a "winbugs" entry is provided in externalMCMCinfo,\n',
                 'it is invalid to provide winbugs_directory or winbugs_program.\n'))
    externalMCMCinfo$winbugs <- list(winbugs_directory = winbugs_directory,
                                     winbugs_program = winbugs_program)
  }
  if(!missing(openbugs_directory) | !missing(openbugs_program)) {
    # message(paste0('Arguments openbugs_directory and openbugs_program may be\n',
    #                'deprecated in the future.  It is now recommended to provide\n',
    #                'this information via externalMCMCinfo.'))
    if('openbugs' %in% names(externalMCMCinfo))
      stop(paste('If a "openbugs" entry is provided in externalMCMCinfo,\n',
                 'it is invalid to provide openbugs_directory or openbugs_program.\n'))
    externalMCMCinfo$openbugs <- list(openbugs_directory = openbugs_directory,
                                     openbugs_program = openbugs_program)
  }
  if(!missing(stan_model) |
     !missing(stan_data) |
     !missing(stan_inits) |
     !missing(stanNameMaps)) {
    # message(paste0('Arguments stan_model, stan_data, stan_inits, and stanNameMaps may be\n',
    #                'deprecated in the future.  It is now recommended to provide\n',
    #                'this information via externalMCMCinfo.'))
    if('stan' %in% names(externalMCMCinfo))
      stop(paste('If a "stan" entry is provided in externalMCMCinfo,\n',
                 'it is invalid to provide other stan-related arguments.\n'))
    externalMCMCinfo$stan <- list(stan_model = stan_model,
                                  stan_data = stan_data,
                                  stan_inits = stan_inits,
                                  stanNameMaps = stanNameMaps)
  }

  modelInfo <- list(code = code,
                    constants = constants,
                    data = data,
                    inits = inits,
                    check = check)
  MCMCcontrol <- list(niter = niter,
                       thin = thin,
                       burnin = burnin)
  results <- compareMCMCs (
    modelInfo = modelInfo,
    MCMCcontrol = MCMCcontrol,
    MCMCs = MCMCs,
    nimbleMCMCdefs = MCMCdefs,
    externalMCMCinfo = externalMCMCinfo,
    seed = setSeed,
    monitors = monitors,
    metrics = summaryStats
  )
}
