MCMCdefs_env <- new.env()

#' @export
registerMCMCengine <- function(name, fun) {
  MCMCdefs_env[[name]] <- fun
}

registerMCMCengine('jags', MCMCdef_jags)
registerMCMCengine('dummy', MCMCdef_dummy)
registerMCMCengine('stan', MCMCdef_stan)