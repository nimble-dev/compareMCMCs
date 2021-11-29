MCMCdefs_env <- new.env()

#' Register an MCMC function for use by compareMCMCs
#' 
#' @param name The name by which the MCMC function (or "engine") is identified
#' in the `MCMCs` argument to \code{\link{compareMCMCs}}.
#' 
#' @param fun The function that runs and times an MCMC.
#' 
#' @details
#' See package vignette for information about the arguments
#' that will be passed to `fun` from `compareMCMCs` and the `MCMCresult`
#' object that should be returned by `fun`.
#' 
#' For more information, see \link{builtin_MCMCs}.
#' 
#' MCMCs from `nimble` are run in a different way, since there can be 
#' multiple MCMCs for the same `nimble` model.  These are run by
#' `runNIMBLE`, which is not exported.
#' 
#' @export
registerMCMCengine <- function(name, fun) {
  MCMCdefs_env[[name]] <- fun
}

.onLoad <- function(libname, pkgname) {
  registerMCMCengine('jags', MCMCdef_jags)
  registerMCMCengine('dummy', MCMCdef_dummy)
  registerMCMCengine('stan', MCMCdef_stan)
  
  registerMetrics(
    list(mean = MCMCmetric_mean,
         median = MCMCmetric_median,
         sd = MCMCmetric_sd,
         CI95_low = MCMCmetric_CI95low,
         CI95_upp = MCMCmetric_CI95upp,
         CI95 = MCMCmetric_CI95,
         ESS = MCMCmetric_ESS,
         ESS_coda = MCMCmetric_ESS,
         efficiency = MCMCmetric_efficiency,
         efficiency_coda = MCMCmetric_efficiency
         )
  )
  ## Set up library for building page components
  registerPageComponents(
    list(
      timing = list(
        make = 'timeComparisonComponent',
        linkText = "MCMC sampling time"),
      efficiencySummary = list(
        make = 'minMeanComparisonComponent',
        fileSuffix = "_efficiencySummary",
        linkText = "MCMC efficiency summary",
        control = list(
          invert = FALSE,  # default, but provided explicitly for clarity
          min_efficiency_name = "min_efficiency", 
          mean_efficiency_name = "mean_efficiency",
          suffix = "")),
      efficiencySummaryAllParams = list(
        make = 'minMeanAllComparisonComponent',
        fileSuffix = "_efficiencySummaryAll",
        linkText = "MCMC efficiency summary (with all parameters)",
        plot = 'plotMinMeanAll',
        control = list(
          invert = FALSE,  # default, but provided explicitly for clarity
          min_efficiency_name = "min_efficiency", 
          mean_efficiency_name = "mean_efficiency",
          efficiency_name = "efficiency",
          suffix = "")),
      paceSummaryAllParams = list(
        make = 'minMeanAllComparisonComponent',
        fileSuffix = "_paceSummaryAll",
        linkText = "MCMC pace summary (with all parameters)",
        plot = 'plotMinMeanAll',
        control = list(invert = TRUE)),
      efficiencyDetails = list
      (make = 'efficiencyDetailsComparisonComponent',
        fileSuffix = "_efficiencyDetails",
        linkText = "MCMC efficiency details",
        control = list(
          ncol = 4,
          efficiencyName = "efficiency",
          suffix = "")),
      posteriorSummary = list(
        make = 'posteriorSummaryComparisonComponent',
        fileSuffix = "_posteriorSummary",
        linkText = "Posterior summaries",
        control = list(ncol = 4)))
  )
}

#' MCMC plugins that come with the compareMCMCs package
#' 
#' These functions are normally called from
#' \code{\link{compareMCMCs}}, which passes its arguments or elements
#' extracted from its arguments to these functions.
#' 
#' @name builtin_MCMCs
#'
#' @aliases MCMCdef_jags MCMCdef_stan MCMCdef_dummy
#'
#' @param MCMCinfo The named element of `externalMCMCinfo` argument to
#'   \code{\link{compareMCMCs}} that matches a particular MCMC.
#'   ("External" refers to any MCMC that is not internal to `nimble`.)
#' @param MCMCcontrol The `MCMCcontrol` argument to
#'   \code{\link{compareMCMCs}}, with the `seed` argument added as a
#'   list element if it was provided.
#' @param monitorInfo A list with elements `monitors` and
#'   `monitorVars`, providing two formats of information on model
#'   parameters for which MCMC output should be recorded.
#' @param modelInfo The `modelInfo` argument to
#'   \code{\link{compareMCMCs}}
#' 
#' @details These functions are called internally from
#'   \code{\link{compareMCMCs}}.  Each one runs an MCMC engine.
#'   Functions to interface to other MCMC engines can be registered
#'   via \code{\link{registerMCMCengine}}.
#' 
#' MCMCs in `nimble` are run from `runNIMBLE`.  This uses a different
#' system because there may be multiple nimble MCMC configurations for
#' one model.
#' 
#' `MCMCdef_dummy` does not run a real MCMC.  It provides a quick way
#' to generate MCMC-formatted output for testing other parts of this
#' package.
#' 
#' `MCMCdef_jags` runs JAGS via package `rjags`.  It uses model
#' information from `modelInfo`.  It does not use `MCMCinfo`.
#' 
#' `MCMCdef_stan` runs Stan via package `rstan`.  It does not use
#' `modelInfo`.  It accepts the following elements of the `MCMCinfo`
#' list:
#' 
#' - `file`: `file` argument to `stan_model` function in `rstan`.
#' This can alternatively be provided via `stan_model_args$file`.
#'
#' - `data`: `data` argument to `sampling` function in `rstan`.  This
#' can alternatively be provided via `sampling_args$data`.
#'
#' - `inits`: `inits` argument to `sampling` function in `rstan`.
#' This can alternatively be provided via `sampling_args$inits`.
#'
#' - `stan_model_args`: list of arguments to `stan_model`.
#'
#' - `sampling_args`: list of arguments to `sampling`.
#' 
#' The elements `file`, `data`, and `inits` take precendence over
#' corresponding entries in `stan_model_args` or `sampling_args`.
#' 
#' If elements `warmup`, `iter`, and/or `thin` are provided in
#' `sampling_args`, those take precedence over corresponding values in
#' the `MCMCcontrol` argument to `compareMCMCs`.  Otherwise `iter` is
#' set to `MCMCcontrol$niter` and `warmup` is set to
#' `MCMCcontrol$niter/2`.  Only one chain will be run.
#' 
#' Total sampling time for Stan is recorded via
#' `system.call(sampling(...))`.  This is similar to how time is
#' recorded for other MCMCs. The warmup time (called "burnin" in
#' `compareMCMCs` for consistency across different MCMCs) is obtained
#' from `rstan` function `get_elapsed_time`.  The post-burnin time is
#' the total sampling time minus the burnin time.
NULL
