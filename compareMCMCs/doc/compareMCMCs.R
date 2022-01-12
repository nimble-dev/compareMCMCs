## ----setup, include=FALSE-----------------------------------------------------
## Note: special steps are needed to build this vignette.
## This is because it generates html output that it links to.
## 1. devtools::build_vignettes(clean = FALSE)
## 2. Move or copy the ten files with "example1" or "example2" in their name
##    from /vignettes to /doc.  This is the directory where build_vignettes
##    will have copied compareMCMCs.[R, Rmd, html].
## 3. Remove outputs from /vignettes if desired.
##
## If rstan generates C++ output with an error compiling foo.c,
## try re-installing rstan. Clues suggest that resulting updates to StanHeaders
## seems to fix the problem.
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(compareMCMCs)
this_system_has_rjags <- requireNamespace("rjags")
if(!this_system_has_rjags) message("Portions of this vignette use package rjags.  That is not installed, so those portions will be skipped.")
this_system_has_rstan <- requireNamespace("rstan")
if(!this_system_has_rstan) message("Portions of this vignette use package rstan.  That is not installed, so those portions will be skipped.")

## ---- message=FALSE-----------------------------------------------------------
# This model code will be used for both nimble and JAGS
modelCode <- nimbleCode({
  a ~ dunif(0, 100)
  y ~ dgamma(a, 2)
})
modelInfo <- list(
  code = modelCode,
  constants = list(y = 2), # data can be included in constants or data list
  inits = list(a = 1)
)
# Here is a custom MCMC configuration function for nimble
configure_nimble_slice <- function(model) {
  configureMCMC(model, onlySlice = TRUE)
}
# Here is information to set the model up for Stan
stan_code <- c("data {real y;}",
               "parameters {real a;}",
               "model {target += uniform_lpdf(a | 0, 100);",
               "       target += gamma_lpdf(y | a, 2);}")
writeLines(stan_code, "toy.stan")
# An alternative way to provide the Stan model is to make a
# list entered below as stan_model_args (similar to how sampling_args
# appears in the externalMCMCinfo list).  The elements of stan_model_args
# (e.g. 'file' or 'model_code') will be passed to the 'stan_model` function.
stan_sampling_args <- list(data = list(y=2),
                           init = list(list(a=1)), # Only one chain will be run
                           warmup = 200, # Stan was not happy with warmup = 100
                           iter = 2000)
# Here is the call to compareMCMCs
res <- compareMCMCs(modelInfo,
                    MCMCs = c(if(this_system_has_rjags) 'jags' else NULL,
                              'nimble',       # nimble with default samplers
                              'nimble_slice', # nimble with slice samplers
                              if(this_system_has_rstan) 'stan' else NULL),
                    nimbleMCMCdefs = 
                      list(nimble_slice = 'configure_nimble_slice'),
                    MCMCcontrol = list(inits = list(a = 1),
                                       niter = 2000,
                                       burnin = 100),
                    externalMCMCinfo = list(stan = list(
                      file = "toy.stan",
                      sampling_args = stan_sampling_args))
                    # Stan was not happy with warmup = 100, which would match the others.
)
file.remove("toy.stan")
make_MCMC_comparison_pages(res, modelName = 'example1')

## ---- eval=!this_system_has_rjags, echo=FALSE---------------------------------
#  message("Skipping this code because rjags is not installed.")

## ---- eval=this_system_has_rjags----------------------------------------------
res_jags <- compareMCMCs(modelInfo,
                MCMCs = c('jags'),
                MCMCcontrol = list(inits = list(a = 1),
                                   niter = 2000,
                                   burnin = 100))
# To illustrate that each MCMC can have its own settings, we run nimble and jags
# different numbers of iterations.
res_nimble <- compareMCMCs(modelInfo,
                MCMCs = c('nimble', 'nimble_slice'),
                nimbleMCMCdefs = list(nimble_slice = 'configure_nimble_slice'),
                MCMCcontrol = list(inits = list(a = 1),
                                   niter = 4000,
                                   burnin = 200))
res <- c(res_jags, res_nimble)
make_MCMC_comparison_pages(res, modelName = 'example2')

## -----------------------------------------------------------------------------
res$nimble$metrics

## ---- eval = FALSE------------------------------------------------------------
#  MCMCmetric_median <- function(result, ...) {
#    res <- apply(result$samples, 2, median)
#    list(byParameter = list(median = res))
#  }

## -----------------------------------------------------------------------------
MCMCmetric_quartiles <- function(result, options) {
  p25 <- apply(result$samples, 2, quantile, probs = 0.25)
  p75 <- apply(result$samples, 2, quantile, probs = 0.75)
  # q25 and q75 are named vectors with names matching model parameters
  # i.e. column names of result$samples
  maxDiff <- max(p75-p25)
  list(byParameter = list(p25 = p25,
                          p75 = p75),
       byMCMC = list(maxQuartileDiff = maxDiff))
}

## -----------------------------------------------------------------------------
addMetrics(res, list(MCMCmetric_quartiles))
res$nimble$metrics

## -----------------------------------------------------------------------------
registerMetrics(
  list(quartiles = MCMCmetric_quartiles)
)

## -----------------------------------------------------------------------------
reparam <- list(log_a  = "log(`a`)", a = NULL)
conversions <- list(nimble = reparam,
                    nimble_slice = reparam,
                    jags = reparam)
res <- compareMCMCs(modelInfo,
                    MCMCs = c(if(this_system_has_rjags) 'jags' else NULL,
                              'nimble', 'nimble_slice'),
                    nimbleMCMCdefs = list(nimble_slice = 'configure_nimble_slice'),
                    conversions = conversions,
                    MCMCcontrol = list(inits = list(a = 1),
                                       niter = 2000,
                                       burnin = 100))

# We will look at the result using combineMetrics
# rather than generating new html pages.
combineMetrics(res)

## -----------------------------------------------------------------------------
reparam <- list(a  = "exp(`log_a`)", log_a = NULL)
conversions <- list(nimble = reparam,
                    nimble_slice = reparam,
                    jags = reparam)
applyConversions(res, conversions)
clearMetrics(res)
addMetrics(res) # use default metrics
combineMetrics(res) # An easy way to see that it worked

## -----------------------------------------------------------------------------
combineMetrics(res, include_times = TRUE)

## -----------------------------------------------------------------------------
registerPageComponents(
  list(myNewComponent = 
         list(make = "myMakeFunction",
              fileSuffix = "_myPageComponent",
              linkText = "My new page component.")
       )
  )

