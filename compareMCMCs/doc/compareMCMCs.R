## ----setup, include=FALSE-----------------------------------------------------
## Note: special steps are needed to build this vignette.
## This is because it generates html output that it links to.
## 1. devtools::build_vignettes(clean = FALSE)
## 2. Move or copy the five files with "example1" in their name
##    from /vignettes to /doc.  This is the directory where build_vignettes
##    will have copied compareMCMCs.[R, Rmd, html].
## 3. Remove outputs from /vignettes if desired.
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(compareMCMCs)
modelCode <- nimbleCode({
  a ~ dunif(0, 100)
  y ~ dgamma(a, 2)
})
modelInfo <- list(
  code = modelCode,
  constants = list(y = 2),
  inits = list(a = 1)
)
configure_nimble_slice <- function(model) {
  configureMCMC(model, onlySlice = TRUE)
}
res <- compareMCMCs(modelInfo,
                    MCMCs = c('jags', 'nimble', 'nimble_slice'),
                    nimbleMCMCdefs = 
                      list(nimble_slice = 'configure_nimble_slice'),
                    MCMCcontrol = list(inits = list(a = 1),
                                       niter = 2000,
                                       burnin = 100))
make_MCMC_comparison_pages(res, modelName = 'example1')

## -----------------------------------------------------------------------------
res_jags <- compareMCMCs(modelInfo,
                MCMCs = c('jags'),
                MCMCcontrol = list(inits = list(a = 1),
                                   niter = 2000,
                                   burnin = 100))
## Perhaps we want to run nimble MCMCs for twice as many iterations
res_nimble <- compareMCMCs(modelInfo,
                MCMCs = c('nimble', 'nimble_slice'),
                nimbleMCMCdefs = list(nimble_slice = 'configure_nimble_slice'),
                MCMCcontrol = list(inits = list(a = 1),
                                   niter = 4000, ## potentially different than above
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
  ## q25 and q75 are named vectors with names matching model parameters
  ## i.e. column names of result$samples
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
                    MCMCs = c('jags', 'nimble', 'nimble_slice'),
                    nimbleMCMCdefs = list(nimble_slice = 'configure_nimble_slice'),
                    conversions = conversions,
                    MCMCcontrol = list(inits = list(a = 1),
                                       niter = 2000,
                                       burnin = 100))

## We will look at the result using combineMetrics (see below)
## rather than generating new html pages.
combineMetrics(res)

## -----------------------------------------------------------------------------
reparam <- list(a  = "exp(`log_a`)", log_a = NULL)
conversions <- list(nimble = reparam,
                    nimble_slice = reparam,
                    jags = reparam)
applyConversions(res, conversions)
clearMetrics(res)
addMetrics(res) # use default metrics
combineMetrics(res) ## An easy way to see that it worked

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

