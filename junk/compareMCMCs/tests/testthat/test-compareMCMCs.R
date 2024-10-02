
# Many of these tests are commented out because they are slow or require
# external packages or configurations.

## test_that("compareMCMCs works with one sampled parameter", {
##   mc <- nimble::nimbleCode({a ~ dnorm(0,1)})
##   res <- compareMCMCs::compareMCMCs(list(code = mc),
##                                     MCMCs = c('jags', 'nimble'),
##                                     MCMCcontrol = list(inits = list(a = 1),
##                                                        niter = 2000,
##                                                        burnin = 100))
##   expect_true(is.list(res))
##   expect_identical(names(res), c("jags", "nimble"))
##   expect_true(inherits(res[['nimble']], "MCMCresult"))
##   expect_true(inherits(res[['jags']], "MCMCresult"))
## }
## )

test_that("compareMCMCs works if the model is provided as an argument instead of built internally", {
  mc <- nimble::nimbleCode({a ~ dnorm(0,1)})
  model <- nimbleModel(mc)
  res <- compareMCMCs::compareMCMCs(list(model = model),
                                    MCMCs = c('nimble'),
                                    MCMCcontrol = list(inits = list(a = 1),
                                                       niter = 2000,
                                                       burnin = 100))
  expect_true(is.list(res))
  expect_identical(names(res), c("nimble"))
  expect_true(inherits(res[['nimble']], "MCMCresult"))
  }
)

## test_that("compareMCMCs works with two sampled parameters and conversions", {
##   mc <- nimble::nimbleCode(
##   {
##     for(i in 1:4) { ## These curly brackets are required for JAGS
##       y[i] ~ dnorm(0,1)
##       z[i] ~ dnorm(0,1)
##     }
##   }
##   )
##   reparam <- list(y1_plus1  = "`y[1]` + 1")
##   res <- compareMCMCs::compareMCMCs(
##     list(code = mc,
##          inits = list(y = rnorm(4))),
##     MCMCs = c('jags', 'nimble'),
##     MCMCcontrol = list(niter = 2000,
##                        thin = 2,
##                        burnin = 100),
##     metrics = c("ESS_coda", "efficiency_coda"),
##     metricOptions = list(efficiency_coda = list(time = "setup+sampling")),
##     conversions = list(nimble = reparam)
##   )
##   expect_true(is.list(res))
##   expect_identical(names(res), c("jags", "nimble"))
##   expect_true(inherits(res[['nimble']], "MCMCresult"))
##   expect_true(inherits(res[['jags']], "MCMCresult"))
##   expect_true("y1_plus1" %in% colnames(res[['nimble']]$samples))
##   expect_false("y1_plus1" %in% colnames(res[['jags']]$samples))
##   expect_true(all(res[['nimble']]$samples[,"y1_plus1"] == res[['nimble']]$samples[,"y[1]"] + 1))
## }
## )

## test_that("compareMCMCs works with a matrix of sampled parameters and monitor names", {
##   mc <- nimble::nimbleCode(
##   {
##     for(i in 1:2) { ## These brackets are required for JAGS
##       for(j in 1:3) {
##         y[i, j] ~ dnorm(0,1)
##       }
##     }
##   }
##   )
##   res <- compareMCMCs::compareMCMCs(
##     list(code = mc,
##          inits = list(y = matrix(rnorm(6), nrow = 2))),
##     MCMCs = c('jags', 'nimble'),
##     monitors = "y",
##     MCMCcontrol = list(niter = 2000,
##                        burnin = 100))
##   expect_true(is.list(res))
##   expect_identical(names(res), c("jags", "nimble"))
##   expect_true(inherits(res[['nimble']], "MCMCresult"))
##   expect_true(inherits(res[['jags']], "MCMCresult"))
##   expect_identical(colnames(res[['nimble']]$samples), colnames(res[['jags']]$samples))
##   expect_identical(colnames(res[['nimble']]$samples), c("y[1, 1]", "y[2, 1]", "y[1, 2]", "y[2, 2]", "y[1, 3]", "y[2, 3]"))
## })

## test_that("compareMCMCs works with Stan", {
##   ## This example is taken from the RStan vignette provided in package rstan
##   message("EXPECT ONE VALID ERROR-TRAPPED WARNING MESSAGE IN THIS TEST.")
##   sink(file.path(tempdir(), "schools.stan"))
##   cat("
##   data {
##     int<lower=0> J;          // number of schools
##     real y[J];               // estimated treatment effects
##     real<lower=0> sigma[J];  // s.e. of effect estimates
##   }
##   parameters {
##     real mu;
##     real<lower=0> tau;
##     vector[J] eta;
##   }
##   transformed parameters {
##     vector[J] theta;
##     theta = mu + tau * eta;
##   }
##   model {
##     target += normal_lpdf(eta | 0, 1);
##     target += normal_lpdf(y | theta, sigma);
##   }\n")
##   sink()
##   schools_data <- list(
##     J = 8,
##     y = c(28,  8, -3,  7, -1,  1, 18, 12),
##     sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
##   )
##   # These commented-out lines show how the example would be called directly via rstan.
##   # These should not be uncommented as part of testing.  They are just for reference.
##   # fit1 <- stan(
##   #   file = "schools.stan",  # Stan program
##   #   data = schools_data,    # named list of data
##   #   chains = 4,             # number of Markov chains
##   #   warmup = 1000,          # number of warmup iterations per chain
##   #   iter = 2000,            # total number of iterations per chain
##   #   cores = 1,              # number of cores (could use one per chain)
##   #   refresh = 0             # no progress shown
##   # )
##   # test error of not providing file
##   expect_true(
##     identical(
##       list(), # compareMCMCs should error trap and return empty list
##       compareMCMCs::compareMCMCs(
##         needRmodel = FALSE,
##         MCMCs = c('stan'),
##         externalMCMCinfo = list(stan = list(
##           sampling_args = list(data = schools_data,
##                                warmup = 1000, iter = 2000))))))
##   # test valid run
##   res <- compareMCMCs::compareMCMCs(
##     needRmodel = FALSE,
##     MCMCs = c('stan'),
##     externalMCMCinfo = list(stan = list(
##       file = file.path(tempdir(), "schools.stan"),
##       sampling_args = list(data = schools_data,
##                            warmup = 1000, iter = 2000))))
##   # data, inits, warmup (burnin) and/or niter
##   # can be provided in the externalMCMCinfo entry or via MCMCcontrol.
##   # For init and data,
##   file.remove(file.path(tempdir(), "schools.stan"))
##   expect_true(inherits(res[['stan']], 'MCMCresult'))
##   expect_true(nrow(res[['stan']]$samples) == 1000)
## })

test_that("compareMCMCs works with custom nimbleMCMCdefs", {
# Borrowed from vignette
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
                      MCMCs = c('nimble_slice_1', 'nimble_slice_2', 'nimble_slice_3'),
                      nimbleMCMCdefs = 
                        list(nimble_slice_1 = 'configure_nimble_slice', # function name
                             nimble_slice_2 = configure_nimble_slice,   # function object
                             nimble_slice_3 = quote({configureMCMC(model, onlySlice = TRUE)})), # quoted code
                      MCMCcontrol = list(inits = list(a = 1),
                                         niter = 2000,
                                         burnin = 100),
                      needRmodel = TRUE)
  expect_true(length(res) == 3)
  expect_true(all(unlist(lapply(res, function(x) nrow(x$samples) == 1900))))
})

test_that("compareMCMCs works with precompiled nimble model and MCMC", {
  modelCode <- nimbleCode({
    a ~ dunif(0, 100)
    y ~ dgamma(a, 2)
  })
  m <- nimbleModel(modelCode,constants = list(y = 2), inits = list(a = 1) )
  cm <- compileNimble(m)
  mcmc <- buildMCMC(m)
  mcmc2 <- buildMCMC(m, onlySlice=TRUE)
  cmcmcs <- compileNimble(mcmc, mcmc2, project = m)
  res <- compareMCMCs(modelInfo = list(model = cm),
                      nimbleMCMCdefs = cmcmcs)
  expect_true(length(res) == 2)
  expect_true(identical(names(res), c("mcmc", "mcmc2")))
  expect_true(all(unlist(lapply(res, function(x) nrow(x$samples) == 8000))))
})
