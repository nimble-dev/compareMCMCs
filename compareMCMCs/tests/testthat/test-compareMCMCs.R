context("Testing compareMCMCs")

test_that("compareMCMCs works with one sampled parameter", {
  mc <- nimbleCode({a ~ dnorm(0,1)})
  res <- compareMCMCs::compareMCMCs(list(code = mc),
                                    MCMCs = c('jags', 'nimble'),
                                    MCMCcontrol = list(inits = list(a = 1),
                                                       niter = 2000,
                                                       burnin = 100))
  expect_true(is.list(res))
  expect_identical(names(res), c("jags", "nimble"))
  expect_true(inherits(res[['nimble']], "MCMCresult"))
  expect_true(inherits(res[['jags']], "MCMCresult"))
}
)

test_that("compareMCMCs works with two sampled parameters", {
  mc <- nimbleCode(
  {
    for(i in 1:4) { ## These curly brackets are required for JAGS
      y[i] ~ dnorm(0,1)
      z[i] ~ dnorm(0,1)
    }
  }
  )
  res <- compareMCMCs::compareMCMCs(list(code = mc,
                                         inits = list(y = rnorm(4))),
                                    MCMCs = c('jags', 'nimble'),
                                    MCMCcontrol = list(niter = 2000,
                                                       thin = 2,
                                                       burnin = 100),
                                    metrics = c("ESS_coda", "efficiency_coda"),
                                    metricOptions = list(efficiency_coda = list(time = "setup+sampling")))
  expect_true(is.list(res))
  expect_identical(names(res), c("jags", "nimble"))
  expect_true(inherits(res[['nimble']], "MCMCresult"))
  expect_true(inherits(res[['jags']], "MCMCresult"))
}
)

test_that("compareMCMCs works with a matrix of sampled parameters", {
  mc <- nimbleCode(
  {
    for(i in 1:2) { ## These brackets are required for JAGS
      for(j in 1:3) {
        y[i, j] ~ dnorm(0,1)
      }
    }
  }
  )
  res <- compareMCMCs::compareMCMCs(list(code = mc,
                                         inits = list(y = matrix(rnorm(6), nrow = 2))),
                                    MCMCs = c('jags', 'nimble'),
                                    MCMCcontrol = list(niter = 2000,
                                                       burnin = 100))
  expect_true(is.list(res))
  expect_identical(names(res), c("jags", "nimble"))
  expect_true(inherits(res[['nimble']], "MCMCresult"))
  expect_true(inherits(res[['jags']], "MCMCresult"))
})

test_that("compareMCMCs works with Stan", {
## This exampe is taken from the RStan vignette provided in package rstan
  sink("schools.stan")
  cat("
  data {
    int<lower=0> J;          // number of schools 
    real y[J];               // estimated treatment effects
    real<lower=0> sigma[J];  // s.e. of effect estimates 
  }
  parameters {
    real mu; 
    real<lower=0> tau;
    vector[J] eta;
  }
  transformed parameters {
    vector[J] theta;
    theta = mu + tau * eta;
  }
  model {
    target += normal_lpdf(eta | 0, 1);
    target += normal_lpdf(y | theta, sigma);
  }\n")
  sink()
  schools_data <- list(
    J = 8,
    y = c(28,  8, -3,  7, -1,  1, 18, 12),
    sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
  )
  # fit1 <- stan(
  #   file = "schools.stan",  # Stan program
  #   data = schools_data,    # named list of data
  #   chains = 4,             # number of Markov chains
  #   warmup = 1000,          # number of warmup iterations per chain
  #   iter = 2000,            # total number of iterations per chain
  #   cores = 1,              # number of cores (could use one per chain)
  #   refresh = 0             # no progress shown
  # )
  res <- compareMCMCs::compareMCMCs(needRmodel = FALSE,
                                    MCMCs = c('stan'),
                                    externalMCMCinfo = list(stan = list(
                                      file = "schools.stan",
                                      sampling_args = list(data = schools_data,
                                                           warmup = 1000, iter = 2000))))
  # data, inits, warmup (burnin) and/or niter
  # can be provided in the externalMCMCinfo entry or via MCMCcontrol.
  # For init and data, 
  })

