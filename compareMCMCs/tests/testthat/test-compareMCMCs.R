context("Testing compareMCMCs")

test_that("compareMCMCs works", {
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

test_that("compareMCMCs works", {
  mc <- nimbleCode(
  {
    for(i in 1:4) { ## These brackets are required for JAGS
      y[i] ~ dnorm(0,1)
    }
  }
  )
  res <- compareMCMCs::compareMCMCs(list(code = mc,
                                         inits = list(y = rnorm(4))),
                                    MCMCs = c('jags', 'nimble'),
                                    MCMCcontrol = list(niter = 2000,
                                                       burnin = 100))
  expect_true(is.list(res))
  expect_identical(names(res), c("jags", "nimble"))
  expect_true(inherits(res[['nimble']], "MCMCresult"))
  expect_true(inherits(res[['jags']], "MCMCresult"))
}
)

test_that("compareMCMCs works", {
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
}
)
