MCMCdefs_nimble <-
  list(nimble        = quote(configureMCMC(Rmodel)),
       nimble_noConj = quote(configureMCMC(Rmodel, useConjugacy = FALSE)),
       nimble_RW     = quote(configureMCMC(Rmodel, onlyRW       = TRUE)),
       nimble_slice  = quote(configureMCMC(Rmodel, onlySlice    = TRUE)),
       autoBlock     = quote(configureMCMC(Rmodel, autoBlock    = TRUE)))
