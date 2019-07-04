MCMCdefs_nimble_builtin <-
  list(nimble        = quote(configureMCMC(model)),
       nimble_noConj = quote(configureMCMC(model, useConjugacy = FALSE)),
       nimble_RW     = quote(configureMCMC(model, onlyRW       = TRUE)),
       nimble_slice  = quote(configureMCMC(model, onlySlice    = TRUE)),
       autoBlock     = quote(configureMCMC(model, autoBlock    = TRUE)))
