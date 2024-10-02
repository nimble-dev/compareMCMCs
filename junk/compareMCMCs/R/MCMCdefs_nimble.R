#' @importFrom nimble configureMCMC
MCMCdefs_nimble_builtin <-
  list(
    nimble        = quote(nimble::configureMCMC(model)),
    nimble_noConj = quote(nimble::configureMCMC(model, useConjugacy = FALSE)),
    nimble_RW     = quote(nimble::configureMCMC(model, onlyRW = TRUE)),
    nimble_slice  = quote(nimble::configureMCMC(model, onlySlice = TRUE)),
    autoBlock     = quote(nimble::configureMCMC(model, autoBlock = TRUE)))
