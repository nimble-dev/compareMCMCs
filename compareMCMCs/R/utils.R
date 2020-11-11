updateDefaults <- function(defaults, controls) {
  defaults[names(controls)] <- controls
  defaults
}
