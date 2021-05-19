# environment variable for storing Python modules
.pkg.env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  assign("numpy", reticulate::import("numpy", delay_load = TRUE), .pkg.env)
  assign("sklearn", reticulate::import("sklearn", delay_load = TRUE), .pkg.env)
  assign("scipy", reticulate::import("scipy", delay_load = TRUE), .pkg.env)
  assign("python_speech_features", reticulate::import("python_speech_features", delay_load = TRUE), .pkg.env)
  assign("speechpy", reticulate::import("speechpy", delay_load = TRUE), .pkg.env)
}
