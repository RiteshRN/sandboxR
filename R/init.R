.onLoad <- function(libname, pkgname) {
  # Forbidden options
  base::options('sandboxR.disabled.options' = 'sandboxR.disabled.options')
}
