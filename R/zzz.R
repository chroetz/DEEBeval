.onLoad <- function(libname, pkgname) {
  ConfigOpts::addPackageToPathDefaults(
    system.file("defaultOpts", package="DEEBdata", lib.loc=libname))
  ConfigOpts::addPackageToPathDefaults(
    system.file("defaultOpts", package=pkgname, lib.loc=libname))
  invisible(NULL)
}
