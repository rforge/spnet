#.onLoad <- function(libname, pkgname) {
#    if (!require(methods)) {
#        stop("Require methods package")
#    }
#}



.onAttach <- function(libname, pkgname) {
  packageStartupMessage('\n', 'Welcome to debate', '.')
  packageStartupMessage('You are running version ', utils::packageVersion("debate"), '.\n')
  packageStartupMessage(
    'For introductory material, type ',
    #"'vignette(package=\"spnet\")'.\n"
    "`vignette('debate-overview')`.\n"
  )
  packageStartupMessage(
    'If you use this package in your work, thank you for rewarding our work by citing the package in your own one. ',
    "Please type `citation(\'debate\')` for citation information.\n"
  )
  packageStartupMessage(
    "Type `?debate` ",
    "to display the help index.\n"
  )
  
}

#' @export
.Last.lib <- function(libpath) {
  message("Thank you for using the 'debate' R-package", '.')
  message('See you soon', '!')
}

.onUnload <- function(libpath) {
  #library.dynam.unload("spnet", libpath )
}
