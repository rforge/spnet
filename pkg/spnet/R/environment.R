.spnet.globalenv <- new.env()

.spnet.globalenv$package.infos <- utils:::packageDescription("spnet")

#' Get your current \code{spnet} package version
#' 
#' The \code{spnet.version} function return the version of the \code{spnet} package you are currently running.
#' 
#' @export
#' @examples 
#' spnet.version()
spnet.version <- function() {
  return(spnet:::.spnet.globalenv$package.infos$Version)
}