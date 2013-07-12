spnet.globalenv <- new.env()

spnet.globalenv$package.infos <- utils:::packageDescription("spnet")

spnet.version <- function() {
  return(spnet.globalenv$package.infos$Version)
}