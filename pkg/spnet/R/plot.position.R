#' Plot a map labelled with the ID numbering
#'
#' The \code{spnet.map.plot.position} function allows to plot maps defined as for example \code{SpatialNetwork} or \code{SpatialPolygons} objects, and render the ID numbering.
#'
#' @param x an object for which a \code{spnet.map.plot.position} method is defined.
#' @param label a character of length 1 for prefixing seat numbering.
#' @param ... other arguments to pass to the plot function. The main usage is setting the \code{cex} value.
#'
#' @return NULL
#' 
#' @family plot map
#' @export
#' @examples
#' # A room with tables in inversed 'U' form
#' col <- 5
#' row <- 6
#' m <- matrix(rep(-1, col*row), nrow = row)
#' m[1,2:4] <- 0
#' m[3,c(1,5)] <- 0
#' m[4,c(1,5)] <- 0
#' m[5,c(1,5)] <- 0
#' m[6,c(1,5)] <- 0
#' m
#' room1 <- room.create.grid(m, seat.width=2, seat.height=1)
#' spnet.map.plot.position(room1)
#' spnet.map.plot.position(room1, label = 'Seat ')
#' spnet.map.plot.position(room1, label = 'Seat ', cex = 0.8)
setGeneric(
  'spnet.map.plot.position',
  function(
    x,
    label = '',
    ...
  ) {
    standardGeneric("spnet.map.plot.position")
  }
)

#' @describeIn spnet.map.plot.position method for \code{SpatialPolygons} objects.
setMethod(
  f = 'spnet.map.plot.position',
  signature = 'SpatialPolygons',
  definition = function(x, label, ...) {
    if(length(x) == 0)
      stop("The map is empty. Please define a valid map.")
    plot(x, ...)
    text(coordinates(x), labels=paste(label, row.names(coordinates(x)), sep=''), ...)
  }
)

#' @describeIn spnet.map.plot.position method for \code{SpatialNetwork} objects.
setMethod(
  f = 'spnet.map.plot.position',
  signature = 'SpatialNetwork',
  definition = function(x, label, ...) {
    if(length(x@map) == 0)
      stop("The map is empty. Please define a valid map.")
    getMethod('spnet.map.plot.position', 'SpatialPolygons')(x@map, label, ...)
  }
)