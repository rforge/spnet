setGeneric(
  'plot.position',
  function(
    x,
    label = '',
    ...
  ) {
    standardGeneric("plot.position")
  }
)

setMethod(
  f = 'plot.position',
  signature = 'SpatialPolygons',
  definition = function(x, label, ...) {
    if(length(x) == 0)
      stop("The map is empty. Please define a valid map.")
    plot(x, ...)
    text(coordinates(x), labels=paste(label, row.names(coordinates(x)), sep=''), ...)
  }
)

setMethod(
  f = 'plot.position',
  signature = 'SpatialNetwork',
  definition = function(x, label, ...) {
    if(length(x@map) == 0)
      stop("The map is empty. Please define a valid map.")
    getMethod('plot.position', 'SpatialPolygons')(x@map, label, ...)
  }
)