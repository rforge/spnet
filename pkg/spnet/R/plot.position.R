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
    plot(x, ...)
    text(coordinates(x), labels=paste(label, row.names(coordinates(x)), sep=''), ...)
  }
)

setMethod(
  f = 'plot.position',
  signature = 'SpatialNetwork',
  definition = function(x, label, ...) {
    getMethod('plot.position', 'SpatialNetwork')(x@sp, label, ...)
  }
)