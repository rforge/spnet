setGeneric(
  'plot.placement',
  function(
    x,
    label = '',
    ...
  ) {
    standardGeneric("plot.placement")
  }
)

setMethod(
  f = 'plot.placement',
  signature = 'SpatialPolygons',
  definition = function(x, label, ...) {
    plot(x, ...)
    text(coordinates(x), labels=paste(label, row.names(coordinates(x)), sep=''), ...)
  }
)

setMethod(
  f = 'plot.placement',
  signature = 'SpatialNetwork',
  definition = function(x, label, ...) {
    getMethod('plot.placement', 'SpatialNetwork')(x@sp, label, ...)
  }
)