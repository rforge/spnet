#' Spnet basic examples
#' 
#' Create SpatialNetwork object examples for demonstration and testing purpose.
#' 
#' @param map logical; if \code{TRUE} an example of map is provided.
#' @param color logical; if \code{TRUE} an example of map colorization is provided.
#' @param symbol logical; if \code{TRUE} an example of symbol use is provided.
#' @param network1 logical; if \code{TRUE} a first example of network is provided.
#' @param network2 logical; if \code{TRUE} a second example of network is provided.
#' @param barplot logical; if \code{TRUE} a example of barplot rendering of a numeric variable is provided.
#' @return a \code{SpatialNetwok} object.
#' @examples
#' data(world.map.simplified, package = "spnet")
#' net1 <- spnet.example.basic()
#' plot(net1)
#' @rdname spnet.example.basic
#' @export
spnet.example.basic <- function(
  map = TRUE,
  color = TRUE,
  symbol = TRUE,
  network1 = TRUE,
  network2 = TRUE,
  barplot = TRUE
) {
  example.basic.env <- new.env()
  data("world.map.simplified", package = "spnet", envir = example.basic.env)
  node <- c("France", "United States", "Brazil", "Australia")
  position <- match(node, example.basic.env$world.map.simplified@data[,'NAME']) - 1
  net1 <- spnet.create(
    data.frame(
      'NODE' =  node,
      'POSITION' = position
    )
  )
  spnet.title.main(net1) <- ""
  if(map) {
    spnet.map(net1) <- example.basic.env$world.map.simplified
  }
  if(color) {
    net1$continent <- c("Europa", "America", "America", "Oceania")
    spnet.color.variable(net1) <- "continent"
    spnet.color.legend(net1) <- c('Europa' = "#BEF7E3", 'America' = "#D7BEF7", 'Oceania' = "#F5E78E")
    spnet.color.background(net1) <- "#B3CAF5"
    spnet.color.border(net1) <- "#555555"
    spnet.color.region(net1) <- "#F5E1B3"
  }
  if(symbol) {
    net1$role <- c('North', 'North', 'South', 'South')
    spnet.symbol.variable(net1) <- 'role'
    spnet.symbol.legend(net1) <- c('North' = 'triangle.up', 'South' = 'triangle.down')
    spnet.symbol.color(net1) <- '#00EE00'
    spnet.symbol.cex(net1) <- 2
    spnet.symbol.translate.x(net1) <- 0
    spnet.symbol.translate.y(net1) <- 6
  }
  if(network1) {
    network1 <- matrix(
      rep(0, length(node)^2),
      nrow = length(node),
      dimnames = list(node, node)
    )
    network1['France', 'United States'] <- 1
    network1['Brazil', 'Australia'] <- 2
    
    spnet.networks.add(net1) <- "network1"
    spnet.network.data(net1, "network1") <- network1
  }
  if(network2) {
    network2 <- matrix(
      rep(0, length(node)^2),
      nrow = length(node),
      dimnames = list(node, node)
    )
    network2['United States', 'France'] <- 1
    network2['United States', 'Australia'] <- 1
    network2['Brazil', 'France'] <- 3
    
    spnet.networks.add(net1) <- "network2"
    spnet.network.data(net1, "network2") <- network2
  }
  if(barplot) {
    net1$num.var <- c(0.1,0.3,0.5,0.9)
    spnet.barplot.variable(net1) <- "num.var"
    spnet.barplot.bound.lower(net1) <- c(-5,-6)
    spnet.barplot.bound.upper(net1) <- c(5,-6)
    spnet.barplot.fgcolor(net1) <- "#3B6E5C"
    spnet.barplot.bgcolor(net1) <- "#77D9B7"
    spnet.barplot.width(net1) <- 8
  }
  return(net1)
}

#' @rdname spnet.example.basic
#' @export
spnet.example.basic.full <- function(){
  net1 <- spnet.example.basic()
  return(net1)
}
#' @rdname spnet.example.basic
#' @export
spnet.example.basic.map <- function(){
  net1 <- spnet.example.basic(
    map = TRUE,
    color = FALSE,
    symbol = FALSE,
    network1 = FALSE,
    network2 = FALSE,
    barplot = FALSE
  )
  return(net1)
}