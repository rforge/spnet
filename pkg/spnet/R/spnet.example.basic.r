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
  node <- c("John", "Elsa", "Brian", "Kate")
  position <- c(2,4,6,8)
  net1 <- spnet.create(
    data.frame(
      'NODE' =  node,
      'POSITION' = position
    )
  )
  spnet.title.main(net1) <- ""
  if(map) {
    spnet.map(net1) <- room.create.u()
  }
  if(color) {
    net1$parti <- c('vert', 'socialiste', 'autre', 'vert')
    spnet.color.variable(net1) <- "parti"
    spnet.color.legend(net1) <- c('vert' = "#32AB58", 'socialiste' = "#E31923")
  }
  if(symbol) {
    net1$role <- c('President', 'Party leader', 'Project leader', 'Partisan')
    spnet.symbol.variable(net1) <- 'role'
    spnet.symbol.legend(net1) <- c('President' = 'square.rotated', 'Party leader' = 'triangle.up', 'Project leader' = 'circle')
    spnet.symbol.color(net1) <- '#0000dd'
    spnet.symbol.cex(net1) <- 1
    spnet.symbol.translate.x(net1) <- 0.36
    spnet.symbol.translate.y(net1) <- 0.36
  }
  if(network1) {
    network1 <- matrix(
      rep(0, length(node)^2),
      nrow = length(node),
      dimnames = list(node, node)
    )
    network1['John', 'Elsa'] <- 1
    network1['Kate', 'Brian'] <- 2
    
    spnet.networks.add(net1) <- "approb"
    spnet.network.data(net1, "approb") <- network1
  }
  if(network2) {
    network2 <- matrix(
      rep(0, length(node)^2),
      nrow = length(node),
      dimnames = list(node, node)
    )
    network2['John', 'Elsa'] <- 1
    network2['John', 'Brian'] <- 1
    network2['Brian', 'Elsa'] <- 3
    
    spnet.networks.add(net1) <- "desapprob"
    spnet.network.data(net1, "desapprob") <- network2
  }
  if(barplot) {
    net1$followers <- c(0.1,0.3,0.5,0.9)
    spnet.barplot.variable(net1) <- "followers"
    spnet.barplot.bound.lower(net1) <- c(-0.5,-0.44)
    spnet.barplot.bound.upper(net1) <- c(0.5,-0.44)
#     spnet.barplot.fgcolor(net1) <- "#00dd00"
#     spnet.barplot.bgcolor(net1) <- "#0000dd"
    spnet.barplot.width(net1) <- 6
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
    color = FALSE,
    symbol = FALSE,
    network1 = FALSE,
    network2 = FALSE,
    barplot = FALSE
  )
  return(net1)
}