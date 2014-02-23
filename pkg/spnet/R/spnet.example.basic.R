#' Spnet basic examples
#' 
#' Create SpatialNetwork object examples for demonstration and testing purpose.
#' 
#' @param map if \code{TRUE} an example of map is provided.
#' @param color if \code{TRUE} an example of map colorization is provided.
#' @param symbol if \code{TRUE} an example of symbol use is provided.
#' @param network1 if \code{TRUE} a first example of network is provided.
#' @param network2 if \code{TRUE} a second example of network is provided.
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
  network2 = TRUE
) {
  node <- c("John", "Elsa", "Brian", "Kate")
  position <- c(2,4,6,8)
  net1 <- spnet.create(
    data.frame(
      'NODE' =  node,
      'POSITION' = position
    )
  )
  if(map) {
    spnet.map(net1) <- room.create.u()
  }
  if(color) {
    net1$parti <- c('vert', 'socialiste', 'autre', 'vert')
    spnet.color.variable(net1) <- "parti"
    spnet.color.legend(net1) <- c('vert' = "#32AB58", 'socialiste' = "#E31923")
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
  net1 <- spnet.example.basic(color = FALSE, symbol = FALSE, network1 = FALSE, network2 = FALSE)
  return(net1)
}