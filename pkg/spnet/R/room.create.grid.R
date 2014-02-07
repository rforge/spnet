#' Create a room designed by a grid
#' 
#' The \code{room.create.grid} function allows to design a room by help of a 2D matrix specifying seats.
#' 
#' The matrix object is read cells line-by-line from the bottom-left to the top-up. For each cell, if the value is positive, a seat will be created in the corresponding location on the map. Else, the function jump to the next cell. If all positive value are '0', seats will be automatically numbered. The order used is from the bottom-left to the top-up. \cr\cr
#' If you want to specify each seat number you just have to give them instead of giving 0. You can use \code{numeric} values or \code{character} values (see \code{examples}). Please warn that your seat numbering have to be unique: you can't give the same seat number to two different seats. \cr\cr
#' If in your room you have some of rows of columns with an odd number of seats, you can adjust their location for a better fitting by specifying \code{'E'} or \code{'O'} in the \code{dimnames} attribute of the \code{matrix} (see \code{examples}). \cr\cr
#' @return A \code{\link{SpatialPolygons}} object.
#'
#' @family map plot
#' @author Emmanuel Rousseaux
#' 
#' @param  x a \code{matrix} object. Use \code{'0'} for specifying a seat and \code{'-1'} for a blank area. See \code{Details} for more flexible options, especially for specifying seats number.
#' @param  seat.width a \code{numeric} of length 1. Define the width of each seat.
#' @param  seat.height a \code{numeric} of length 1. Define the height of each seat.
#' 
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
#' plot.position(room1)
#' 
#' 
#' # A room with tables in inversed 'U', asymetric, with a customized seat numbering
#' col <- 5
#' row <- 6
#' m <- matrix(rep(-1, col*row), nrow = row)
#' m[1,2:4] <- c('C01','C02','C03')
#' m[3,c(1,5)] <- c('L01', 'R01')
#' m[4,c(1,5)] <- c('L02', 'R02')
#' m[5,1] <- c('L03')
#' m[6,1] <-c('L04')
#' m
#' room2 <- room.create.grid(m, seat.width=2, seat.height=1)
#' plot.position(room2)
#' 
#' 
#' # A room with tables in square, and with odd and even number of seats
#' col <- 6
#' row <- 11
#' m <- matrix(rep(-1, col*row), nrow = row)
#' m[1,2:4] <- 0
#' m[4:9,c(1,6)] <- 0
#' m[11,2:5] <- 0
#' dimnames(m) <- list(
#'   c('O', rep('E',row-1)),
#'   rep('E',col)
#' )
#' room3 <- room.create.grid(m, seat.width=2, seat.height=1)
#' plot.position(room3)
room.create.grid <- function(
  x,
  seat.width = 1,
  seat.height = 1
) {
  stopifnot(inherits(x, 'matrix'))
  stopifnot(inherits(seat.width, 'numeric'))
  stopifnot(inherits(seat.height, 'numeric'))
#   u <- unique(as.character(x))
  u <- unique(x)
  u.positive <- u[ u>=0 ]
  
  flag0 <- FALSE # TRUE iif all positive values are 0 or '0'
  if(all(u.positive == 0)) {
    flag0 <- TRUE
  }
  
  col.n <- ncol(x)
  row.n <- nrow(x)
  seat.n <- sum(x != -1)
  
  # dimnames management
  x.row <- rep('E', row.n)
  x.col <- rep('E', col.n)
  dn <- dimnames(x)
  if(!is.null(dn)) {
    if(!is.null(dn[[1]]))
      x.row <- dn[[1]]
    if(!is.null(dn[[2]]))
      x.col <- dn[[2]]
  }
  stopifnot(all(unique(c(x.row, x.col) %in% c('E', 'O'))))
  
  count <- 0
  ID <- count
  
  rooms.poly <- vector(seat.n, mode = 'list')
  for (i in row.n:1) { # warning: we start to plot from the bottom
    for(j in 1:col.n) {
      if(x[i,j] != -1) { # '-1' == -1 => TRUE
        #         print(paste('i:',i, 'j', j, 'value:',x[i,j]))
        k <- row.n - i + 1
        count <- count + 1
        
        if(flag0) ID <- count
        else ID <- x[i,j]
        
        row.odd <- 0
        if(x.row[i] == 'O') row.odd <- seat.width/2
        
        col.odd <- 0
        if(x.col[j] == 'O') col.odd <- seat.height/2
        
        #         print(row.odd)
        #         print(col.odd)
        rooms.poly[[count]] <- Polygons(
          list(Polygon(cbind(
            c(j-1,j-1,j,j,j-1)*seat.width + rep(row.odd,5),
            c(k-1,k,k,k-1,k-1)*seat.height + rep(col.odd,5)))
          ),
          ID
        )
      }
    }
  }
  
  rooms <- SpatialPolygons(
    rooms.poly,
    1:length(rooms.poly)
  )
  
  return(rooms)
}


# col <- 5
# row <- 6
# m <- matrix(rep(-1, col*row), nrow = row)
# m[1,2:4] <- 0
# m[3,c(1,5)] <- 0
# m[4,c(1,5)] <- 0
# m[5,1] <- 0
# m[6,1] <- 0
# room1 <- room.create.grid(m, seat.width=2, seat.height=1)
# plot.position(room1)

# 
# 
# col <- 5
# row <- 6
# m <- matrix(rep(-1, col*row), nrow = row)
# m[1,2:4] <- c('C01','C02','C03')
# m[3,c(1,5)] <- c('L01', 'R01')
# m[4,c(1,5)] <- c('L02', 'R02')
# m[5,1] <- c('L03')
# m[6,1] <-c('L04')
# room1 <- room.create.grid(m, seat.width=2, seat.height=1)
# plot(room1)
# text(coordinates(room1), labels=paste("", row.names(coordinates(room1)), sep=' '), cex=0.8)
# 
# 
# 
# col <- 5
# row <- 6
# m <- matrix(rep(-1, col*row), nrow = row)
# m[1,2:4] <- 0
# m[3,c(1,5)] <- 0
# m[4,c(1,5)] <- 0
# m[5,1] <- 0
# m[6,1] <- 0
# dimnames(m) <- list(
#   c('E','E','O','E','E','E'),
#   c('E','E','O','E','E')
# )
# room1 <- room.create.grid(m, seat.width=2, seat.height=1)
# plot(room1)
# text(coordinates(room1), labels=paste("Seat", row.names(coordinates(room1)), sep=' '), cex=0.8)
# 
# 
# col <- 5
# row <- 6
# m <- matrix(rep(-1, col*row), nrow = row)
# m[1,2:4] <- c('C01','C02','C03')
# m[3,c(1,5)] <- c('L01', 'R01')
# m[4,c(1,5)] <- c('L02', 'R02')
# m[5,1] <- c('L03')
# m[6,1] <-c('L04')
# dimnames(m) <- list(
#   c('E','E','O','E','E','E'),
#   c('E','E','O','E','E')
# )
# room1 <- room.create.grid(m, seat.width=2, seat.height=1)
# plot(room1)
# text(coordinates(room1), labels=paste("", row.names(coordinates(room1)), sep=' '), cex=0.8)







#' Create a room with a U shape
#' 
#' The \code{room.create.u} function allows to design a room with a U shape and 4 possible orientations: top, bottom, left and right. Gaps between tables can be defined.
#' 
#' If you want to assign seat numbering you can use the \code{out} argument to export the room as a \code{matrix} object, then assign the numbering in the matrix, and then use the \code{\link{room.create.grid}} function to create the final room. \cr\cr
#' @return Either a \code{\link{SpatialPolygons}} object or a \code{matrix} (according to the value given to the \code{out} parameter).
#' 
#' @family map plot
#' @author Emmanuel Rousseaux
#' @param  x a \code{numeric} object of length 3. The first value defines the number of seats of the table corresponding to the vertical bar on the left of the U. The second value defines the number of seats for the table of the middel, and the third value defines the number of seats of the table corresponding to the vertical bar on the right of the U.
#' @param  gap a \code{numeric} object of length 3. This parameter allows to define gap between tables. The first value defines the number of empty column(s) to create between the left table and the middle table. The second value defines the vertical position of the middle table: a positive value translate the table to the bottom (creating gaps), while a negative value will translate the table to the top.
#' The third value defines the number of empty column(s) to create between the middle table and the right table. Default is 0 for all parameters.
#' @param  orientation allow to change the orientation of the U. Four orientations are available: \code{top}, \code{bottom}, \code{left} and \code{right}. Default is \code{top}.
#' @param  seat.width see \code{\link{room.create.grid}}.
#' @param  seat.height see \code{\link{room.create.grid}}.
#' @param  out a \code{character}. Allow to specify whether you want to get a \code{\link{SpatialPolygons}} object (default) or a \code{matrix} you can edit and reuse in the \code{\link{room.create.grid}} function.
#' @export
#' @examples
#' room.u.0 <- room.create.u()
#' plot(room.u.0)
#' 
#' room.u.1 <- room.create.u(c(9,4,9))
#' plot(room.u.1)
#' 
#' room.u.2 <- room.create.u(c(9,4,9), orientation = 'left')
#' plot(room.u.2)
#' 
#' room.u.3 <- room.create.u(c(9,4,9), orientation = 'right')
#' plot(room.u.3)
#' 
#' room.u.4 <- room.create.u(c(9,4,9), orientation = 'bottom')
#' plot(room.u.4)
#' 
#' room.u.5 <- room.create.u(c(9,4,9), gap=c(1,1,1))
#' plot(room.u.5)
#' 
#' room.u.5 <- room.create.u(c(9,4,9), gap=c(0,-1,0))
#' plot(room.u.5)
#' 
#' room.u.6 <- room.create.u(c(9,4,9), gap=c(0,-5,0))
#' plot(room.u.6)
#' 
#' room.u.7 <- room.create.u(c(9,4,9), seat.width=2)
#' plot(room.u.7)
#' 
#' room.u.8 <- room.create.u(c(9,4,9), out = 'matrix')
#' room.u.8
#' plot(room.create.grid(room.u.8, seat.width=2))
room.create.u <- function(
  x = c(7,3,7),
  gap = c(0, 0, 0),
  orientation = 'top',
  seat.width = 1,
  seat.height = 1,
  out = 'SpatialPolygons'
){
  stopifnot(length(x) == 3)
  stopifnot(!any(is.na(x)))
  stopifnot(all(x >= 1))
  
  stopifnot(length(gap) == 3)
  stopifnot(!any(is.na(gap)))
  stopifnot(all(gap[c(1,3)] >= 0))
  
  stopifnot(out %in% c('matrix','SpatialPolygons'))
  
  maxl <- max(x[c(1,3)])
  if(-gap[2] > maxl -1){
    stop("The gap for the table in the middle is to high. If you want to change the orientation please use the 'orientation' argument")
  }
  
  stopifnot(orientation %in% c('top', 'bottom', 'left', 'right'))
  
  
  ncol <- x[2] + 2 + gap[1] + gap[3]
  nrow <- max(maxl + 1, maxl + 1 + gap[2])
  
  m <- matrix(rep(-1, nrow*ncol), nrow = nrow)
  
  m[min(nrow, nrow + gap[2]),] <- c(rep(-1, gap[1]+1),rep(0,x[2]),rep(-1, gap[3]+1))
  for (i in maxl:1){
    x[c(1,3)] <- x[c(1,3)] - 1
    m[i,c(1,ncol)] <- c(x[1],x[3])
  }
  m[m >= 0] <- 0
  m[m < 0] <- -1
  
  if(orientation == 'left')
    m <- t(m)
  if(orientation == 'bottom')
    m <- matrix(rev(m), nrow = maxl+1)
  if(orientation == 'right')
    m <- t(matrix(rev(m), nrow = maxl+1))
  
  if (out == 'matrix')
    return(m)
  
  if (out == 'SpatialPolygons') {
    return(room.create.grid(
      x = m,
      seat.width = seat.width,
      seat.height = seat.height
    ))
  }
  
}

# room.u.0 <- room.create.u()
# plot(room.u.0)
# room.u.1 <- room.create.u(c(9,4,9))
# plot(room.u.1)
# room.u.2 <- room.create.u(c(9,4,9), orientation = 'left')
# plot(room.u.2)
# room.u.3 <- room.create.u(c(9,4,9), orientation = 'right')
# plot(room.u.3)
# room.u.4 <- room.create.u(c(9,4,9), orientation = 'bottom')
# plot(room.u.4)
# room.u.5 <- room.create.u(c(9,4,9), gap=c(1,1,1))
# plot(room.u.5)
# room.u.5 <- room.create.u(c(9,4,9), gap=c(0,-1,0))
# plot(room.u.5)
# room.u.6 <- room.create.u(c(9,4,9), gap=c(0,-5,0))
# plot(room.u.6)
# room.u.7 <- room.create.u(c(9,4,9), seat.width=2)
# plot(room.u.7)
# room.u.8 <- room.create.u(c(9,4,9), out = 'matrix')
# room.u.8
# plot(room.create.grid(room.u.8, seat.width=2))