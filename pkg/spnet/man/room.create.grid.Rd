\name{room.create.grid}
\alias{room.create.grid}
\title{
Create a room designed by a grid
}
\description{
The \code{room.create.grid} function allows to design a room by help of a 2D matrix specifying seats.
}
\usage{
room.create.grid(
  x,
  seat.width = 1,
  seat.height = 1
)
}
\arguments{
  \item{x}{
a \code{matrix} object. Use \code{'0'} for specifying a seat and \code{'-1'} for a blank area. See \code{Details} for more flexible options, especially for specifying seats number.
}
  \item{seat.width}{
a \code{numeric} of length 1. Define the width of each seat.
}
  \item{seat.height}{
a \code{numeric} of length 1. Define the height of each seat.
}
}
\details{
The matrix object is read cells line-by-line from the bottom-left to the top-up. For each cell, if the value is positive, a seat will be created in the corresponding location on the map. Else, the function jump to the next cell. If all positive value are '0', seats will be automatically numbered. The order used is from the bottom-left to the top-up.

If you want to specify each seat number you just have to give them instead of giving 0. You can use \code{numeric} values or \code{character} values (see \code{examples}). Please warn that your seat numbering have to be unique: you can't give the same seat number to two different seats.

If in your room you have some of rows of columns with an odd number of seats, you can adjust their location for a better fitting by specifying \code{'E'} or \code{'O'} in the \code{dimnames} attribute of the \code{matrix} (see \code{examples}).
}
\value{
A \code{\link{SpatialPolygons}} object.
}
\author{
Emmanuel Rousseaux
}
\seealso{
\code{\link{plot.placement}}
}
\examples{
# A room with tables in inversed 'U' form
col <- 5
row <- 6
m <- matrix(rep(-1, col*row), nrow = row)
m[1,2:4] <- 0
m[3,c(1,5)] <- 0
m[4,c(1,5)] <- 0
m[5,c(1,5)] <- 0
m[6,c(1,5)] <- 0
m
room1 <- room.create.grid(m, seat.width=2, seat.height=1)
plot.placement(room1)


# A room with tables in inversed 'U', asymetric, with a customized seat numbering
col <- 5
row <- 6
m <- matrix(rep(-1, col*row), nrow = row)
m[1,2:4] <- c('C01','C02','C03')
m[3,c(1,5)] <- c('L01', 'R01')
m[4,c(1,5)] <- c('L02', 'R02')
m[5,1] <- c('L03')
m[6,1] <-c('L04')
m
room2 <- room.create.grid(m, seat.width=2, seat.height=1)
plot.placement(room2)


# A room with tables in square, and with odd and even number of seats
col <- 6
row <- 11
m <- matrix(rep(-1, col*row), nrow = row)
m[1,2:4] <- 0
m[4:9,c(1,6)] <- 0
m[11,2:5] <- 0
dimnames(m) <- list(
  c('O', rep('E',row-1)),
  rep('E',col)
)
room3 <- room.create.grid(m, seat.width=2, seat.height=1)
plot.placement(room3)
}
\keyword{ map }
\keyword{ room }
\keyword{ seat }
