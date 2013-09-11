\docType{methods}
\name{plot.position}
\alias{plot.position}
\alias{plot.position,SpatialNetwork,SpatialNetwork-method}
\alias{plot.position,SpatialPolygons,SpatialPolygons-method}
\title{Plot a map labelled with the ID numbering}
\arguments{
  \item{x}{an object for which a \code{plot.position}
  method is defined. See
  \code{\link{plot.position-methods}} for a list of object
  supported.}

  \item{label}{a character of length 1 for prefixing seat
  numbering.}

  \item{...}{other arguments to pass to the plot function.
  The main usage is setting the \code{cex} value.}
}
\value{
  NULL
}
\description{
  The \code{plot.position} function allows to plot maps
  defined as for example \code{SpatialNetwork} or
  \code{SpatialPolygons} objects, and render the ID
  numbering.
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
plot.position(room1)
plot.position(room1, label = 'Seat ')
plot.position(room1, label = 'Seat ', cex = 0.8)
}
\author{
  Emmanuel Rousseaux
}

