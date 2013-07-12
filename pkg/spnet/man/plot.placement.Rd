\name{plot.placement}
\alias{plot.placement}
\title{
Plot a room with the seat numbering
}
\description{
The \code{plot.placement} function allows to plot rooms defined as for example \code{SpatialNetwork} or \code{SpatialPolygons} objects, and render the seat numbering.
}
\usage{
plot.placement(
  x,
  label = "",
  ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object for which a \code{plot.placement} method is defined. See \code{\link{plot.placement-methods}} for a list of object supported.
}
  \item{label}{
a character of length 1 for prefixing seat numbering.
}
  \item{\dots}{
other arguments to pass to the plot function. The main usage is setting the \code{cex} value.
}
}
\author{
Emmanuel Rousseaux
}
\seealso{
\code{\link{plot.placement-methods}} methods
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
plot.placement(room1, label = 'Seat ')
plot.placement(room1, label = 'Seat ', cex = 0.8)
}
\keyword{ plot }
\keyword{ spatial }
\keyword{ SpatialPolygons }
\keyword{ SpatialNetwork }
