\docType{class}
\name{SpatialNetwork-class}
\alias{SpatialNetwork-class}
\title{Class \code{"SpatialNetwork"}}
\description{
Allow to store spatial networks, especially for rendering
them
}
\section{Slots}{

\describe{
\item{\code{.Data}}{object of class \code{"list"}}

\item{\code{map}}{object of class \code{"SpatialPolygons"}}

\item{\code{networks}}{object of class \code{"list"}}

\item{\code{plot.title}}{object of class \code{"list"}}

\item{\code{plot.label}}{object of class \code{"list"}}

\item{\code{plot.color}}{object of class \code{"list"}}

\item{\code{plot.symbol}}{object of class \code{"list"}}

\item{\code{plot.arrow}}{object of class \code{"list"}}

\item{\code{plot.legend}}{object of class \code{"list"}}

\item{\code{plot.layout}}{object of class \code{"list"}}

\item{\code{plot.par}}{object of class \code{"list"}}

\item{\code{infos}}{object of class \code{"list"}}

\item{\code{meta}}{object of class \code{"list"}}

\item{\code{warnings}}{object of class \code{"list"}}

\item{\code{names}}{object of class \code{"character"}}

\item{\code{row.names}}{object of class \code{"data.frameRowLabels"}}

\item{\code{.S3Class}}{object of class \code{"character"}}
}}
\section{Objects from the Class}{
  Objects can be created with the \code{\link{spnet}}
  function (official class builder).
}

\section{Extends}{
  Class \code{"\linkS4class{data.frame}"}, directly. Class
  \code{"\linkS4class{list}"}, by class "data.frame",
  distance 2. Class \code{"\linkS4class{oldClass}"}, by
  class "data.frame", distance 2. Class
  \code{"\linkS4class{vector}"}, by class "data.frame",
  distance 3.
}

\section{Methods}{
  \describe{ \item{plot}{\code{signature(x =
  "SpatialNetwork", y = "ANY")}: ... }
  \item{plot.position}{\code{signature(x =
  "SpatialNetwork")}: ... } \item{print}{\code{signature(x
  = "SpatialNetwork")}: ... }
  \item{show}{\code{signature(object = "SpatialNetwork")}:
  ... } }
}
\examples{
people <- c("John", "Elsa", "Brian", "Kate")
position <- c(2,4,6,8)

net1.df <- data.frame(
  'NODE' = people,
  'POSITION' = position
)

net1 <- spnet.create(
  x = net1.df
)
net1

net2 <- spnet.create(
  x = people
)
net2
}
\keyword{classes}
\keyword{network}
\keyword{sp}
\keyword{spatial}

