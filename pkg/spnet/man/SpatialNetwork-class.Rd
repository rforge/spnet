\docType{class}
\name{SpatialNetwork}
\alias{SpatialNetwork}
\alias{SpatialNetwork-class}
\title{Class \code{"SpatialNetwork"}}
\description{
  Allow to store spatial networks, especially for rendering
  them
}
\section{Objects from the Class}{
  Objects can be created with the \code{\link{spnet}}
  function (official class builder).
}

\section{Slots}{
  \describe{ \item{\code{.Data}:}{Object of class
  \code{"list"} ~~ } \item{\code{sp}:}{Object of class
  \code{"SpatialPolygons"} ~~ }
  \item{\code{networks}:}{Object of class \code{"list"} ~~
  } \item{\code{plot.title}:}{Object of class \code{"list"}
  ~~ } \item{\code{plot.color}:}{Object of class
  \code{"list"} ~~ } \item{\code{plot.symbol}:}{Object of
  class \code{"list"} ~~ } \item{\code{plot.arrow}:}{Object
  of class \code{"list"} ~~ } \item{\code{infos}:}{Object
  of class \code{"list"} ~~ } \item{\code{meta}:}{Object of
  class \code{"list"} ~~ } \item{\code{warnings}:}{Object
  of class \code{"list"} ~~ } \item{\code{names}:}{Object
  of class \code{"character"} ~~ }
  \item{\code{row.names}:}{Object of class
  \code{"data.frameRowLabels"} ~~ }
  \item{\code{.S3Class}:}{Object of class
  \code{"character"} ~~ } }
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
\author{
  Emmanuel Rousseaux
}
\keyword{classes}
\keyword{network}
\keyword{sp}
\keyword{spatial}

