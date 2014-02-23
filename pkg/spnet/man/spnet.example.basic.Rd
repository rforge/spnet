\name{spnet.example.basic}
\alias{spnet.example.basic}
\alias{spnet.example.basic.full}
\alias{spnet.example.basic.map}
\title{Spnet basic examples}
\usage{
spnet.example.basic(map = TRUE, color = TRUE, symbol = TRUE,
  network1 = TRUE, network2 = TRUE)

spnet.example.basic.full()

spnet.example.basic.map()
}
\arguments{
  \item{map}{if \code{TRUE} an example of map is provided.}

  \item{color}{if \code{TRUE} an example of map
  colorization is provided.}

  \item{symbol}{if \code{TRUE} an example of symbol use is
  provided.}

  \item{network1}{if \code{TRUE} a first example of network
  is provided.}

  \item{network2}{if \code{TRUE} a second example of
  network is provided.}
}
\value{
a \code{SpatialNetwok} object.
}
\description{
Create SpatialNetwork object examples for demonstration and
testing purpose.
}
\examples{
net1 <- spnet.example.basic()
plot(net1)
}

