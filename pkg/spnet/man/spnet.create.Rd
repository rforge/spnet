\name{spnet.create}
\alias{spnet.create}
\title{Create a \code{SpatialNetwork} object}
\usage{
  spnet.create(x, map, networks, plot.title, plot.color,
    plot.symbol, plot.arrow, infos, quiet = FALSE)
}
\arguments{
  \item{x}{AAA}

  \item{map}{AAA}

  \item{networks}{AAA}

  \item{plot.title}{AAA}

  \item{plot.color}{AAA}

  \item{plot.symbol}{AAA}

  \item{plot.arrow}{AAA}

  \item{infos}{AAA}

  \item{quiet}{= FALSE AAA}
}
\description{
  The \code{spnet.create} function is the official builder
  for creating \code{SpatialNetwork} objects.
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

