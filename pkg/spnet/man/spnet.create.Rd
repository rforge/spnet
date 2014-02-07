\name{spnet.create}
\alias{spnet.create}
\title{Create a \code{SpatialNetwork} object}
\usage{
spnet.create(x, map, networks, plot.title = list(main =
  "Untitled SPNET object", sub = "", cex = 2, col = "#333333"),
  plot.label = list(cex = 1, col = "#333333"), plot.color, plot.symbol,
  plot.arrow, plot.legend = list(print = TRUE, cex = 1, ncol = 1, horiz =
  FALSE), plot.layout = list(ratios = c(title = 1/10, graphic = 7/10, legend =
  2/10), mat = NULL, reset = TRUE), plot.par = list(mar = c(1, 1, 1, 1)),
  infos, quiet = FALSE)
}
\arguments{
  \item{x}{AAA}

  \item{map}{AAA}

  \item{networks}{AAA}

  \item{plot.title}{AAA}

  \item{plot.label}{list of arguments to be passed to the
  \code{\link{text}} function.}

  \item{plot.color}{AAA}

  \item{plot.symbol}{AAA}

  \item{plot.arrow}{AAA}

  \item{plot.legend}{AAA}

  \item{plot.layout}{AAA}

  \item{plot.par}{AAA}

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

