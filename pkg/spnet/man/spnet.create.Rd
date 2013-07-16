\name{spnet.create}
\alias{spnet.create}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a \code{SpatialNetwork} object
}
\description{
The \code{spnet.create} function is the official builder for creating \code{SpatialNetwork} objects.
}
\usage{
spnet.create(
  x,
  sp,
  networks,
  plot.title,
  plot.color,
  plot.symbol,
  plot.arrow,
  infos,
  quiet = FALSE
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{sp}{
%%     ~~Describe \code{sp} here~~
}
  \item{networks}{
%%     ~~Describe \code{networks} here~~
}
  \item{plot.title}{
%%     ~~Describe \code{plot.title} here~~
}
  \item{plot.color}{
%%     ~~Describe \code{plot.color} here~~
}
  \item{plot.symbol}{
%%     ~~Describe \code{plot.symbol} here~~
}
  \item{plot.arrow}{
%%     ~~Describe \code{plot.arrow} here~~
}
  \item{infos}{
%%     ~~Describe \code{infos} here~~
}
  \item{quiet}{
%%     ~~Describe \code{quiet} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{\link{SpatialNetwork-class}} object.
}
\author{
Emmanuel Rousseaux
}

\seealso{
\code{\link{SpatialNetwork-class}}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatial }
\keyword{ network }% __ONLY ONE__ keyword per line
