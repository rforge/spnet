% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/SpatialNetwork.r
\docType{methods}
\name{graph.network.arrow.color}
\alias{graph.network.arrow.color}
\alias{graph.network.arrow.color,SpatialNetwork,character-method}
\alias{graph.network.arrow.color<-,SpatialNetwork,character,character-method}
\title{Get the arrow color of a given network of a \code{SpatialNetwork} object}
\usage{
graph.network.arrow.color(object, network.name)

\S4method{graph.network.arrow.color}{SpatialNetwork,character}(object,
  network.name)


  \S4method{graph.network.arrow.color}{SpatialNetwork,character,character}(object,
  network.name) <- value
}
\arguments{
\item{object}{a \code{SpatialNetwork} object.}

\item{network.name}{character; the name of the network.}

\item{value}{the arrow color.}
}
\description{
This generic method intends to extract the arrow color of a given network of a \code{SpatialNetwork} object.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = SpatialNetwork,network.name = character}: method for \code{SpatialNetwork} objects.

\item \code{object = SpatialNetwork,network.name = character,value = character}: method for \code{SpatialNetwork} objects.
}}

