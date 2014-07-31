#' Class \code{"SpatialNetwork"}
#' 
#' Allow to store spatial networks, especially for rendering them
#'
#' @rdname SpatialNetwork
#' @export
#' @keywords classes spatial network sp
#' @family spnet-class
#' @slot .Data object of class \code{"list"}
#' @slot map object of class \code{"SpatialPolygons"}
#' @slot networks object of class \code{"list"}
#' @slot plot.title object of class \code{"list"}
#' @slot plot.label object of class \code{"list"}
#' @slot plot.color object of class \code{"list"}
#' @slot plot.symbol object of class \code{"list"}
#' @slot plot.arrow object of class \code{"list"}
#' @slot plot.barplot object of class \code{"list"}
#' @slot plot.legend object of class \code{"list"}
#' @slot plot.layout object of class \code{"list"}
#' @slot plot.par object of class \code{"list"}
#' @slot infos object of class \code{"list"}
#' @slot meta object of class \code{"list"}
#' @slot warnings object of class \code{"list"}
#' @slot names object of class \code{"character"}
#' @slot row.names object of class \code{"data.frameRowLabels"}
#' @slot .S3Class object of class \code{"character"}
#'    
#' @section Objects from the Class:
#' Objects can be created with the \code{\link{spnet}} function (official class builder).
#' 
#'   
#' @examples
#' people <- c("John", "Elsa", "Brian", "Kate")
#' position <- c(2,4,6,8)
#' 
#' net1.df <- data.frame(
#'   'NODE' = people,
#'   'POSITION' = position
#' )
#' 
#' net1 <- spnet.create(
#'   x = net1.df
#' )
#' net1
#' 
#' net2 <- spnet.create(
#'   x = people
#' )
#' net2
setClass(
  Class = "SpatialNetwork",
  contains = 'data.frame',
  slots = c(
    #     'edges' = 'data.frame',
    'map' = 'SpatialPolygons',
    'networks' = 'list',
    'plot.title' = 'list',
    'plot.label' = 'list',
    'plot.color' = 'list',
    'plot.symbol' = 'list',
    'plot.arrow' = 'list',
    'plot.barplot' = 'list',
    'plot.legend' = 'list',
    'plot.layout' = 'list',
    'plot.par' = 'list',
    'infos' = 'list', # for the user
    'meta' = 'list', # for the dev
    'warnings' = 'list' # for the dev
  ),
  prototype = prototype(
    meta = list(
      date.created = Sys.time(),
      plot.color.default = "grey90",
      plot.symbol.default = list(
        color = 'grey10',
        cex = 4,
        space = 0.5,
        translate.x = 0,
        translate.y = 0
      ),
      plot.arrow.default = list(
        max.networks = 5,
        color = c('dodgerblue2', 'brown3', 'darkorange3', 'olivedrab', 'hotpink3'),
        translate.x = c(0,0.2,-0.2,0,0),
        translate.y = c(0,0,0,-0.2,0.2),
        opacity = 0.9,
        thickness = 2.00,
        length.rate = 1,
        shortening = 0.3,
        head.length = 0.20,
        head.type = 'curved'
      )
    )
  ),
  #   contains=character(),
  #   sealed = FALSE,
  validity = function(object) {
    flag = TRUE
    
    # .Data: NODE
    if(flag && (!'NODE' %in% names(object))){
      stop("You have to provide a 'NODE' column.")
    }
    # .Data: If POSITION exists
    if(flag && ('POSITION' %in% names(object))){
      if (length(object@map) > 0) { # there is a map
        exist.in.map <- object[,'POSITION'] %in% row.names(coordinates(object@map))
        if(!all(exist.in.map)) {
          stop(paste(
            "Some elements of the 'POSITION' column are not referenced on the map:",
            paste(object[which(!exist.in.map),'POSITION'], collapse = ', ')
          )
          )
        }
      }
    }
    
    # layout
    
    # colors
    #     color <- object@plot.color
    #     if(flag && (length(color) > 0)){
    #       if(!all(names(color) %in% c('variable', 'legend'))) stop("Elements in 'plot.color' have to be named by one of the following names: 'variable', 'legend'.")
    #       if(!'variable' %in% names(color)) stop("The 'plot.color' list should contain a 'variable' element")
    #       if(!'legend' %in% names(color)) stop("The 'plot.color' list should contain a 'legend' element")
    #       if(!color$variable %in% names(object)) stop("The 'variable' element of 'plot.color' doesn't exist in data.")
    #       exist.in.data <- names(color$legend) %in% unique(object[,color$variable])
    #       if(!all(exist.in.data)) {
    #         stop(paste(
    #           "Some values in the 'legend' referenced in 'plot.color' doesn't exist in the variable ",
    #           color$variable,
    #           ': ',
    #           paste(names(color$legend)[which(!exist.in.data)], collapse = ', '),
    #           sep = ''
    #           )
    #         )
    #       }
    #     }
    
    # symbol
    #     symbol <- object@plot.symbol
    #     if(flag && (length(symbol) > 0)){
    #       if(!all(names(symbol) %in% c('variable', 'legend', 'color', 'cex', 'space', 'translate.x', 'translate.y'))) stop("Elements in 'plot.symbol' have to be named by one of the following names: 'variable', 'legend', 'color', 'cex', 'space', 'translate.x', 'translate.y'.")
    #       if(!'variable' %in% names(symbol)) stop("The 'plot.symbol' list should contain a 'variable' element")
    #       if(!'legend' %in% names(symbol)) stop("The 'plot.symbol' list should contain a 'legend' element")
    #       if(!symbol$variable %in% names(object)) stop("The 'variable' element of 'plot.symbol' doesn't exist in data.")
    #       values.of.symbol.column <- unique(.extract.multiple.strings(object[,symbol$variable]))
    #       exist.in.data <- names(symbol$legend) %in% values.of.symbol.column
    #       if(!all(exist.in.data)) {
    #         stop(paste(
    #           "Some values in the 'legend' referenced in 'plot.symbol' doesn't exist in the variable ",
    #           symbol$variable,
    #           ': ',
    #           paste(names(symbol$legend)[which(!exist.in.data)], collapse = ', '),
    #           sep = ''
    #           )
    #         )
    #       }
    #       exist.in.symbol <- symbol$legend %in% names(spnet::.spnet.symbol.list)
    #       if(!all(exist.in.symbol)) stop(
    #         paste(
    #           "Some symbol names you provided doesn't exist:",
    #           paste(symbol$legend[which(!exist.in.symbol)])
    #         ))
    #     }
    
    # barplots
    # v??rif variable num??rique, 
    
    # networks
    nets <- object@networks
    if(flag && (length(nets) > 0)){
      
      if(!(length(nets) <= object@meta$plot.arrow.default$max.networks)) {
        stop(paste("The number of networks is limited to", object@meta$plot.arrow.default$max.networks))
      }
      
      for (net in nets) {
        #         if(!'data' %in% names(net)) {
        #           stop("One of the network doesn't contain a 'data' element")
        #         }
        net.data <- net$data
        if(is.matrix(net.data)) {
          if(flag && (nrow(net.data)>0 || ncol(net.data)>0)) {
            if(nrow(net.data) != ncol(net.data)) {
              message("When network data are provided by a 'matrix', it is expected to be squared.")
              message("Here ncol=", ncol(net.data), "and nrow=", nrow(net.data))
              stop("Invalid 'networks' matrix dimensions")
            }
          }
        }
        if(flag && 'opacity' %in% net) {
          if(net$opacity < 0 || net$opacity > 1)
            stop("In each network the opacity has to be in [0;1]")
        }
      }
    }
    
    return(flag)
    
    # COLOR
    ## legend in list
    ## variable in list
    ##
  }
)


#' Extract or replace parts of a SpatialNetwork object
#'
#' @name [
#' @aliases [,SpatialNetwork-method
#' @docType methods
#' @rdname extract-methods
NULL
# setMethod(
#   "[", signature(x = "SpatialNetwork", i = "ANY", j="ANY"),
#   function (x, i, j, ..., drop) {
#     x[i,j]
#   }
# )

#' set parts of SpatialNetwork
#'
#' @name [<-
#' @aliases [<-,SpatialNetwork-method
#' @docType methods
#' @rdname extract-methods
NULL
# setReplaceMethod(
#   "[", signature(x = "SpatialNetwork", i = "ANY", j="ANY"),
#   function (x, i, j, ..., drop, value) {
#     x[i,j] <- value
#   }
# )














#' Get the map to a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the map object. Currently only \code{SpatialPolygons} from the \code{sp} package are supported.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get the map.
#' @export
setGeneric("spnet.map", function(object){ standardGeneric("spnet.map") })

#' @describeIn spnet.map method for \code{SpatialPolygons} objects.
setMethod(
  f = "spnet.map",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "map"))
  }
)

#' Set the map to a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the map object. Currently only \code{SpatialPolygons} from the \code{sp} package are supported.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set the map.
#' @param value the map.
#' @export
setGeneric("spnet.map<-", function(object, value){ standardGeneric("spnet.map<-") })

#' @describeIn spnet.map method for \code{SpatialPolygons} objects.
setMethod(
  f = "spnet.map<-" ,
  signature = c("SpatialNetwork", 'SpatialPolygons'),
  definition = function(object, value){
    object@map <- value
    validObject(object)
    return(object)
  }
)











#' Get the list of all networks parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract networks parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @export
setGeneric("spnet.networks.list", function(object){ standardGeneric("spnet.networks.list") })

#' @describeIn spnet.networks.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.networks.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "networks"))
  }
)

#' Set the list of all networks parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace networks parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.networks.list<-", function(object, value){ standardGeneric("spnet.networks.list<-") })

#' @describeIn spnet.networks.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.networks.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@networks <- value
    validObject(object)
    return(object)
  }
)




#' Get the list of all parameters of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract all parameters of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @param network.name character; the name of the network.
#' @export
setGeneric("spnet.network.list", function(object, network.name){ standardGeneric("spnet.network.list") })

#' @describeIn spnet.network.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.list",
  signature = c("SpatialNetwork", "character"),
  definition = function (object, network.name) { 
    return(slot(object, "networks")[[network.name]])
  }
)

#' Set the list of all parameters of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace all parameters of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param network.name character; the name of the network.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.network.list<-", function(object, network.name, value){ standardGeneric("spnet.network.list<-") })

#' @describeIn spnet.network.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.list<-" ,
  signature = c("SpatialNetwork", "character", 'list'),
  definition = function(object, network.name, value){
    object@networks[[network.name]] <- value
    validObject(object)
    return(object)
  }
)












#' Test if a network exist
#' 
#' This function tests if the network name given in parameter match the name of a network defined within a \code{SpatialNetwork} object.
#' @param object a \code{SpatialNetwork} object.
#' @param network.name a character; the name of the network.
#' @export
spnet.network.exists <- function(object, network.name) {
  stopifnot(inherits(object, 'SpatialNetwork'))
  return(is.element(network.name, names(spnet.networks.list(object))))
}






#' Add a network
#' 
#' This function defines a new network item in a \code{SpatialNetwork} object.
#' @param object a \code{SpatialNetwork} object.
#' @param value a character; the name of the network.
#' @export
setGeneric("spnet.networks.add<-", function(object, value){ standardGeneric("spnet.networks.add<-") })

#' @rdname spnet.networks.add-set
setMethod(
  f = "spnet.networks.add<-",
  signature = c("SpatialNetwork", "character"),
  definition = function(object, value){
    network.name <- value
    if(make.names(network.name) != network.name) {
      stop("The name is not valid. Please check it with 'make.names()'.")
    }
    if(spnet.network.exists(object, network.name)) {
      stop("This network name is already defined.")
    }
    spnet.networks.list(object) <- eval(parse(text = paste0("c(spnet.networks.list(object), list(", network.name, " = list()))")))
    return(object)
  }
)


#' Remove a network
#' 
#' This function remove a network item in a \code{SpatialNetwork} object.
#' @param object a \code{SpatialNetwork} object.
#' @param value a character; the name of the network.
#' @export
setGeneric("spnet.networks.remove<-", function(object, value){ standardGeneric("spnet.networks.remove<-") })

#' @rdname spnet.networks.remove-set
setMethod(
  f = "spnet.networks.remove<-",
  signature = c("SpatialNetwork", "character"),
  definition = function(object, value){
    network.name <- value
    if(make.names(network.name) != network.name) {
      stop("The name is not valid. Please check it with 'make.names()'.")
    }
    if(!spnet.network.exists(object, network.name)) {
      stop("This network name doesn't exist.")
    }
    if(length(spnet.networks.list(object)) == 1) {
      spnet.networks.list(object) <- list()
    } else {
      spnet.networks.list(object) <- spnet.networks.list(object)[-which(names(spnet.networks.list(object)) == network.name)]
    }
    return(object)
  }
)







#' Get the data of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the data of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.data", function(object, network.name){ standardGeneric("spnet.network.data") })

#' @describeIn spnet.network.data method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.data",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'. Please use the 'spnet.network.add' function to define a network before trying to add data.")
    }
    return(object@networks[[network.name]]$data)
  }
)


#' Set the data of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the data of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the network data. Currently only support a \code{matrix} object.
#' @export
setGeneric("spnet.network.data<-", function(object, network.name, value){ standardGeneric("spnet.network.data<-") })

#' @describeIn spnet.network.data method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.data<-" ,
  signature = c("SpatialNetwork", "character", "matrix"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'. Please use the 'spnet.networks.add' function to define a network before trying to add data.")
    }
    object@networks[[network.name]]$data <- value
    validObject(object)
    return(object)
  }
)













#' Get the label of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the label of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.label", function(object, network.name){ standardGeneric("spnet.network.label") })

#' @describeIn spnet.network.label method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.label",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$label)
  }
)


#' Set the label of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the label of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the network label. Currently only support a \code{matrix} object.
#' @export
setGeneric("spnet.network.label<-", function(object, network.name, value){ standardGeneric("spnet.network.label<-") })

#' @describeIn spnet.network.label method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.label<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    object@networks[[network.name]]$label <- value
    validObject(object)
    return(object)
  }
)












#' Get the arrow color of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the arrow color of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.arrow.color", function(object, network.name){ standardGeneric("spnet.network.arrow.color") })

#' @describeIn spnet.network.arrow.color method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.color",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$color)
  }
)


#' Set the arrow color of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the arrow color of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the arrow color.
#' @export
setGeneric("spnet.network.arrow.color<-", function(object, network.name, value){ standardGeneric("spnet.network.arrow.color<-") })

#' @describeIn spnet.network.arrow.color method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.color<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    object@networks[[network.name]]$color <- value
    validObject(object)
    return(object)
  }
)












#' Get the arrow opacity of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the arrow opacity of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.arrow.opacity", function(object, network.name){ standardGeneric("spnet.network.arrow.opacity") })

#' @describeIn spnet.network.arrow.opacity method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.opacity",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$opacity)
  }
)


#' Set the arrow opacity of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the arrow opacity of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the arrow opacity.
#' @export
setGeneric("spnet.network.arrow.opacity<-", function(object, network.name, value){ standardGeneric("spnet.network.arrow.opacity<-") })

#' @describeIn spnet.network.arrow.opacity method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.opacity<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no arrow called '", network.name, "'.")
    }
    object@networks[[network.name]]$opacity <- value
    validObject(object)
    return(object)
  }
)














#' Get the arrow thickness of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the arrow thickness of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.arrow.thickness", function(object, network.name){ standardGeneric("spnet.network.arrow.thickness") })

#' @describeIn spnet.network.arrow.thickness method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.thickness",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$thickness)
  }
)


#' Set the arrow thickness of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the arrow thickness of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the arrow thickness.
#' @export
setGeneric("spnet.network.arrow.thickness<-", function(object, network.name, value){ standardGeneric("spnet.network.arrow.thickness<-") })

#' @describeIn spnet.network.arrow.thickness method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.thickness<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    object@networks[[network.name]]$thickness <- value
    validObject(object)
    return(object)
  }
)













#' Get the arrow shortening of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the arrow shortening of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.arrow.shortening", function(object, network.name){ standardGeneric("spnet.network.arrow.shortening") })

#' @describeIn spnet.network.arrow.shortening method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.shortening",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$shortening)
  }
)


#' Set the arrow shortening of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the arrow shortening of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the arrow shortening.
#' @export
setGeneric("spnet.network.arrow.shortening<-", function(object, network.name, value){ standardGeneric("spnet.network.arrow.shortening<-") })

#' @describeIn spnet.network.arrow.shortening method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.shortening<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'. Please use the 'spnet.networks.add' function to define a network before trying to add shortening.")
    }
    object@networks[[network.name]]$shortening <- value
    validObject(object)
    return(object)
  }
)

















#' Get the arrow head type of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the arrow head type of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.arrow.head.type", function(object, network.name){ standardGeneric("spnet.network.arrow.head.type") })

#' @describeIn spnet.network.arrow.head.type method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.head.type",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$head.type)
  }
)


#' Set the arrow head type of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the arrow head type of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the arrow head type.
#' @export
setGeneric("spnet.network.arrow.head.type<-", function(object, network.name, value){ standardGeneric("spnet.network.arrow.head.type<-") })

#' @describeIn spnet.network.arrow.head.type method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.head.type<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    object@networks[[network.name]]$head.type <- value
    validObject(object)
    return(object)
  }
)












#' Get the arrow head length of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the arrow head length of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.arrow.head.length", function(object, network.name){ standardGeneric("spnet.network.arrow.head.length") })

#' @describeIn spnet.network.arrow.head.length method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.head.length",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$head.length)
  }
)


#' Set the arrow head length of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the arrow head length of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the arrow head length.
#' @export
setGeneric("spnet.network.arrow.head.length<-", function(object, network.name, value){ standardGeneric("spnet.network.arrow.head.length<-") })

#' @describeIn spnet.network.arrow.head.length method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.head.length<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    object@networks[[network.name]]$head.length <- value
    validObject(object)
    return(object)
  }
)












#' Get the arrow translation on the x axis of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the arrow translation on the x axis of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.arrow.translate.x", function(object, network.name){ standardGeneric("spnet.network.arrow.translate.x") })

#' @describeIn spnet.network.arrow.translate.x method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.translate.x",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$translate.x)
  }
)


#' Set the arrow translation on the x axis of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the arrow translation on the x axis of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the arrow translation on the x axis.
#' @export
setGeneric("spnet.network.arrow.translate.x<-", function(object, network.name, value){ standardGeneric("spnet.network.arrow.translate.x<-") })

#' @describeIn spnet.network.arrow.translate.x method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.translate.x<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    object@networks[[network.name]]$translate.x <- value
    validObject(object)
    return(object)
  }
)










#' Get the arrow translation on the y axis of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the arrow translation on the y axis of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.network.arrow.translate.y", function(object, network.name){ standardGeneric("spnet.network.arrow.translate.y") })

#' @describeIn spnet.network.arrow.translate.y method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.translate.y",
  signature = c("SpatialNetwork", "character"), 
  definition = function (object, network.name) {
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    return(object@networks[[network.name]]$translate.y)
  }
)


#' Set the arrow translation on the y axis of a given network of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the arrow translation on the y axis of a given network of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param network.name character; the name of the network.
#' @param value the arrow translation on the y axis.
#' @export
setGeneric("spnet.network.arrow.translate.y<-", function(object, network.name, value){ standardGeneric("spnet.network.arrow.translate.y<-") })

#' @describeIn spnet.network.arrow.translate.y method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.network.arrow.translate.y<-" ,
  signature = c("SpatialNetwork", "character", "character"),
  definition = function(object, network.name, value){
    if(!spnet.network.exists(object, network.name)) {
      stop("There is no network called '", network.name, "'.")
    }
    object@networks[[network.name]]$translate.y <- value
    validObject(object)
    return(object)
  }
)
















#' Get the list of all title parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract title parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @export
setGeneric("spnet.title.list", function(object){ standardGeneric("spnet.title.list") })

#' @describeIn spnet.title.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.title.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.title"))
  }
)

#' Set the list of all title parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace title parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.title.list<-", function(object, value){ standardGeneric("spnet.title.list<-") })

#' @describeIn spnet.networks.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.title.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.title <- value
    validObject(object)
    return(object)
  }
)








#' Get the main title of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the main title of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.title.main", function(object){ standardGeneric("spnet.title.main") })

#' @describeIn spnet.title.main method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.title.main",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.title")$main)
  }
)


#' Set the main title  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the main title of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the new title.
#' @export
setGeneric("spnet.title.main<-", function(object, value){ standardGeneric("spnet.title.main<-") })

#' @describeIn spnet.title.main method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.title.main<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.title$main <- value
    validObject(object)
    return(object)
  }
)













#' Get the sub title of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the sub title of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.title.sub", function(object){ standardGeneric("spnet.title.sub") })

#' @describeIn spnet.title.sub method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.title.sub",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.title")$sub)
  }
)


#' Set the sub title  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the sub title of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the new title.
#' @export
setGeneric("spnet.title.sub<-", function(object, value){ standardGeneric("spnet.title.sub<-") })

#' @describeIn spnet.title.sub method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.title.sub<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.title$sub <- value
    validObject(object)
    return(object)
  }
)












#' Get the list of all label parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract label parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @export
setGeneric("spnet.label.list", function(object){ standardGeneric("spnet.label.list") })

#' @describeIn spnet.label.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.label.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.label"))
  }
)

#' Set the list of all label parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace label parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.label.list<-", function(object, value){ standardGeneric("spnet.label.list<-") })

#' @describeIn spnet.label.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.label.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.label <- value
    validObject(object)
    return(object)
  }
)















#' Get the label variable of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the label variable of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.label.variable", function(object){ standardGeneric("spnet.label.variable") })

#' @describeIn spnet.label.variable method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.label.variable",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.label")$variable)
  }
)

#' Set the label variable  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the label variable of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the new label, for example "#000000".
#' @export
setGeneric("spnet.label.variable<-", function(object, value){ standardGeneric("spnet.label.variable<-") })

#' @describeIn spnet.label.variable method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.label.variable<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.label$variable <- value
    validObject(object)
    return(object)
  }
)













#' Get the label cex of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the label cex of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.label.cex", function(object){ standardGeneric("spnet.label.cex") })

#' @describeIn spnet.label.cex method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.label.cex",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.label")$cex)
  }
)

#' Set the label cex  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the label cex of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value numeric; the cex parameter.
#' @export
setGeneric("spnet.label.cex<-", function(object, value){ standardGeneric("spnet.label.cex<-") })

#' @describeIn spnet.label.cex method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.label.cex<-" ,
  signature = c("SpatialNetwork", 'numeric'),
  definition = function(object, value){
    object@plot.label$cex <- value
    validObject(object)
    return(object)
  }
)















#' Get the label color of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the label color of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.label.color", function(object){ standardGeneric("spnet.label.color") })

#' @describeIn spnet.label.color method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.label.color",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.label")$col)
  }
)

#' Set the label color  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the label color of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the new label, for example "#000000".
#' @export
setGeneric("spnet.label.color<-", function(object, value){ standardGeneric("spnet.label.color<-") })

#' @describeIn spnet.label.color method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.label.color<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.label$col <- value
    validObject(object)
    return(object)
  }
)


















#' Get the list of all color parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract color parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @export
setGeneric("spnet.color.list", function(object){ standardGeneric("spnet.color.list") })

#' @describeIn spnet.color.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.color.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.color"))
  }
)

#' Set the list of all color parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace color parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.color.list<-", function(object, value){ standardGeneric("spnet.color.list<-") })

#' @describeIn spnet.color.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.color.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.color <- value
    validObject(object)
    return(object)
  }
)














#' Get the color variable of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the color variable of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.color.variable", function(object){ standardGeneric("spnet.color.variable") })

#' @describeIn spnet.color.variable method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.color.variable",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.color")$variable)
  }
)

#' Set the color variable  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the color variable of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the new color, for example "#000000".
#' @export
setGeneric("spnet.color.variable<-", function(object, value){ standardGeneric("spnet.color.variable<-") })

#' @describeIn spnet.color.variable method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.color.variable<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.color$variable <- value
    validObject(object)
    return(object)
  }
)













#' Get the color legend of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the color legend of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.color.legend", function(object){ standardGeneric("spnet.color.legend") })

#' @describeIn spnet.color.legend method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.color.legend",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.color")$legend)
  }
)

#' Set the color legend  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the color legend of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the color legend.
#' @export
setGeneric("spnet.color.legend<-", function(object, value){ standardGeneric("spnet.color.legend<-") })

#' @describeIn spnet.color.legend method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.color.legend<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.color$legend <- value
    validObject(object)
    return(object)
  }
)














#' Get the list of all symbol parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract symbol parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @export
setGeneric("spnet.symbol.list", function(object){ standardGeneric("spnet.symbol.list") })

#' @describeIn spnet.symbol.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.symbol"))
  }
)

#' Set the list of all symbol parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace symbol parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.symbol.list<-", function(object, value){ standardGeneric("spnet.symbol.list<-") })

#' @describeIn spnet.symbol.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.symbol <- value
    validObject(object)
    return(object)
  }
)















#' Get the symbol variable of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the symbol variable of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.symbol.variable", function(object){ standardGeneric("spnet.symbol.variable") })

#' @describeIn spnet.symbol.variable method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.variable",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.symbol")$variable)
  }
)

#' Set the symbol variable  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the symbol variable of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the symbol variable.
#' @export
setGeneric("spnet.symbol.variable<-", function(object, value){ standardGeneric("spnet.symbol.variable<-") })

#' @describeIn spnet.symbol.variable method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.variable<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.symbol$variable <- value
    validObject(object)
    return(object)
  }
)

















#' Get the symbol legend of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the symbol legend of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.symbol.legend", function(object){ standardGeneric("spnet.symbol.legend") })

#' @describeIn spnet.symbol.legend method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.legend",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.symbol")$legend)
  }
)

#' Set the symbol legend  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the symbol legend of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the new legend.
#' @export
setGeneric("spnet.symbol.legend<-", function(object, value){ standardGeneric("spnet.symbol.legend<-") })

#' @describeIn spnet.symbol.legend method for \code{SpatialNetwork} objects.

setMethod(
  f = "spnet.symbol.legend<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.symbol$legend <- value
    validObject(object)
    return(object)
  }
)












#' Get the symbol cex parameter of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the symbol cex parameter of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.symbol.cex", function(object){ standardGeneric("spnet.symbol.cex") })

#' @describeIn spnet.symbol.cex method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.cex",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.symbol")$cex)
  }
)

#' Set the symbol cex parameter  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the symbol cex parameter of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the new cex parameter.
#' @export
setGeneric("spnet.symbol.cex<-", function(object, value){ standardGeneric("spnet.symbol.cex<-") })

#' @describeIn spnet.symbol.cex method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.cex<-" ,
  signature = c("SpatialNetwork", 'numeric'),
  definition = function(object, value){
    object@plot.symbol$cex <- value
    validObject(object)
    return(object)
  }
)

















#' Get the symbol color of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the symbol color of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.symbol.color", function(object){ standardGeneric("spnet.symbol.color") })

#' @describeIn spnet.symbol.color method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.color",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.symbol")$color)
  }
)

#' Set the symbol color of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the symbol color of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the color.
#' @export
setGeneric("spnet.symbol.color<-", function(object, value){ standardGeneric("spnet.symbol.color<-") })

#' @describeIn spnet.symbol.color method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.color<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.symbol$color <- value
    validObject(object)
    return(object)
  }
)














#' Get the symbol translation on the x axis of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the value of symbol translation on the x axis of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.symbol.translate.x", function(object){ standardGeneric("spnet.symbol.translate.x") })

#' @describeIn spnet.symbol.translate.x method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.translate.x",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.symbol")$translate.x)
  }
)

#' Set the symbol translation on the x axis of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the value of symbol translation on the x axis of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value a numeric; the value of the translation.
#' @export
setGeneric("spnet.symbol.translate.x<-", function(object, value){ standardGeneric("spnet.symbol.translate.x<-") })

#' @describeIn spnet.symbol.translate.x method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.translate.x<-" ,
  signature = c("SpatialNetwork", 'numeric'),
  definition = function(object, value){
    object@plot.symbol$translate.x <- value
    validObject(object)
    return(object)
  }
)












#' Get the symbol translation on the y axis of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the value of the symbol translation on the y of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.symbol.translate.y", function(object){ standardGeneric("spnet.symbol.translate.y") })

#' @describeIn spnet.symbol.translate.y method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.translate.y",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.symbol")$translate.y)
  }
)

#' Set the symbol translation on the y axis of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the value of the symbol translation on the y axis of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value a numeric; the value of the translation.
#' @export
setGeneric("spnet.symbol.translate.y<-", function(object, value){ standardGeneric("spnet.symbol.translate.y<-") })

#' @describeIn spnet.symbol.translate.y method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.symbol.translate.y<-" ,
  signature = c("SpatialNetwork", 'numeric'),
  definition = function(object, value){
    object@plot.symbol$translate.y <- value
    validObject(object)
    return(object)
  }
)












#' Get the list of all barplot parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract barplot parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @export
setGeneric("spnet.barplot.list", function(object){ standardGeneric("spnet.barplot.list") })

#' @describeIn spnet.barplot.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.barplot"))
  }
)

#' Set the list of all barplot parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace barplot parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.barplot.list<-", function(object, value){ standardGeneric("spnet.barplot.list<-") })

#' @describeIn spnet.barplot.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.barplot <- value
    validObject(object)
    return(object)
  }
)

















#' Get the barplot variable of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the barplot variable of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.barplot.variable", function(object){ standardGeneric("spnet.barplot.variable") })

#' @describeIn spnet.barplot.variable method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.variable",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.barplot")$variable)
  }
)

#' Set the barplot variable  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the barplot variable of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the name of the variable to use for plotting barplots.
#' @export
setGeneric("spnet.barplot.variable<-", function(object, value){ standardGeneric("spnet.barplot.variable<-") })

#' @describeIn spnet.barplot.variable method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.variable<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.barplot$variable <- value
    validObject(object)
    return(object)
  }
)
















#' Get the barplot foreground color of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the barplot foreground color of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.barplot.fgcolor", function(object){ standardGeneric("spnet.barplot.fgcolor") })

#' @describeIn spnet.barplot.fgcolor method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.fgcolor",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.barplot")$fgcolor)
  }
)


#' Set the barplot foreground color  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the barplot foreground color of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the color.
#' @export
setGeneric("spnet.barplot.fgcolor<-", function(object, value){ standardGeneric("spnet.barplot.fgcolor<-") })

#' @describeIn spnet.barplot.fgcolor method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.fgcolor<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.barplot$fgcolor <- value
    validObject(object)
    return(object)
  }
)


















#' Get the barplot background color of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the barplot background color of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.barplot.bgcolor", function(object){ standardGeneric("spnet.barplot.bgcolor") })

#' @describeIn spnet.barplot.bgcolor method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.bgcolor",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.barplot")$bgcolor)
  }
)

#' Set the barplot background color  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the barplot background color of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value the new color.
#' @export
setGeneric("spnet.barplot.bgcolor<-", function(object, value){ standardGeneric("spnet.barplot.bgcolor<-") })

#' @describeIn spnet.barplot.bgcolor method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.bgcolor<-" ,
  signature = c("SpatialNetwork", 'character'),
  definition = function(object, value){
    object@plot.barplot$bgcolor <- value
    validObject(object)
    return(object)
  }
)



















#' Get the barplot lower bound position of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the barplot lower bound position of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.barplot.bound.lower", function(object){ standardGeneric("spnet.barplot.bound.lower") })

#' @describeIn spnet.barplot.bound.lower method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.bound.lower",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.barplot")$bound.lower)
  }
)

#' Set the barplot lower bound position  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the barplot lower bound position of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value a numeric vector of coordinates, c(x,y), specifying a translation from the center of each country.
#' @export
setGeneric("spnet.barplot.bound.lower<-", function(object, value){ standardGeneric("spnet.barplot.bound.lower<-") })

#' @describeIn spnet.barplot.bound.lower method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.bound.lower<-" ,
  signature = c("SpatialNetwork", 'numeric'),
  definition = function(object, value){
    object@plot.barplot$bound.lower <- value
    validObject(object)
    return(object)
  }
)
















#' Get the barplot upper bound position of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the barplot upper bound position of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.barplot.bound.upper", function(object){ standardGeneric("spnet.barplot.bound.upper") })

#' @describeIn spnet.barplot.bound.upper method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.bound.upper",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.barplot")$bound.upper)
  }
)

#' Set the barplot upper bound position  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the barplot upper bound position of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value a numeric vector of coordinates, c(x,y), specifying a translation from the center of each country.
#' @export
setGeneric("spnet.barplot.bound.upper<-", function(object, value){ standardGeneric("spnet.barplot.bound.upper<-") })

#' @describeIn spnet.barplot.bound.upper method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.bound.upper<-" ,
  signature = c("SpatialNetwork", 'numeric'),
  definition = function(object, value){
    object@plot.barplot$bound.upper <- value
    validObject(object)
    return(object)
  }
)















#' Get the barplot width of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract the barplot width of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @export
setGeneric("spnet.barplot.width", function(object){ standardGeneric("spnet.barplot.width") })

#' @describeIn spnet.barplot.width method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.width",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.barplot")$width)
  }
)

#' Set the barplot width  of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace the barplot width of a \code{SpatialNetwork} object.
#' 
#' @param object a \code{SpatialNetwork} object.
#' @param value a numeric.
#' @export
setGeneric("spnet.barplot.width<-", function(object, value){ standardGeneric("spnet.barplot.width<-") })

#' @describeIn spnet.barplot.width method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.barplot.width<-" ,
  signature = c("SpatialNetwork", 'numeric'),
  definition = function(object, value){
    object@plot.barplot$width <- value
    validObject(object)
    return(object)
  }
)
















#' Get the list of all legend parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract legend parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @export

setGeneric("spnet.legend.list", function(object){ standardGeneric("spnet.legend.list") })

#' @describeIn spnet.legend.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.legend.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.legend"))
  }
)

#' Set the list of all legend parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace legend parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.legend.list<-", function(object, value){ standardGeneric("spnet.legend.list<-") })

#' @describeIn spnet.legend.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.legend.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.legend <- value
    validObject(object)
    return(object)
  }
)














#' Get the list of all layout parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract layout parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
setGeneric("spnet.layout.list", function(object){ standardGeneric("spnet.layout.list") })

#' @describeIn spnet.layout.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.layout.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.layout"))
  }
)

#' Set the list of all layout parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace layout parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.layout.list<-", function(object, value){ standardGeneric("spnet.layout.list<-") })

#' @describeIn spnet.layout.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.layout.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.layout <- value
    validObject(object)
    return(object)
  }
)

















#' Get the list of all par parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to extract par parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to get parameters.
#' @export
setGeneric("spnet.par.list", function(object){ standardGeneric("spnet.par.list") })

#' @describeIn spnet.par.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.par.list",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.par"))
  }
)

#' Set the list of all par parameters of a \code{SpatialNetwork} object
#' 
#' This generic method intends to add or replace par parameters of a \code{SpatialNetwork} object.
#' 
#' @param object the \code{SpatialNetwork} object for which we want to set parameters.
#' @param value a list of parameters.
#' @export
setGeneric("spnet.par.list<-", function(object, value){ standardGeneric("spnet.par.list<-") })

#' @describeIn spnet.par.list method for \code{SpatialNetwork} objects.
setMethod(
  f = "spnet.par.list<-" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.par <- value
    validObject(object)
    return(object)
  }
)









#' Create a \code{SpatialNetwork} object
#' 
#' The \code{spnet.create} function is the official builder for creating \code{SpatialNetwork} objects.
#' 
#' @author Emmanuel Rousseaux
#' 
#' @param x AAA
#' @param map AAA
#' @param networks AAA
#' @param plot.title AAA
#' @param plot.label list of arguments to be passed to the \code{\link{text}} function.
#' @param plot.color AAA
#' @param plot.symbol AAA
#' @param plot.barplot AAA
#' @param plot.arrow AAA
#' @param plot.legend AAA
#' @param plot.layout AAA
#' @param plot.par AAA
#' @param infos AAA
#' @param quiet = FALSE AAA
#' @export
#' @examples
#' people <- c("John", "Elsa", "Brian", "Kate")
#' position <- c(2,4,6,8)
#' 
#' net1.df <- data.frame(
#'   'NODE' = people,
#'   'POSITION' = position
#' )
#' 
#' net1 <- spnet.create(
#'   x = net1.df
#' )
#' net1
#' 
#' net2 <- spnet.create(
#'   x = people
#' )
#' net2
spnet.create <- function(
  x,
  map,
  networks,
  plot.title = list(main = "Untitled SPNET object", sub = "", cex = 2, col = "#333333"),
  plot.label = list(cex = 1, col = '#333333'),
  plot.color,
  plot.symbol,
  plot.barplot = list(variable = "", bound.lower = c(-0.5,-0.5), bound.upper = c(0.5,-0.5), fgcolor = "#666666", bgcolor = "#eeeeee", width = 8),
  plot.arrow,
  plot.legend = list(print = TRUE, cex = 1, ncol = 1, horiz = FALSE),
  plot.layout = list(ratios = c('title' = 1/10, 'graphic' = 7/10, 'legend' = 2/10), mat = NULL, reset = TRUE),
  plot.par = list(mar = c(1,1,1,1)), # par(mar = c(5,4,2,2))
  infos,
  quiet = FALSE
) {
  
  if(inherits(x, 'data.frame')) {
    df <- x
  } else if(length(x) > 0) {
    df <- data.frame('NODE' = x)
  } else stop("Invalid 'x' argument")
  
  out <- new(
    Class = 'SpatialNetwork',
    .Data = df,
    row.names = 1:nrow(df),
    plot.title =plot.title,
    plot.label = plot.label,
    plot.barplot = plot.barplot,
    plot.legend = plot.legend,
    plot.layout = plot.layout,
    plot.par = plot.par
  )
  
  if(!missing(networks)) {
    spnet.networks.list(out) <- networks
  }
  
  return(out)
}


setMethod(
  f = 'show',
  signature = 'SpatialNetwork',
  definition = function(object) {
    cat("This is a valid 'SpatialNetwork' object.\n\n")
    
    cat("- Data: (first rows) \n\n")
    print.data.frame(head(object))
    cat("\n")
    
    if(length(spnet.map(object)) > 0) {
      cat("- Map:\n")
      cat("    Length:", length(spnet.map(object)), "\n\n")
    }
    
    if(length(object@networks) > 0) {
      cat("- Network data:\n")
      #       for (net in nets) {
      cat("    ", "Number of network(s): ", length(object@networks), sep = "")
      #       }
      cat("\n\n")
    }
    
    color <- object@plot.color
    symbol <- object@plot.symbol
    barplot <- object@plot.barplot
    
    if('variable' %in% c(names(color), names(symbol), names(barplot))) {
      cat("- Plotting options:\n")
      if('variable' %in% names(color))
        cat("    ", "Variable used to colorize: '", color$variable, "'\n", sep = "")
      if('variable' %in% names(symbol))
        cat("    ", "Variable used to draw symbols: '", symbol$variable, "'\n", sep = "")
      if(nzchar(barplot$variable))
        cat("    ", "Variable used to draw barplots: '", barplot$variable, "'\n", sep = "")
      cat("\n")
    }
  }
)

setMethod(
  f = 'print',
  signature = 'SpatialNetwork',
  definition = function(x, ...) {
    show(x)
  }
)

setMethod(
  f = 'plot',
  signature = 'SpatialNetwork',
  definition = function(x, ...) {
    if(length(spnet.map(x)) == 0)
      stop("The map is empty. Please define a valid map.")
    
    tit <- x@plot.title
    
    color <- x@plot.color
    flag.color <- ifelse(length(color) > 0, T, F)
    
    symbol <- x@plot.symbol
    flag.symbol <- ifelse(length(symbol) > 0, T, F)
    
    barplot <- x@plot.barplot
    flag.barplot <- ifelse(nzchar(barplot$variable), T, F)
    
    nets <- x@networks
    if(length(nets) > 0) {flag.arrow <- T} else {flag.arrow <- F}
    
    lay <- x@plot.layout
    
    arg.col <- numeric()
    
    ## PLOT
    def.par <- par(no.readonly = TRUE) # save default, for resetting...
    #     nf <- layout(
    #       mat = matrix(c(1,2),nrow=2,byrow = TRUE),
    #       widths = c(6.8),
    #       heights = c(4,1),
    #       respect = TRUE
    #     )
    
    if(!is.null(lay$mat)) {
      #       nf <- layout(
      #         mat = matrix(c(1,3,2,2),nrow=2,byrow = TRUE),
      #         widths = c(3,3),
      #         heights = c(4,1), 
      #         respect = TRUE
      #       )
    } else {
      nf <- layout(
        mat = matrix(c(1,2,3),nrow=3,byrow = TRUE),
        widths = c(1),
        heights = lay$ratios, 
        respect = TRUE
      )
    }
    #     layout.show(nf)
    par(spnet.par.list(x))
    
    plot.new()
    # plot the title
    if(any(c('main', 'sub') %in% names(tit))) {
      tit.main.clean = tit[-which(names(tit) %in% c('main', 'sub'))]
    } else {
      tit.main.clean = tit
    }
    main.call = as.call(c(
      list(
        fun=text,
        x=0.5,
        y=0.65,
        labels = tit$main
      ),
      tit.main.clean
    ))
    eval(main.call)
    if(any(c('main', 'sub', 'cex') %in% names(tit))) {
      tit.sub.clean = tit[-which(names(tit) %in% c('main', 'sub', 'cex'))]
    } else {
      tit.sub.clean = tit
    }
    sub.call = as.call(c(
      list(
        fun=text,
        x=0.5,
        y=0.20,
        labels = tit$sub,
        cex = tit$cex / 1.5
      ),
      tit.sub.clean
    ))
    eval(sub.call)
    
    if(!'POSITION' %in% names(x)) { # we only plot the map
      plot(spnet.map(x), ... = ...)
      plot.new()
    } else { # we plot position referenced
      
      coord <- coordinates(x@map)
      ids <- row.names(coord)
      seats <- x[, 'POSITION']
      names(seats) <- rep(x@meta$plot.color.default, length(seats))
      #       print(seats)
      
      if(flag.color) {
        names(seats) <- x[, color$variable] # we store color labels in the names
        names(seats)[!names(seats) %in% names(color$legend)] <- x@meta$plot.color.default
        for(k in names(color$legend)){
          names(seats)[names(seats) == k] <- color$legend[[k]]
        }
      }
      
      col <- rep(par("bg"), nrow(coord))
      #       print(col)
      col[seats] <- names(seats)
      #       print(col[seats])
      arg.col <- col[as.numeric(ids)]
      #       print(arg.col)
      
      
      plot(
        x@map,
        col = arg.col,
        ... = ...
      )
      
      # Then we also plot the NODE names to corresponding positions
      coord <- coordinates(x@map)
      ids <- row.names(coord)
      seats <- x[, 'POSITION']
      seats.which <- match(seats, ids)
      lab <- rep("", nrow(coord))
      
      lab.opt <- spnet.label.list(x)
      if(any(c('variable') %in% names(lab.opt))) {
        lab.opt.clean = lab.opt[-which(names(lab.opt) %in% c('variable'))]
        lab[seats.which] <- as.character(x[, lab.opt$variable])
      } else {
        lab.opt.clean = lab.opt
        lab[seats.which] <- as.character(x[, 'NODE'])
      }
      
      label.call = as.call(c(
        list(
          fun=text,
          x=coord,
          labels = lab
        ),
        lab.opt.clean
      ))
      eval(label.call)
      
      
      
      if(flag.symbol) {
        coord <- coordinates(x@map)
        ids <- row.names(coord)
        seats <- x[, 'POSITION']
        symb.coord <- coord[match(seats, as.numeric(ids)),] # symb.coord give the coordinates of each existing seat
        names(seats) <- x[,symbol$variable] # we store the data.frame column to match to symbols in seat names
        
        seats <- .expand.multiple.names(seats)
        
        role.match <- names(seats) %in% names(symbol$legend)
        seats <- seats[role.match] # then if no role match we remove
        
        #symb.coord <- symb.coord[seats,]
        
        if('cex' %in% names(symbol)) {
          symb.cex <- symbol$cex
        } else {
          symb.cex <- x@meta$plot.symbol.default$cex
        }
        if('space' %in% names(symbol)) {
          symb.space <- symbol$space
        } else {
          symb.space <- x@meta$plot.symbol.default$space
        }
        
        symb.coord.multiple.flag <- TRUE
        for(l in unique(seats)){
          nb <- sum(seats == l)
          val <- .position.multiple.symbols(symb.coord[match(l, rownames(symb.coord)),], n = nb, cex = symb.cex, space = symb.space)
          dimnames(val) <- list(rep(l, nb), NULL) # rownames
          if(symb.coord.multiple.flag) {
            symb.coord.multiple <- val
            symb.coord.multiple.flag <- FALSE
          }
          else {
            symb.coord.multiple <- rbind(symb.coord.multiple, val)
          }
        }      
        
        for(k in names(symbol$legend)){ # we replace by the symbol code
          role.match2 <- names(seats) == k # we select elements which match
          if(any(role.match2)){
            names(seats)[role.match2] <- symbol$legend[[k]] # and we replace
          }
        }
        #         print(seats)
        arg.pch <- names(seats)
        allsymb <- .spnet.symbol.list
        arg.pch <- allsymb[match(arg.pch, names(allsymb))]
        
        if('color' %in% names(symbol)) {
          symb.color <- symbol$color
        } else {
          symb.color <- x@meta$plot.symbol.default$color
        }
        
        if('translate.x' %in% names(symbol)) {
          symb.translate.x <- symbol$translate.x
        } else {
          symb.translate.x <- x@meta$plot.symbol.default$translate.x
        }
        if('translate.y' %in% names(symbol)) {
          symb.translate.y <- symbol$translate.y
        } else {
          symb.translate.y <- x@meta$plot.symbol.default$translate.y
        }
        
        symb.coord.multiple[,1] <- symb.coord.multiple[,1] + symb.translate.x
        symb.coord.multiple[,2] <- symb.coord.multiple[,2] + symb.translate.y
        
        points(
          symb.coord.multiple,
          pch = arg.pch,
          cex = symb.cex,
          col = symb.color
        )
      }
      
      if(flag.barplot) {
        coord <- coordinates(x@map)
        ids <- row.names(coord)
        seats <- x[, 'POSITION']
        seats.which <- match(seats, ids)
        
        values <- x[, spnet.barplot.variable(x)]
        
        for(i in 1:length(values)) {
          value <- values[i]
          if(!is.na(value)) {
            lines.barplot(
              value = value,
              bound.lower = coord[seats.which[i],] + spnet.barplot.bound.lower(x),
              bound.upper = coord[seats.which[i],] + spnet.barplot.bound.upper(x),
              bgcolor = spnet.barplot.bgcolor(x),
              fgcolor = spnet.barplot.fgcolor(x),
              lwd = spnet.barplot.width(x)
            )
          }
        }
        
      }
      
      if(flag.arrow) {
        arrow.col.list <- c()
        arrow.translate.x.list <- c()
        arrow.translate.y.list <- c()
        arrow.label.list <- names(nets)
        
        coord <- coordinates(x@map)
        ids <- row.names(coord)
        seats <- x[, 'POSITION']
        seats.which <- match(seats, ids)
        names(seats.which) <- x[, 'NODE']
        
        default.color = x@meta$plot.arrow.default$color
        default.translate.x = x@meta$plot.arrow.default$translate.x
        default.translate.y = x@meta$plot.arrow.default$translate.y
        default.opacity = x@meta$plot.arrow.default$opacity
        default.thickness = x@meta$plot.arrow.default$thickness
        default.length.rate = x@meta$plot.arrow.default$length.rate
        default.shortening = x@meta$plot.arrow.default$shortening
        default.head.length = x@meta$plot.arrow.default$head.length
        default.head.type = x@meta$plot.arrow.default$head.type
        
        for (k in 1:length(nets)) {
          net.list <- nets[[k]]
          net <- net.list$data
          
          if('opacity' %in% names(net.list)) {
            arrow.opacity <- net.list$opacity
          } else {
            arrow.opacity <- default.opacity
          }
          
          if('color' %in% names(net.list)) {
            arrow.col <- net.list$color
          } else {
            arrow.col <- x@meta$plot.arrow.default$color[k]
          }
          arrow.col <- rgb(t(col2rgb(arrow.col)), alpha = round(arrow.opacity*255), maxColorValue = 255)
          arrow.col.list <- c(arrow.col.list, arrow.col)
          
          if('translate.x' %in% names(net.list)) {
            arrow.translate.x <- net.list$translate.x
          } else {
            arrow.translate.x <- x@meta$plot.arrow.default$translate.x[k]
          }
          arrow.translate.x.list <- c(arrow.translate.x.list, arrow.translate.x)
          
          if('translate.y' %in% names(net.list)) {
            arrow.translate.y <- net.list$translate.y
          } else {
            arrow.translate.y <- x@meta$plot.arrow.default$translate.y[k]
          }
          arrow.translate.y.list <- c(arrow.translate.x.list, arrow.translate.y)
          
          if('length.rate' %in% names(net.list)) {
            arrow.length.rate <- net.list$length.rate
          } else {
            arrow.length.rate <- default.length.rate
          }
          if('shortening' %in% names(net.list)) {
            arrow.shortening <- net.list$shortening
          } else {
            arrow.shortening <- default.shortening
          }
          
          if('label' %in% names(net.list)) {
            arrow.label.list[k] <- net.list$label
          }
          
          if('head.length' %in% names(net.list)) {
            arrow.head.length <- net.list$head.length
          } else {
            arrow.head.length <- default.head.length
          }
          
          if('head.type' %in% names(net.list)) {
            arrow.head.type <- net.list$head.type
          } else {
            arrow.head.type <- default.head.type
          }
          
          if('thickness' %in% names(net.list)) {
            arrow.thickness <- net.list$thickness
          } else {
            arrow.thickness <- default.thickness
          }
          
          for (i in dimnames(net)[[1]]){
            for(j in dimnames(net)[[2]]) {
              if (i != j){
                if (net[i,j] > 0) {
                  arrow.start <- coord[seats.which[i],]
                  arrow.stop <- coord[seats.which[j],]
                  arrow.coords <- .arrow.resize(
                    arrow.start[1],
                    arrow.start[2],
                    arrow.stop[1],
                    arrow.stop[2],
                    size = arrow.length.rate
                  )
                  arrow.coords <- .arrow.cut(
                    x0 = arrow.coords['x0'],
                    y0 = arrow.coords['y0'],
                    x1 = arrow.coords['x1'],
                    y1 = arrow.coords['y1'],
                    cut = arrow.shortening
                  )
                  #                   print(arrow.coords)
                  Arrows(
                    x0 = arrow.coords['x0'] + arrow.translate.x,
                    y0 = arrow.coords['y0'] + arrow.translate.y,
                    x1 = arrow.coords['x1'] + arrow.translate.x,
                    y1 = arrow.coords['y1'] + arrow.translate.y,
                    col=arrow.col,
                    arr.col=arrow.col,
                    arr.length=arrow.head.length,
                    lwd=net[i,j] * arrow.thickness,
                    arr.type = arrow.head.type
                  )
                }
              }
            }
          }
        }
      }
      
      
      
      
      ## LEGEND
      leg.pring = spnet.legend.list(x)$print
      leg.cex = spnet.legend.list(x)$cex
      leg.ncol = spnet.legend.list(x)$ncol
      leg.horiz = spnet.legend.list(x)$horiz
      
      par(spnet.par.list(x))
      plot.new()
      if(leg.pring) {
        if(flag.color) {
          legend(
            x = "topleft",
            legend = names(x@plot.color$legend),
            fill = x@plot.color$legend,
            bty = 'n',
            cex = leg.cex,
            ncol = leg.ncol,
            horiz = leg.horiz
          )
        }
        if(flag.symbol) {
          legend(
            x = "top",
            legend = names(x@plot.symbol$legend),
            pch = .spnet.symbol.list[match(x@plot.symbol$legend, names(.spnet.symbol.list))],
            bty = 'n',
            cex = leg.cex
          )
        }
        if(flag.arrow) {
          legend(
            x = "topright",
            legend = arrow.label.list,
            col = arrow.col.list,
            lty = 1,
            bty = 'n',
            cex = leg.cex
          )
        }
        #     if(flag.arrow){
        #       dn <- x@plot.arrow$legend
        #       if(nchar(dn) > 0) {
        #         xx <- 0.8
        #         yy <- 0.66
        #         tt <- 0.04
        #         arrows(xx, yy, xx + tt, yy, col=arrow.col, length=arrow.length.rate)
        #         
        #         text(x= xx+tt+0.11, y = yy, labels = dn, ...=...)
        #       }
        #     }
      }
    }
    
    reset = TRUE
    if(is.element('reset',names(lay)))
      if(lay$reset == FALSE)
        reset = FALSE
    if(reset)
      par(def.par)  #- reset to default
  }
)


# ---------------------------------------------------------------
# Symbol list
# ---------------------------------------------------------------
.spnet.symbol.list <- c(
  "circle" = 1,
  "triangle.up" = 2,
  "triangle.down" = 6,
  "square" = 22,
  "square.rotated" = 5,
  "square.triangle.down" = 14,
  "cross" = 3,
  "times" = 4  
)
plot.symbol.list <- function(){
  l <- .spnet.symbol.list
  coord <- 2:(length(l)+1)
  plot(
    coord,
    pch = l,
    ylim = c(1, length(l)+1),
    xlim = c(0, length(l)+1),
    xaxt = 'n',
    xlab = '',
    yaxt = 'n',
    ylab = '',
    bty = "n"
  )
  text(
    coord - 0.5,
    names(l)
  )
}
# plot.symbol.list()
.plot.symbol.list.all <- function(){
  n.symbols <- 25
  plot(
    1:n.symbols,
    pch = 1:n.symbols
  )
}
# .plot.symbol.list.all()
.arrow.resize <- function(x0, y0, x1, y1, size = 0.9) {
  stopifnot(size > 0 && size <= 1)
  
  segment <- c(0,1)
  breaks.size <- (1-size)/2
  breaks <- c(segment[1] + breaks.size, segment[2] - breaks.size)
  
  new.x0 <- (1-breaks[1]) * x0 + breaks[1] * x1
  new.y0 <- (1-breaks[1]) * y0 + breaks[1] * y1
  new.x1 <- (1-breaks[2]) * x0 + breaks[2] * x1
  new.y1 <- (1-breaks[2]) * y0 + breaks[2] * y1
  
  out <- c(new.x0, new.y0, new.x1, new.y1)
  names(out) <- c('x0', 'y0', 'x1', 'y1')
  return(out)
}
# .arrow.resize(0,0,1,1)
# .arrow.resize(0,0,1,1)['x0']
.arrow.cut <- function(x0, y0, x1, y1, cut = 0.2) {
  
  arrow.norm <- sqrt((x1-x0)^2 + (y1-y0)^2)
  unitaire.x <- (x1-x0)/arrow.norm
  unitaire.y <- (y1-y0)/arrow.norm
  
  new.x0 <- x0 + unitaire.x * cut
  new.y0 <- y0 + unitaire.y * cut
  new.x1 <- x1 - unitaire.x * cut
  new.y1 <- y1 - unitaire.y * cut
  
  out <- c(new.x0, new.y0, new.x1, new.y1)
  names(out) <- c('x0', 'y0', 'x1', 'y1')
  return(out)
}
# .arrow.cut('x0' = 0,'y0' = 0,'x1' = 1,'y1' = 1)
# .arrow.cut(0,0,2,2)
# .arrow.cut(1,1,2,2)
# .arrow.resize(0,0,1,1)['x0']


# .base1to256 <- function(x) {
#   return(round(x*255))
# }
# 
# .base256tohex <- function(x) {
#   sprintf("%X", x) 
# }
# .base256tohex(0)
# .base256tohex(10)
# .base256tohex(255)
# .base1tohex <- function(x) {
#   return(.base256tohex(.base1to256(x)))
# }
# .addzero <- function(x) {
#   if(nchar(x) == 1)
#     x <- paste('0', x, sep = '')
#   
#   return(x)
#   }
# }
# .addzero(.base1tohex(0))
# .addzero(.base1tohex(0.5))
# .addzero(.base1tohex(1))