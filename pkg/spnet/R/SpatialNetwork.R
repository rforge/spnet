#' Class \code{"SpatialNetwork"}
#' 
#' Allow to store spatial networks, especially for rendering them
#' 
#' @rdname SpatialNetwork-class
#' @name SpatialNetwork
#' @aliases SpatialNetwork-class
#' @docType class
#' @author Emmanuel Rousseaux
#' @keywords classes spatial network sp
#' @family spnet-class
#' @section Objects from the Class:
#' Objects can be created with the \code{\link{spnet}} function (official class builder).
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{sp}:}{Object of class \code{"SpatialPolygons"} ~~ }
#'    \item{\code{networks}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{plot.title}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{plot.label}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{plot.color}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{plot.symbol}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{plot.arrow}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{plot.legend}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{plot.layout}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{infos}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{meta}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{warnings}:}{Object of class \code{"list"} ~~ }
#'    \item{\code{names}:}{Object of class \code{"character"} ~~ }
#'    \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} ~~ }
#'    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
#' }
#' 
#' @section Extends:
#' Class \code{"\linkS4class{data.frame}"}, directly.
#' Class \code{"\linkS4class{list}"}, by class "data.frame", distance 2.
#' Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
#' Class \code{"\linkS4class{vector}"}, by class "data.frame", distance 3.
#' 
#' @section Methods:
#'   \describe{
#'     \item{plot}{\code{signature(x = "SpatialNetwork", y = "ANY")}: ... }
#'     \item{plot.position}{\code{signature(x = "SpatialNetwork")}: ... }
#'     \item{print}{\code{signature(x = "SpatialNetwork")}: ... }
#'     \item{show}{\code{signature(object = "SpatialNetwork")}: ... }
#'   }
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
        length.fixed.cut = 0.3,
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
    color <- object@plot.color
    if(flag && (length(color) > 0)){
      if(!all(names(color) %in% c('variable', 'legend'))) stop("Elements in 'plot.color' have to be named by one of the following names: 'variable', 'legend'.")
      if(!'variable' %in% names(color)) stop("The 'plot.color' list should contain a 'variable' element")
      if(!'legend' %in% names(color)) stop("The 'plot.color' list should contain a 'legend' element")
      if(!color$variable %in% names(object)) stop("The 'variable' element of 'plot.color' doesn't exist in data.")
      exist.in.data <- names(color$legend) %in% unique(object[,color$variable])
      if(!all(exist.in.data)) {
        stop(paste(
          "Some values in the 'legend' referenced in 'plot.color' doesn't exist in the variable ",
          color$variable,
          ': ',
          paste(names(color$legend)[which(!exist.in.data)], collapse = ', '),
          sep = ''
          )
        )
      }
    }
    
    # symbol
    symbol <- object@plot.symbol
    if(flag && (length(symbol) > 0)){
      if(!all(names(symbol) %in% c('variable', 'legend', 'color', 'cex', 'space', 'translate.x', 'translate.y'))) stop("Elements in 'plot.symbol' have to be named by one of the following names: 'variable', 'legend', 'color', 'cex', 'space', 'translate.x', 'translate.y'.")
      if(!'variable' %in% names(symbol)) stop("The 'plot.symbol' list should contain a 'variable' element")
      if(!'legend' %in% names(symbol)) stop("The 'plot.symbol' list should contain a 'legend' element")
      if(!symbol$variable %in% names(object)) stop("The 'variable' element of 'plot.symbol' doesn't exist in data.")
      values.of.symbol.column <- unique(.extract.multiple.strings(object[,symbol$variable]))
      exist.in.data <- names(symbol$legend) %in% values.of.symbol.column
      if(!all(exist.in.data)) {
        stop(paste(
          "Some values in the 'legend' referenced in 'plot.symbol' doesn't exist in the variable ",
          symbol$variable,
          ': ',
          paste(names(symbol$legend)[which(!exist.in.data)], collapse = ', '),
          sep = ''
          )
        )
      }
      exist.in.symbol <- symbol$legend %in% names(spnet:::.spnet.symbol.list)
      if(!all(exist.in.symbol)) stop(
        paste(
          "Some symbol names you provided doesn't exist:",
          paste(symbol$legend[which(!exist.in.symbol)])
        ))
    }
    
    # networks
    nets <- object@networks
    if(flag && (length(nets) > 0)){
      
      if(!(length(nets) <= object@meta$plot.arrow.default$max.networks)) {
        stop(paste("The number of networks is limited to", object@meta$plot.arrow.default$max.networks))
      }
      
      for (net in nets) {
        if(!'matrix' %in% names(net)) {
          stop("One of the network doesn't contain a 'matrix' element")
        }
        net.matrix <- net$matrix
        if(flag && (nrow(net.matrix)>0 || ncol(net.matrix)>0)) {
          if(nrow(net.matrix) != ncol(net.matrix)) {
            message("Each network of the 'networks' attribute has to be a squared matrix")
            message("Here ncol=", ncol(net.matrix), "and nrow=", nrow(net.matrix))
            stop("Invalid 'networks' matrix dimensions")
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


setGeneric("spnet.map", function(object){ standardGeneric("spnet.map") })
setMethod(
  f = "spnet.map",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "map"))
  }
)
setGeneric("spnet.map<-", function(object, value){ standardGeneric("spnet.map<-") })
setReplaceMethod(
  f = "spnet.map" ,
  signature = c("SpatialNetwork", 'SpatialPolygons'),
  definition = function(object, value){
    object@map <- value
    validObject(object)
    return(object)
  }
)

setGeneric("spnet.networks", function(object){ standardGeneric("spnet.networks") })
setMethod(
  f = "spnet.networks",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "networks"))
  }
)
setGeneric("spnet.networks<-", function(object, value){ standardGeneric("spnet.networks<-") })
setReplaceMethod(
  f = "spnet.networks" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@networks <- value
    validObject(object)
    return(object)
  }
)
setReplaceMethod(
  f = "spnet.networks" ,
  signature = c("SpatialNetwork", 'matrix'),
  definition = function(object, value){
    object@networks <- list(value)
    validObject(object)
    return(object)
  }
)

setGeneric("spnet.title", function(object){ standardGeneric("spnet.title") })
setMethod(
  f = "spnet.title",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.title"))
  }
)
setGeneric("spnet.title<-", function(object, value){ standardGeneric("spnet.title<-") })
setReplaceMethod(
  f = "spnet.title" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.title <- value
    validObject(object)
    return(object)
  }
)
setGeneric("spnet.label", function(object){ standardGeneric("spnet.label") })
setMethod(
  f = "spnet.label",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.label"))
  }
)
setGeneric("spnet.label<-", function(object, value){ standardGeneric("spnet.label<-") })
setReplaceMethod(
  f = "spnet.label" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.label <- value
    validObject(object)
    return(object)
  }
)
setGeneric("spnet.color", function(object){ standardGeneric("spnet.color") })
setMethod(
  f = "spnet.color",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.color"))
  }
)
setGeneric("spnet.color<-", function(object, value){ standardGeneric("spnet.color<-") })
setReplaceMethod(
  f = "spnet.color" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.color <- value
    validObject(object)
    return(object)
  }
)

setGeneric("spnet.symbol", function(object){ standardGeneric("spnet.symbol") })
setMethod(
  f = "spnet.symbol",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.symbol"))
  }
)
setGeneric("spnet.symbol<-", function(object, value){ standardGeneric("spnet.symbol<-") })
setReplaceMethod(
  f = "spnet.symbol" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.symbol <- value
    validObject(object)
    return(object)
  }
)
setGeneric("spnet.legend", function(object){ standardGeneric("spnet.legend") })
setMethod(
  f = "spnet.legend",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.legend"))
  }
)
setGeneric("spnet.legend<-", function(object, value){ standardGeneric("spnet.legend<-") })
setReplaceMethod(
  f = "spnet.legend" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.legend <- value
    validObject(object)
    return(object)
  }
)
setGeneric("spnet.layout", function(object){ standardGeneric("spnet.layout") })
setMethod(
  f = "spnet.layout",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.layout"))
  }
)
setGeneric("spnet.layout<-", function(object, value){ standardGeneric("spnet.layout<-") })
setReplaceMethod(
  f = "spnet.layout" ,
  signature = c("SpatialNetwork", 'list'),
  definition = function(object, value){
    object@plot.layout <- value
    validObject(object)
    return(object)
  }
)
setGeneric("spnet.par", function(object){ standardGeneric("spnet.par") })
setMethod(
  f = "spnet.par",
  signature = "SpatialNetwork", 
  definition = function (object) { 
    return(slot(object, "plot.par"))
  }
)
setGeneric("spnet.par<-", function(object, value){ standardGeneric("spnet.par<-") })
setReplaceMethod(
  f = "spnet.par" ,
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
#' @param plot.arrow AAA
#' @param plot.legend AAA
#' @param plot.layout AAA
#' @param plot.par AAA
#' @param infos AAA
#' @param quiet = FALSE AAA
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
  plot.arrow,
  plot.legend = list(print = TRUE, cex = 1),
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
    plot.legend = plot.legend,
    plot.layout = plot.layout,
    plot.par = plot.par
  )
  
  if(!missing(networks)) {
    spnet.networks(out) <- networks
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
    
    if('variable' %in% c(names(color), names(symbol))) {
      cat("- Plotting options:\n")
      if('variable' %in% names(color))
        cat("    ", "Variable used to colorize: '", color$variable, "'\n", sep = "")
      if('variable' %in% names(symbol))
        cat("    ", "Variable used to draw symbols: '", symbol$variable, "'\n", sep = "")
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
    if(length(color) > 0) {flag.color <- T} else {flag.color <- F}
        
    symbol <- x@plot.symbol
    if(length(symbol) > 0) {flag.symbol <- T} else {flag.symbol <- F}
    
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
    par(spnet.par(x))
    
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

      lab.opt <- spnet.label(x)
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
        allsymb <- spnet:::.spnet.symbol.list
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
        default.length.fixed.cut = x@meta$plot.arrow.default$length.fixed.cut
        default.head.length = x@meta$plot.arrow.default$head.length
        default.head.type = x@meta$plot.arrow.default$head.type
        
        for (k in 1:length(nets)) {
          net.list <- nets[[k]]
          net <- net.list$matrix
          
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
          if('length.fixed.cut' %in% names(net.list)) {
            arrow.length.fixed.cut <- net.list$length.fixed.cut
          } else {
            arrow.length.fixed.cut <- default.length.fixed.cut
          }
          
          if('name' %in% names(net.list)) {
            arrow.label.list[k] <- net.list$name
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
                    cut = arrow.length.fixed.cut
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
      leg.pring = spnet.legend(x)$print
      leg.cex = spnet.legend(x)$cex

      par(spnet.par(x))
      plot.new()
      if(leg.pring) {
        if(flag.color) {
          legend(
            x = "topleft",
            legend = names(x@plot.color$legend),
            fill = x@plot.color$legend,
            bty = 'n',
            cex = leg.cex
          )
        }
        if(flag.symbol) {
          legend(
            x = "top",
            legend = names(x@plot.symbol$legend),
            pch = spnet:::.spnet.symbol.list[match(x@plot.symbol$legend, names(.spnet.symbol.list))],
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