setClass(
  Class = "SpatialNetwork",
  contains = 'data.frame',
  slots = c(
    #     'edges' = 'data.frame',
    'map' = 'SpatialPolygons',
    'networks' = 'list',
    'plot.title' = 'list',
    'plot.color' = 'list',
    'plot.symbol' = 'list',
    'plot.arrow' = 'list',
    'infos' = 'list', # for the user
    'meta' = 'list', # for the dev
    'warnings' = 'list' # for the dev
  ),
  prototype = prototype(
    meta = list(
      date.created = Sys.time(),
      layout = c(1/10, 7/10, 2/10),
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
        color = c('blue', 'red', 'green', 'pink', 'yellow'),
        opacity = 1,
        thickness = 1.00,
        length.rate = 1,
        length.fixed.cut = 0.2,
        head.length = 0.20,
        head.type = NULL
      )
    )
  ),
  #   contains=character(),
  #   sealed = FALSE,
  validity = function(object) {
    flag <- TRUE
    
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

spnet.create <- function(
  x,
  map,
  networks,
  plot.title,
  plot.color,
  plot.symbol,
  plot.arrow,
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
    row.names = 1:nrow(df)
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
    
    color <- x@plot.color
    if(length(color) > 0) {flag.color <- T} else {flag.color <- F}
        
    symbol <- x@plot.symbol
    if(length(symbol) > 0) {flag.symbol <- T} else {flag.symbol <- F}
    
    nets <- x@networks
    if(length(nets) > 0) {flag.arrow <- T} else {flag.arrow <- F}
    
    arg.col <- numeric()
    
    ## PLOT
    def.par <- par(no.readonly = TRUE) # save default, for resetting...
    nf <- layout(
      mat = matrix(c(1,2),nrow=2,byrow = TRUE),
      widths = c(6),
      heights = c(5,1),
      respect = TRUE
    )
    layout.show(nf)
    par(mar = c(5,4,2,2))
    par(mar = c(5,4,2,2))
    
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
      lab[seats.which] <- as.character(x[, 'NODE'])
      text(coord, labels = lab)
      
      
      
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
        arrow.label.list <- names(nets)
        
        coord <- coordinates(x@map)
        ids <- row.names(coord)
        seats <- x[, 'POSITION']
        seats.which <- match(seats, ids)
        names(seats.which) <- x[, 'NODE']
        
        default.color = x@meta$plot.arrow.default$color
        default.opacity = x@meta$plot.arrow.default$opacity
        default.thickness = x@meta$plot.arrow.default$thickness
        default.length.rate = x@meta$plot.arrow.default$length.rate
        default.length.fixed.cut = x@meta$plot.arrow.default$length.fixed.cut
        default.head.length = x@meta$plot.arrow.default$head.length
        default.head.type = x@meta$plot.arrow.default$head.type
        
        for (k in 1:length(nets)) {
          net.list <- nets[[k]]
          net <- net.list$matrix
          if('color' %in% names(net.list)) {
            arrow.col.list <- c(arrow.col.list, net.list$color)
          } else {
            arrow.col.list <- c(arrow.col.list, x@meta$plot.arrow.default$color[k])
          }
          arrow.col <- arrow.col.list[k]
          
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
          
          arrow.head.length <- x@meta$plot.arrow.default$head.length
          arrow.thickness <- x@meta$plot.arrow.default$thickness
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
                  print(arrow.coords)
                  arrows(
                    x0 = arrow.coords['x0'],
                    y0 = arrow.coords['y0'],
                    x1 = arrow.coords['x1'],
                    y1 = arrow.coords['y1'],
                    col=arrow.col,
                    length=arrow.head.length,
                    lwd=net[i,j] * arrow.thickness
                  )
                }
              }
            }
          }
        }
      }
      

      
      
      ## LEGEND
      par(mar = c(1,1,1,1))
      plot.new()
      if(flag.color) {
        legend(
          x = "topleft",
          legend = names(x@plot.color$legend),
          fill = x@plot.color$legend,
          bty = 'n'
        )
      }
      if(flag.symbol) {
        legend(
          x = "top",
          legend = names(x@plot.symbol$legend),
          pch = spnet:::.spnet.symbol.list[match(x@plot.symbol$legend, names(.spnet.symbol.list))],
          bty = 'n'
        )
      }
      if(flag.arrow) {
        legend(
          x = "topright",
          legend = arrow.label.list,
          col = arrow.col.list,
          lty = 1,
          bty = 'n'
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