setClass(
  Class = "SpatialNetwork",
  contains = 'data.frame',
  slots = c(
    #     'edges' = 'data.frame',
    'sp' = 'SpatialPolygons',
    'networks' = 'list',
    'plot.title' = 'list',
    'plot.color' = 'list',
    'plot.symbol' = 'list',
    'plot.arrow' = 'list',
    'infos' = 'list', # for the user
    'meta' = 'list', # for the dev
    'warnings' = 'list' # for the dev
  ),
  #   prototype,
  #   contains=character(),
  #   sealed = FALSE,
  validity = function(object) {
    flag <- TRUE
    
    # .Data: NODE
    if(flag && (!'NODE' %in% names(object))){
      stop("The NODE column doesn't exist")
    }
    
    # networks
    nets <- object@networks
    for (net in nets) {
      if(flag && (nrow(net)>0 || ncol(net)>0)) {
        if(nrow(net) != ncol(net)) {
          message("Each network of the 'networks' attribute has to be a squared matrix")
          message("Here ncol=", ncol(net), "and nrow=", nrow(net))
          stop("Invalid 'networks' matrix dimensions")
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



spnet.create <- function(
  x,
  sp,
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
  return(new(
    Class = 'SpatialNetwork',
    .Data = df,
    row.names = 1:nrow(df)
  ))
}

setMethod(
  f = 'show',
  signature = 'SpatialNetwork',
  definition = function(object) {
    cat("This is a valid 'SpatialNetwork' object.\n\n")
    cat("- Data:\n\n")
    print.data.frame(object)
    cat("\n")
    
    if(length(object@networks) > 0) {
      cat("- networks:\n\n")
#       for (net in nets) {
        print(object@networks)
#       }
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
    flag.color <- F
    flag.symbol <- F
    flag.arrow <- F
    arrow.col <- 'blue'
    arrow.length <- 0.10
    
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
    
    color <- x@plot.color
    if(length(color) > 0) flag.color <- T
    if(flag.color) {
      coord <- coordinates(x@sp)
      ids <- row.names(coord)
      seats <- x[, 'SEAT']
      #       seats.which <- match(seats, ids)
      #       names(seats.which) <- x[, color$variable]
      names(seats) <- x[, color$variable] # we store color labels in the names
      names(seats)[!names(seats) %in% names(color$legend)] <- 'grey'
      for(k in names(color$legend)){
        names(seats)[names(seats) == k] <- color$legend[[k]]
      }
      col <- rep(par("bg"), nrow(coord))
      col[seats] <- names(seats)
      arg.col <- col[as.numeric(ids)]
      
    }
    
    plot(
      x@sp,
      col = arg.col,
      ... = ...
    )
    
    
    if('SEAT' %in% names(x)) {
      coord <- coordinates(x@sp)
      ids <- row.names(coord)
      seats <- x[, 'SEAT']
      seats.which <- match(seats, ids)
      lab <- rep("", nrow(coord))
      lab[seats.which] <- as.character(x[, 'NAME'])
      text(coord, labels = lab)
    }
    
    net <- x@networks
    if(nrow(net) > 0) flag.arrow <- T
    if(flag.arrow) {
      coord <- coordinates(x@sp)
      ids <- row.names(coord)
      seats <- x[, 'SEAT']
      seats.which <- match(seats, ids)
      names(seats.which) <- x[, 'NAME']
      for (i in dimnames(net)[[1]]){
        for(j in dimnames(net)[[2]]) {
          if (i != j){
            if (net[i,j] > 0) {
              arrow.start <- coord[seats.which[i],]
              arrow.stop <- coord[seats.which[j],]
              arrows(
                x0 = arrow.start[1],
                y0 = arrow.start[2],
                x1 = arrow.stop[1],
                y1 = arrow.stop[2],
                col=arrow.col,
                length=arrow.length,
                lwd=net[i,j]
              )
            }
          }
        }
      }
    }
    
    symbol <- x@plot.symbol
    if(length(symbol) > 0) flag.symbol <- T
    if(flag.symbol) {
      coord <- coordinates(x@sp)
      ids <- row.names(coord)
      seats <- x[, 'SEAT']
      symb.coord <- coord[match(seats, as.numeric(ids)),]
      names(seats) <- x[,symbol$variable] # we store symbols in names
      
      role.match <- names(seats) %in% names(symbol$legend)
      seats <- seats[role.match] # if no role match we remove
      symb.coord <- symb.coord[role.match,]
      
      for(k in names(symbol$legend)){ # we replace by the symbol code
        role.match2 <- names(seats) == k
        if(any(role.match2)){
          names(seats)[role.match2] <- symbol$legend[[k]]
        }
      }
      arg.pch <- as.numeric(names(seats))
      
      points(
        symb.coord,
        pch = arg.pch,
        cex = 4,
        col = "black"
      )
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
        pch = x@plot.symbol$legend,
        bty = 'n'
      )
    }
    if(flag.arrow){
      dn <- x@plot.arrow$legend
      if(nchar(dn) > 0) {
        xx <- 0.8
        yy <- 0.66
        tt <- 0.04
        arrows(xx, yy, xx + tt, yy, col=arrow.col, length=arrow.length)
        
        text(x= xx+tt+0.11, y = yy, labels = dn, ...=...)
      }
      
    }
    
  }
)