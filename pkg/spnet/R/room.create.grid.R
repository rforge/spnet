room.create.grid <- function(
  x,
  seat.width = 1,
  seat.height = 1
) {
  require(sp)
  stopifnot(inherits(x, 'matrix'))
  stopifnot(inherits(seat.width, 'numeric'))
  stopifnot(inherits(seat.height, 'numeric'))
#   u <- unique(as.character(x))
  u <- unique(x)
  u.positive <- u[ u>=0 ]
  
  flag0 <- FALSE # TRUE iif all positive values are 0 or '0'
  if(all(u.positive == 0)) {
    flag0 <- TRUE
  }
  
  col.n <- ncol(x)
  row.n <- nrow(x)
  seat.n <- sum(x != -1)
  
  # dimnames management
  x.row <- rep('E', row.n)
  x.col <- rep('E', col.n)
  dn <- dimnames(x)
  if(!is.null(dn)) {
    if(!is.null(dn[[1]]))
      x.row <- dn[[1]]
    if(!is.null(dn[[2]]))
      x.col <- dn[[2]]
  }
  stopifnot(all(unique(c(x.row, x.col) %in% c('E', 'O'))))
  
  count <- 0
  ID <- count
  
  rooms.poly <- vector(seat.n, mode = 'list')
  for (i in row.n:1) { # warning: we start to plot from the bottom
    for(j in 1:col.n) {
      if(x[i,j] != -1) { # '-1' == -1 => TRUE
        #         print(paste('i:',i, 'j', j, 'value:',x[i,j]))
        k <- row.n - i + 1
        count <- count + 1
        
        if(flag0) ID <- count
        else ID <- x[i,j]
        
        row.odd <- 0
        if(x.row[i] == 'O') row.odd <- seat.width/2
        
        col.odd <- 0
        if(x.col[j] == 'O') col.odd <- seat.height/2
        
        #         print(row.odd)
        #         print(col.odd)
        rooms.poly[[count]] <- Polygons(
          list(Polygon(cbind(
            c(j-1,j-1,j,j,j-1)*seat.width + rep(row.odd,5),
            c(k-1,k,k,k-1,k-1)*seat.height + rep(col.odd,5)))
          ),
          ID
        )
      }
    }
  }
  
  rooms <- SpatialPolygons(
    rooms.poly,
    1:length(rooms.poly)
  )
  
  return(rooms)
}


# col <- 5
# row <- 6
# m <- matrix(rep(-1, col*row), nrow = row)
# m[1,2:4] <- 0
# m[3,c(1,5)] <- 0
# m[4,c(1,5)] <- 0
# m[5,1] <- 0
# m[6,1] <- 0
# room1 <- room.create.grid(m, seat.width=2, seat.height=1)
# plot.placement(room1)

# 
# 
# col <- 5
# row <- 6
# m <- matrix(rep(-1, col*row), nrow = row)
# m[1,2:4] <- c('C01','C02','C03')
# m[3,c(1,5)] <- c('L01', 'R01')
# m[4,c(1,5)] <- c('L02', 'R02')
# m[5,1] <- c('L03')
# m[6,1] <-c('L04')
# room1 <- room.create.grid(m, seat.width=2, seat.height=1)
# plot(room1)
# text(coordinates(room1), labels=paste("", row.names(coordinates(room1)), sep=' '), cex=0.8)
# 
# 
# 
# col <- 5
# row <- 6
# m <- matrix(rep(-1, col*row), nrow = row)
# m[1,2:4] <- 0
# m[3,c(1,5)] <- 0
# m[4,c(1,5)] <- 0
# m[5,1] <- 0
# m[6,1] <- 0
# dimnames(m) <- list(
#   c('E','E','O','E','E','E'),
#   c('E','E','O','E','E')
# )
# room1 <- room.create.grid(m, seat.width=2, seat.height=1)
# plot(room1)
# text(coordinates(room1), labels=paste("Seat", row.names(coordinates(room1)), sep=' '), cex=0.8)
# 
# 
# col <- 5
# row <- 6
# m <- matrix(rep(-1, col*row), nrow = row)
# m[1,2:4] <- c('C01','C02','C03')
# m[3,c(1,5)] <- c('L01', 'R01')
# m[4,c(1,5)] <- c('L02', 'R02')
# m[5,1] <- c('L03')
# m[6,1] <-c('L04')
# dimnames(m) <- list(
#   c('E','E','O','E','E','E'),
#   c('E','E','O','E','E')
# )
# room1 <- room.create.grid(m, seat.width=2, seat.height=1)
# plot(room1)
# text(coordinates(room1), labels=paste("", row.names(coordinates(room1)), sep=' '), cex=0.8)

