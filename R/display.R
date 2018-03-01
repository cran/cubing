
###################################################

# plot.cube plot3D plot3D.cube animate

###################################################

plot.cube <- function(x, colvec = getOption("cubing.colors"), recolor = FALSE,   
                      xlab = "", ylab = "", main = "", centres = TRUE, numbers = FALSE, text.size = 1,
                      text.col = "black", rand.col = FALSE, blank = FALSE, ...) 
{
  spor <- 1:6
  aCube <- x
  if(rand.col) colvec <- rgb(runif(6), runif(6),runif(6))
  if(blank) colvec <- rep("ghostwhite", 6)
  if(is.cubieCube(aCube)) {
    if(is.null(aCube$spor)) stop("cubieCube has no spor coordinate")
    if(!recolor) spor <- aCube$spor 
    aCube <- as.stickerCube(aCube)
  } 
  if(!is.stickerCube(aCube)) 
    stop("argument must be a cube object")
  if(length(colvec) != 6) stop("must have six colors")
  
  color <- c("U", "R", "F", "D", "L", "B")
  vals <- as.numeric(factor(aCube, levels = color))
  plot(2, 2, type = "n", xlim = c(0,12), ylim = c(0,9), axes = FALSE, pty = "s",
       xlab =xlab, ylab = ylab, main = main, ...)
  
  xleft <- c(rep(3:5,3),rep(6:8,3),rep(3:5,3),rep(3:5,3),rep(0:2,3),rep(9:11,3))
  ybot <- c(rep(8:6,each=3),rep(5:3,each=3),rep(5:3,each=3),rep(2:0,each=3),rep(5:3,each=3),rep(5:3,each=3))
  
  rect(xleft, ybot, xleft+1, ybot+1, col = colvec[spor][vals])
  segments(rep(0,4),3:6,rep(12,4),3:6,lwd=c(2,1,1,2))
  segments(3:6,rep(0,4),3:6,rep(9,4),lwd=c(2,1,1,2))
  segments(rep(3,6),c(0:2,7:9),rep(6,6),c(0:2,7:9),lwd=c(2,1,1,1,1,2))
  segments(c(0:2,7:12),rep(3,9),c(0:2,7:12),rep(6,9),lwd=c(2,1,1,1,1,2,1,1,2))
  if(centres && !numbers) {
    text(c(4.5,7.5,4.5,4.5,1.5,10.5), c(7.5,4.5,4.5,1.5,4.5,4.5), labels = color, font = 2, cex = text.size*1.5, col = text.col)
  }
  if(centres && numbers) {
    text(c(rep(c(3.5,4.5,5.5),3),rep(c(6.5,7.5,8.5),3),rep(c(3.5,4.5,5.5),3),rep(c(3.5,4.5,5.5),3),rep(c(0.5,1.5,2.5),3),rep(c(9.5,10.5,11.5),3)),
         c(rep(c(8.5,7.5,6.5),each=3),rep(c(5.5,4.5,3.5),each=3),rep(c(5.5,4.5,3.5),each=3),rep(c(2.5,1.5,0.5),each=3),rep(c(5.5,4.5,3.5),each=3),rep(c(5.5,4.5,3.5),each=3)),
         labels = paste0(rep(color,each=9),rep(1:9,length(color))), font = 2, cex = text.size*1, col = text.col)
  }
  invisible(aCube)
}

plot3D <- function (x, ...) UseMethod("plot3D")
plot3D.cube <- function(x, colvec = getOption("cubing.colors"), recolor = FALSE, 
                        bg = grey(0.8), rand.col = FALSE, size = 0.98, col.interior = grey(0.5), 
                        al.interior = 0.4, al.exterior = 1, rinit = 30, bbox = TRUE, bbcolor = "#333377", bbemission = "#333377", 
                        bbspecular = "#3333FF", bbshininess = 5, bbalpha = 0.5, ...) 
{
  spor <- 1:6
  aCube <- x
  if(rand.col) colvec <- rgb(runif(6), runif(6),runif(6))
  if(is.cubieCube(aCube)) {
    if(is.null(aCube$spor)) stop("cubieCube has no spor coordinate")
    if(!recolor) spor <- aCube$spor 
    aCube <- as.stickerCube(aCube)
  } 
  if(!is.stickerCube(aCube)) 
    stop("argument must be a cube object")
  if(length(colvec) != 6) stop("must have six colors")
  if(length(size) != 1 || size >= 1) stop("size must be value less than 1")
  
  ord <- c("D", "B", "R", "L", "F", "U")
  z18 <- numeric(18); z6 <- numeric(6); z2 <- numeric(2)
  cubieSticker <- c(c(z18,7:9,4:6,1:3), 
                    c(3:1,z6,6:4,z6,9:7,z6),
                    c(z2,3,z2,2,z2,1,z2,6,z2,5,z2,4,z2,9,z2,8,z2,7), 
                    c(1,z2,2,z2,3,z2,4,z2,5,z2,6,z2,7,z2,8,z2,9,z2), 
                    c(z6,1:3,z6,4:6,z6,7:9), 
                    c(1:9, z18))
  cubieSticker <- matrix(paste0(rep(ord,each=27), cubieSticker), nrow = 27, ncol = 6)
  
  color <- c("U", "R", "F", "D", "L", "B")
  vals <- as.numeric(factor(aCube, levels = color))
  flatcol <- colvec[spor][vals]
  names(flatcol) <- names(aCube)
  
  cubevec <- vector("list", length = 27)
  cubevec[[14]] <- rgl::cube3d(color = rep(col.interior, each = 4*6), alpha = al.interior)
  for(i in 1:27) {
    if(i == 14) next
    cubiei <- flatcol[cubieSticker[i,]]; alpha <- rep(al.exterior, 6)
    alpha[is.na(cubiei)] <- al.interior; cubiei[is.na(cubiei)] <- col.interior
    cubevec[[i]] <- rgl::cube3d(color = rep(rep(cubiei, each = 4), length = 4*6), 
                                alpha = rep(rep(alpha, each = 4), length = 4*6))
  }
  
  rgl::open3d()
  M <- rgl::par3d("userMatrix")
  rgl::par3d(userMatrix = rotate3d(M, -rinit*(2*pi)/360, 0, 0, 1))
  xx <- rep(-1:1, 9)
  yy <- rep(rep(1:-1, each=3), 3) 
  zz <- rep(1:-1, each=9) 
  szvec <- rep(size/2, 27)
  shapelist3d(cubevec, xx,yy,zz, size = szvec, ...) 
  if(bbox) rgl::bbox3d(color = bbcolor, emission = bbemission, specular = bbspecular, 
                  shininess = bbshininess, alpha = bbalpha,
                  xat = c(-1,0,1), yat = c(-1,0,1), zat = c(-1,0,1),
                  xlab = c("L","M","R"), ylab = c("F","S","B"), zlab = c("D","E","U"))
  rgl::bg3d(bg)
  
  invisible(aCube)
}

animate <- function(aCube, moves, fpt = 8, colvec = getOption("cubing.colors"), recolor = FALSE, bg = grey(0.8), 
                    rand.col = FALSE, size = 0.98, col.interior = grey(0.5), al.interior = 0.4, al.exterior = 1, start.delay = 2, move.delay = 0, 
                    rinit = 30, bbox = TRUE, bbcolor = "#333377", bbemission = "#333377", bbspecular = "#3333FF", bbshininess = 5, bbalpha = 0.5, 
                    movie = NULL, dir = file.path(getwd(), movie), verbose = TRUE, start.fdelay = fpt, end.fdelay = fpt, move.fdelay = 1, ...)
{
  writeframe <- function(dir, movie, frame, verbose) {
    #if(is.null(movie)) stop("coding problem: please report to maintainer") 
    filename <- sprintf("%s%04d.png", movie, frame)
    if (verbose) {
      cat(gettextf("Writing '%s'\n", filename))
      flush.console()
    }
    rgl::rgl.snapshot(file.path(dir, filename), fmt = "png", top = TRUE)
  }
  if(!is.null(movie) && (!is.character(movie) || length(movie) != 1))
    stop("movie must be a single character string")
  if(!is.null(movie) && !dir.exists(dir)) {
    dir.create(dir)
  }
  spor <- 1:6
  if(rand.col) colvec <- rgb(runif(6), runif(6),runif(6))
  if(is.cubieCube(aCube)) {
    if(is.null(aCube$spor)) stop("cubieCube has no spor coordinate")
    if(!recolor) spor <- aCube$spor 
    aCube <- as.stickerCube(aCube)
  } 
  if(!is.stickerCube(aCube)) 
    stop("argument must be a cube object")
  if(!is.atomic(moves) || !is.character(moves)) 
    stop("argument must be character vector")
  if(length(moves) == 1) {
    moves <- gsub("\\s", "", moves)
    moves <- strsplit(gsub("([URFDLBEMSxyz])", " \\1", moves), " ")[[1]][-1]
  }
  if(length(size) != 1 || size >= 1) 
    stop("size must be value less than 1")
  size <- size/2
  fpt <- as.integer(fpt)
  if(length(size) != 1 || fpt %% 2 != 0 || fpt < 2)
    stop("fpt must be an even non-negative integer")
  
  colv <- c("U", "R", "F", "D", "L", "B")
  colv <- paste0(rep(colv,each = 6), rep(c("","1","2","2'","3","'"), length(colv)))
  slv <- c("E", "M", "S")
  slv <- paste0(rep(slv,each = 6), rep(c("","1","2","2'","3","'"), length(slv)))
  rotv <- c("x", "y", "z")
  rotv <- paste0(rep(rotv,each = 6), rep(c("","1","2","2'","3","'"), length(rotv)))
  legal <- (moves %in% c(colv,slv,rotv))
  if(!all(legal)) 
    stop("only URFDLBEMS face turns and xyz rotations allowed")
  
  ord <- c("D", "B", "R", "L", "F", "U")
  z18 <- numeric(18); z6 <- numeric(6); z2 <- numeric(2)
  cubieSticker <- c(c(z18,7:9,4:6,1:3), 
                    c(3:1,z6,6:4,z6,9:7,z6),
                    c(z2,3,z2,2,z2,1,z2,6,z2,5,z2,4,z2,9,z2,8,z2,7), 
                    c(1,z2,2,z2,3,z2,4,z2,5,z2,6,z2,7,z2,8,z2,9,z2), 
                    c(z6,1:3,z6,4:6,z6,7:9), 
                    c(1:9, z18))
  cubieSticker <- matrix(paste0(rep(ord,each=27), cubieSticker), nrow = 27, ncol = 6)
  
  color <- c("U", "R", "F", "D", "L", "B")
  vals <- as.numeric(factor(aCube, levels = color))
  flatcol <- colvec[spor][vals]
  names(flatcol) <- names(aCube)
  
  cubevec <- vector("list", length = 27)
  class(cubevec) <- c("shapelist3d", "shape3d")
  cubevec[[14]] <- rgl::cube3d(color = rep(col.interior, each = 4*6), alpha = al.interior)
  for(i in 1:27) {
    if(i == 14) next
    cubiei <- flatcol[cubieSticker[i,]]; alpha <- rep(al.exterior, 6)
    alpha[is.na(cubiei)] <- al.interior; cubiei[is.na(cubiei)] <- col.interior
    cubevec[[i]] <- rgl::cube3d(color = rep(rep(cubiei, each = 4), length = 4*6), 
                                alpha = rep(rep(alpha, each = 4), length = 4*6))
  }
  
  x <- rep(-1:1, 9)
  y <- rep(rep(1:-1, each=3), 3) 
  z <- rep(1:-1, each=9) 
  nc <- 27
  
  for (i in seq_len(nc)) 
  {
    cubevec[[i]] <- rgl::scale3d(cubevec[[i]], size, size, size)
    cubevec[[i]] <- rgl::translate3d(cubevec[[i]], x[i], y[i], z[i])
  }
  
  rgl::open3d()
  M <- rgl::par3d("userMatrix")
  rgl::par3d(userMatrix = rotate3d(M, -rinit*(2*pi)/360, 0, 0, 1))
  rgl::shade3d(cubevec, ...)
  if(bbox) rgl::bbox3d(color = bbcolor, emission = bbemission, specular = bbspecular, 
                  shininess = bbshininess, alpha = bbalpha,
                  xat = c(-1,0,1), yat = c(-1,0,1), zat = c(-1,0,1),
                  xlab = c("L","M","R"), ylab = c("F","S","B"), zlab = c("D","E","U"))
  rgl::bg3d(bg)
  
  if(!is.null(movie)) {
    writeframe(dir, movie, 0, verbose)
    nf <- 1
    for(h in seq_len(start.fdelay)) {
      writeframe(dir, movie, nf, verbose)
      nf <- nf + 1
    }
  }
  Sys.sleep(start.delay)
  
  ids <- rgl::rgl.ids()$id
  cornervec <- cubevec[c(9,7,1,3,27,25,19,21)]
  cornerids <- ids[c(9,7,1,3,27,25,19,21)]
  edgevec <- cubevec[c(18,16,10,12,6,8,4,2,24,26,22,20)]
  edgeids <- ids[c(18,16,10,12,6,8,4,2,24,26,22,20)]
  centrevec <- cubevec[c(5,15,17,23,13,11)]
  centreids <- ids[c(5,15,17,23,13,11)]
  
  for(j in seq_along(moves)) 
  {
    mvj <- moves[j]
    if(mvj %in% rotv) 
    {
      
      ivec <- switch(EXPR = mvj,
                     "y1" =, "y" =, "y'" =, "y3" =, "y2'" =, "y2" = 
                       list(list(c(1,2,3,4),c(5,6,7,8),1),list(c(5,6,7,8),c(9,10,11,12),4),list(numeric(0),c(1,2,3,4),c(2,3,5,6))),
                     "x1" =, "x" =, "x'" =, "x3" =, "x2'" =, "x2" = 
                       list(list(c(1,4,5,8),c(1,4,5,9),2),list(c(2,3,6,7),c(2,3,7,11),5),list(numeric(0),c(6,8,10,12),c(1,3,4,6))),
                     "z1" =, "z" =, "z'" =, "z3" =, "z2'" =, "z2" = 
                       list(list(c(1,2,5,6),c(1,2,6,10),3),list(c(3,4,7,8),c(3,4,8,12),6),list(numeric(0),c(5,7,9,11),c(1,2,4,5))))
      
      
      tr <- switch(EXPR = mvj,
                   "y1" =, "y" =, "y'" =, "y3" =, "y2'" =, "y2" = 
                     list(c(0, 0, -1),c(0, 0, 1),c(0, 0, 0)),
                   "x1" =, "x" =, "x'" =, "x3" =, "x2'" =, "x2" = 
                     list(c(-1, 0, 0),c(1, 0, 0),c(0, 0, 0)),
                   "z1" =, "z" =, "z'" =, "z3" =, "z2'" =, "z2" = 
                     list(c(0, 1, 0),c(0, -1, 0),c(0, 0, 0)))
      rt <- switch(EXPR = mvj,
                   "y1" =, "y" =, "y2" = c(0, 0, 1),
                   "x1" =, "x" =, "x2" = c(1, 0, 0),
                   "z1" =, "z" =, "z2" = c(0, -1, 0),
                   "y'" =, "y2'" =, "y3" = c(0, 0, -1),
                   "x'" =, "x2'" =, "x3" = c(-1, 0, 0),
                   "z'" =, "z2'" =, "z3" = c(0, 1, 0))
      
      ht <- fpt/2*(1 + as.numeric(grepl(2, mvj)))
      for(k in 1:ht) 
      {
        rgl::par3d(skipRedraw = TRUE)
        for(slice in 1:3) {
          trs <- tr[[slice]]
          for (i in ivec[[slice]][[1]]) 
          {
            cornervec[[i]] <- rgl::translate3d(cornervec[[i]], trs[1], trs[2], trs[3])
            cornervec[[i]] <- rgl::rotate3d(cornervec[[i]], matrix = rgl::rotationMatrix(pi/fpt,rt[1],rt[2],rt[3]))
            cornervec[[i]] <- rgl::translate3d(cornervec[[i]], -trs[1], -trs[2], -trs[3])
            rgl::rgl.pop(id = cornerids[i])
            cornerids[i] <- rgl::shade3d(cornervec[[i]], ...)
          }
          for (i in ivec[[slice]][[2]]) 
          {
            edgevec[[i]] <- rgl::translate3d(edgevec[[i]], trs[1], trs[2], trs[3])
            edgevec[[i]] <- rgl::rotate3d(edgevec[[i]], matrix = rgl::rotationMatrix(pi/fpt,rt[1],rt[2],rt[3]))
            edgevec[[i]] <- rgl::translate3d(edgevec[[i]], -trs[1], -trs[2], -trs[3])
            rgl::rgl.pop(id = edgeids[i])
            edgeids[i] <- rgl::shade3d(edgevec[[i]], ...)
          }
          for (i in ivec[[slice]][[3]]) 
          {
            centrevec[[i]] <- rgl::translate3d(centrevec[[i]], trs[1], trs[2], trs[3])
            centrevec[[i]] <- rgl::rotate3d(centrevec[[i]], matrix = rgl::rotationMatrix(pi/fpt,rt[1],rt[2],rt[3]))
            centrevec[[i]] <- rgl::translate3d(centrevec[[i]], -trs[1], -trs[2], -trs[3])
            rgl::rgl.pop(id = centreids[i])
            centreids[i] <- rgl::shade3d(centrevec[[i]], ...)
          }
        }
        rgl::par3d(skipRedraw = FALSE)
        if(!is.null(movie)) {
          writeframe(dir, movie, nf, verbose)
          nf <- nf + 1
        }
      }
      getPO <- getPOMoveCube(mvj)
      cornervec <- cornervec[getPO$cp]
      cornerids <- cornerids[getPO$cp]
      edgevec <- edgevec[getPO$ep]
      edgeids <- edgeids[getPO$ep]
      centrevec <- centrevec[getPO$spor]
      centreids <- centreids[getPO$spor]
      
    } else {
      
      ivec <- switch(EXPR = mvj,
                     "U1" =, "U" =, "U'" =, "U3" =, "U2'" =, "U2" = list(c(1,2,3,4),c(5,6,7,8),1),
                     "R1" =, "R" =, "R'" =, "R3" =, "R2'" =, "R2" = list(c(1,4,5,8),c(1,4,5,9),2),
                     "F1" =, "F" =, "F'" =, "F3" =, "F2'" =, "F2" = list(c(1,2,5,6),c(1,2,6,10),3),
                     "D1" =, "D" =, "D'" =, "D3" =, "D2'" =, "D2" = list(c(5,6,7,8),c(9,10,11,12),4),
                     "L1" =, "L" =, "L'" =, "L3" =, "L2'" =, "L2" = list(c(2,3,6,7),c(2,3,7,11),5),
                     "B1" =, "B" =, "B'" =, "B3" =, "B2'" =, "B2" = list(c(3,4,7,8),c(3,4,8,12),6),
                     "E1" =, "E" =, "E'" =, "E3" =, "E2'" =, "E2" = list(numeric(0),c(1,2,3,4),c(2,3,5,6)),
                     "M1" =, "M" =, "M'" =, "M3" =, "M2'" =, "M2" = list(numeric(0),c(6,8,10,12),c(1,3,4,6)),
                     "S1" =, "S" =, "S'" =, "S3" =, "S2'" =, "S2" = list(numeric(0),c(5,7,9,11),c(1,2,4,5)))
      
      tr <- switch(EXPR = mvj,
                   "U1" =, "U" =, "U'" =, "U3" =, "U2'" =, "U2" = c(0, 0, -1),
                   "R1" =, "R" =, "R'" =, "R3" =, "R2'" =, "R2" = c(-1, 0, 0),
                   "F1" =, "F" =, "F'" =, "F3" =, "F2'" =, "F2" = c(0, 1, 0),
                   "D1" =, "D" =, "D'" =, "D3" =, "D2'" =, "D2" = c(0, 0, 1),
                   "L1" =, "L" =, "L'" =, "L3" =, "L2'" =, "L2" = c(1, 0, 0),
                   "B1" =, "B" =, "B'" =, "B3" =, "B2'" =, "B2" = c(0, -1, 0),
                   c(0, 0, 0))
      rt <- switch(EXPR = mvj,
                   "U1" =, "U" =, "U2" =, "D'" =, "D2'" =, "D3" =, "E'" =, "E2'" =, "E3" = c(0, 0, 1),
                   "R1" =, "R" =, "R2" =, "L'" =, "L2'" =, "L3" =, "M'" =, "M2'" =, "M3" = c(1, 0, 0),
                   "F1" =, "F" =, "F2" =, "B'" =, "B2'" =, "B3" =, "S'" =, "S2'" =, "S3" = c(0, -1, 0),
                   "U'" =, "U2'" =, "U3" =, "D1" =, "D" =, "D2" =, "E1" =, "E" =, "E2" = c(0, 0, -1),
                   "R'" =, "R2'" =, "R3" =, "L1" =, "L" =, "L2" =, "M1" =, "M" =, "M2" = c(-1, 0, 0),
                   "F'" =, "F2'" =, "F3" =, "B1" =, "B" =, "B2" =, "S1" =, "S" =, "S2" = c(0, 1, 0))
      
      
      ht <- fpt*(1 + as.numeric(grepl(2, mvj)))
      for(k in 1:ht) 
      {
        rgl::par3d(skipRedraw = TRUE)
        for (i in ivec[[1]]) 
        {
          cornervec[[i]] <- rgl::translate3d(cornervec[[i]], tr[1], tr[2], tr[3])
          cornervec[[i]] <- rgl::rotate3d(cornervec[[i]], matrix = rgl::rotationMatrix(pi/(fpt*2),rt[1],rt[2],rt[3]))
          cornervec[[i]] <- rgl::translate3d(cornervec[[i]], -tr[1], -tr[2], -tr[3])
          rgl::rgl.pop(id = cornerids[i])
          cornerids[i] <- rgl::shade3d(cornervec[[i]], ...)
        }
        for (i in ivec[[2]]) 
        {
          edgevec[[i]] <- rgl::translate3d(edgevec[[i]], tr[1], tr[2], tr[3])
          edgevec[[i]] <- rgl::rotate3d(edgevec[[i]], matrix = rgl::rotationMatrix(pi/(fpt*2),rt[1],rt[2],rt[3]))
          edgevec[[i]] <- rgl::translate3d(edgevec[[i]], -tr[1], -tr[2], -tr[3])
          rgl::rgl.pop(id = edgeids[i])
          edgeids[i] <- rgl::shade3d(edgevec[[i]], ...)
        }
        for (i in ivec[[3]]) 
        {
          centrevec[[i]] <- rgl::translate3d(centrevec[[i]], tr[1], tr[2], tr[3])
          centrevec[[i]] <- rgl::rotate3d(centrevec[[i]], matrix = rgl::rotationMatrix(pi/(fpt*2),rt[1],rt[2],rt[3]))
          centrevec[[i]] <- rgl::translate3d(centrevec[[i]], -tr[1], -tr[2], -tr[3])
          rgl::rgl.pop(id = centreids[i])
          centreids[i] <- rgl::shade3d(centrevec[[i]], ...)
        }
        rgl::par3d(skipRedraw = FALSE)
        if(!is.null(movie)) {
          writeframe(dir, movie, nf, verbose)
          nf <- nf + 1
        }
      }
      getPO <- getPOMoveCube(mvj)
      cornervec <- cornervec[getPO$cp]
      cornerids <- cornerids[getPO$cp]
      edgevec <- edgevec[getPO$ep]
      edgeids <- edgeids[getPO$ep]
      centrevec <- centrevec[getPO$spor]
      centreids <- centreids[getPO$spor]
    }
    if(!is.null(movie)) {
      for(h in seq_len(move.fdelay)) {
        writeframe(dir, movie, nf, verbose)
        nf <- nf + 1
      }
    }
    Sys.sleep(move.delay)
  }
  if(!is.null(movie)) {
    for(h in seq_len(end.fdelay)) {
      writeframe(dir, movie, nf, verbose)
      nf <- nf + 1
    }
  }
  invisible(moves)
}



