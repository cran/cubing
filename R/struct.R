
###################################################

# stickerCube cubieCube
# is.stickerCube is.cubieCube 
# as.stickerCube as.cubieCube
# print.stickerCube print.cubieCube

# randCube randMoves scramble       
# rotate slice rotations plot.rotCubes ==.cube 
# is.solved is.solvable parity (cyclengths) (psign)

# %v% %e% %c%
# getMovesCube move plot.seqCubes invCube invMoves rotMoves mirMoves
# cycleEdges cycleCorners flipEdges twistCorners

###################################################

  #             |*U1**U2**U3*|
  #             |************|
  #             |*U4**U5**U6*|
  #             |************|
  #             |*U7**U8**U9*|
  #             |************|
  # ************|************|************|************|
  # *L1**L2**L3*|*F1**F2**F3*|*R1**R2**F3*|*B1**B2**B3*|
  # ************|************|************|************|
  # *L4**L5**L6*|*F4**F5**F6*|*R4**R5**R6*|*B4**B5**B6*|
  # ************|************|************|************|
  # *L7**L8**L9*|*F7**F8**F9*|*R7**R8**R9*|*B7**B8**B9*|
  # ************|************|************|************|
  #             |************|
  #             |*D1**D2**D3*|
  #             |************|
  #             |*D4**D5**D6*|
  #             |************|
  #             |*D7**D8**D9*|
  #             |************|
  
###################################################

# create cube representations from string

stickerCube <- function(string) 
{
  if(!is.atomic(string) || !is.character(string)) 
    stop("argument must be character vector")
  if(length(string) == 1) {
    string <- gsub("\\s", "", string)
    out <- strsplit(string, "")[[1]]
  } else {
    out <- string
  }
  
  n_corner <- 8; n_edge <- 12
  color <- c("U", "R", "F", "D", "L", "B")
  sticker <- paste0(rep(color,each=9), rep(1:9,length(color)))
  n_sticker <- length(sticker)
  if(length(out) != n_sticker && length(out) != n_sticker-6) 
    stop("string of incorrect length")
  
  cvec <- seq(3 + 1 + 3 %/% 2, by=3*3, length = 6)
  if(length(out) == n_sticker && !all(out[cvec] == color))
    stop("centre pieces are incorrect color")
  if(length(out) == n_sticker-6) {
    tmpout <- numeric(n_sticker)
    tmpout[-cvec] <- out; tmpout[cvec] <- color
    out <- tmpout
  }
  names(out) <- sticker
  if(!all(out %in% color)) stop("strings must contain only the characters URFLBD")
  if(!all(tabulate(factor(out)) == 9)) stop("must have nine of each color URFLBD")
  
  cornerSticker <- matrix(c("U9", "R1", "F3", "U7", "F1", "L3", "U1", "L1", "B3", "U3", "B1", "R3", 
                     "D3", "F9", "R7", "D1", "L9", "F7", "D7", "B9", "L7", "D9", "R9", "B7"), ncol = 3, byrow = TRUE)
  edgeSticker <- matrix(c("F6", "R4", "F4", "L6", "B6", "L4", "B4", "R6", "U6", "R2", "U8", "F2", 
                   "U4", "L2", "U2", "B2", "D6", "R8", "D2", "F8", "D4", "L8", "D8", "B8"), ncol = 2, byrow = TRUE) 
  cornerColor <- substr(cornerSticker,1,1)
  edgeColor <- substr(edgeSticker,1,1)
  
  for(i in 1:n_corner) {
    cols <- sort(out[cornerSticker[i,]])
    cornerColorSort <- as.data.frame(apply(cornerColor, 1, sort))
    if(!any(sapply(cornerColorSort, function(x) all(cols == x))))
      stop("corner piece has invalid stickers")
  }
  for(i in 1:n_edge) {
    cols <- sort(out[edgeSticker[i,]])
    edgeColorSort <- as.data.frame(apply(edgeColor, 1, sort))
    if(!any(sapply(edgeColorSort, function(x) all(cols == x))))
      stop("edge piece has invalid stickers")
  }
  
  class(out) <- c("stickerCube", "cube")
  out
}

cubieCube <- function(string) {
  sCube <- stickerCube(string)
  as.cubieCube(sCube)
}

# is and as functions

is.stickerCube <- function(aCube) {
  inherits(aCube, "stickerCube")
}

is.cubieCube <- function(aCube) {
  inherits(aCube, "cubieCube")
}

as.stickerCube <- function(aCube) 
{
  if(is.stickerCube(aCube)) return(aCube)
  if(!is.cubieCube(aCube)) 
    stop("argument must be a cube object")
  
  cornerSticker <- matrix(c("U9", "R1", "F3", "U7", "F1", "L3", "U1", "L1", "B3", "U3", "B1", "R3", 
                            "D3", "F9", "R7", "D1", "L9", "F7", "D7", "B9", "L7", "D9", "R9", "B7"), ncol = 3, byrow = TRUE)
  edgeSticker <- matrix(c("F6", "R4", "F4", "L6", "B6", "L4", "B4", "R6", "U6", "R2", "U8", "F2", 
                          "U4", "L2", "U2", "B2", "D6", "R8", "D2", "F8", "D4", "L8", "D8", "B8"), ncol = 2, byrow = TRUE) 
  cornerColor <- substr(cornerSticker,1,1)
  edgeColor <- substr(edgeSticker,1,1)
  
  n_corner <- 8; n_edge <- 12
  out <- getStickerCube()
  for(i in 1:n_corner) {
    j <- aCube$cp[i]
    ori <- aCube$co[i]
    for (k in 1:3)
      out[cornerSticker[i, (k + ori - 1) %% 3 + 1]] <- cornerColor[j,k]
  }
  for(i in 1:n_edge) {
    j <- aCube$ep[i]
    ori <- aCube$eo[i]
    for (k in 1:2)
      out[edgeSticker[i, (k + ori - 1) %% 2 + 1]] <- edgeColor[j,k]
  }
  out
}

as.cubieCube <- function(aCube) 
{
  if(is.cubieCube(aCube)) return(aCube)
  if(!is.stickerCube(aCube)) 
    stop("argument must be a cube object")
  
  n_corner <- 8; n_edge <- 12
  out <- list(cp = seq(1,n_corner,1), ep = seq(1,n_edge,1), co = rep(0,n_corner), eo = rep(0,n_edge), spor = 1:6)
  names(out$cp) <- names(out$co) <- c("URF", "UFL", "ULB", "UBR", "DFR", "DLF", "DBL", "DRB")
  names(out$ep) <- names(out$eo) <- c("FR", "FL", "BL", "BR", "UR", "UF", "UL", "UB", "DR", "DF", "DL", "DB")
  names(out$spor) <- c("U", "R", "F", "D", "L", "B")
  class(out) <- c("cubieCube", "cube")
  
  cornerSticker <- matrix(c("U9", "R1", "F3", "U7", "F1", "L3", "U1", "L1", "B3", "U3", "B1", "R3", 
                            "D3", "F9", "R7", "D1", "L9", "F7", "D7", "B9", "L7", "D9", "R9", "B7"), ncol = 3, byrow = TRUE)
  edgeSticker <- matrix(c("F6", "R4", "F4", "L6", "B6", "L4", "B4", "R6", "U6", "R2", "U8", "F2", 
                          "U4", "L2", "U2", "B2", "D6", "R8", "D2", "F8", "D4", "L8", "D8", "B8"), ncol = 2, byrow = TRUE) 
  cornerColor <- substr(cornerSticker,1,1)
  edgeColor <- substr(edgeSticker,1,1)
  
  for(i in 1:n_corner) {
    # coli is colors at ith corner starting with U/D
    ori <- which(aCube[cornerSticker[i,]] %in% c("U","D"))
    if(length(ori) != 1) stop("cube has invalid stickering")
    coli <- aCube[cornerSticker[i,(((ori-1):(ori+1)) %% 3 + 1)]]

    for(j in 1:n_corner) {
      if(all(coli == cornerColor[j,])) {
        out$cp[i] <- j
        out$co[i] <- ori-1
        break
      }
    }
  }
    
  for(i in 1:n_edge) {
    for(j in 1:n_edge) {
      if(all(aCube[edgeSticker[i,]] == edgeColor[j,])) {
        out$ep[i] <- j
        out$eo[i] <- 0
        break
      }
      if(all(aCube[edgeSticker[i,]] == rev(edgeColor[j,]))) {
        out$ep[i] <- j
        out$eo[i] <- 1
        break
      }
    }
  }
  out
}

# printing of cube representations

print.stickerCube <- function(x, ...) {
  print(noquote(unclass(x)))
  invisible(x)
}

print.cubieCube <- function(x, ...) {
  print(noquote(unclass(x)))
  invisible(x)
}

# random state and scramble

randCube <- function(n = 1, cubie = TRUE, solvable = TRUE, drop = TRUE, spor = 1:6) 
{
  n_corner <- 8; n_edge <- 12
  out <- vector("list", length = n)
  for(i in seq(length = n)) {
    outi <- list(cp = sample(n_corner), ep = sample(n_edge), 
                co = sample(0:2,n_corner,replace=TRUE), eo = sample(0:1,n_edge,replace=TRUE))
    if(solvable) {
      outi$co[n_corner] <- -sum(outi$co[-n_corner]) %% 3
      outi$eo[n_edge] <- -sum(outi$eo[-n_edge]) %% 2
      if(psign(outi$cp) != psign(outi$ep)) {
        tmp <- outi$ep[n_edge]
        outi$ep[n_edge] <- outi$ep[n_edge-1]
        outi$ep[n_edge-1] <- tmp
      }
    }
    names(outi$cp) <- names(outi$co) <- c("URF", "UFL", "ULB", "UBR", "DFR", "DLF", "DBL", "DRB")
    names(outi$ep) <- names(outi$eo) <- c("FR", "FL", "BL", "BR", "UR", "UF", "UL", "UB", "DR", "DF", "DL", "DB")
    outi$spor <- spor
    names(outi$spor) <- c("U", "R", "F", "D", "L", "B")
    class(outi) <- c("cubieCube", "cube")
    if(cubie) out[[i]] <- outi else out[[i]] <- as.stickerCube(outi)
  }
  if(n == 1 && drop) out <- out[[1]]
  out
}

randMoves <- function(n = 1, nm = 20, drop = TRUE) 
{
  out <- vector("list", length = n)
  color <- c("U", "R", "F", "D", "L", "B")
  mvvec <- paste0(rep(color, each = 3), rep(c("","2","'"), length(color)))
  for(i in seq(length = n)) {
    outi.ax <- c(sample(1:6,1), numeric(nm-1))
    if(nm >= 2) outi.ax[2] <- sample((1:6)[-outi.ax[1]], 1)
    outi.pw <- sample(1:3, nm, replace=TRUE)
    if(nm >= 3) {
      for(k in 3:nm) {
        outi.ax[k] <- sample((1:6)[-outi.ax[k-1]], 1)
        if((outi.ax[k] %% 3) == (outi.ax[k-1] %% 3) && (outi.ax[k] %% 3) == (outi.ax[k-2] %% 3)) 
          outi.ax[k] <- sample((1:6)[-outi.ax[(k-1):(k-2)]], 1)
      }
    }
    out[[i]] <- mvvec[3*(outi.ax-1) + outi.pw]
  }
  if(n == 1 && drop) out <- out[[1]]
  out
}
  
scramble <- function(n = 1, state = FALSE, nm = 20, drop = TRUE,
  type = c("KB", "ZT", "TF", "ZZ", "CFOP"), maxMoves = 24, bound = TRUE)
{
  type <- match.arg(type)
  if(state) {
    out <- vector("list", length = n)
    cubes <- randCube(n, drop = FALSE)
    for(i in seq(length = n)) {
      out[[i]] <- solver(cubes[[i]], inv = TRUE, 
        type = type, maxMoves = maxMoves, bound = bound)
    }
    if(n == 1 && drop) out <- out[[1]]
  } else {
    out <- randMoves(n, nm = nm, drop = drop)
  }
  out
}

# rotate slice and move functions
# rotate function changes the spor vector

rotate <- function(aCube, rot)
{
  if(!is.cubieCube(aCube)) 
    stop("argument must be a cubieCube object")
  colmat <- c(c(1,2,3,4,5,6), c(1,6,2,4,3,5), c(1,5,6,4,2,3), c(1,3,5,4,6,2))
  colmat <- c(colmat, c(4,5,3,1,2,6)[colmat])
  colmat <- c(colmat, c(3,1,2,6,4,5)[colmat], c(2,3,1,5,6,4)[colmat])
  colmat <- matrix(colmat, nrow = 6, ncol = 24)
  
  if(is.character(rot))
  {
    crot <- rot
    rot <- switch(EXPR = crot,
           "M'" = , "M3" = , "x1" = , "x" = 12, 
           "E'" = , "E3" = , "y1" = , "y" = 2, 
           "S'" = , "S3" = , "z1" = , "z" = 24,
           "M" = , "M1" = , "x3" = , "x'" = 16, 
           "E" = , "E1" = , "y3" = , "y'" = 4, 
           "S" = , "S1" = , "z3" = , "z'" = 18,
           "M2" = , "M2'" = , "x2" = 7, 
           "E2" = , "E2'" = , "y2" = 3, 
           "S2" = , "S2'" = , "z2" = 5, 
           "0" = 1,
         stop("unrecognized rotation"))
  }
  rCube <- getRotCube(rot)
  if(is.null(aCube$spor)) stop("cubieCube has no spor coordinate")
  spor <- aCube$spor 
  aCube <- invCube(rCube) %v% aCube %v% rCube
  aCube$spor <- spor[colmat[,rot]]
  aCube
}

slice <- function(aCube, smv)
{
  if(!is.cubieCube(aCube)) 
    stop("argument must be a cubieCube object")
  rotate(aCube, smv) %v% getSliceCube(smv)
}

rotations <- function(aCube)
{
  if(!is.cubieCube(aCube)) 
    stop("argument must be a cubieCube object")
  out <- list()
  for(i in 1:24) out[[i]] <- rotate(aCube, i)
  class(out) <- "rotCubes"
  out
}

plot.rotCubes <- function(x, which = 1:24, ask = FALSE, 
  colvec = getOption("cubing.colors"),  recolor = FALSE, ...)
{
  if(!inherits(x, "rotCubes")) {
    stop("first argument must be a rotation cubes object")
  }
  oldpar <- par(ask=ask)
  for(i in which) plot(x[[i]], main = paste("Rotation", i), 
                       recolor = recolor, colvec = colvec, ...)
  par(oldpar)
  invisible(x[which])
}

all.equal.cube <- function(target, current, ...) 
{
  if(is.stickerCube(target))
    target <- as.cubieCube(target)
  if(is.stickerCube(current))
    current <- as.cubieCube(current)
  rCubes <-rotations(target)
  for(i in 1:24) {
    target <- rCubes[[i]]
    if(all(c(target$cp,target$ep) == c(current$cp,current$ep)))
      if(all(c(target$co,target$eo) == c(current$co,current$eo))) return(TRUE)
  }
  FALSE
}

"==.cube" <- function(aCube, bCube) 
{
  if(is.stickerCube(aCube))
    aCube <- as.cubieCube(aCube)
  if(is.stickerCube(bCube))
    bCube <- as.cubieCube(bCube)
  if(all(c(aCube$cp,aCube$ep) == c(bCube$cp,bCube$ep)))
      if(all(c(aCube$co,aCube$eo) == c(bCube$co,bCube$eo))) return(TRUE)
  FALSE
}

# test solved and solvability via cubieCube

is.solved <- function(aCube, split = FALSE, co = TRUE, eo = TRUE)
{
  if(is.stickerCube(aCube)) aCube <- as.cubieCube(aCube)
  if(!is.cubieCube(aCube)) 
    stop("argument must be a cube object")

  cp_ind <- all(aCube$cp == 1:8)
  ep_ind <- all(aCube$ep == 1:12)
  co_ind <- all(aCube$co == 0)
  eo_ind <- all(aCube$eo == 0)
  indvec <- c(cp = cp_ind, ep  = ep_ind)
  if(co) indvec <- c(indvec, co = co_ind)
  if(eo) indvec <- c(indvec, eo = eo_ind)
  
  if(split) return(indvec)
  all(indvec)
}

is.solvable <- function(aCube, split = FALSE)
{
  if(is.stickerCube(aCube)) aCube <- as.cubieCube(aCube)
  if(!is.cubieCube(aCube)) 
    stop("argument must be a cube object")
  
  cparity <- parity(aCube)
  cparity <- as.logical(cparity["edge"] == cparity["corner"]) 
  co_ind <- !as.logical(sum(aCube$co) %% 3)
  eo_ind <- !as.logical(sum(aCube$eo) %% 2)
  indvec <- c(parity = cparity, co = co_ind, eo = eo_ind)
  
  if(split) return(indvec)
  all(indvec)
}

parity <- function(aCube)
{
  if(is.stickerCube(aCube)) aCube <- as.cubieCube(aCube)
  if(!is.cubieCube(aCube)) 
    stop("argument must be a cube object")
  
  c(corner = psign(aCube$cp), edge = psign(aCube$ep))
}

# calculates cycle lengths and sign of permutation
# both functions are internal

cyclengths <- function(p) 
{
  n <- length(p); x <- integer(n) 
  ii <- seq_len(n) 
  if(!all(sort(p) == 1:n)) stop("argument must be a permutation of 1:n")
  for (i in ii) { 
    z <- ii[!x][1] 
    if (is.na(z)) break 
    repeat { 
      x[z] <- i 
      z <- p[z] 
      if (x[z]) break 
    } 
  } 
  tabulate(x, i - 1L)
}

psign <- function(p) 
{
  1L - (sum(cyclengths(p) %% 2L == 0) %% 2L)*2L
}

# composition functions for cubieCubes

"%v%" <- function(aCube, bCube) {
  aCube <- aCube %e% bCube
  aCube <- aCube %c% bCube
  aCube
}

"%e%" <- function(aCube, bCube) 
{
  n_edge <- 12
  pvec <- aCube$ep; ovec <- aCube$eo
  for(i in 1:n_edge) {
    pvec[i] <- aCube$ep[bCube$ep[i]]
    ovec[i] <- (aCube$eo[bCube$ep[i]] + bCube$eo[i]) %% 2 
  }
  aCube$ep <- pvec; aCube$eo <- ovec
  aCube
}

"%c%" <- function(aCube, bCube) 
{
  n_corner <- 8 
  pvec <- aCube$cp; ovec <- aCube$co
  for(i in 1:n_corner) {
    pvec[i] <- aCube$cp[bCube$cp[i]]
    ovec[i] <- (aCube$co[bCube$cp[i]] + bCube$co[i]) %% 3
  }
  aCube$cp <- pvec; aCube$co <- ovec
  aCube
}

# multiple moves cube
# uses getMoveCube with %v% composition

getMovesCube <- function(moves, cubie = TRUE)
{
  if(!is.atomic(moves) || !is.character(moves)) 
    stop("argument must be character vector")
  if(length(moves) == 1) {
    moves <- gsub("\\s", "", moves)
    moves <- strsplit(gsub("([URFDLBEMSxyz])", " \\1", moves), " ")[[1]][-1]
  } 

  color <- c("U", "R", "F", "D", "L", "B")
  mvnm <- paste0(rep(color,each = 6), rep(c("","1","2","2'","3","'"), 6))
  legal <- (moves %in% mvnm)
  if(!all(legal)) 
    stop("only URFDLB face turns allowed")
  
  if(length(moves) == 0) {
    if(!cubie) return(getStickerCube())
    return(getCubieCube())
  }
  out <- getMoveCube(moves[1])
  if(length(moves) == 1) {
    if(!cubie) return(as.stickerCube(out))
    out$spor <- 1:6
    names(out$spor) <- color
    return(out)
  }
  for(i in 2:length(moves)) out <- out %v% getMoveCube(moves[i])
  if(!cubie) out <- as.stickerCube(out)
  out$spor <- 1:6
  names(out$spor) <- color
  out
}

# move a cubieCube allows rotations and middle slice moves
# uses getMoveCube getSliceCube and rotate

move <- function(aCube, moves, history = FALSE)
{
  if(!is.cubieCube(aCube))
    stop("aCube must be a cubieCube object")
  if(!is.atomic(moves) || !is.character(moves)) 
    stop("argument must be character vector")
  if(length(moves) == 1) {
    moves <- gsub("\\s", "", moves)
    moves <- strsplit(gsub("([URFDLBEMSxyz])", " \\1", moves), " ")[[1]][-1]
  }
  
  colv <- c("U", "R", "F", "D", "L", "B")
  colv <- paste0(rep(colv,each = 6), rep(c("","1","2","2'","3","'"), length(colv)))
  slv <- c("E", "M", "S")
  slv <- paste0(rep(slv,each = 6), rep(c("","1","2","2'","3","'"), length(slv)))
  rotv <- c("x", "y", "z")
  rotv <- paste0(rep(rotv,each = 6), rep(c("","1","2","2'","3","'"), length(rotv)))
  legal <- (moves %in% c(colv,slv,rotv))
  if(!all(legal)) 
    stop("only URFDLBEMS face turns and xyz rotations allowed")

  if(!history) {
    for(i in 1:length(moves)) {
      if(moves[i] %in% colv) {
        aCube <- aCube %v% getMoveCube(moves[i])
      } else if(moves[i] %in% slv) {
        aCube <- rotate(aCube, moves[i]) %v% getSliceCube(moves[i])
      } else {
        aCube <- rotate(aCube, moves[i])
      }
    }
  } else {
    aCubeL <- vector("list", length = length(moves)+1)
    class(aCubeL) <- "seqCubes"
    attr(aCubeL, 'moves') <- moves
    aCubeL[[1]] <- aCube
    for(i in 1:length(moves)) {
      if(moves[i] %in% colv) {
        aCubeL[[i+1]] <- aCubeL[[i]] %v% getMoveCube(moves[i])
      } else if(moves[i] %in% slv) {
        aCubeL[[i+1]] <- rotate(aCubeL[[i]], moves[i]) %v% getSliceCube(moves[i])
      } else {
        aCubeL[[i+1]] <- rotate(aCubeL[[i]], moves[i])
      }
    }
    aCube <- aCubeL
  }
  
  aCube
}

# plot cube sequence created by move function

plot.seqCubes <- function(x, initial = TRUE, which = 1:length(moves), ask = FALSE, colvec = 
    getOption("cubing.colors"), recolor = FALSE, show.rot = TRUE, title = NULL, cex.title = 1, 
    font.title = 2, ...)
{
  if(!inherits(x, "seqCubes")) {
    stop("first argument must be a sequential cubes object")
  }
  moves <- attr(x, "moves")
  rots <- grep("[xyz]", moves)
  cnt <- cumsum(!(1:length(moves) %in% rots))
  
  oldpar <- par(ask=ask)
  if(!is.null(title)) {
    plot(0:1, 0:1, type="n", axes=FALSE, xlab="", ylab = "")
    text(0.5, 0.7, title, cex = 1.5*cex.title, adj = 0.5, font = font.title)
  }
  if(initial) plot(x[[1]], main = "Initial Position", 
       recolor = recolor, colvec = colvec, ...)
  
  for(i in which) {
    if(i %in% rots) {
      if(!show.rot) next
      mlab <- "Rotation"
    } else {
      mlab <- paste0("Move ", cnt[i])
    }
    plot(x[[i+1]], main = paste0(mlab, ": ", moves[i]), 
                       recolor = recolor, colvec = colvec, ...)
  }
  par(oldpar)
  invisible(x[which])
}

# inverse cube and inverse moves

invCube <- function(aCube, edges = TRUE, corners = TRUE)
{
  if(!is.cubieCube(aCube)) 
    stop("aCube must be a cubieCube object")
  bCube <- aCube
  n_corner <- 8; n_edge <- 12
  if(edges) {
    for(i in 1:n_edge)
      bCube$ep[aCube$ep[i]] <- i
    for(i in 1:n_edge)
      bCube$eo[i] <- aCube$eo[bCube$ep[i]]
  }
  if(corners) {
    for(i in 1:n_corner)
      bCube$cp[aCube$cp[i]] <- i
    for(i in 1:n_corner)
      bCube$co[i] <- -aCube$co[bCube$cp[i]] %% 3
  }
  bCube
}

invMoves <- function(moves, revseq = TRUE, collapse = NULL)
{
  if(!is.atomic(moves) || !is.character(moves)) 
    stop("argument must be character vector")
  if(length(moves) == 1) {
    moves <- gsub("\\s", "", moves)
    moves <- strsplit(gsub("([URFDLBEMSxyz])", " \\1", moves), " ")[[1]][-1]
  }
  
  moves <- sub("1", "", moves)
  moves <- sub("2'", "2", moves)
  moves <- sub("3", "'", moves)
  
  mvvec <- c("U", "R", "F", "D", "L", "B", "E", "M", "S", "x", "y", "z")
  mvvec2 <- paste0(rep(mvvec, each = 3), rep(c("","2","'"), length(mvvec)))
  if(!all(moves %in% mvvec2)) stop("move not recognized")
  
  if(revseq) moves <- rev(moves)
  
  swap2 <- function(mv, a, b) {
    smv <- mv
    for(k in 1:length(a)) {
      smv[mv == a[k]] <- b[k]
      smv[mv == b[k]] <- a[k]
    }
    smv
  }

  outst <- swap2(moves, mvvec, paste0(mvvec, "'"))
  if(!is.null(collapse)) outst <- paste(outst, collapse = collapse)
  outst
}

# rotate and mirror moves

rotMoves <- function(moves, rotation = 
  c("0","x","x1","y","y1","z","z1","x2","x2'","y2","y2'","z2","z2'","x'","x3","y'","y3","z'","z3"),
  invrot = FALSE, collapse = NULL)
{
  rotation <- match.arg(rotation)
  if(!is.atomic(moves) || !is.character(moves)) 
    stop("argument must be character vector")
  if(length(moves) == 1) {
    moves <- gsub("\\s", "", moves)
    moves <- strsplit(gsub("([URFDLBEMSxyz])", " \\1", moves), " ")[[1]][-1]
  }
  
  moves <- sub("1", "", moves)
  moves <- sub("2'", "2", moves)
  moves <- sub("3", "'", moves)
  
  if(rotation == "0") return(moves)
  if(invrot) {
    if(grepl("'$", rotation)) {
      rotation <- sub("'$", "", rotation)
    } else {
      rotation <- sub("([^2]$)", "\\1'", rotation)
    }
  }
  
  mvvec <- c("U", "R", "F", "D", "L", "B", "E", "M", "S", "x", "y", "z")
  mvvec2 <- paste0(rep(mvvec, each = 3), rep(c("","2","'"), length(mvvec)))
  if(!all(moves %in% mvvec2)) stop("move not recognized")
  
  swap2 <- function(mv, a, b) {
    smv <- mv
    for(k in 1:length(a)) {
      smv[mv == a[k]] <- b[k]
      smv[mv == b[k]] <- a[k]
    }
    smv
  }
  swap4 <- function(mv, a, b, c, d) {
    smv <- mv
    for(k in 1:length(a)) {
      smv[mv == a[k]] <- b[k]; smv[mv == b[k]] <- c[k]
      smv[mv == c[k]] <- d[k]; smv[mv == d[k]] <- a[k]
    }
    smv
  }
  
  Uv <- paste0("U", c("","2","'")); Rv <- paste0("R", c("","2","'"))
  Fv <- paste0("F", c("","2","'")); Dv <- paste0("D", c("","2","'"))
  Lv <- paste0("L", c("","2","'")); Bv <- paste0("B", c("","2","'"))
  outst <- switch(EXPR = rotation,
    "y1" = ,
    "y" = swap2(swap4(moves, c(Rv,"M","z"), c(Fv,"S","x'"), c(Lv,"M'","z'"), c(Bv,"S'","x")), 
                c("M2", "x2"), c("S2", "z2")),
    "x1" = ,
    "x" = swap2(swap4(moves, c(Uv,"E","y"), c(Bv,"S'","z'"), c(Dv,"E'","y'"), c(Fv,"S","z")),
               c("E2", "y2"), c("S2", "z2")),
    "z1" = ,
    "z" = swap2(swap4(moves, c(Uv,"M","x"), c(Rv,"E'","y'"), c(Dv,"M'","x'"), c(Lv,"E","y")),
                c("M2", "x2"), c("E2", "y2")),
    "y'" = ,
    "y3" = swap2(swap4(moves, c(Rv,"M","z"), c(Bv,"S'","x"), c(Lv,"M'","z'"), c(Fv,"S","x'")),
                 c("M2", "x2"), c("S2", "z2")),
    "x'" = ,
    "x3" = swap2(swap4(moves, c(Uv,"E","y"), c(Fv,"S","z"), c(Dv,"E'","y'"), c(Bv,"S'","z'")),
                 c("E2", "y2"), c("S2", "z2")),
    "z'" = ,
    "z3" = swap2(swap4(moves, c(Uv,"M","x"), c(Lv,"E","y"), c(Dv,"M'","x'"), c(Rv,"E'","y'")),
                 c("M2", "x2"), c("E2", "y2")),
    "y2'" = ,
    "y2" = swap2(moves, c(Rv,"M","z",Fv,"S","x'"), c(Lv,"M'","z'",Bv,"S'","x")),
    "x2'" = ,
    "x2" = swap2(moves, c(Uv,"E","y",Bv,"S'","z'"), c(Dv,"E'","y'",Fv,"S","z")),
    "z2'" = ,
    "z2" =  swap2(moves, c(Uv,"M","x",Rv,"E'","y'"), c(Dv,"M'","x'",Lv,"E","y")))
  
  if(!is.null(collapse)) outst <- paste(outst, collapse = collapse)
  outst
}

mirMoves <- function(moves, mirror = c("0","UD","DU","RL","LR","FB","BF"), collapse = NULL)
{
  mirror <- match.arg(mirror)
  if(!is.atomic(moves) || !is.character(moves)) 
    stop("argument must be character vector")
  if(length(moves) == 1) {
    moves <- gsub("\\s", "", moves)
    moves <- strsplit(gsub("([URFDLBEMSxyz])", " \\1", moves), " ")[[1]][-1]
  }
  
  moves <- sub("1", "", moves)
  moves <- sub("2'", "2", moves)
  moves <- sub("3", "'", moves)
  
  if(mirror == "0") return(moves)
  
  mvvec <- c("U", "R", "F", "D", "L", "B", "E", "M", "S", "x", "y", "z")
  mvvec2 <- paste0(rep(mvvec, each = 3), rep(c("","2","'"), length(mvvec)))
  if(!all(moves %in% mvvec2)) stop("move not recognized")
  
  swap2 <- function(mv, a, b) {
    smv <- mv
    for(k in 1:length(a)) {
      smv[mv == a[k]] <- b[k]
      smv[mv == b[k]] <- a[k]
    }
    smv
  }

  outst <-switch(EXPR = mirror,
         "UD" = ,
         "DU" = swap2(moves, c("R", "L", "F", "B", "U", "D", "U2", "D2", "M", "S", "x", "z"), 
                      c("R'", "L'", "F'", "B'", "D'", "U'", "D2", "U2", "M'", "S'", "x'", "z'")),
         "RL" = ,
         "LR" = swap2(moves, c("F", "B", "U", "D", "R", "L", "R2", "L2", "E", "S", "y", "z"), 
                      c("F'", "B'", "U'", "D'", "L'", "R'", "L2", "R2", "E'", "S'", "y'", "z'")),
         "FB" = ,
         "BF" = swap2(moves, c("U", "D", "R", "L", "F", "B", "F2", "B2", "M", "E", "x", "y"), 
                      c("U'", "D'", "R'", "L'", "B'", "F'", "B2", "F2", "M'", "E'", "x'", "y'")))
  
  if(!is.null(collapse)) outst <- paste(outst, collapse = collapse)
  outst
}

# permuation cycles of edges and corners
# flip edges and twist corners

cycleEdges <- function(aCube, cycle, right = TRUE, orient = TRUE)
{
  nc <- length(cycle)
  if(!is.cubieCube(aCube))
    stop("aCube must be a cubieCube object")
  if(nc == 0 || !is.atomic(cycle)) 
    stop("cycle must be non-zero length vector")
  if(is.character(cycle)) {
    edge <- c("FR", "FL", "BL", "BR", "UR", "UF", "UL", "UB", "DR", "DF", "DL", "DB")
    if(!all(cycle %in% edge)) stop("edge names not recognized")
    cycle <- as.numeric(factor(cycle, levels = edge))
  }
  if(nc == 1) return(aCube)
  if(any(duplicated(cycle))) stop("cycle must have unique elements")
  if(!right) cycle <- rev(cycle)
  
  tmp <- aCube$ep[cycle[nc]]
  aCube$ep[cycle[-1]] <- aCube$ep[cycle[-nc]]
  aCube$ep[cycle[1]] <- tmp
  if(!orient) {
    tmp <- aCube$eo[cycle[nc]]
    aCube$eo[cycle[-1]] <- aCube$eo[cycle[-nc]]
    aCube$eo[cycle[1]] <- tmp
  }
  aCube
}

cycleCorners <- function(aCube, cycle, right = TRUE, orient = TRUE)
{
  nc <- length(cycle)
  if(!is.cubieCube(aCube))
    stop("aCube must be a cubieCube object")
  if(nc == 0 || !is.atomic(cycle)) 
    stop("cycle must be non-zero length vector")
  if(is.character(cycle)) {
    corner <- c("URF", "UFL", "ULB", "UBR", "DFR", "DLF", "DBL", "DRB")
    if(!all(cycle %in% corner)) stop("corner names not recognized")
    cycle <- as.numeric(factor(cycle, levels = corner))
  }
  if(nc == 1) return(aCube)
  if(any(duplicated(cycle))) stop("cycle must have unique elements")
  if(!right) cycle <- rev(cycle)
  
  tmp <- aCube$cp[cycle[nc]]
  aCube$cp[cycle[-1]] <- aCube$cp[cycle[-nc]]
  aCube$cp[cycle[1]] <- tmp
  if(!orient) {
    tmp <- aCube$co[cycle[nc]]
    aCube$co[cycle[-1]] <- aCube$co[cycle[-nc]]
    aCube$co[cycle[1]] <- tmp
  }
  aCube
}

flipEdges <- function(aCube, flip = 1:12)
{
  if(is.numeric(flip) && !all(flip %in% 1:12)) 
    stop("flip must contain integers in [1,12]")
  if(any(duplicated(flip))) stop("flip must have unique elements")
  if(is.character(flip)) {
    edge <- c("FR", "FL", "BL", "BR", "UR", "UF", "UL", "UB", "DR", "DF", "DL", "DB")
    if(!all(flip %in% edge)) stop("edge names not recognized")
    flip <- as.numeric(factor(flip, levels = edge))
  }
  if(!is.cubieCube(aCube))
    stop("aCube must be a cubieCube object")
  
  aCube$eo[flip] <- (aCube$eo[flip] + 1) %% 2
  aCube
}

twistCorners <- function(aCube, clock = numeric(0), anti = numeric(0))
{
  if(is.numeric(clock) && !all(clock %in% 1:8)) 
    stop("clock must contain integers in [1,8]")
  if(is.numeric(anti) && !all(anti %in% 1:8)) 
    stop("anti must contain integers in [1,8]")
  if(any(duplicated(c(clock,anti)))) stop("clock and anti must have unique elements")
  if(is.character(clock)) {
    corner <- c("URF", "UFL", "ULB", "UBR", "DFR", "DLF", "DBL", "DRB")
    if(!all(clock %in% corner)) stop("corner names not recognized in clock")
    clock <- as.numeric(factor(clock, levels = corner))
  }
  if(is.character(anti)) {
    corner <- c("URF", "UFL", "ULB", "UBR", "DFR", "DLF", "DBL", "DRB")
    if(!all(anti %in% corner)) stop("corner names not recognized in anti")
    anti <- as.numeric(factor(anti, levels = corner))
  }
  if(!is.cubieCube(aCube))
    stop("aCube must be a cubieCube object")
  
  aCube$co[clock] <- (aCube$co[clock] + 1) %% 3
  aCube$co[anti] <- (aCube$co[anti] + 2) %% 3
  aCube
}
