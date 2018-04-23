
###################################################

# solver
# [zborowski] [fridrich]
# (getTwist) (getFlip) [getCollPll] [getOllPll]
# (twistflip)
# (zemtwist)  
# (kociemba) 

# (solver_R)
# [zborowski_R] [fridrich_R]
# (kociemba_R) (kociemba2_R)
# (twistflip_R)
# (zemtwist_R)

###################################################

# solvers for KB ZT TF 
# ZZ CFOP solvers currently not implemented
# all internal functions except solver

solver <- function(aCube, tCube, type = c("KB", "ZT", "TF"), 
  inv = FALSE, maxMoves = switch(type, KB = 24, ZT = 20, TF = 16), 
  bound = TRUE, collapse = NULL, divide = FALSE, history = FALSE, verbose = FALSE) 
{
  type <- match.arg(type)
  mvvec <- c("U", "R", "F", "D", "L", "B")
  mvvec <- paste0(rep(mvvec, each = 3), rep(c("","2","'"), length(mvvec)))
  
  if(!missing(tCube)) {
    if(!is.cubieCube(aCube) || !is.cubieCube(tCube)) 
      stop("first two arguments must be cubieCube objects")
    aCube <- invCube(tCube) %v% aCube
  } else {
    if(!is.cubieCube(aCube)) 
      stop("first argument must be a cubieCube object")
  }
  if(!is.solvable(aCube)) {
    stop("cube is not solvable")
  }
  
  if(type == "KB" && is.solved(aCube)) {
    outst <- character(0)
    if(!is.null(collapse)) outst <- ""
    return(outst)
  }
  if(type == "ZT" && is.solved(aCube, co = FALSE)) {
    outst <- character(0)
    if(!is.null(collapse)) outst <- ""
    covec <- aCube$co
    if(!inv) covec <- -covec %% 3L
    attr(outst, "twist") <- covec
    return(outst)
  }
  if(type == "TF" && is.solved(aCube, co = FALSE, eo = FALSE)) {
    outst <- character(0)
    if(!is.null(collapse)) outst <- ""
    covec <- aCube$co; eovec <- aCube$eo
    if(!inv) covec <- -covec %% 3L
    attr(outst, "twist") <- covec
    attr(outst, "flip") <- eovec
    return(outst)
  }
  
  if(verbose) cat("Starting Search Phase One\n")
  if(type == "KB") {
    if(bound && (maxMoves > 30 || maxMoves < 20))
      stop("maxMoves for KB must be in the interval [20,30] unless bound is FALSE")
    init <- c(get.cp(aCube), get.eMSp(aCube), get.eEp(aCube), get.eEb(aCube), get.co(aCube), 
      get.eo(aCube), get.eMt(aCube),get.eEt(aCube), get.eSt(aCube))
    out <- kociemba(init, maxDepth = maxMoves, verbose = verbose)
  } else if(type == "ZT") {
    if(bound && (maxMoves > 27 || maxMoves < 16))
      stop("maxMoves for ZT must be in the interval [16,27] unless bound is FALSE")
    init <- c(get.cp(aCube), get.eMSp(aCube), get.eEp(aCube), get.eEb(aCube), get.co(aCube), 
              get.eo(aCube), get.eMt(aCube),get.eEt(aCube), get.eSt(aCube))
    out <- zemtwist(init, maxDepth = maxMoves, verbose = verbose)
    if(verbose) cat("Getting Twist Vector\n")
    twist <- getTwist(out$search["co", out$depthtotal+1], inv = inv)

  } else if(type == "TF") {
    if(bound && (maxMoves > 23 || maxMoves < 12))
      stop("maxMoves for TF must be in the interval [12,23] unless bound is FALSE")
    init <- c(get.cp(aCube), get.eMSp(aCube), get.eEp(aCube), get.eEb(aCube), get.co(aCube), 
              get.eo(aCube), get.eMt(aCube),get.eEt(aCube), get.eSt(aCube))
    out <- twistflip(init, maxDepth = maxMoves, verbose = verbose)
    if(verbose) cat("Getting Twist and Flip Vectors\n")
    twist <- getTwist(out$search["co", out$depthtotal+1], inv = inv)
    flip <- getFlip(out$search["eo", out$depthtotal+1])
    
  } else if(type == "ZZ") {
    stop("ZZ solver currently not implemented")
    if(bound && (maxMoves > 30 || maxMoves < 18))
      stop("maxMoves for ZZ must be in the interval [22,30] unless bound is FALSE")
    init <- c(get.cDt(aCube), get.eEt(aCube), get.eSiDt(aCube), get.co(aCube), 
              get.eMiDt(aCube), get.eo(aCube), get.cUt(aCube), get.eUt(aCube))
    out <- zborowski(init, maxDepth = maxMoves, verbose = verbose)
    
    # COLL + PLL
    if(verbose) cat("Starting Last Layer\n")
    co <- out$search["co", out$depthtotal+1]
    cU <- out$search["cUt", out$depthtotal+1]
    eU <- out$search["eUt", out$depthtotal+1]
    mvll <- getCollPll(co, cU, eU, mvvec, inv, divide)
    
  } else if(type == "CFOP") {
    stop("CFOP solver currently not implemented")
    if(bound && (maxMoves > 30 || maxMoves < 22))
      stop("maxMoves for CFOP must be in the interval [22,30] unless bound is FALSE")
    init <- c(get.cDt(aCube), get.eEt(aCube), get.eSiDt(aCube), get.co(aCube), 
              get.eMiDt(aCube), get.eo(aCube), get.cUt(aCube), get.eUt(aCube))
    out <- fridrich(init, maxDepth = maxMoves, verbose = verbose)
    
    # OLL + PLL
    if(verbose) cat("Starting Last Layer\n")
    eo <- out$search["eo", out$depthtotal+1]
    co <- out$search["co", out$depthtotal+1]
    cU <- out$search["cUt", out$depthtotal+1]
    eU <- out$search["eUt", out$depthtotal+1]
    mvll <- getOllPll(eo, co, cU, eU, mvvec, inv, divide)
  } 

  axis <- as.integer(out$search["axis",1:out$depthtotal])
  power <- as.integer(out$search["power",1:out$depthtotal])
  if(inv) power <- -power %% 3 + 1
  axis <- 3*(axis-1) + power

  if(!divide) outst <- mvvec[axis] else {
    iv <- seq(len = out$depthA)
    outst <- c(mvvec[axis[iv]],".",mvvec[axis[-iv]])
  }
  if(type == "ZZ") outst <- c(outst, mvll)
  if(inv) outst <- rev(outst)
  if(!is.null(collapse)) outst <- paste(outst, collapse = collapse)
  if(type == "ZT") attr(outst, "twist") <- twist
  if(type == "TF") {
    attr(outst, "twist") <- twist
    attr(outst, "flip") <- flip
  }
  if(verbose) cat("Returning Solution\n\n")
  if(history) return(list(moves = outst, history = out$search[,1:(out$depthtotal+1)]))

  outst
}

zborowski <- function(init, maxDepth, verbose) 
{
  return(init)
}

fridrich <- function(init, maxDepth, verbose) 
{
  return(init)
}

getTwist <- function(twistv, inv) 
{
  twist <- integer(8)
  twistv <- twistv - 1L
  for(i in 1:7) {
    twist[i] <- twistv %% 3L
    twistv <- twistv %/% 3L
  }
  twist[8] <- -sum(twist[-8]) %% 3L
  names(twist) <- c("URF", "UFL", "ULB", "UBR", "DFR", "DLF", "DBL", "DRB")
  if(!inv) twist <- -twist %% 3L
  twist
}

getFlip <- function(flipv)
{
  flip <- integer(12)
  flipv <- flipv - 1L
  for(i in 1:11) {
    flip[i] <- flipv %% 2L
    flipv <- flipv %/% 2L
  }
  flip[12] <- -sum(flip[-12]) %% 2L
  names(flip) <- c("FR", "FL", "BL", "BR", "UR", "UF", "UL", "UB", "DR", "DF", "DL", "DB")
  flip
}

getCollPll <- function(co, cU, eU, mvvec, inv, divide)
{
  cov <- set.co(getCubieCube(), co)$co[1:4]
  cov <- sum(cov[-4] * 3^(0:2)) + 1  # number in [1,27]
  mvoll2 <- getOption("cubing.oll2")[[cov]]
  oll2.mv <- match(mvoll2, mvvec)
  for(j in oll2.mv) {
    co <- mt_co[co, j]
    cU <- mt_c4t[cU, j]
    eU <- mt_e4t[eU, j]
  }
  if(co != 1) warning("OLL algorithm failed to solve corner orientation")

  cUv <- (cU - 1) %% factorial(4) + 1 # number in [1,24]

  mvpll1 <- getOption("cubing.pll1")[[cUv]]
  pll1.mv <- match(mvpll1, mvvec)
  for(j in pll1.mv) {
    cU <- mt_c4t[cU, j]
    eU <- mt_e4t[eU, j]
  }
  if(cU != 1) warning("PLL algorithm failed to solve corner permutation")

  eUv <- (eU - 1) %% factorial(4) + 1 # one of 12 numbers in [1,24]

  mvpll2 <- getOption("cubing.pll2")[[eUv]]
  pll2.mv <- match(mvpll2, mvvec)
  for(j in pll2.mv) {
    eU <- mt_e4t[eU, j]
  }
  if(eU != 1657) warning("PLL algorithm failed to solve edge permutation")

  if(inv) {
    mvoll2 <- invMoves(mvoll2, revseq = FALSE)
    mvpll1 <- invMoves(mvpll1, revseq = FALSE)
    mvpll2 <- invMoves(mvpll2, revseq = FALSE)
  }
  if(!divide) {
    mvll <- c(mvoll2, mvpll1, mvpll2) 
  } else {
    mvll <- c(".", mvoll2, ".", mvpll1, ".", mvpll2)
  }
  mvll
}

getOllPll <- function(eo, co, cU, eU, mvvec, inv, divide)
{
  eov <- set.eo(getCubieCube(), eo)$eo[5:8]
  eov <- sum(eov[-4] * 2^(0:2)) + 1  # number in [1,8]
  mvoll1 <- getOption("cubing.oll1")[[eov]]
  oll1.mv <- match(mvoll1, mvvec)
  for(j in oll1.mv) {
    eo <- mt_eo[eo, j]
    co <- mt_co[co, j]
    cU <- mt_c4t[cU, j]
    eU <- mt_e4t[eU, j]
  }
  if(eo != 1) warning("OLL algorithm failed to solve edge orientation")
  
  cov <- set.co(getCubieCube(), co)$co[1:4]
  cov <- sum(cov[-4] * 3^(0:2)) + 1  # number in [1,27]
  mvoll2 <- getOption("cubing.oll2")[[cov]]
  oll2.mv <- match(mvoll2, mvvec)
  for(j in oll2.mv) {
    co <- mt_co[co, j]
    cU <- mt_c4t[cU, j]
    eU <- mt_e4t[eU, j]
  }
  if(co != 1) warning("OLL algorithm failed to solve corner orientation")
  
  cUv <- (cU - 1) %% factorial(4) + 1 # number in [1,24]
  
  mvpll1 <- getOption("cubing.pll1")[[cUv]]
  pll1.mv <- match(mvpll1, mvvec)
  for(j in pll1.mv) {
    cU <- mt_c4t[cU, j]
    eU <- mt_e4t[eU, j]
  }
  if(cU != 1) warning("PLL algorithm failed to solve corner permutation")
  
  eUv <- (eU - 1) %% factorial(4) + 1 # one of 12 numbers in [1,24]
  
  mvpll2 <- getOption("cubing.pll2")[[eUv]]
  pll2.mv <- match(mvpll2, mvvec)
  for(j in pll2.mv) {
    eU <- mt_e4t[eU, j]
  }
  if(eU != 1657) warning("PLL algorithm failed to solve edge permutation")
  
  if(inv) {
    mvoll1 <- invMoves(mvoll1, revseq = FALSE)
    mvoll2 <- invMoves(mvoll2, revseq = FALSE)
    mvpll1 <- invMoves(mvpll1, revseq = FALSE)
    mvpll2 <- invMoves(mvpll2, revseq = FALSE)
  }
  if(!divide) {
    mvll <- c(mvoll1, mvoll2, mvpll1, mvpll2) 
  } else {
    mvll <- c(".", mvoll1, ".", mvoll2, ".", mvpll1, ".", mvpll2)
  }
  mvll
}

kociemba <- function(init, maxDepth = 26, verbose = FALSE)
{
  # initialize
  
  #search <- matrix(-1L, nrow = 13, ncol = 31)
  #search[,1] <- c(1L, 0L, init, -1L, -1L)
  #search[nrow(search)-1, 2] <- 1L
  
  search <- rep(-1L, 13*31)
  search[seq(1, 13*31, 31)] <- c(1L, 0L, init, -1L, -1L)
  search[11*31 + 2] <- 1L
  
  sout <- .C(kociemba1, 
             mt_co, mt_eo, mt_e4b, mt_cp, mt_e4t, mt_eMSp, mt_eEp,
             pt_eoXeEb, pt_coXeEb, pt_cpXeEp, pt_eMSpXeEp,
             tt_eMSp, tt_eMSp_Map,
             as.integer(maxDepth), as.logical(verbose),
             search = search,
             depthtotal = integer(1),
             depthA = integer(1))[c("search","depthtotal","depthA")]
  
  sout$search <- matrix(sout$search, nrow = 13, ncol = 31, byrow = TRUE)
  colnames(sout$search) <- 0:30
  rownames(sout$search) <- c("axis","power",
    "cp","eMSp","eEp","eEb","co","eo","eMt","eEt","eSt",
    "minDist1","minDist2")
  
  return(sout)
}

zemtwist <- function(init, maxDepth = 22, verbose = FALSE)
{
  # initialize
  
  #search <- matrix(-1L, nrow = 13, ncol = 31)
  #search[,1] <- c(1L, 0L, init, -1L, -1L)
  #search[nrow(search)-1, 2] <- 1L
  
  search <- rep(-1L, 13*31)
  search[seq(1, 13*31, 31)] <- c(1L, 0L, init, -1L, -1L)
  search[11*31 + 2] <- 1L
  
  sout <- .C(zemtwist1, 
             mt_co, mt_eo, mt_e4b, mt_cp, mt_e4t, mt_eMSp, mt_eEp,
             pt_eoXeEb, pt_cpXeEp, pt_eMSpXeEp,
             tt_eMSp, tt_eMSp_Map,
             as.integer(maxDepth), as.logical(verbose),
             search = search,
             depthtotal = integer(1),
             depthA = integer(1))[c("search","depthtotal","depthA")]
  
  sout$search <- matrix(sout$search, nrow = 13, ncol = 31, byrow = TRUE)
  colnames(sout$search) <- 0:30
  rownames(sout$search) <- c("axis","power",
                             "cp","eMSp","eEp","eEb","co","eo","eMt","eEt","eSt",
                             "minDist1","minDist2")
  
  return(sout)
}

twistflip <- function(init, maxDepth = 26, verbose = FALSE)
{
  # initialize
  
  #search <- matrix(-1L, nrow = 13, ncol = 31)
  #search[,1] <- c(1L, 0L, init, -1L, -1L)
  #search[nrow(search)-1, 2] <- 1L
  
  search <- rep(-1L, 13*31)
  search[seq(1, 13*31, 31)] <- c(1L, 0L, init, -1L, -1L)
  search[11*31 + 2] <- 1L
  
  sout <- .C(twistflip1, 
             mt_co, mt_eo, mt_e4b, mt_cp, mt_e4t, mt_eMSp, mt_eEp,
             pt_eEb, pt_cpXeEp, pt_eMSpXeEp,
             tt_eMSp, tt_eMSp_Map,
             as.integer(maxDepth), as.logical(verbose),
             search = search,
             depthtotal = integer(1),
             depthA = integer(1))[c("search","depthtotal","depthA")]
  
  sout$search <- matrix(sout$search, nrow = 13, ncol = 31, byrow = TRUE)
  colnames(sout$search) <- 0:30
  rownames(sout$search) <- c("axis","power",
                             "cp","eMSp","eEp","eEb","co","eo","eMt","eEt","eSt",
                             "minDist1","minDist2")
  
  return(sout)
}

# Pure R solver code for testing purposes

solver_R <- function(aCube, tCube, type = c("KB", "ZT", "TF"), 
                   inv = FALSE, maxMoves = switch(type, KB = 26, ZT = 22, TF = 18), 
                   bound = TRUE, collapse = NULL, divide = FALSE, history = FALSE, verbose = FALSE) 
{
  type <- match.arg(type)
  mvvec <- c("U", "R", "F", "D", "L", "B")
  mvvec <- paste0(rep(mvvec, each = 3), rep(c("","2","'"), length(mvvec)))
  
  if(!missing(tCube)) {
    if(!is.cubieCube(aCube) || !is.cubieCube(tCube)) 
      stop("first two arguments must be cubieCube objects")
    aCube <- invCube(tCube) %v% aCube
  } else {
    if(!is.cubieCube(aCube)) 
      stop("first argument must be a cubieCube object")
  }
  if(!is.solvable(aCube)) {
    stop("cube is not solvable")
  }
  
  if(type == "KB" && is.solved(aCube)) {
    outst <- character(0)
    if(!is.null(collapse)) outst <- ""
    return(outst)
  }
  if(type == "ZT" && is.solved(aCube, co = FALSE)) {
    outst <- character(0)
    if(!is.null(collapse)) outst <- ""
    covec <- aCube$co
    if(!inv) covec <- -covec %% 3
    attr(outst, "twist") <- covec
    return(outst)
  }
  if(type == "TF" && is.solved(aCube, co = FALSE, eo = FALSE)) {
    outst <- character(0)
    if(!is.null(collapse)) outst <- ""
    covec <- aCube$co; eovec <- aCube$eo
    if(!inv) covec <- -covec %% 3
    attr(outst, "twist") <- covec
    attr(outst, "flip") <- eovec
    return(outst)
  }
  
  if(verbose) cat("Starting Search Phase One\n")
  if(type == "KB") {
    if(bound && (maxMoves > 30 || maxMoves < 22))
      stop("maxMoves for KB must be in the interval [22,30] unless bound is FALSE")
    init <- c(get.cp(aCube), get.eMSp(aCube), get.eEp(aCube), get.eEb(aCube), get.co(aCube), 
              get.eo(aCube), get.eMt(aCube),get.eEt(aCube), get.eSt(aCube))
    out <- kociemba_R(init, maxDepth = maxMoves, verbose = verbose)
  } else if(type == "ZT") {
    if(bound && (maxMoves > 27 || maxMoves < 18))
      stop("maxMoves for ZT must be in the interval [18,27] unless bound is FALSE")
    init <- c(get.cp(aCube), get.eMSp(aCube), get.eEp(aCube), get.eEb(aCube), get.co(aCube), 
              get.eo(aCube), get.eMt(aCube),get.eEt(aCube), get.eSt(aCube))
    out <- zemtwist_R(init, maxDepth = maxMoves, verbose = verbose)
    if(verbose) cat("Getting Twist Vector\n")
    twist <- getTwist(out$search["co", out$depthtotal+1], inv = inv)
    
  } else if(type == "TF") {
    if(bound && (maxMoves > 23 || maxMoves < 14))
      stop("maxMoves for TF must be in the interval [14,23] unless bound is FALSE")
    init <- c(get.cp(aCube), get.eMSp(aCube), get.eEp(aCube), get.eEb(aCube), get.co(aCube), 
              get.eo(aCube), get.eMt(aCube),get.eEt(aCube), get.eSt(aCube))
    out <- twistflip_R(init, maxDepth = maxMoves, verbose = verbose)
    if(verbose) cat("Getting Twist and Flip Vectors\n")
    twist <- getTwist(out$search["co", out$depthtotal+1], inv = inv)
    flip <- getFlip(out$search["eo", out$depthtotal+1])
    
  } else if(type == "ZZ") {
    stop("ZZ solver currently not implemented")
    if(bound && (maxMoves > 30 || maxMoves < 18))
      stop("maxMoves for ZZ must be in the interval [22,30] unless bound is FALSE")
    init <- c(get.cDt(aCube), get.eEt(aCube), get.eSiDt(aCube), get.co(aCube), 
              get.eMiDt(aCube), get.eo(aCube), get.cUt(aCube), get.eUt(aCube))
    out <- zborowski_R(init, maxDepth = maxMoves, verbose = verbose)
    
    # COLL + PLL
    if(verbose) cat("Starting Last Layer\n")
    co <- out$search["co", out$depthtotal+1]
    cU <- out$search["cUt", out$depthtotal+1]
    eU <- out$search["eUt", out$depthtotal+1]
    mvll <- getCollPll(co, cU, eU, mvvec, inv, divide)
    
  } else if(type == "CFOP") {
    stop("CFOP solver currently not implemented")
    if(bound && (maxMoves > 30 || maxMoves < 22))
      stop("maxMoves for CFOP must be in the interval [22,30] unless bound is FALSE")
    init <- c(get.cDt(aCube), get.eEt(aCube), get.eSiDt(aCube), get.co(aCube), 
              get.eMiDt(aCube), get.eo(aCube), get.cUt(aCube), get.eUt(aCube))
    out <- fridrich_R(init, maxDepth = maxMoves, verbose = verbose)
    
    # OLL + PLL
    if(verbose) cat("Starting Last Layer\n")
    eo <- out$search["eo", out$depthtotal+1]
    co <- out$search["co", out$depthtotal+1]
    cU <- out$search["cUt", out$depthtotal+1]
    eU <- out$search["eUt", out$depthtotal+1]
    mvll <- getOllPll(eo, co, cU, eU, mvvec, inv, divide)
  } 
  
  axis <- as.integer(out$search["axis",1:out$depthtotal])
  power <- as.integer(out$search["power",1:out$depthtotal])
  if(inv) power <- -power %% 3 + 1
  axis <- 3*(axis-1) + power
  
  if(!divide) outst <- mvvec[axis] else {
    iv <- seq(len = out$depthA)
    outst <- c(mvvec[axis[iv]],".",mvvec[axis[-iv]])
  }
  if(type == "ZZ") outst <- c(outst, mvll)
  if(inv) outst <- rev(outst)
  if(!is.null(collapse)) outst <- paste(outst, collapse = collapse)
  if(type == "ZT") attr(outst, "twist") <- twist
  if(type == "TF") {
    attr(outst, "twist") <- twist
    attr(outst, "flip") <- flip
  }
  if(verbose) cat("Returning Solution\n\n")
  if(history) return(list(moves = outst, history = out$search[,1:(out$depthtotal+1)]))
  
  outst
}

zborowski_R <- function(init, maxDepth, verbose) 
{
  return(init)
}

fridrich_R <- function(init, maxDepth, verbose) 
{
  return(init)
}

kociemba_R <- function(init, maxDepth = 26, verbose = FALSE)
{
  mt_co <- matrix(mt_co, 2187, 18, byrow = TRUE)
  mt_eo <- matrix(mt_eo, 2048, 18, byrow = TRUE)
  mt_e4b <- matrix(mt_e4b, 495, 18, byrow = TRUE)
  mt_cp <- matrix(mt_cp, 40320, 18, byrow = TRUE)
  mt_e4t <- matrix(mt_e4t, 11880, 18, byrow = TRUE)
  mt_eMSp <- matrix(mt_eMSp, 40320, 18, byrow = TRUE)
  mt_eEp <- matrix(mt_eEp, 24, 18, byrow = TRUE)
  tt_eMSp <- aperm(array(tt_eMSp, dim = c(24,24,70)), 3:1)
  
  search <- matrix(-1, nrow = 13, ncol = 31)
  colnames(search) <- 0:30
  rownames(search) <- c("axis","power",
                        "cp","eMSp","eEp","eEb","co","eo","eMt","eEt","eSt",
                        "minDist1","minDist2")
  
  # initialize
  search[,1] <- c(1, 0, init, -1, -1)
  search["minDist1", 2] <- 1
  
  n <- 1
  busy <- FALSE
  depth1 <- 1
  
  n_eEb <- choose(12,4)
  #n_eo <- 2^11
  #n_co <- 3^7
  
  repeat {
    
    repeat {
      
      if (((depth1 - n + 1) > search["minDist1", n + 1]) && !busy) {
        
        if (search["axis",n] == 1 || search["axis",n] == 4) {  
          n <- n + 1
          search["axis",n] <- 2
          search["power",n] <- 1   
        }
        else {  
          n <- n + 1
          search["axis",n] <- 1
          search["power",n] <- 1    
        }
      } else if ((search["power",n] <- search["power",n] + 1) > 3) {
        
        repeat {
          
          if ((search["axis",n] <- search["axis",n] + 1) > 6) { 
            
            if(n == 1) {
              if (depth1 >= maxDepth) {
                stop("Phase 1 depth exceeds maximum")
              } else {
                depth1 <- depth1 + 1
                search["axis",n] <- 1
                search["power",n] <- 1
                busy <- FALSE
                break
              }
            } else {
              n <- n - 1
              busy <- TRUE
              break
            }
          } else {
            search["power",n] <- 1
            busy <- FALSE
          }
          if(n == 1 || (search["axis",n-1] != search["axis",n] && search["axis",n-1] != search["axis",n]+3)) break
          #if(!(n != 1 && (search["axis",n-1] == search["axis",n] || search["axis",n-1] == search["axis",n]+3))) break
        }
      } else {
        busy <- FALSE
      }
      if(!busy) break
    }
    
    # compute new coordinates for move and new minDist1 
    # if minDist1 = 0 then the subgroup is reached
    
    mv <- 3 * (search["axis",n] - 1) + search["power",n]     
    search["eEb", n + 1] <- mt_e4b[search["eEb",n], mv]
    search["co", n + 1] <- mt_co[search["co",n], mv]
    search["eo", n + 1] <- mt_eo[search["eo",n], mv]
    
    search["minDist1", n + 1] <- max(
      pt_eoXeEb[n_eEb * (search["eo", n + 1] - 1L) + search["eEb", n + 1]], 
      pt_coXeEb[n_eEb * (search["co", n + 1] - 1L) + search["eEb", n + 1]])
    
    if(search["minDist1", n + 1] == 0) 
    {  
      search["minDist1", n + 1] <- 100  
      if(n == depth1) {  
        if(verbose) cat("Entering Phase Two: ")
        out <- kociemba2_R(search, depth1, maxDepth,
                           mt_cp, mt_e4t, mt_eMSp, mt_eEp, tt_eMSp)
        search <- out$search; s <- out$s
        if(verbose) {
          if(s == -2) cat("Immediate Return To Phase One\n")
          if(s == -1) cat("Return To Phase One\n")
          if(s >= 0) cat(paste(s, "Move Solution Found\n"))
          flush.console()
        }
        if (s >= 0) {  
          if ((s == depth1) || (search["axis",depth1] != search["axis",depth1+1])) {
            return(list(search = search, depthtotal = s, depthA = depth1))  
          } else {
            if(verbose) cat(paste(s, "Move Solution Rejected Due To Phase Break\n"))
          }
        }
      }
    }
    
  }
  invisible(1)
}

kociemba2_R <- function(search, depth1, maxDepth,
                        mt_cp, mt_e4t, mt_eMSp, mt_eEp, tt_eMSp)
{
  
  n_eEp <- factorial(4)
  #n_eMSp <- factorial(8)
  #n_cp <- factorial(8)
  
  for (i in 1:depth1) {
    mv <- 3 * (search["axis",i] - 1) + search["power",i]
    search["cp", i + 1] <- mt_cp[search["cp",i], mv]
    search["eMt", i + 1] <- mt_e4t[search["eMt",i], mv]
    search["eEt", i + 1] <- mt_e4t[search["eEt",i], mv]
    search["eSt", i + 1] <- mt_e4t[search["eSt",i], mv]
  }
  
  eMbval <- (search["eMt", depth1 + 1] - 1) %/% factorial(4) + 1
  eMbval <- which(tt_eMSp_Map == eMbval)
  eMpval <- (search["eMt", depth1 + 1] - 1) %% factorial(4) + 1
  eSpval <- (search["eSt", depth1 + 1] - 1) %% factorial(4) + 1
  
  search["eEp", depth1 + 1] <- (search["eEt", depth1 + 1] - 1) %% factorial(4) + 1 
  search["eMSp", depth1 + 1] <- tt_eMSp[eMbval, eMpval, eSpval] 
  
  maxDepth2 <- maxDepth - depth1
  if (maxDepth2 < (dd <- max(
    pt_cpXeEp[n_eEp * (search["cp", depth1+1] - 1L) + search["eEp", depth1+1]],
    pt_eMSpXeEp[n_eEp * (search["eMSp", depth1+1] - 1L) + search["eEp", depth1+1]]))) {
    
    return(list(search = search, s = -2)) 
  }
  
  if ((search["minDist2", depth1+1] <- dd) == 0)  
    return(list(search = search, s = depth1))
  
  #initialize phase two
  n <- depth1 + 1
  busy <- FALSE
  depth2 <- 1
  
  search["power", depth1 + 1] <- 0
  search["axis", depth1 + 1] <- 1   
  search["minDist2", n + 1] <- 1    
  
  repeat {
    
    repeat {
      
      if (((depth1 + depth2 - n + 1) > search["minDist2", n + 1]) && !busy) {
        
        if (search["axis",n] == 1 || search["axis",n] == 4) { 
          n <- n + 1
          search["axis",n] <- 2    
          search["power",n] <- 2  
        }
        else {  
          n <- n + 1
          search["axis",n] <- 1   
          search["power",n] <- 1  
        }
      } else if ( if(search["axis",n] == 1 || search["axis",n] == 4) ((search["power",n] <- search["power",n] + 1) > 3) else ((search["power",n] <- search["power",n] + 2) > 3)) {
        
        repeat {
          if ((search["axis",n] <- search["axis",n] + 1) > 6) {
            
            if(n == depth1 + 1) {
              if (depth2 >= maxDepth2) { 
                return(list(search = search, s = -1))
              } else {
                depth2 <- depth2 + 1
                search["axis",n] <- 1
                search["power",n] <- 1
                busy <- FALSE
                break
              }
            } else {
              n <- n - 1
              busy <- TRUE
              break
            }
          } else {
            if(search["axis",n] == 1 || search["axis",n] == 4) {
              search["power",n] <- 1
            } else {
              search["power",n] <- 2
            }
            busy <- FALSE
          }
          #if(!(n != (depth1 + 1) && (search["axis",n-1] == search["axis",n] || search["axis",n-1] == search["axis",n]+3))) break
          if(n == (depth1 + 1) || (search["axis",n-1] != search["axis",n] && search["axis",n-1] != search["axis",n]+3)) break
        }
      } else {
        busy <- FALSE
      }
      
      if(!busy) break
    }
    
    # compute new coordinates for move and new minDist2 
    # if minDist2 = 0 then the cube is solved 
    
    mv <- 3 * (search["axis",n] - 1) + search["power",n]    
    search["cp", n + 1] <- mt_cp[search["cp",n], mv]
    search["eMSp", n + 1] <- mt_eMSp[search["eMSp",n], mv]
    search["eEp", n + 1] <- mt_eEp[search["eEp",n], mv]
    
    search["minDist2", n + 1] = max(
      pt_cpXeEp[n_eEp * (search["cp", n + 1] - 1L) + search["eEp", n + 1]],
      pt_eMSpXeEp[n_eEp * (search["eMSp", n + 1] - 1L) + search["eEp", n + 1]])
    
    if(search["minDist2", n + 1] == 0) break 
  }
  
  return(list(search = search, s = depth1 + depth2))
}

zemtwist_R <- function(init, maxDepth = 22, verbose = FALSE)
{
  mt_co <- matrix(mt_co, 2187, 18, byrow = TRUE)
  mt_eo <- matrix(mt_eo, 2048, 18, byrow = TRUE)
  mt_e4b <- matrix(mt_e4b, 495, 18, byrow = TRUE)
  mt_cp <- matrix(mt_cp, 40320, 18, byrow = TRUE)
  mt_e4t <- matrix(mt_e4t, 11880, 18, byrow = TRUE)
  mt_eMSp <- matrix(mt_eMSp, 40320, 18, byrow = TRUE)
  mt_eEp <- matrix(mt_eEp, 24, 18, byrow = TRUE)
  tt_eMSp <- aperm(array(tt_eMSp, dim = c(24,24,70)), 3:1)
  
  search <- matrix(-1, nrow = 13, ncol = 31)
  colnames(search) <- 0:30
  rownames(search) <- c("axis","power",
    "cp","eMSp","eEp","eEb","co","eo","eMt","eEt","eSt",
    "minDist1","minDist2")
  
  # initialize
  search[,1] <- c(1, 0, init, -1, -1)
  search["minDist1", 2] <- 1    
  
  n <- 1
  busy <- FALSE
  depth1 <- 1
  
  n_eEb <- choose(12,4)
  #n_eo <- 2^11
  #n_co <- 3^7
  
  repeat {
    
    repeat {
      
      if (((depth1 - n + 1) > search["minDist1", n + 1]) && !busy) {
        
        if (search["axis",n] == 1 || search["axis",n] == 4) {  
          n <- n + 1
          search["axis",n] <- 2
          search["power",n] <- 1    
        }
        else {  
          n <- n + 1
          search["axis",n] <- 1
          search["power",n] <- 1    
        }
      } else if ((search["power",n] <- search["power",n] + 1) > 3) { 
        
        repeat {
          
          if ((search["axis",n] <- search["axis",n] + 1) > 6) {
            
            if(n == 1) {
              if (depth1 >= maxDepth) {
                stop("Phase 1 depth exceeds maximum")
              } else {
                depth1 <- depth1 + 1
                search["axis",n] <- 1
                search["power",n] <- 1
                busy <- FALSE
                break
              }
            } else {
              n <- n - 1
              busy <- TRUE
              break
            }
          } else {
            search["power",n] <- 1
            busy <- FALSE
          }
          if(n == 1 || (search["axis",n-1] != search["axis",n] && search["axis",n-1] != search["axis",n]+3)) break
          #if(!(n != 1 && (search["axis",n-1] == search["axis",n] || search["axis",n-1] == search["axis",n]+3))) break
        }
      } else {
        busy <- FALSE
      }
      if(!busy) break
    }
    
    # compute new coordinates for move and new minDist1
    # if minDist1 = 0 then the subgroup is reached
    
    mv <- 3 * (search["axis",n] - 1) + search["power",n]     
    search["eEb", n + 1] <- mt_e4b[search["eEb",n], mv]
    search["eo", n + 1] <- mt_eo[search["eo",n], mv]
    
    search["minDist1", n + 1] <- 
      pt_eoXeEb[n_eEb * (search["eo", n + 1] - 1L) + search["eEb", n + 1]] 
    
    if(search["minDist1", n + 1] == 0) 
    {  
      search["minDist1", n + 1] <- 100
      if(n == depth1) {
        if(verbose) cat("Entering Phase Two: ")
        out <- kociemba2_R(search, depth1, maxDepth,
                           mt_cp, mt_e4t, mt_eMSp, mt_eEp, tt_eMSp)
        search <- out$search; s <- out$s
        if(verbose) {
          if(s == -2) cat("Immediate Return To Phase One\n")
          if(s == -1) cat("Return To Phase One\n")
          if(s >= 0) cat(paste(s, "Move Solution Found\n"))
          flush.console()
        }
        if (s >= 0) {  
          if ((s == depth1) || (search["axis",depth1] != search["axis",depth1+1])) {
            for (i in 1:s) {
              mv <- 3 * (search["axis",i] - 1) + search["power",i]  # move number
              search["co", i + 1] <- mt_co[search["co",i], mv]
            }
            return(list(search = search, depthtotal = s, depthA = depth1))
          } else {
            if(verbose) cat(paste(s, "Move Solution Rejected Due To Phase Break\n"))
          }
        }
      }
    }
    
  }
  invisible(1)
}

twistflip_R <- function(init, maxDepth = 18, verbose = FALSE)
{
  mt_co <- matrix(mt_co, 2187, 18, byrow = TRUE)
  mt_eo <- matrix(mt_eo, 2048, 18, byrow = TRUE)
  mt_e4b <- matrix(mt_e4b, 495, 18, byrow = TRUE)
  mt_cp <- matrix(mt_cp, 40320, 18, byrow = TRUE)
  mt_e4t <- matrix(mt_e4t, 11880, 18, byrow = TRUE)
  mt_eMSp <- matrix(mt_eMSp, 40320, 18, byrow = TRUE)
  mt_eEp <- matrix(mt_eEp, 24, 18, byrow = TRUE)
  tt_eMSp <- aperm(array(tt_eMSp, dim = c(24,24,70)), 3:1)
  
  search <- matrix(-1, nrow = 13, ncol = 31)
  colnames(search) <- 0:30
  rownames(search) <- c("axis","power",
                        "cp","eMSp","eEp","eEb","co","eo","eMt","eEt","eSt",
                        "minDist1","minDist2")
  
  # initialize
  search[,1] <- c(1, 0, init, -1, -1)
  search["minDist1", 2] <- 1    
  
  n <- 1
  busy <- FALSE
  depth1 <- 1
  
  n_eEb <- choose(12,4)
  #n_eo <- 2^11
  #n_co <- 3^7
  
  repeat {
    
    repeat {
      
      if (((depth1 - n + 1) > search["minDist1", n + 1]) && !busy) {
        
        if (search["axis",n] == 1 || search["axis",n] == 4) {  
          n <- n + 1
          search["axis",n] <- 2
          search["power",n] <- 1    
        }
        else {  
          n <- n + 1
          search["axis",n] <- 1
          search["power",n] <- 1    
        }
      } else if ((search["power",n] <- search["power",n] + 1) > 3) {  
        
        repeat {
          
          if ((search["axis",n] <- search["axis",n] + 1) > 6) {  
            
            if(n == 1) {
              if (depth1 >= maxDepth) {
                stop("Phase 1 depth exceeds maximum")
              } else {
                depth1 <- depth1 + 1
                search["axis",n] <- 1
                search["power",n] <- 1
                busy <- FALSE
                break
              }
            } else {
              n <- n - 1
              busy <- TRUE
              break
            }
          } else {
            search["power",n] <- 1
            busy <- FALSE
          }
          if(n == 1 || (search["axis",n-1] != search["axis",n] && search["axis",n-1] != search["axis",n]+3)) break
          #if(!(n != 1 && (search["axis",n-1] == search["axis",n] || search["axis",n-1] == search["axis",n]+3))) break
        }
      } else {
        busy <- FALSE
      }
      if(!busy) break
    }
    
    # compute new coordinates for move and new minDist1
    # if minDist1 = 0 then the subgroup is reached
    
    mv <- 3 * (search["axis",n] - 1) + search["power",n]      
    search["eEb", n + 1] <- mt_e4b[search["eEb",n], mv]
    
    search["minDist1", n + 1] <- pt_eEb[search["eEb", n + 1]] 
    
    if(search["minDist1", n + 1] == 0) 
    {  
      search["minDist1", n + 1] <- 100  
      if(n == depth1) {
        if(verbose) cat("Entering Phase Two: ")
        out <- kociemba2_R(search, depth1, maxDepth,
                            mt_cp, mt_e4t, mt_eMSp, mt_eEp, tt_eMSp)
        search <- out$search; s <- out$s
        if(verbose) {
          if(s == -2) cat("Immediate Return To Phase One\n")
          if(s == -1) cat("Return To Phase One\n")
          if(s >= 0) cat(paste(s, "Move Solution Found\n"))
          flush.console()
        }
        if (s >= 0) {  
          
          if ((s == depth1) || (search["axis",depth1] != search["axis",depth1+1])) {
            for (i in 1:s) {
              mv <- 3 * (search["axis",i] - 1) + search["power",i]  
              search["co", i + 1] <- mt_co[search["co",i], mv]
              search["eo", i + 1] <- mt_eo[search["eo",i], mv]
            }
            return(list(search = search, depthtotal = s, depthA = depth1)) 
          } else {
            if(verbose) cat(paste(s, "Move Solution Rejected Due To Phase Break\n"))
          }
        }
      }
    }
    
  }
  invisible(1)
}


  
