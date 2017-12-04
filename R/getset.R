
###################################################

# (get.co) (get.eo) (get.cp) (get.ep)   
# (set.co) (set.eo) (get.cp) (get.ep) 
# (get.cZo) (get.cDo) (get.cUo)
# (set.cZo) (set.cDo) (set.cUo) 
# (get.cZt) (get.cUt) (get.cDt) 
# (set.cZt) (set.cUt) (set.cDt) 

# (get.eZt) (get.eEt) (get.eUt) (get.eDt) (get.eMt) (get.eSt) (get.eMiDt) (get.eSiDt)
# (set.eZt) (set.eEt) (set.eUt) (set.eDt) (set.eMt) (set.eSt) (set.eMiDt) (set.eSiDt)
# (get.eZb) (get.eEb) (get.eUb) (get.eDb) (get.eMb) (get.eSb)
# (set.eZb) (set.eEb) (set.eUb) (set.eDb) (set.eMb) (set.eSb)

# (get.eZp) (get.eEp) (get.eUp) (get.eDp) (get.eMp) (get.eSp) (get.eUDp) (get.eMSp)
# (set.eZp) (set.eEp) (set.eUp) (set.eDp) (set.eMp) (set.eSp) (set.eUDp) (set.eMSp)

###################################################

# functions for coords to cubie (set) and cubie to coords (get) 
# get functions return integers
# all functions are internal
# several functions are currently unused 

get.co <- function(aCube)
{
  n_corner <- 8
  as.integer(sum(aCube$co[-n_corner] * 3^(0:6)) + 1)
}
set.co <- function(aCube, val) 
{ 
  n_corner <- 8
  if(val > 3^7) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  for(i in 1:(n_corner-1)) {
    aCube$co[i] <- val %% 3
    val <- val %/% 3
  }
  aCube$co[n_corner] <- -sum(aCube$co[-n_corner]) %% 3
  aCube
}

## unrestricted orientation
get.cZo <- function(aCube, id)
{
  #id <- match(id, aCube$cp)
  n_icorner <- length(id)
  as.integer(sum(aCube$co[id] * 3^(0:(n_icorner-1))) + 1)
}
set.cZo <- function(aCube, val, id) 
{ 
  #id <- match(id, aCube$cp)
  n_icorner  <- length(id)
  if(val > 3^n_icorner) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  for(i in id) {
    aCube$co[i] <- val %% 3
    val <- val %/% 3
  }
  aCube
}

get.cDo <- function(aCube) get.cZo(aCube, id = 5:8)
set.cDo <- function(aCube) set.cZo(aCube, id = 5:8)
get.cUo <- function(aCube) get.cZo(aCube, id = 1:4)
set.cUo <- function(aCube) set.cZo(aCube, id = 1:4)

get.eo <- function(aCube) 
{
  n_edge <- 12
  as.integer(sum(aCube$eo[-n_edge] * 2^(0:10)) + 1)
}
set.eo <- function(aCube, val) 
{ 
  n_edge <- 12
  if(val > 2^11) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  for(i in 1:(n_edge-1)) {
    aCube$eo[i] <- val %% 2
    val <- val %/% 2
  }
  aCube$eo[n_edge] <- -sum(aCube$eo[-n_edge]) %% 2
  aCube
}

get.cp <- function(aCube) 
{
  n_corner <- 8
  mvec <- numeric(n_corner)
  for(i in 1:n_corner) mvec[i] <- sum(aCube$cp[i] > aCube$cp[i:n_corner])
  as.integer(sum(mvec * factorial((n_corner-1):0)) + 1)
}
set.cp <- function(aCube, val) 
{ 
  n_corner <- 8
  if(val > factorial(n_corner)) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  mvec <- numeric(n_corner)
  for(i in 1:n_corner) {
    mvec[i] <- val %% i
    val <- val %/% i
  }
  mvec <- rev(mvec) + 1
  indx <- 1:n_corner
  for(i in 1:n_corner) {
    aCube$cp[i] <- indx[mvec[i]]
    indx <- indx[-mvec[i]]
  }
  aCube
}

get.cZt <- function(aCube, id) 
{
  n_icorner <- length(id)
  pos <- which(aCube$cp %in% id)
  vala <- sum(choose(pos - 1, 1:n_icorner))
  
  cornerVec <- aCube$cp[pos]  
  mvec <- numeric(n_icorner)
  for(i in 1:n_icorner) mvec[i] <- sum(cornerVec[i] > cornerVec[i:n_icorner])
  valb <- sum(mvec * factorial((n_icorner-1):0))
  
  as.integer(factorial(n_icorner) * vala + valb + 1)
} 
set.cZt <- function(aCube, val, id) 
{
  n_icorner <- length(id)
  if(val > prod(8:(8 - n_icorner + 1))) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  vala <- val %/% factorial(n_icorner)     # choose(8, n_icorner)
  valb <- val %% factorial(n_icorner)    # factorial(n_icorner)
  
  # permutation
  mvec <- numeric(n_icorner)
  for(i in 1:n_icorner) {
    mvec[i] <- valb %% i
    valb <- valb %/% i
  }
  mvec <- rev(mvec) + 1
  cornerVec <- numeric(n_icorner)
  indx <- id
  for(i in 1:n_icorner) {
    cornerVec[i] <- indx[mvec[i]]
    indx <- indx[-mvec[i]]
  }
  
  # combination and remaining corners
  # keeping remaining in-place corners fixed
  
  x <- n_icorner
  pos <- npos <- which(aCube$cp %in% id)
  tmp_corners <- aCube$cp
  for (i in 8:1) {
    if (vala >= choose(i-1, x)) {
      aCube$cp[i] <- cornerVec[x]
      vala <- vala - choose(i-1, x)
      npos[x] <- i
      x <- x - 1
    } 
  }
  
  apos <- npos[match(npos, pos, 0L) == 0L]
  bpos <- pos[match(pos, npos, 0L) == 0L]
  aCube$cp[bpos] <- tmp_corners[apos]
  aCube
}

get.cUt <- function(aCube) get.cZt(aCube, id = 1:4)
set.cUt <- function(aCube, val) set.cZt(aCube, val, id = 1:4)
get.cDt <- function(aCube) get.cZt(aCube, id = 5:8)
set.cDt <- function(aCube, val) set.cZt(aCube, val, id = 5:8)

get.ep <- function(aCube) 
{
  n_edge <- 12
  mvec <- numeric(n_edge)
  for(i in 1:n_edge) mvec[i] <- sum(aCube$ep[i] > aCube$ep[i:n_edge])
  as.integer(sum(mvec * factorial((n_edge-1):0)) + 1)
}
set.ep <- function(aCube, val) 
{ 
  n_edge <- 12
  if(val > factorial(n_edge)) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  mvec <- numeric(n_edge)
  for(i in 1:n_edge) {
    mvec[i] <- val %% i
    val <- val %/% i
  }
  mvec <- rev(mvec) + 1
  indx <- 1:n_edge
  for(i in 1:n_edge) {
    aCube$ep[i] <- indx[mvec[i]]
    indx <- indx[-mvec[i]]
  }
  aCube
}

get.eZt <- function(aCube, id) 
{
  n_iedge <- length(id)
  pos <- which(aCube$ep %in% id)
  vala <- sum(choose(pos - 1, 1:n_iedge))
  
  edgeVec <- aCube$ep[pos]  
  mvec <- numeric(n_iedge)
  for(i in 1:n_iedge) mvec[i] <- sum(edgeVec[i] > edgeVec[i:n_iedge])
  valb <- sum(mvec * factorial((n_iedge-1):0))
  
  as.integer(factorial(n_iedge) * vala + valb + 1)
} 
set.eZt <- function(aCube, val, id) 
{
  n_iedge <- length(id)
  if(val > prod(12:(12 - n_iedge + 1))) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  vala <- val %/% factorial(n_iedge)     # choose(12, n_iedge)
  valb <- val %% factorial(n_iedge)    # factorial(n_iedge)
  
  # permutation
  mvec <- numeric(n_iedge)
  for(i in 1:n_iedge) {
    mvec[i] <- valb %% i
    valb <- valb %/% i
  }
  mvec <- rev(mvec) + 1
  edgeVec <- numeric(n_iedge)
  indx <- id
  for(i in 1:n_iedge) {
    edgeVec[i] <- indx[mvec[i]]
    indx <- indx[-mvec[i]]
  }
  
  # combination and remaining edges
  # keeping remaining in-place edges fixed
  
  x <- n_iedge
  pos <- npos <- which(aCube$ep %in% id)
  tmp_edges <- aCube$ep
  for (i in 12:1) {
    if (vala >= choose(i-1, x)) {
      aCube$ep[i] <- edgeVec[x]
      vala <- vala - choose(i-1, x)
      npos[x] <- i
      x <- x - 1
    } 
  }
  
  apos <- npos[match(npos, pos, 0L) == 0L]
  bpos <- pos[match(pos, npos, 0L) == 0L]
  aCube$ep[bpos] <- tmp_edges[apos]
  aCube
}

get.eEt <- function(aCube) get.eZt(aCube, id = 1:4)
set.eEt <- function(aCube, val) set.eZt(aCube, val, id = 1:4)
get.eUt <- function(aCube) get.eZt(aCube, id = 5:8)
set.eUt <- function(aCube, val) set.eZt(aCube, val, id = 5:8)
get.eDt <- function(aCube) get.eZt(aCube, id = 9:12)
set.eDt <- function(aCube, val) set.eZt(aCube, val, id = 9:12)
get.eMt <- function(aCube) get.eZt(aCube, id = c(6,8,10,12))
set.eMt <- function(aCube, val) set.eZt(aCube, val, id = c(6,8,10,12))
get.eSt <- function(aCube) get.eZt(aCube, id = c(5,7,9,11))
set.eSt <- function(aCube, val) set.eZt(aCube, val, id = c(5,7,9,11))

get.eMiDt <- function(aCube) get.eZt(aCube, id = c(10,12))
set.eMiDt <- function(aCube, val) set.eZt(aCube, val, id = c(10,12))
get.eSiDt <- function(aCube) get.eZt(aCube, id = c(9,11))
set.eSiDt <- function(aCube, val) set.eZt(aCube, val, id = c(9,11))

get.eZb <- function(aCube, id) 
{
  n_iedge <- length(id)
  pos <- which(aCube$ep %in% id)
  as.integer(sum(choose(pos - 1, 1:n_iedge)) + 1)
}
set.eZb <- function(aCube, val, id) 
{ 
  n_iedge <- length(id)
  if(val > choose(12,n_iedge)) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  
  # keeping remaining in-place edges fixed
  # maintaining ordering of edges
  
  nE <- n_iedge 
  tmp_edges <- aCube$ep
  pos <- npos <- which(aCube$ep %in% id)
  edgeVec <- aCube$ep[pos]
  for (i in 12:1) {
    if (val >= choose(i-1, nE)) {
      aCube$ep[i] <- edgeVec[nE]
      val <- val - choose(i-1, nE)
      npos[nE] <- i
      nE <- nE - 1
    } 
  }
  apos <- npos[match(npos, pos, 0L) == 0L]
  bpos <- pos[match(pos, npos, 0L) == 0L]
  aCube$ep[bpos] <- tmp_edges[apos]
  aCube
}

get.eEb <- function(aCube) get.eZb(aCube, id = 1:4)
set.eEb <- function(aCube, val) set.eZb(aCube, val, id = 1:4)
get.eUb <- function(aCube) get.eZb(aCube, id = 5:8)
set.eUb <- function(aCube, val) set.eZb(aCube, val, id = 5:8)
get.eDb <- function(aCube) get.eZb(aCube, id = 9:12)
set.eDb <- function(aCube, val) set.eZb(aCube, val, id = 9:12)
get.eMb <- function(aCube) get.eZb(aCube, id = c(6,8,10,12))
set.eMb <- function(aCube, val) set.eZb(aCube, val, id = c(6,8,10,12))
get.eSb <- function(aCube) get.eZb(aCube, id = c(5,7,9,11))
set.eSb <- function(aCube, val) set.eZb(aCube, val, id = c(5,7,9,11))

get.eZp <- function(aCube, id) 
{
  n_iedge <- length(id)
  pos <- which(aCube$ep %in% id) 
  edgeVec <- aCube$ep[pos]  
  mvec <- numeric(n_iedge)
  for(i in 1:n_iedge) mvec[i] <- sum(edgeVec[i] > edgeVec[i:n_iedge])
  as.integer(sum(mvec * factorial((n_iedge-1):0)) + 1)
}
set.eZp <- function(aCube, val, id) 
{ 
  n_iedge <- length(id)
  if(val > factorial(n_iedge)) stop("value too large")
  if(val <= 0) stop("value must be positive integer")
  val <- val - 1
  
  mvec <- numeric(n_iedge)
  for(i in 1:n_iedge) {
    mvec[i] <- val %% i
    val <- val %/% i
  }
  mvec <- rev(mvec) + 1
  indx <- id
  
  pos <- which(aCube$ep %in% id)
  for(i in 1:n_iedge) {
    aCube$ep[pos[i]] <- indx[mvec[i]]
    indx <- indx[-mvec[i]]
  }
  aCube
}

get.eEp <- function(aCube) get.eZp(aCube, id = 1:4)
set.eEp <- function(aCube, val) set.eZp(aCube, val, id = 1:4)
get.eUp <- function(aCube) get.eZp(aCube, id = 5:8)
set.eUp <- function(aCube, val) set.eZp(aCube, val, id = 5:8)
get.eDp <- function(aCube) get.eZp(aCube, id = 9:12)
set.eDp <- function(aCube, val) set.eZp(aCube, val, id = 9:12)
get.eMp <- function(aCube) get.eZp(aCube, id = c(6,8,10,12))
set.eMp <- function(aCube, val) set.eZp(aCube, val, id = c(6,8,10,12))
get.eSp <- function(aCube) get.eZp(aCube, id = c(5,7,9,11))
set.eSp <- function(aCube, val) set.eZp(aCube, val, id = c(5,7,9,11))
get.eUDp <- get.eMSp <- function(aCube) get.eZp(aCube, id = 5:12)
set.eUDp <- set.eMSp <- function(aCube, val) set.eZp(aCube, val, id = 5:12)




