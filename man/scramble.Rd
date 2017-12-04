\name{scramble}
\alias{scramble}
\alias{randCube}
\alias{randMoves}
\title{Generate Random Cubes, Moves and Scrambles}
\description{
  Generate random cubes, random move sequences, and scrambles. 
}

\usage{
randCube(n = 1, cubie = TRUE, solvable = TRUE, drop = TRUE, spor = 1:6)
randMoves(n = 1, nm = 20, drop = TRUE)
scramble(n = 1, state = FALSE, nm = 20, drop = TRUE, type = c("KB", "ZT", 
  "TF", "ZZ", "CFOP"), maxMoves = 24, bound = TRUE)
}
\arguments{
  \item{n}{Number of cubes, move sequences or scrambles to generate.}
  \item{cubie}{If \code{FALSE}, simulate stickerCubes rather than cubieCubes.}
  \item{solvable}{If \code{FALSE}, then solvable and unsolvable cubes can
    be simulated. See Details.}
  \item{drop}{If \code{FALSE}, then a list of one element is returned when \code{n}
    is equal to one.}
  \item{spor}{The spatial orientation vector that is added to each simulated cubieCube.}
  \item{nm}{The number of moves in the move sequence or the scramble. Ignored for
    scrambles if \code{state} is \code{TRUE}.}
  \item{state}{If \code{FALSE} (the default), use a random moves scramble. If
    \code{TRUE}, use a random state scramble.}
  \item{type}{The type of solver used for the random state scramble. This and all
    following arguments are ignored unless \code{state} is \code{TRUE}.}
  \item{maxMoves}{Argument passed to the solver.}
  \item{bound}{Argument passed to the solver.}  
}
\details{
  If \code{solvable} is \code{TRUE}, the \code{randCube} function generates a 
  solvable cube where every solvable state is equally likely. If \code{solvable} 
  is \code{FALSE}, it generates a random cube where every obtainable physical
  construction is equally likely. The resulting cube may or may not be solvable: 
  the chance of it being solvable is only 1 in 12.

  The \code{randMoves} function generates a random move sequence using URFDLB moves,
  restricted so that you cannot get two moves in a row on the same face, or
  three moves in a row on opposing faces. An alternative way of constructing a
  random cube is \code{getMovesCube(randMoves())}. If \code{nm} is 20 or more
  then every solvable state has a non-zero chance of occuring, but the states
  will not be equally likely to occur.  
  
  The \code{scramble} function generates random move scrambles by default (which
  just uses the \code{randMoves} function), but will generate random state
  scrambles when \code{state} is \code{TRUE}.
}
\value{
  A cube object or list of cube objects for \code{randCube}.
  A move sequence or list of move sequences for \code{randMoves} and \code{scramble}.
}
\seealso{\code{\link{getMovesCube}}, \code{\link{invCube}}, \code{\link{invMoves}}, 
  \code{\link{is.solvable}}, \code{\link{solver}}}

\examples{
randCube()
getMovesCube(randMoves())
sapply(randCube(20, solvable = FALSE), is.solvable)
randMoves(5, nm = 25)
scramble(nm = 17)
scramble(state = TRUE, type = "ZT", maxMoves = 24)
}
\keyword{distribution}

