\name{invMoves}
\alias{invMoves}
\alias{mirMoves}
\alias{rotMoves}
\title{Manipulate Move Sequences}
\description{
  Invert, mirror and rotate move sequences. 
}
\usage{
invMoves(moves, revseq = TRUE, collapse = NULL)
mirMoves(moves, mirror = c("0","UD","DU","RL","LR","FB","BF"), collapse = NULL)
rotMoves(moves, rotation =  c("0","x","x1","y","y1","z","z1","x2","x2'","y2","y2'",
  "z2","z2'","x'","x3","y'","y3","z'","z3"), invrot = FALSE, collapse = NULL)
}
\arguments{
  \item{moves}{A move sequence. Either a character sting, which may include 
    white space, or a character vector where each element is a single move.
	Can include URFDLBEMS face turns and xyz rotations.}
  \item{revseq}{If \code{FALSE}, the move sequence is not reversed so that only
    the direction of the turns is altered.}
  \item{mirror}{The mirror to be used. The U/D mirror can be specified using the UD
    or DU character string. Similarly for R/L and F/B.}
  \item{rotation}{The rotation to be used.}
  \item{invrot}{Inverts the direction of the rotation.}
  \item{collapse}{If not \code{NULL} then the returned moves are output as a 
    single string with \code{collapse} as the separator, rather than a character
	vector of moves. If \code{collapse} is the empty string then a single
	string with no separator is returned.}
}
\details{
  The returned move sequence will always use the canonical form for the turn
  direction: U not U1, U2 not U2', U' not U3. However any form may be used for
  the input.

  The Examples section below demonstrates the relationship between rotated move 
  sequences. If the rotation is r and the rotated move sequence is \code{m}, then the 
  move sequence r\code{m}r' is equivalent to the original. If \code{invrot} is \code{TRUE},
  then this becomes r'\code{m}r. 
}
\value{
  A character vector of moves, or a character string if \code{collapse} is 
  not \code{NULL}.
}
\seealso{\code{\link{move}}, \code{\link{invCube}}, 
  \code{\link{rotate}}, \code{\link{scramble}}}

\examples{
mv <- "RB'y'F2MD'"
invMoves(mv)
mirMoves(mv, mirror = "RL")

iCube <- getCubieCube("twchickfeet")
rmv <- rotMoves(mv, rotation = "x")
aCube <- move(iCube, c("x", rmv, "x'"))
bCube <- move(iCube, mv)
identical(aCube, bCube)
}
\keyword{manip}

