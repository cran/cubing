\name{invMoves}
\alias{invMoves}
\alias{mirMoves}
\alias{rotMoves}
\alias{moveOrder}
\title{Manipulate Move Sequences}
\description{
  Invert, mirror and rotate move sequences, and calculate the order of a 
  move sequence.  
}
\usage{
invMoves(moves, revseq = TRUE, collapse = NULL)
mirMoves(moves, mirror = c("0","UD","DU","RL","LR","FB","BF"), collapse = NULL)
rotMoves(moves, rotation = c("0","x","x1","x3'","y","y1","y3'","z","z1","z3'",
	"x2","x2'","y2","y2'","z2","z2'","x'","x3","x1'","y'","y3","y1'","z'","z3",
	"z1'"), invrot = FALSE, collapse = NULL)
moveOrder(moves)
}
\arguments{
  \item{moves}{A move sequence. Either a character sting, which may include 
    white space, or a character vector where each element is a single move.
	For \code{moveOrder}, can include only URFDLB face turns. For other 
	functions, can include URFDLBEMS face turns, URFDLB wide turns and xyz 
	rotations.}
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
  For \code{moveOrder} an integer value is returned giving the order of the move 
  sequence, which is the number of times it needs to be applied for the solved cube 
  to return to its solved state. The largest order for any sequence is known to be 
  1260; for example, the order of \code{"R U2 D' B D'"} is 1260.
  
  For other functions, a move sequence is returned. The returned move sequence will 
  always use the canonical form for the turn notation: U not U1, U' not U1', and 
  Uw not u for wide turns. However any form may be used for the input.

  The Examples section below demonstrates the relationship between rotated move 
  sequences. If the rotation is r and the rotated move sequence is \code{m}, then the 
  move sequence r\code{m}r' is equivalent to the original. If \code{invrot} is 
  \code{TRUE}, then this becomes r'\code{m}r. 
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

iCube <- getCubieCube("TwistedChickenFeet")
rmv <- rotMoves(mv, rotation = "x")
aCube <- move(iCube, c("x", rmv, "x'"))
bCube <- move(iCube, mv)
identical(aCube, bCube)

moveOrder("RU2D'BD'")
}
\keyword{manip}

