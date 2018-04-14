\name{getMovesCube}
\alias{getMovesCube}

\title{Create a Cube for a Move Sequence}
\description{
  Creates a cube that corresponds to a move sequence via post-multiplication.
}
\usage{
getMovesCube(moves = character(0), cubie = TRUE)
}
\arguments{
  \item{moves}{A move sequence; either a single string or a character vector with 
    one element per move. Can include URFDLB face turns. Cannot include rotations, 
	middle slice moves or wide moves.}
  \item{cubie}{If \code{FALSE}, produce a stickerCube rather than a cubieCube.}
}
\details{
  The cube object created by this function represents the application of the move sequence
  by means of post-multiplication with the composition operator \code{\%v\%}. If A
  is a cube and \code{m} is the move sequence, then A \code{\%v\% getMovesCube(m)} is the 
  cube that results from applying the move sequence to A. In particular, if A
  is the solved state then this is just \code{getMovesCube(m)}. If \code{m} represents
  a scramble sequence, then \code{getMovesCube(m)} is the scrambled cube state.

  The move sequence cannot include rotations, middle slice moves or wide moves because 
  these cannot be expressed via post-multiplication. To implement these moves, see the \code{move} and \code{rotate} functions.  
}
\value{
  A cube object.
}

\seealso{\code{\link{\%v\%}}, \code{\link{invMoves}},
  \code{\link{move}}, \code{\link{rotate}}, \code{\link{slice}}}

\examples{
scramb <- "D2F2UF2DR2DBL'BRULRUL2FL'U'"
aCube <- getMovesCube(scramb)
mvs <- "x2D'R'L2'U'FU'F'D'U'U'R'y'R'U'Ry'RU'R'U'RUR'U'R'U'F'UFRU'"
is.solved(move(aCube, mvs))
}
\keyword{manip}
