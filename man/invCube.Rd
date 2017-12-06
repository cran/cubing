\name{invCube}
\alias{invCube}
\title{Calculate Inverse Cube}
\description{
  Calculates the inverse of a cube. 
}
\usage{
invCube(aCube, edges = TRUE, corners = TRUE)
}
\arguments{
  \item{aCube}{A cubieCube object.}
  \item{edges}{If \code{FALSE}, the inverse is not taken for the edges.}
  \item{corners}{If \code{FALSE}, the inverse is not taken for the corners.}
}
\details{
  Every 3x3x3 cube A has a unique inverse cube A' where AA' and 
  A'A are both equal to the solved state. The cube A does not need to be solvable. 
  Larger cubes do not have unique inverses because larger cubes have indistinct 
  pieces in the centres.

  One use of the inverse is to enable the \code{solver} function to generate
  moves towards a target state that is not the solved state. For an initial 
  cube A and a target state B the solver is applied to B'A. The moves of the
  solver then represent post-multiplication by A'B, which when applied to A
  gives AA'B which is equal to B, the target state. Only B'A needs to be 
  solvable; both A and B could be unsolvable.

  A solvable cube will always remain solvable after the function \code{invCube}
  is applied, even if \code{edges} or \code{corners} is \code{FALSE}. This is
  because the sign (even or odd) of a permutation is the same as the sign of the
  inverse permutation.  
}
\value{
  A cubieCube object.
}
\seealso{\code{\link{\%v\%}}, \code{\link{invMoves}}, 
  \code{\link{is.solvable}}, \code{\link{solver}}}

\examples{
aCube <- getCubieCube("Tetris")
is.solved(aCube \%v\% invCube(aCube))
is.solved(invCube(aCube) \%v\% aCube)

\dontrun{plot(aCube)}
\dontrun{plot(invCube(aCube))}
\dontrun{plot3d(aCube)}
\dontrun{plot3d(invCube(aCube))}
}
\keyword{manip}

