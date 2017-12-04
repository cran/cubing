\name{solvable}
\alias{is.solved}
\alias{is.solvable}
\alias{parity}

\title{Solved and Solvability Tests for Cube Objects}
\description{
  Determine if a cube is solved or solvable, and calculate the
  sign of the corner and edge permutations. 
}
\usage{
is.solved(aCube, split = FALSE, co = TRUE, eo = TRUE)
is.solvable(aCube, split = FALSE)
parity(aCube)
}
\arguments{
  \item{aCube}{Any cube object.}
  \item{split}{Split output into logical vector? See Details.}
  \item{co}{If \code{FALSE}, ignore corner orientation.}
  \item{eo}{If \code{FALSE}, ignore edge orientation.}
}
\details{
  The cubieCube and stickerCube objects contain Rubik's cubes that can 
  be physically constructed from properly stickered cubies, but they are 
  not necessarily solvable. These functions test for solved and solvable 
  cubes. The \code{parity} function gives the permuation sign (+1 for even 
  and -1 for odd) for the corner and edge permuations. For a cube to be 
  solvable, the two signs must be the same.
  
  For \code{is.solved}, a logical value for each separate permutation and 
  orientation component will be given if \code{split} is \code{TRUE}.  

  For \code{is.solvable}, logical values corresponding to permuatation 
  parity, edge orientation and corner orientation will be given if 
  \code{split} is \code{TRUE}. The cube is only solvable if all three 
  values are \code{TRUE}. The edge and corner orientation values
  correspond to the fact that if all but one edge (or corner) orientation
  is known, then the orientation of the final edge (or corner) must 
  be fixed for the cube to be solvable. More precisely, the sum of the
  edge orientation vector must be even, and the sum of the corner
  orientation vector must be divisible by three.  
}
\value{
  A logical value or vector for \code{is.solved} and \code{is.solvable}.
  A named integer vector of length two for \code{parity}.
}

\seealso{\code{\link{==.cube}}, \code{\link{randCube}}, \code{\link{solver}}}

\examples{
aCube <- randCube()
is.solvable(aCube)
aCube <- randCube(solvable = FALSE)
is.solvable(aCube)
}
\keyword{manip}

