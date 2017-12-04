\name{rotate}
\alias{rotate}
\alias{slice}
\title{Perform Rotations and Middle Slice Moves}
\description{
  Functions for rotating the whole cube and performing middle slice (E M S) moves. 
}
\usage{
slice(aCube, smv)
rotate(aCube, rot)
}
\arguments{
  \item{aCube}{A cubieCube object.}
  \item{smv}{A single string giving a middle slice move. Must be one of
     E E1 E2 E2' E3 E' M M1 M2 M2' M3 M' S S1 S2 S2' S3 S'.}
  \item{rot}{A rotation. This can be specified using an integer or a character
    string. If an integer, must be a number between 1 and 24 representing one
	of the 24 possible rotations in space. The number 1 is the no rotation case.
	If a character string, must be one of 0 x x1 x2 x2' x3 x' y y1 y2 y2' y3 y' 
	z z1 z2 z2' z3 z' where the 0 character string represents no rotation. 
	Character strings correspond only to the 10 rotations around the 
	the x, y and z axes (including the no rotation case).}
}
\details{
  These functions are used internally by the \code{move} function for
  performing rotations and middle slice moves, but can also be called directly
  if you only need to perform a single move. In most circumstances it is better
  to use the \code{move} function.
  
  The definition of the E M S middle slice moves is respectively given by D'Uy' L'Rx' B'Fz'.
  In particular, the S slice direction is different to what you may find elsewhere;
  the definition used for S in this package is mathematically consistent with E and M.  
}
\value{
  A cubieCube object.
}
\seealso{\code{\link{move}}}

\examples{
aCube <- getCubieCube("henrysnake")
bCube <- slice(rotate(aCube, "z'"), "M2") 
cCube <- move(aCube, "z'M2")
identical(bCube, cCube)

\dontrun{plot(cCube)}
\dontrun{plot3d(cCube)}
}
\keyword{manip}

