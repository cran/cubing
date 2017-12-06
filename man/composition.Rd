\name{composition}
\alias{\%v\%}
\alias{\%e\%}
\alias{\%c\%}
\title{Composition Operators For Cube Objects}
\description{
  Composition operators for cubieCube objects. 
}
\usage{
aCube \%v\% bCube
aCube \%e\% bCube
aCube \%c\% bCube
}
\arguments{
  \item{aCube}{A cubieCube object.}
  \item{bCube}{A cubieCube object.}
}
\details{
  If A and B are cubes then A \code{\%v\%} B is the composition 
  (or multiplication) of A and B. This means that if \code{a} and 
  \code{b} are move sequences that produce A and B respectively from
  the solved cube \code{I}, then A \code{\%v\%} B is produced using the combined 
  move sequence \code{ab} applied to \code{I}. Typically we just write AB 
  for A \code{\%v\%} B.
  
  Similarly to matrix multiplication, the operator \code{\%v\%} is associative but 
  in general not commutative, with A\code{I} and \code{I}A both equal to A. 
  Every 3x3x3 cube A has a unique inverse cube A' where AA' 
  and A'A are both equal to \code{I}. The inverse cube can be calculated 
  using the \code{invCube} function.  

  Both objects must be cubieCube objects, not stickerCube objects; unlike other functions
  in the package that require a specific cube type, no expicit error checking is performed 
  by these operators.

  The \code{\%e\%} and \code{\%c\%} operators are similar to \code{\%v\%} but they compose 
  only the edges and corners respectively. Use of these operators may create an unsolvable 
  cube from two solvable cubes. They largely exist for internal reasons. See the help page on 
  \code{cycleEdges} for details of their impact on solvability.
}
\value{
  A cubieCube object
}
\seealso{\code{\link{cycleEdges}}, \code{\link{getMovesCube}}, \code{\link{invCube}}, 
  \code{\link{is.solvable}}, \code{\link{move}}}

\examples{
aCube <- getCubieCube("Superflip")
bCube <- getCubieCube("EasyCheckerboard")
aCube \%v\% bCube
}
\keyword{manip}

