\name{comparison}
\alias{==.cube}
\alias{all.equal.cube}
\title{Logical Comparison for Cube Objects}
\description{
  Determine if two cubes are equal, accounting for recoloring and rotation. 
}
\usage{
\method{==}{cube}(aCube, bCube) 
\method{all.equal}{cube}(target, current, \dots) 
}
\arguments{
  \item{aCube, target}{Any cube object.}
  \item{bCube, current}{Any cube object.}
  \item{\dots}{Not used.}
}
\details{
  Two cubes are defined to be equal via \code{==} if they are the same except perhaps for 
  a recoloring. This means that the permutation and orientation components are the 
  same but the spatial orientation component may be different.
  
  Two cubes are defined to be the same via \code{all.equal} if and only if one cube is equal 
  to the other following any of the 24 rotations of the whole cube (including 
  the no rotation case).
  
  For testing if two cubes are exactly identical, you can use the \R function
  \code{identical}, however the cubes must be of the same type; either both cubieCubes 
  or both stickerCubes.
}
\value{
  A logical value.
}

\seealso{\code{\link{is.solvable}}, \code{\link{is.solved}}}

\examples{
aCube <- randCube()
bCube <- rotate(aCube, "y'")
aCube == bCube
}
\keyword{manip}

