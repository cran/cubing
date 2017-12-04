\name{rotations}
\alias{rotations}
\alias{plot.rotCubes}

\title{Create and Plot All Rotations of a Cube}
\description{
  Creates and plots all 24 possible whole cube rotations.
}
\usage{
rotations(aCube)
\method{plot}{rotCubes}(x, which = 1:24, ask = FALSE,  colvec = 
  getOption("cubing.colors"), recolor = FALSE, \dots)
}
\arguments{
  \item{aCube}{A cubieCube object.}
  \item{x}{An object produced by the \code{rotations} function.}
  \item{which}{If only a subset of the rotations is to be plotted, 
    specify a subset of the numbers \code{1:24}.}
  \item{ask}{If \code{TRUE}, the user is asked before each plot.}
  \item{colvec}{Vector of sticker colors. The default is the
    \code{cubing.colors} option.}
  \item{recolor}{If \code{TRUE}, the spatial orientation is ignored 
    and therefore the cube is recolored.}
  \item{\dots}{Other parameters to be passed through to plotting
    functions.}
}
\details{
  There are 24 possible spatial orientations, including the original 
  no rotation case. The \code{rotations} function produces all of these
  cubes as a list of class \code{rotCubes}, which can be subsequently 
  plotted by \code{plot.rotCubes}.
  
  The 24 cubes produced by \code{rotations} are all related to each 
  other via some rotation and therefore they are all the same as defined
  by the \code{all.equal.cube} function. If the original cube has symmetric 
  properties then the set of 24 may contain equivalent cubes as defined
  by the \code{==.cube} operator. In the case of the solved cube all 24 
  will be equivalent.
}
\value{
  For \code{rotations}, an object of class \code{rotCubes}, which is a 
  list of length 24 where each element is a cubieCube.
}

\seealso{\code{\link{==.cube}}, \code{\link{all.equal.cube}},
  \code{\link{rotate}}}

\examples{
rCubes <- rotations(randCube())
all.equal(rCubes[[5]], rCubes[[10]])
rCubes[[5]] == rCubes[[10]]
\dontrun{plot(rCubes)}
}
\keyword{manip}
\keyword{hplot}
