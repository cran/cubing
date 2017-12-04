\name{plot3d.cube}
\alias{plot3d.cube}

\title{Cube Object Interactive 3D Plot}
\description{
  Plots an interactive 3D representation of a cube object using OpenGL.
}
\usage{
\method{plot3d}{cube}(x, colvec = getOption("cubing.colors"), recolor = FALSE, 
   bg = grey(0.8), rand.col = FALSE, size = 0.98, col.interior = grey(0.5), 
   al.interior = 0.4, al.exterior = 1, rinit = 30, bbox = TRUE, bbcolor = 
   "#333377", bbemission = "#333377", bbspecular = "#3333FF", bbshininess = 
   5, bbalpha = 0.5, \dots)
}
\arguments{
  \item{x}{Any cube object.}
  \item{colvec}{Vector of sticker colors. The default is the
    \code{cubing.colors} option.}
  \item{recolor}{If \code{TRUE}, previous rotations are ignored 
    and therefore the cube is recolored.}
  \item{bg}{Background color.}
  \item{rand.col}{If \code{TRUE} then sticker colors are chosen at
    random and \code{colvec} is ignored.}
  \item{size}{Size of the individual cubies. Must be less than one. Values closer
    to one give cubes that look stickerless because the gap between cubies 
	decreases. Smaller sizes give exploded cubes.}
  \item{col.interior}{Color of the cube interior.}
  \item{al.interior}{Alpha value of cube interior.}
  \item{al.exterior}{Alpha value of cube exterior.}
  \item{rinit}{The initial plot is rotated \code{rinit} degrees about the z-axis.}
  \item{bbox}{Use a bounding box?}
  \item{bbcolor}{Bounding box parameter.}
  \item{bbemission}{Bounding box parameter.}
  \item{bbspecular}{Bounding box parameter.}
  \item{bbshininess}{Bounding box parameter.}
  \item{bbalpha}{Bounding box parameter.}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
}
\details{
  This function uses the \R package \bold{rgl} which is an interface to OpenGL.
  The cube can be rotated using a mouse. See the documentation for the \bold{rgl}
  package to explore the large number of options available.

  The \code{animate} function also uses the \bold{rgl} package to produce cubing
  animations.  
}

\seealso{\code{\link{animate}}, \code{\link{plot.cube}}, 
  \code{\link{plot.rotCubes}}, \code{\link{plot.seqCubes}}}

\examples{
aCube <- getCubieCube("superflip")
\dontrun{plot3d(aCube)}
}
\keyword{dynamic}
