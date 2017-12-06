\name{plot.cube}
\alias{plot.cube}

\title{Cube Object 2D Plot}
\description{
  Plots a 2D representation of a cube object.
}
\usage{
\method{plot}{cube}(x, colvec = getOption("cubing.colors"), recolor = FALSE,   
   xlab = "", ylab = "", main = "", centres = TRUE, numbers = FALSE, 
   text.size = 1, text.col = "black", rand.col = FALSE, blank = FALSE, 
   \dots) 
}
\arguments{
  \item{x}{Any cube object.}
  \item{colvec}{Vector of sticker colors. The default is the
    \code{cubing.colors} option.}
  \item{recolor}{If \code{TRUE}, previous rotations are ignored 
    and therefore the cube is recolored.}
  \item{xlab, ylab}{Plot labels.}
  \item{main}{Plot title.}
  \item{centres}{Add identifier text to the centre stickers.}
  \item{numbers}{Add identifier text to all stickers.}
  \item{text.size}{Size of text.}
  \item{text.col}{Color of text. The default is black but if you are
    using black stickers then purple is a good choice. White does not 
	read well on yellow stickers.}
  \item{rand.col}{If \code{TRUE} then sticker colors are chosen at
    random and \code{colvec} is ignored.}
  \item{blank}{If \code{TRUE} then all colors are set to ghost white.
    This is designed to create a template plot for the sticker naming 
	schemes.}
  \item{\dots}{Other parameters to be passed through to plotting
    functions.}
}

\seealso{\code{\link{animate}}, \code{\link{plot3d.cube}}, 
  \code{\link{plot.rotCubes}}, \code{\link{plot.seqCubes}}}

\examples{
aCube <- getCubieCube("Superflip")
\dontrun{plot(aCube)}
}
\keyword{hplot}
