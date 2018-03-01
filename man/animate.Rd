\name{animate}
\alias{animate}

\title{Create Cube Animations}
\description{
  Create cubing animation and record png frames using OpenGL.
}
\usage{
animate(aCube, moves, fpt = 8, colvec = getOption("cubing.colors"), recolor = FALSE, 
   bg = grey(0.8), rand.col = FALSE, size = 0.98, col.interior = grey(0.5), 
   al.interior = 0.4, al.exterior = 1, start.delay = 2, move.delay = 0, rinit = 30, 
   bbox = TRUE, bbcolor = "#333377", bbemission = "#333377", bbspecular = "#3333FF", 
   bbshininess = 5, bbalpha = 0.5, movie = NULL, dir = file.path(getwd(), movie), 
   verbose = TRUE, start.fdelay = fpt, end.fdelay = fpt, move.fdelay = 1, \dots)
}
\arguments{
  \item{aCube}{Any cube object.}
  \item{moves}{A move sequence; either a single string or a character vector with 
    one element per move. Can include URFDLBEMS face turns and xyz rotations.}
  \item{fpt}{Number of frames per cube turn (quarter turn or half turn). Must 
    be a non-negative even integer. Whole cube rotations use half the number of frames 
	as a turn.}
  \item{colvec}{Vector of sticker colors. The default is the \code{cubing.colors} option.}
  \item{recolor}{If \code{TRUE}, previous rotations are ignored 
    and therefore the cube is recolored when initially displayed.}
  \item{bg}{Background color.}
  \item{rand.col}{If \code{TRUE} then sticker colors are chosen at
    random and \code{colvec} is ignored.}
  \item{size}{Size of the individual cubies. Must be less than one. Values closer
    to one give cubes that look stickerless because the gap between cubies 
	decreases. Smaller sizes give exploded cubes.}
  \item{col.interior}{Color of the cube interior.}
  \item{al.interior}{Alpha value of cube interior.}
  \item{al.exterior}{Alpha value of cube exterior.}
  \item{start.delay}{The delay in seconds added to the start.}
  \item{move.delay}{The delay in seconds between moves 
    (turns or rotations).}
  \item{rinit}{The initial plot is rotated \code{rinit} degrees about the z-axis.}
  \item{bbox}{Use a bounding box?}
  \item{bbcolor}{Bounding box parameter.}
  \item{bbemission}{Bounding box parameter.}
  \item{bbspecular}{Bounding box parameter.}
  \item{bbshininess}{Bounding box parameter.}
  \item{bbalpha}{Bounding box parameter.}
  \item{movie}{If \code{movie} is a character string, then a png file is saved for every frame,
    using \code{movie} as the base file name. The following arguments are only relevant when
	\code{movie} is a character string.}
  \item{dir}{The directory where the png frames are stored. If the directory does not
    exist then it is created. By default the name of the directory is the same as the base 
	filename and is located within the working directory.}
  \item{verbose}{Print progress on the saving of the png frames?}	
  \item{start.fdelay}{The number of additional repeated frames added to the start.}
  \item{end.fdelay}{The number of additional repeated frames added to the end.}
  \item{move.fdelay}{The number of additional repeated frames between moves 
    (turns or rotations).}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
}
\details{
  This function uses the \R package \bold{rgl} which is an interface to OpenGL.
  During the animation, the cube can be rotated using a mouse, and the rotations
  will be captured in the png frames if \code{movie} is not \code{NULL}. See the 
  documentation for the \bold{rgl} package to explore the large number of options available.
  
  Following the production of the png frames, you can create movies or gifs using
  external utilities. One powerful command line utility is ffmpeg. ImageMagick is
  a software suite which performs similar conversions.

  The \code{plot3D} function also uses the \bold{rgl} package to produce interactive plots 
  for individual cubes.

  For a 2D version of the animate function, see \code{plot.seqCubes}.  
}

\seealso{\code{\link{plot3D.cube}}, \code{\link{plot.cube}}, 
  \code{\link{plot.rotCubes}}, \code{\link{plot.seqCubes}}}

\examples{
scramb <- "D2F2UF2DR2DBL'BRULRUL2FL'U'"
aCube <- getMovesCube(scramb)
mvs <- "x2D'R'L2'U'FU'F'D'U'U'R'y'R'U'Ry'RU'R'U'RUR'U'R'U'F'UFRU'"
\dontrun{animate(aCube, mvs, movie = "ChoWRSolve")}
}
\keyword{dynamic}
