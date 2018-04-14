\name{cubieCube}
\alias{getCubieCube}
\alias{cubieCube}
\alias{as.cubieCube}
\alias{is.cubieCube}
\title{Create and Convert CubieCubes}
\description{
  Creates, converts and tests for cubieCube objects. 
}
\usage{
getCubieCube(pattern = c("Solved","Superflip","EasyCheckerboard","Wire","PlusMinus",
  "Tablecloth","Spiral","SpeedsolvingLogo","VerticalStripes","OppositeCorners",
  "Cross","UnionJack","CubeInTheCube","CubeInACubeInACube","Anaconda","Python",
  "BlackMamba","GreenMamba","FourSpots","SixSpots","Twister","Kilt","Tetris",
  "DontCrossLine","Hi","HiAllAround","AreYouHigh","CUAround","OrderInChaos","Quote",
  "MatchingPictures","3T","LooseStrap","ZZLine","Doubler","CheckerZigzag",
  "ExchangedDuckFeet","StripeDotSolved","Picnic","PercentSign","Mirror",
  "PlusMinusCheck","FacingCheckerboards","OppositeCheckerboards","4Plus2Dots",
  "Rockets","Slash","Pillars","TwistedDuckFeet","RonsCubeInACube","Headlights",
  "CrossingSnake","Cage","4Crosses","Pyraminx","EdgeTriangle","TwistedRings",
  "ExchangedRings","TwistedChickenFeet","ExchangedChickenFeet","CornerPyramid",
  "TwistedPeaks","ExchangedPeaks","SixTwoOne","YinYang","YanYing","HenrysSnake",
  "TwistedCorners","QuickMaths"))
cubieCube(string)
as.cubieCube(aCube)
is.cubieCube(aCube)
}
\arguments{
  \item{pattern}{A character string giving a pattern for the returned cube.
    Approximately seventy different patterns are available. The default pattern
	is the solved cube. The patterns and names are derived from the ruwix.com website.}
  \item{string}{A character string representing the color on each cube sticker. The string
    must contain only the letters URFLBD, representing the color on each face, and may contain 
	any amount of white space. There must be 9 occurrences of each letter, or 8 occurrences if 
	the centre stickers are omitted. A character vector can also be given instead of a character 
	string, with one element for each letter. The sticker template can be displayed using the 
	code at the end of the Examples section below.}
  \item{aCube}{Any object.}
}
\details{
  The \code{is.cubieCube} function returns \code{TRUE} for cubieCube objects and \code{FALSE} 
  otherwise. The \code{as.cubieCube} function converts a cube object to a cubieCube object and
  returns an error for other arguments. 

  The \code{getCubieCube} function creates cubieCube objects using known patterns. The 
  \code{cubieCube} function creates cubieCube objects using colors entered by the user. 
  For alternative ways of creating cubieCube objects, see \code{randCube} and \code{getMovesCube}.
  
  A cubieCube is a list with five vector elements. The first four are cp ep co eo for the
  corner permuation, edge permuation, corner orientation, and edge orientation. The fifth, spor,
  tracks the fixed centres and therefore represents the spatial orientation. It exists to avoid
  recoloring the cube when plotting it after a rotation, middle slice move or wide move. 
  
  A stickerCube object does not hold information on spatial orientation, therefore if
  you convert a cubieCube to a stickerCube, and then convert back to a cubieCube, the 
  spor vector will be reset to \code{1:6}.
  
  The \code{cubieCube} function contains a large amount of bulletproofing to ensure the cube
  has valid cubies that are stickered correctly, but the cube may or may not be solvable. Both 
  stickerCube and cubieCube objects are designed to hold both solvable and
  unsolvable cubes. You can test solvability with the \code{is.solvable} function.
}
\value{
  A logical value for \code{is.cubieCube}. 
  A cubieCube object for all other functions.
}
\seealso{\code{\link{getMovesCube}}, \code{\link{is.solvable}}, 
  \code{\link{randCube}}, \code{\link{stickerCube}}}

\examples{
aCube <- getCubieCube("Wire")
bCube <- cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRL FFBBBBBFF")
cCube <- cubieCube("FBBBUFRRB DUUFRUFFB DBRBFDUFL FRDDDLDDL UUFULLLLL RDRRBLURB")
identical(aCube, bCube)
is.cubieCube(aCube)

\dontrun{plot(aCube)}
\dontrun{plot3D(aCube)}
\dontrun{plot(cCube)}
\dontrun{plot3D(cCube)}

\dontrun{plot(getCubieCube(), numbers = TRUE)}
\dontrun{plot(getCubieCube(), numbers = TRUE, blank = TRUE)}
}
\keyword{manip}

