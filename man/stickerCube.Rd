\name{stickerCube}
\alias{getStickerCube}
\alias{stickerCube}
\alias{as.stickerCube}
\alias{is.stickerCube}
\title{Create and Convert StickerCubes}
\description{
  Creates, converts and tests for stickerCube objects. 
}
\usage{
getStickerCube(pattern = c("Solved","Superflip","EasyCheckerboard","Wire","PlusMinus",
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
stickerCube(string)
as.stickerCube(aCube)
is.stickerCube(aCube)
}
\arguments{
  \item{pattern}{A character string giving a pattern for the returned cube.
    Approximately seventy different patterns are available. The default pattern
	is the solved cube. The patterns and names are derived from the ruwix.com website.}
  \item{string}{A character string representing the color on each cube sticker. The string
    must contain only the letters URFLBD, representing the color on each face, and may contain 
	any amount of white space. There must be 9 occurrences of each letter, or 8 occurrences if 
	the centre stickers are omitted. A character vector can also be given instead of a character 
	string, with one element for each letter. See the package vignette. The sticker template 
	can be displayed using the code at the end of the Examples section below.}
  \item{aCube}{Any object.}
}
\details{
  The \code{is.stickerCube} function returns \code{TRUE} for stickerCube objects and \code{FALSE} 
  otherwise. The \code{as.stickerCube} function converts a cube object to a stickerCube object and
  returns an error for other arguments. 

  The \code{getStickerCube} function creates stickerCube objects using known patterns. The 
  \code{stickerCube} function creates stickerCube objects using colors entered by the user.
  For alternative ways of creating stickerCube objects, see \code{randCube} and \code{getMovesCube}. 
  
  A \code{stickerCube} is a named character vector of length 54 where each element is one of the
  six letters URFLBD. The element named U5 is always equal to the character string U as this 
  represents a centre sticker. The elements named R5 F5 L5 B5 D5 are similarly fixed.
  
  The \code{stickerCube} function contains a large amount of bulletproofing to ensure the cube
  has valid cubies that are stickered correctly, but the cube may or may not be solvable. Both 
  stickerCube and cubieCube objects are designed to hold both solvable and
  unsolvable cubes. You can test solvability with the \code{is.solvable} function.
}
\value{
  A logical value for \code{is.stickerCube}. 
  A stickerCube object for all other functions.
}
\seealso{\code{\link{getMovesCube}}, \code{\link{is.solvable}}, 
  \code{\link{randCube}}, \code{\link{cubieCube}}}

\examples{
aCube <- getStickerCube("Wire")
bCube <- stickerCube("UUUUUUUUU RLLRRRLLR BBFFFFFBBD DDDDDDDDL RRLLLRRLF FBBBBBFF")
cCube <- stickerCube("FBBBUFRRB DUUFRUFFB DBRBFDUFLF RDDDLDDLU UFULLLLLR DRRBLURB")
identical(aCube, bCube)
is.stickerCube(aCube)

\dontrun{plot(aCube)}
\dontrun{plot3d(aCube)}
\dontrun{plot(cCube)}
\dontrun{plot3d(cCube)}

\dontrun{plot(getStickerCube(), numbers = TRUE)}
\dontrun{plot(getStickerCube(), numbers = TRUE, blank = TRUE)}
}
\keyword{manip}

