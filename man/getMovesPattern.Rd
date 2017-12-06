\name{getMovesPattern}
\alias{getMovesPattern}
\title{Get Moves for Patterned Cubes}
\description{
  Extracts the move sequence for a patterned cube. 
}
\usage{
getMovesPattern(pattern = c("Solved","Superflip","EasyCheckerboard","Wire","PlusMinus",
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
}
\arguments{
  \item{pattern}{A character string giving a pattern for the corresponding cube.
    Approximately seventy different patterns are available. The default pattern
	is the solved cube, returning an empty character vector. The patterns and 
	names are derived from the ruwix.com website.}
}
\details{
  This function is mainly for internal use. It is used by the functions
  \code{getCubieCube} and \code{getStickerCube} to produce patterned cubes
  via \code{getMovesCube}.
}
\value{
  A character vector of moves. 
}
\seealso{\code{\link{getCubieCube}}, \code{\link{getMovesCube}}, 
  \code{\link{getStickerCube}}}

\examples{
getMovesPattern()
getMovesPattern("Solved")
getMovesPattern("Wire")
getMovesPattern("UnionJack")
}
\keyword{manip}

