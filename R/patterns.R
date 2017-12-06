
###################################################

# getStickerCube getCubieCube

###################################################

# get patterned cubes for each representation
# isolated other than single call to getMovesCube in getCubieCube and getStickerCube
# 68 patterns excluding solved case 

getMovesPattern <- function(pattern = c("Solved","Superflip","EasyCheckerboard","Wire","PlusMinus",
  "Tablecloth","Spiral","SpeedsolvingLogo","VerticalStripes","OppositeCorners","Cross","UnionJack",
  "CubeInTheCube","CubeInACubeInACube","Anaconda","Python","BlackMamba","GreenMamba","FourSpots",
  "SixSpots","Twister","Kilt","Tetris","DontCrossLine","Hi","HiAllAround","AreYouHigh","CUAround",
  "OrderInChaos","Quote","MatchingPictures","3T","LooseStrap","ZZLine","Doubler","CheckerZigzag",
  "ExchangedDuckFeet","StripeDotSolved","Picnic","PercentSign","Mirror","PlusMinusCheck",
  "FacingCheckerboards","OppositeCheckerboards","4Plus2Dots","Rockets","Slash","Pillars",
  "TwistedDuckFeet","RonsCubeInACube","Headlights","CrossingSnake","Cage","4Crosses","Pyraminx",
  "EdgeTriangle","TwistedRings","ExchangedRings","TwistedChickenFeet","ExchangedChickenFeet",
  "CornerPyramid","TwistedPeaks","ExchangedPeaks","SixTwoOne","YinYang","YanYing","HenrysSnake",
  "TwistedCorners","QuickMaths"))
{
  pattern <- match.arg(pattern)
  out <- switch(EXPR = pattern,
                "Solved" = character(0),
                "Superflip" = c("U","R2","F","B","R","B2","R","U2","L","B2","R","U'","D'","R2","F","R'","L","B2","U2","F2"),
                "EasyCheckerboard" = c("U2","D2","R2","L2","F2","B2"),
                "Wire" = c("R","L","F","B","R","L","F","B","R","L","F","B","R2","B2","L2","R2","B2","L2"),
                "PlusMinus" = c("U2","R2","L2","U2","R2","L2"),
                "Tablecloth" = c("R","L","U2","F'","U2","D2","R2","L2","F'","D2","F2","D","R2","L2","F2","B2","D","B2","L2"),
                "Spiral" = c("L'","B'","D","U","R","U'","R'","D2","R2","D","L","D'","L'","R'","F","U"),
                "SpeedsolvingLogo" = c("R'","L'","U2","F2","D2","F2","R","L","B2","U2","B2","U2"),
                "VerticalStripes" = c("F","U","F","R","L2","B","D'","R","D2","L","D'","B","R2","L","F","U","F"),
                "OppositeCorners" = c("R","L","U2","F2","D2","F2","R","L","F2","D2","B2","D2"),
                "Cross" = c("R2","L'","D","F2","R'","D'","R'","L","U'","D","R","D","B2","R'","U","D2"),
                "UnionJack" = c("U","F","B'","L2","U2","L2","F'","B","U2","L2","U"),
                "CubeInTheCube" = c("F","L","F","U'","R","U","F2","L2","U'","L'","B","D'","B'","L2","U"),
                "CubeInACubeInACube" = c("U'","L'","U'","F'","R2","B'","R","F","U","B2","U","B'","L","U'","F","U","R","F'"),
                "Anaconda" = c("L","U","B'","U'","R","L'","B","R'","F","B'","D","R","D'","F'"),
                "Python" = c("F2","R'","B'","U","R'","L","F'","L","F'","B","D'","R","B","L2"),
                "BlackMamba" = c("R","D","L","F'","R","L'","D","R'","U","D'","B","U'","R'","D'"),
                "GreenMamba" = c("R","D","R","F","R'","F'","B","D","R'","U'","B'","U","D2"),
                "FourSpots" = c("F2","B2","U","D'","R2","L2","U","D'"),
                "SixSpots" = c("U","D'","R","L'","F","B'","U","D'"),
                "Twister" = c("F","R'","U","L","F'","L'","F","U'","R","U","L'","U'","L","F'"),
                "Kilt" = c("U'","R2","L2","F2","B2","U'","R","L","F","B'","U","F2","D2","R2","L2","F2","U2","F2","U'","F2"),
                "Tetris" = c("L","R","F","B","U'","D'","L'","R'"),
                "DontCrossLine" = c("F2","L2","R2","B2","U2","D2"),
                "Hi" = c("R2","L2","D2","R2","L2","U2"),
                "HiAllAround" = c("U2","R2","F2","U2","D2","F2","L2","U2"),
                "AreYouHigh" = c("L","R'","U2","D2","L'","R","U2","D2","R2","L2"),
                "CUAround" = c("U'","B2","U","L2","D","L2","R2","D'","B'","R","D'","L","R'","B2","U2","F'","L'","U'"),
                "OrderInChaos" = c("B","L2","B'","U2","B","F2","U","L","U","B","U'","R","U'","B","F","U'","R","D","R","B'","U'"),
                "Quote" = c("U2","F2","D2","B2","R2","U2","B2","R2","U2","R2"),
                "MatchingPictures" = c("R'","D2","R","L","D2","L'"),
                "3T" = c("B","U2","L2","F2","R2","F","D2","F2","R2","F'","R2","U2"),
                "LooseStrap" = c("F2","U2","B2","U2","F2","U2"),
                "ZZLine" = c("R","L","U2","R","L'","U2","F2","R2","U2","F2","D2","B2","L2","U2","L2"),
                "Doubler" = c("R","L","U2","R","L'","D2","R2","F2","U2","F2","U2","L2","F2","U2","L2"),
                "CheckerZigzag" = c("R2","L2","F2","B2","U","F2","B2","U2","F2","B2","U"),
                "ExchangedDuckFeet" = c("U","F","R2","F'","D'","R","U","B2","U2","F'","R2","F","D","B2","R","B'"),
                "StripeDotSolved" = c("D","U","B2","F2","D'","U'"),
                "Picnic" = c("D2","R2","L2","F2","B2"),
                "PercentSign" = c("R","L","U2","R","L'","U2","F2","L2","U2","F2","U2","F2","R2","U2","R2"),
                "Mirror" = c("U","D","F2","B2","U'","D'","R2","L2","B2"),
                "PlusMinusCheck" = c("U","D","R2","L2","U","D","R2","L2"),
                "FacingCheckerboards" = c("U2","F2","U2","F2","B2","U2","F2","D2"),
                "OppositeCheckerboards" = c("U","D","R2","L2","U","D'","R2","L2","D2"),
                "4Plus2Dots" = c("F","U2","D2","R","L","U'","D","F","B","R","U2","R2","U2","F2","L2","U2","F2","L2","B2"),
                "Rockets" = c("U","R2","F2","R2","U'","D","F2","R2","F2","D"),
                "Slash" = c("R","L","F","B","R","L","F","B","R","L","F","B"),
                "Pillars" = c("L2","R2","B2","F2"),
                "TwistedDuckFeet" = c("F","R'","B","R","U","F'","L'","F'","U2","L'","U'","D2","B","D'","F","B'","U2"),
                "RonsCubeInACube" = c("F","D'","F'","R","D","F'","R'","D","R","D","L'","F","L","D","R'","F","D'"),
                "Headlights" = c("U2","F2","U2","D2","B2","D2"),
                "CrossingSnake" = c("R","L","U2","R","L'","U2","F2","R2","D2","B2","D2","B2","L2","D2","R2"),
                "Cage" = c("L","U","F2","R","L'","U2","B'","U","D","B2","L","F","B'","R'","L","F'","R"),
                "4Crosses" = c("F2","B2","R","F2","B2","R","F2","B2","R","F2","B2","R","F2","B2","R","F2","B2","R"),
                "Pyraminx" = c("D","L'","U","R'","B'","R","B","U2","D","B","D'","B'","L","U","D'"),
                "EdgeTriangle" = c("U","B2","U'","F'","U'","D","L'","D2","L","U","D'","F","D'","L2","B2","D'"),
                "TwistedRings" = c("F","D","F'","D2","L'","B'","U","L","D","R","U","L'","F'","U","L","U2"),
                "ExchangedRings" = c("B'","U'","B'","L'","D","B","U","D2","B","U","L","D'","L'","U'","L2","D"),
                "TwistedChickenFeet" = c("F","L'","D","F'","U'","B","U","F","U'","F","R'","F2","L","U'","R'","D2"),
                "ExchangedChickenFeet" = c("F","L'","D'","B'","L","F","U","F'","D'","F","L2","B'","R'","U","L2","D'","F"),
                "CornerPyramid" = c("U'","D","B","R'","F","R","B'","L'","F'","B","L","F","R'","B'","R","F'","U'","D"),
                "TwistedPeaks" = c("F","B'","U","F","U","F","U","L","B","L2","B'","U","F'","L","U","L'","B"),
                "ExchangedPeaks" = c("F","U2","L","F","L'","B","L","U","B'","R'","L'","U","R'","D'","F'","B","R2"),
                "SixTwoOne" = c("U","B2","D2","L","B'","L'","U'","L'","B","D2","B2"),
                "YinYang" = c("R","L","B","F","R","L","U'","D'","F'","B'","U","D"),
                "YanYing" = c("L","R","F","B","U'","D'","L'","R'"),
                "HenrysSnake" = c("R2","F2","U'","D'","B2","L2","F2","L2","U","D","R2","F2"),
                "TwistedCorners" = c("F","U","D","R2","F2","U'","B","U","B","U","R2","B'","U","L2","U","B2","U2","F2","L2","U'"),
                "QuickMaths" = c("R","L","F2","U2","R'","L'","F2","U2","R2","F2","U","R2","L2","F2","B2","D'"))
  out
}

getCubieCube <- function(pattern = c("Solved","Superflip","EasyCheckerboard","Wire","PlusMinus",
  "Tablecloth","Spiral","SpeedsolvingLogo","VerticalStripes","OppositeCorners","Cross","UnionJack",
  "CubeInTheCube","CubeInACubeInACube","Anaconda","Python","BlackMamba","GreenMamba","FourSpots",
  "SixSpots","Twister","Kilt","Tetris","DontCrossLine","Hi","HiAllAround","AreYouHigh","CUAround",
  "OrderInChaos","Quote","MatchingPictures","3T","LooseStrap","ZZLine","Doubler","CheckerZigzag",
  "ExchangedDuckFeet","StripeDotSolved","Picnic","PercentSign","Mirror","PlusMinusCheck",
  "FacingCheckerboards","OppositeCheckerboards","4Plus2Dots","Rockets","Slash","Pillars",
  "TwistedDuckFeet","RonsCubeInACube","Headlights","CrossingSnake","Cage","4Crosses","Pyraminx",
  "EdgeTriangle","TwistedRings","ExchangedRings","TwistedChickenFeet","ExchangedChickenFeet",
  "CornerPyramid","TwistedPeaks","ExchangedPeaks","SixTwoOne","YinYang","YanYing","HenrysSnake",
  "TwistedCorners","QuickMaths")) 
{
  out <- getMovesCube(getMovesPattern(pattern), cubie = TRUE)
  out
}

getStickerCube <- function(pattern = c("Solved","Superflip","EasyCheckerboard","Wire","PlusMinus",
  "Tablecloth","Spiral","SpeedsolvingLogo","VerticalStripes","OppositeCorners","Cross","UnionJack",
  "CubeInTheCube","CubeInACubeInACube","Anaconda","Python","BlackMamba","GreenMamba","FourSpots",
  "SixSpots","Twister","Kilt","Tetris","DontCrossLine","Hi","HiAllAround","AreYouHigh","CUAround",
  "OrderInChaos","Quote","MatchingPictures","3T","LooseStrap","ZZLine","Doubler","CheckerZigzag",
  "ExchangedDuckFeet","StripeDotSolved","Picnic","PercentSign","Mirror","PlusMinusCheck",
  "FacingCheckerboards","OppositeCheckerboards","4Plus2Dots","Rockets","Slash","Pillars",
  "TwistedDuckFeet","RonsCubeInACube","Headlights","CrossingSnake","Cage","4Crosses","Pyraminx",
  "EdgeTriangle","TwistedRings","ExchangedRings","TwistedChickenFeet","ExchangedChickenFeet",
  "CornerPyramid","TwistedPeaks","ExchangedPeaks","SixTwoOne","YinYang","YanYing","HenrysSnake",
  "TwistedCorners","QuickMaths")) 
{
  out <- getMovesCube(getMovesPattern(pattern), cubie = FALSE)
  out
}




