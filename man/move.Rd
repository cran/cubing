\name{move}
\alias{move}
\alias{plot.seqCubes}

\title{Moving a Cube and Creating a Move Sequence}
\description{
  Applies moves to a cube and creates a move sequence.
}
\usage{
move(aCube, moves, history = FALSE)
\method{plot}{seqCubes}(x, initial = TRUE, which = 1:length(moves), ask = FALSE, 
    colvec = getOption("cubing.colors"), recolor = FALSE, show.rot = TRUE, 
    title = NULL, cex.title = 1, font.title = 2, ...)
}
\arguments{
  \item{aCube}{A cubieCube object.}
  \item{moves}{A move sequence; either a single string or a character vector with 
    one element per move. Can include URFDLBEMS face turns, URFDLB wide turns and 
	xyz rotations.}
  \item{history}{If \code{TRUE} the output is a list containing the initial cube 
    state and every subsequent cube state created during the move sequence. 
	If \code{FALSE} (the default) only the final cube state is returned.}	
  \item{x}{An object produced by the \code{move} function when \code{history}
    is set to \code{TRUE}.}
  \item{initial}{Plot the initial cube state?}
  \item{which}{If only a subset of subsequent cube states are to be plotted, 
    specify a subset of the numbers \code{1:length(moves)}.}
  \item{ask}{If \code{TRUE}, the user is asked before each plot.}
  \item{colvec}{Vector of sticker colors. The default is the
    \code{cubing.colors} option.}
  \item{recolor}{If \code{TRUE}, the spatial orientation is ignored 
    and therefore the cube is recolored.}
  \item{show.rot}{Should rotation moves be plotted?}
  \item{title}{If specified as a character string, the first plot represents a
    title page with \code{title} in the centre. This is useful for putting a 
	title page on multiple page graphing devices such as pdf.}
  \item{cex.title}{Size of title on title page.}
  \item{font.title}{Font of title on title page.}
  \item{\dots}{Other parameters to be passed through to plotting
    functions.}
}
\details{
  By default \code{move} gives the result of applying the move sequence
  \code{moves} to the cube \code{aCube}. All rotations xyz, face turns
  URFDLBEMS, and wide moves URFDLB are allowed. Moves specifications such 
  as U U1 U' U1' U2 U2' U3 U3' and rotation specifications such as x x1 x' 
  x1' x2 x2' x3 x3' are all allowed. For wide moves, lower case lettering 
  and w notation are both allowed, so u2 and Uw2 are equivalent. If 
  \code{moves} is a single string, it may contain any amount of white space.
  
  The definition of the E M S middle slice moves is respectively given by D'Uy' L'Rx' B'Fz'.
  In particular, the S slice direction is different to what you may find elsewhere;
  the definition used for S in this package is consistent with the rotation directions. 
  
  When \code{history} is \code{TRUE} a list is created of class seqCubes. The
  list contains cubieCube objects. The length of the list is the number of moves 
  plus one, where the first element of the list is the original cube.

  The \code{plot.seqCubes} function plots a list of class seqCubes. It can be
  regarded and a 2D version of the animate function. For permanent recording 
  of the 2D plots for the move sequence, it is helpful to open a multiple page 
  graphing device such as pdf. A pdf file can then be created and used as a flick 
  book.
}
\value{
  For \code{move}, either a cubieCube object, or (if \code{history} is \code{TRUE})
  an object of class \code{seqCubes}, which is a list where each element is a 
  cubieCube. An attribute of the list stores the move sequence.
}

\seealso{\code{\link{animate}}, \code{\link{getMovesCube}}, \code{\link{invMoves}},
  \code{\link{plot.cube}}, \code{\link{rotate}}, \code{\link{slice}}}

\examples{
scramb <- "D2F2UF2DR2DBL'BRULRUL2FL'U'"
aCube <- getMovesCube(scramb)
mvs <- "x2D'R'L2'U'FU'F'D'U'U'R'y'R'U'Ry'RU'R'U'RUR'U'R'U'F'UFRU'"
is.solved(move(aCube, mvs))
sCubes <- move(aCube, mvs, history = TRUE)
\dontrun{plot(sCubes, title = "SeungBeom Cho\nWorld Record Solve\n4.59")}

\dontrun{pdf("SeungBeomCho.pdf")}
\dontrun{plot(sCubes, title = "SeungBeom Cho\nWorld Record Solve\n4.59")}
\dontrun{dev.off()}
}
\keyword{manip}
\keyword{hplot}
