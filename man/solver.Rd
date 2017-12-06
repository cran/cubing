\name{solver}
\alias{solver}
\title{Rubik's Cube Solver}
\description{
  Cube solvers to generate moves to a target cube state. 
}
\usage{
solver(aCube, tCube, type = c("KB", "ZT", "TF"), inv = FALSE, 
    maxMoves = switch(type, KB = 24, ZT = 20, TF = 16), bound = 
	TRUE, collapse = NULL, divide = FALSE, history = FALSE, 
	verbose = FALSE) 
}
\arguments{
  \item{aCube}{A cubieCube object giving the cube to be solved. 
    If \code{tCube} is not specified then the object must be solvable.}
  \item{tCube}{A cubieCube object giving the target state. If not specified,
    then the target state is the solved state. See Details.}
  \item{type}{The type of solver used. KB is Kociemba. ZT is Zemdegs-Twist.
    TF is Twist-Flip.}
  \item{inv}{If \code{TRUE} the moves are inverted. For producing
    random state scrambles.}
  \item{maxMoves}{The maximum number of moves allowed for the search phases
    of the algorithm. The search algorithm may take a long time for smaller
	move requirements. The default value depends on the solver.}
  \item{bound}{By default the \code{maxMoves} value cannot be too small to
    avoid the search algorithm taking an excessively long time or never 
	returning. If \code{bound} is set to \code{FALSE} this safety measure is 
	removed, allowing any value of \code{maxMoves} to be specified. This is
	not recommended unless you know the cube can be solved within a small
	number of moves.}
  \item{collapse}{If not \code{NULL} then the returned moves are output as a 
    single string with \code{collapse} as the separator, rather than a character
	vector of moves. If \code{collapse} is the empty string then a single
	string with no separator is returned.}
  \item{divide}{If \code{TRUE}, a period symbol is placed between the phases
    of the search algorithm and the looks of the last layer.}
  \item{history}{If \code{TRUE} the solver returns a list where the second element 
    gives a matrix object that provides information on the history of the search 
	algorithm. Mainly used for debugging.}
  \item{verbose}{If \code{TRUE} print details of the status of the search. Mainly
    used for debugging.}
}
\details{
  The solver produces a move sequence that brings \code{aCube} to either a solved 
  state or to the target state \code{tCube}. If the target state is specified, then
  \code{invCube(tCube) \%v\% aCube} must be solvable, but the two cubes \code{aCube}
  and \code{tCube} could be unsolvable. See the help file on \code{invCube} for
  more details.

  The KB algorithm is a 2-phase search. The ZT algorithm is similar but allows for
  twisting corners at the end to solve the corner orientation. The TF algorithm
  allows for twisting corners and flipping edges at the end to solve both corner and
  edge orientation. The twisting and flipping procedures are given as attributes in 
  the returned object. If \code{inv} is \code{TRUE}, then they need to be performed 
  at the start from the solved (or target) state.
  
  The ZT and TF solvers may not produce a smaller move count than KB because the aim of 
  the solver is to return any solution that consists of \code{maxMoves} moves or less.
  If smaller move counts are required then \code{maxMoves} should be specified.
  
  These solvers are lightweight in the sense that they use small look-up tables (move 
  tables and prune tables). If \code{maxMoves} is small then it can take a few seconds
  to find a solution.
  
  The look-up tables for a solver are silently loaded into memory the first time the solver 
  is used. The tables are hidden objects that are not visible to the user. If you wish to 
  ensure that all tables are already loaded into memory (for example, if you want to do timing
  comparisons), then you can run each type of solver once, using any cube other than the 
  solved (or target) state.
  
  The solvers will never produce two moves in a row on the same face, but may produce three
  (or even four) moves in a row on opposite faces if this coincides with the break
  between the two search phases. They cannot produce three moves in a row on opposite faces 
  within the same search phase. This behaviour is a design choice in order to minimize second
  phase solutions that are rejected due to move sequences across the phase break.
}
\value{
  A character vector of moves, or a character string if \code{collapse} is not \code{NULL}.
  For ZT the vector (or string) has a twist attribute. For TF the vector (or string) has
  twist and flip attributes.
  
  If history is \code{TRUE}, then a list of length two is returned where the second element
  is a matrix that provides information on the history of the search algorithm.
}
\seealso{\code{\link{getMovesCube}}, \code{\link{invCube}}, \code{\link{invMoves}}, 
  \code{\link{is.solvable}}, \code{\link{scramble}}}

\examples{
aCube <- getCubieCube("EasyCheckerboard")
\dontrun{plot(aCube)}
\dontrun{plot3d(aCube)}
mvs <- solver(aCube, type = "KB")
is.solved(aCube \%v\% getMovesCube(mvs))
}
\keyword{manip}

