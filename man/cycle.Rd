\name{cycle}
\alias{cycleEdges}
\alias{cycleCorners}
\alias{flipEdges}
\alias{twistCorners}
\title{Cycle and Twist Cubies}
\description{
  Functions for cycling permuatations and altering orientations. 
}
\usage{
flipEdges(aCube, flip = 1:12)
twistCorners(aCube, clock = numeric(0), anti = numeric(0))
cycleEdges(aCube, cycle, right = TRUE, orient = TRUE)
cycleCorners(aCube, cycle, right = TRUE, orient = TRUE)
}
\arguments{
  \item{aCube}{A cubieCube object.}
  \item{flip}{An integer vector subset of \code{1:12} giving the set of edges to flip. 
    By default, all edges are flipped.}
  \item{clock}{An integer vector subset of \code{1:8} giving the set of corners to twist 
    clockwise. By default, none are twisted.}
  \item{anti}{An integer vector subset of \code{1:8} giving the set of corners to twist 
    anti-clockwise. By default, none are twisted.}
  \item{cycle}{An integer vector representing the permutation cycle. See Details.}
  \item{right}{If \code{FALSE}, cycles to the left. See Details.}
  \item{orient}{Controls the orientation change for the permuation cycle. If \code{TRUE}
    each cubie is re-oriented according to the cubie it replaces, so the orientation
    vector does not change. If \code{FALSE} each cubie orientation is fixed so that the
    orientation vector also cycles.}	
}
\details{
  The \code{cycle} vector should be given according to mathematical cycle notation. For
  example, the vector \code{c(5,3,7)} means that the edge at position 5 moves to 3, 3 moves
  to 7 and 7 moves to 5. The vectors \code{c(3,7,5)} and \code{c(7,5,3)} are equivalent.
  The length of the vector is the length of the cycle, and so this example is a 3-cycle.
  If \code{right} is \code{FALSE} it cycles in the opposite direction, so 7 moves to 3,
  3 moves to 5 and 5 moves to 7.
  
  All of these functions can change the solvability of a cube. Solvability of a cube can be 
  tested using the \code{is.solvable} function. For orientation solvability, the sum of the 
  edge orientation vector must be even, and the sum of the corner orientation 
  vector must be divisible by three. For the edge orientation to remain solvable, you must flip an
  even number of edges. For the corner orientation to remain solvable, the difference between
  the number of clockwise and anti-clockwise twists must be divisible by three. For example,
  the number of twists in each direction could be the same (a difference of zero), or you could
  have three clockwise twists and no anti-clockwise twists.

  The sign of a permuation (even or odd) changes under a 2-cycle, which is just a swapping of two 
  elements. In mathematical terminology this is called a transposition. A k-cycle can be written 
  as k-1 transpositions, and therefore a k-cycle will change the sign of a permuatation if and 
  only if k is even. So for a solvable cube to remain solvable, the length of \code{cycle} should
  be odd.
  
  Two binary operators within the package that also impact solvability are \code{\%e\%} and 
  \code{\%c\%}, which are composition operators for only edges and only corners respectively.
  If two cubes A and B are solvable, then A \code{\%e\%} B may be unsolvable because the edge 
  and corner permuations may be of different sign; the orientations will always remain solvable.
  It is also possibe for A \code{\%e\%} B to be solvable but B \code{\%e\%} A to be unsolvable.
  The same reasoning applies to \code{\%c\%}. 
  
  In detail: if A and B have odd permutations, then both A \code{\%e\%} B and B \code{\%e\%} A
  become unsolvable. If A and B have even permutations, then both A \code{\%e\%} B and 
  B \code{\%e\%} A remain solvable. If A has even and B has odd, then A \code{\%e\%} B is
  unsolvable but B \code{\%e\%} A is solvable.
  
}
\value{
  A cubieCube object.
}
\seealso{\code{\link{\%v\%}}, \code{\link{is.solvable}}, \code{\link{is.solved}}, \code{\link{invCube}}}

\examples{
aCube <- getCubieCube("Superflip")
aCube <- flipEdges(aCube, flip = 1:12)
is.solved(aCube)
aCube <- twistCorners(aCube, clock = 3:6, anti = 2)
is.solvable(aCube)
aCube <- cycleEdges(aCube, c(2,10,5,6))
is.solvable(aCube)
}
\keyword{manip}

