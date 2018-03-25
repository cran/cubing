\name{read.cubesolve}
\alias{read.cubesolve}

\title{Read Cube Solving Reconstructions}
\description{
  Reads cube solving scrambles and solutions for the cube solves website.
}
\usage{
read.cubesolve(n, warn = FALSE)
}
\arguments{
  \item{n}{A single integer \code{n} giving the id for the solve.}
  \item{warn}{If \code{FALSE}, an error is returned when the id does not
    exist. If \code{TRUE}, a warning is given.}
}
\details{
  The function reads from the html source of the www.cubesolv.es website, and
  may fail if the website subsequently changes. 
  
  Round brackets immediately followed by any single digit are expanded upon 
  reading, for example (R U)4 becomes R U R U R U R U. All other round brackets 
  are removed. Commutator and conjugate notation is not implemented, so square 
  brackets cannot be used and a warning is given if they are found.
  
  The website does not have automated checking and therefore a typographic
  error may lead to non-valid moves (or cubes that do not solve). This most
  commonly occurs when the space between two valid moves is accidentally omitted.  
}
\value{
  A list containing the scramble, solution and description. The scramble and solution
  items are character vectors of moves. The description is a character string. If the 
  scramble or solution (or both) is missing, then length zero vectors are returned
  for these items.   
}

\seealso{\code{\link{getMovesCube}}, \code{\link{invMoves}}, \code{\link{is.solved}},
  \code{\link{move}}}

\examples{
\dontrun{cho <- read.cubesolve(4995)}
\dontrun{aCube <- getMovesCube(cho$scramble)}
\dontrun{mv <- cho$solution}
\dontrun{is.solved(move(aCube, mv))}
}
\keyword{manip}
