\name{majority}
\alias{majority}
\title{ Determines majority class }
\description{
A function which determines the majority class of a vector (treated as factor).
}
\usage{
majority(x)
}
\arguments{
  \item{x}{ a one dimensional vector }
}
\details{
This function treats the input vector as a factor and determines which level (class)
of the factor is present most often.  If two or more levels tie for majority then a
random selection is made among the ties.
}
\value{
The factor level which occurs most often in x.
}
\author{ Atina Dunlap Brooks }
\seealso{ \code{\link{factor}} }
\examples{
x <- sample( c("a","b","c","d","e"), 10, replace=TRUE )
majority(x)
}
\keyword{ utilities }
