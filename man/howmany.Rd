\name{howmany}
\alias{howmany}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Number of correct rejections, for independent test statistics }
\description{
  Lower bounds for the number of correct rejections, for independent
  test statistics.
}
\usage{
howmany(pvalues, alpha = 0.05, cutoff = 0.05/length(pvalues))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pvalues}{ a numeric vector of p-values  }
  \item{alpha}{ the level, a scalar in [0,1]}
  \item{cutoff}{ a scalar in [0,1] }
}
\details{
  When testing multiple hypotheses simultaneously (test statistics are
  supposed to be independent), a quantity of
  interest is the number of correctly rejected hypotheses. 
  Given a list of p-values, the function provides a 
  lower bound for the number of correct rejections, which is
  simultaneously valid for all possible number of rejections.
  The bound is monotonically increasing with the number of made
  rejections.

  The level is asymptotically valid (for a large number of tested
  hypotheses). To ensure better small sample behaviour, it is
  recommended to truncate p-values by setting a non-zero value of \code{cutoff}.
  For a value c of \code{cutoff}, p-values below c are set to c.
  
}
\value{
  An object of class \code{howmany}, for which summary, plot, and print
  methods are available. 
  
  The lower bound for the number of correct rejections (as a function of
  the number of rejections) can be accessed with the function \code{\link{lowerbound}}.
}
\references{ N. Meinshausen and J. Rice (2006) "Estimating the
  proportion of false discoveries among a large number of independently
  tested hypotheses", Annals of Statistics 34(1) }
  
\author{ Nicolai Meinshausen, \email{nicolai@stat.math.ethz.ch} }


\seealso{  \code{\link{lowerbound}} for extracting the number of
  correct rejections (as a function of the number of made rejections).

\code{\link{howmany_dependent}} for similar functionality for multiple
tests of associations, where test statistics can be dependent.}
\examples{
##  create a list of pvalues,
##  of which 1000 are uniform on [0,1]
##  (1000 true null hypotheses),
##  and 200 follow a (truncated) chi-squared distribution
##  (200 false null hypotheses).
pvalues <- c(   runif(1000),   pmin(1,0.05*rchisq(200,df=1))  )

## compute object of class 'howmany' and print the result
(object <- howmany(pvalues))

## extract the lower bound
(lower <- lowerbound(object))

## plot the result
plot(object)

## for comparison: number of rejections with Bonferroni's correction
(bonf <- sum( pvalues < (0.05/1200) ))

}
\keyword{htest}