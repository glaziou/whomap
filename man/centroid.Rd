\name{centroid}
\alias{centroid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Internal function 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
centroid(x1, y1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{y1}{
%%     ~~Describe \code{y1} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x1, y1) 
{
    n <- length(x1)
    wrap <- c(n, 1:(n - 1))
    x2 <- x1[wrap]
    y2 <- y1[wrap]
    a <- x1 * y2 - x2 * y1
    s <- sum(a) * 3
    if (s < 0.001) {
        c(mean(x1), mean(y1))
    }
    else {
        c(sum((x1 + x2) * a)/s, sum((y1 + y2) * a)/s)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
