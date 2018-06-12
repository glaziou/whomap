\name{whomap}
\alias{whomap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Main function, prints a map of the world using WHO shapefiles
}
\usage{
whomap(X, Z = scale_fill_brewer(palette = "YlGnBu", type = "seq"),
      legendpos = c(0.1, 0.3), col = "black", cut = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
      Dataset - must contain a variable "iso" (factor) with standard WHO ISO3 country codes.
      The categorical variable to me mapped should be named "var" (see examples).
}
  \item{Z}{
    Optional parameter. scale_fill_manual or scale_fill_brewer object (see examples).
}
  \item{legendpos}{
      Legend position - see legend.pos options in ggplot2 and examples.
}
  \item{col}{
      colour of borders (defaults to black)
}
  \item{cut}{
      cuts out the tip of Western Siberia to avoid having Russia on two sides of the map (defaults to TRUE)
}
}
\details{
    Prints a ggplot2 map.
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
    Philippe Glaziou, based on a script from Tom Hiatt
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    \code{\link{geom_map}}
}
\examples{
# Map of the BRICS countries in red
brics <- data.frame(iso3=c('BRA','RUS','IND','CHN','ZAF','SWZ','LSO'),
                    var=c(rep("yes", 5), rep("no", 2)))
whomap(X=brics, Z=scale_fill_manual(values=c('white','red')), legendpos='none')
}
\keyword{ ~map }
\keyword{ ~world }% __ONLY ONE__ keyword per line
