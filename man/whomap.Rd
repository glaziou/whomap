\name{whomap}
\alias{whomap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Main function, prints a map of the world using WHO shapefiles
}
\usage{
whomap(X, colours = NULL, low.col = "#BDD7E7", high.col = "#08519C",
    line.col = "black", map.title = "", legend.title = "",
    background = NA, na.label = "No data", na.col = 'grey95',
    disclaimer = FALSE, legend.pos = c(0.09, 0.26), recentre = 29)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
      Dataset - must contain a variable "iso" (factor) with standard WHO ISO3 country codes.
      The categorical variable to be mapped should be named "var" (see examples).
}
  \item{colours}{
    Optional parameter, scale_fill_manual or scale_fill_brewer object (see examples).
}
  \item{low.col}{
    Optional parameter, first level of a colour gradient.
}
  \item{high.col}{
    Optional parameter, last level of a colour gradient.
}
  \item{line.col}{
    Optional parameter, colour of border lines.
}
  \item{map.title}{
    Optional parameter, plot title.
}
  \item{legend.title}{
    Optional parameter, legend title.
}
  \item{background}{
    Optional parameter, background colour.
}
  \item{na.label}{
    Optional parameter, legend label for missing values.
}
  \item{na.col}{
    Optional parameter, colour for missing values.
}
  \item{disclaimer}{
    Optional parameter, show WHO disclaimer.
}
  \item{legend.pos}{
    Optional parameter, legend position - see legend.pos options in ggplot2.
}
  \item{recentre}{
    Optional parameter, a longitude between 0 and 360 which will be the centre of the map.
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
    Philippe Glaziou, based on scripts from Tom Hiatt and Hazim Timimi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    \code{\link{geom_map}}
}
\examples{
# Map of the BRICS countries in red, no legend
brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'), var=1)
whomap(X=brics, colours=c('red'), legend.pos='none')

# with legend
brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'),
                    var=5:1)
whomap(brics, legend.title='BRICS')
}
\keyword{ ~map }
\keyword{ ~world }% __ONLY ONE__ keyword per line
