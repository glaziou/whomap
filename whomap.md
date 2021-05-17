# R package whomap

Draws choropleth maps of the world, based on WHO shapefiles.


## Install:

devtools::install_github('glaziou/whomap')


## Usage

whomap(X, colours = NULL, low.col = "#BDD7E7", high.col = "#08519C",
    line.col = "black", map.title = "", legend.title = "",
    background = NA, na.label = "No data", disclaimer = FALSE,
    legend.pos = c(0.09, 0.26))
    
X is a dataframe. It must contain a variable named #iso# holding country ISO3 codes, and a second
categorical variable named #var#. There should be no more than 6 categories (excluding "No data" and 
"Not applicable") for optimal display of the legend. The category labels should be short.

Example:

brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'),
                    var=5:1)
                    
whomap(brics, legend.title='BRICS')


