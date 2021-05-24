# R package whomap

Draws choropleth maps of the world, based on WHO shapefiles and scripts from Tom Hyatt and Hazim Timimi.


## Install:

devtools::install_github('glaziou/whomap')


## Usage

whomap(X, colours = NULL, low.col = "#BDD7E7", high.col = "#08519C",
    line.col = "black", map.title = "", legend.title = "",
    background = NA, na.label = "No data", disclaimer = FALSE,
    legend.pos = c(0.09, 0.26))
    
X is a dataframe. It must contain a variable named "iso3" holding country ISO3 codes, and a second
categorical variable named "var". There should be no more than 6 categories (excluding "No data" and 
"Not applicable") for optimal display of the legend. The category labels should be short.

Example:

brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'),
                    var=1)

whomap(brics, colour='red', legend.pos='none')

![image](https://user-images.githubusercontent.com/233963/119390299-20d3c880-bccd-11eb-9062-31d224a9757a.png)


brics$var <- 1:5

whomap(brics, legend.title='BRICS')

![image](https://user-images.githubusercontent.com/233963/119390355-3812b600-bccd-11eb-895d-c73c0593a9c0.png)


Recentred on the region Asia-Pacific, with the legend repositioned:

whomap(brics, legend.title = 'BRICS', legend.pos = c(0.3, 0.26), recentre = 173)

![image](https://user-images.githubusercontent.com/233963/119390841-e4ed3300-bccd-11eb-8df5-bc272f365ed3.png)




