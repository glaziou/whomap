# R package whomap
version 0.9.0

Draws choropleth maps of the world, based on WHO shapefiles (simpler or more detailed issued in 2022), adapted from scripts from Tom Hiatt and Hazim Timimi.


## Install:

devtools::install_github('glaziou/whomap')


## Usage

whomap(X, colours = NULL, low.col = "#BDD7E7", high.col = "#08519C",
    line.col = "black", map.title = "", legend.title = "",
    background = NA, na.label = "No data", disclaimer = FALSE,
    legend.pos = c(0.14, 0.26), recentre = 12)
    
X is a dataframe. It must contain a variable named "iso3" holding country ISO3 codes, and a second
categorical variable named "var". There should be no more than 6 categories (excluding "No data" and 
"Not applicable") for optimal display of the legend. The category labels should be short.

Examples:

#### Univariate

brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'),
                    var=1)

whomap(brics, colour='red', legend.pos='none', water.col = 'white')

![image](https://user-images.githubusercontent.com/233963/119449613-2663f980-bd33-11eb-99ea-fa4f567cba3d.png)


Oceans and lakes in light blue

whomap(brics, colour='red', legend.pos='none', water.col = 'lightblue')

![image](https://user-images.githubusercontent.com/233963/119449734-4398c800-bd33-11eb-9f5b-25ce8218c944.png)


brics$var <- 1:5

whomap(brics, legend.title='BRICS', water.col = 'white')

![image](https://user-images.githubusercontent.com/233963/120228265-13798980-c24b-11eb-9ce6-7f62ae383fa7.png)


Recentered on the region Asia-Pacific, with the legend repositioned:

whomap(brics, legend.title = 'BRICS', legend.pos = c(0.7, 0.26), recentre = 163, water.col = 'white')

![image](https://user-images.githubusercontent.com/233963/119449970-98d4d980-bd33-11eb-89f3-24ca5c8be36f.png)


The above maps can be drawn using high-definition 2022 WHO shapefiles by passing a parameter "hidef = T" (default is F). The drawing in high-definition is considerably slower. The previous map in high-definition with the default colour for water bodies is shown below:

whomap(brics, legend.title = 'BRICS', legend.pos = c(0.7, 0.26), recentre = 163, hidef = TRUE)

![image](https://user-images.githubusercontent.com/233963/173190001-bcd107c7-17cc-483b-8419-968ccd06ec4c.png)



### Bivariate

World map also showing a secondary country marker denoting a second variable

whomap(brics, legend.title='BRICS', legend.pos=c(0.14, 0.34)) +
   add_marker('BRA', lab='Subnational\ndata')

![image](https://user-images.githubusercontent.com/233963/120228390-44f25500-c24b-11eb-848c-1673771848a2.png)



