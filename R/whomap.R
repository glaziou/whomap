#' Choropleth world map
#'
#' `whomap()` prints a choropleth world map based on shape files
#' from the World Health Organization. It requires ggplot2.
#'
#' @param X a dataframe. It must contain a variable "iso" (factor)
#' with standard WHO ISO3 country codes.The categorical variable to be
#' mapped should be named "var" (see examples).
#' @param colours A vector of colour values for each category in "var", excepting missing values.
#' @param low.col First value of a gradient of colours.
#' @param high.col Last value of a gradient of colours.
#' @param line.col Colour of country border lines.
#' @param map.title Map title.
#' @param legend.title Legend title.
#' @param water.col Colour of oceans and lakes.
#' @param na.label Legend lable for missing values.
#' @param na.col Colour of countries with missing values.
#' @param disclaimer A boolean, inserts a standard WHO disclaimer.
#' @param legend.pos A vector of two numbers, positions the legend.
#' @param recentre A longitude value between -180 and 180 set at the middle of the map.
#' @return A ggplot2 plot.
#' @author Philippe Glaziou, adapted from scripts from Tom Hiatt and Hazim Timimi.
#' @import ggplot2
#' @import scales
#' @export
#' @examples
#' # A simple world map
#' whomap()
#'
#' # Map of the BRICS countries in red, no legend
#' brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'), var=1)
#' whomap(X=brics, colours=c('red'), legend.pos='none')
#'
#' # with legend
#' brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'), var=1:5)
#' whomap(brics, legend.title='BRICS')
#'
whomap <- function (X = data.frame(iso3=NA, var=NA),
                    colours = NULL,
                    low.col = '#BDD7E7',
                    high.col = '#08519C',
                    line.col = 'black',
                    map.title = "",
                    legend.title = "",
                    water.col = 'white',
                    na.label = 'No data',
                    na.col = 'grey95',
                    disclaimer = FALSE,
                    legend.pos = c(0.09, 0.26),
                    recentre = 12)
{
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "var") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'var'")

  X <- as.data.frame(X[!is.na(X$var) & X$var != "", ])
  if (!is.factor(X$var)) X$var <- as.factor(X$var)


  #   recentre
  stopifnot(is.numeric(recentre))
  if (recentre <= -180 | recentre >= 180)
    stop('recentre must be a number betwen -180 and 180')
  if (recentre < 0) recentre <- recentre + 360
  if (recentre == 180) recentre <- 0



  #   colors
  if (!is.null(colours) &
      length(levels(X$var)) != length(colours))
    stop(
      paste(
        "var categories (excluding missing values) and colors do not match. var has",
        length(unique(X[["var"]])),
        "levels and colours has",
        length(colours),
        "levels"
      )
    )

  if (is.null(colours)) {
    xc <- seq(0, 1, length = length(levels(X[["var"]])))
    col <- scales::seq_gradient_pal(low.col, high.col)(xc)
  } else
    col <- colours

  col2 <- c(col, na.col, 'grey75')


  #   add GUF (=FRA), SJM (=NOR), ESH (NA)
  x1 <- X[X$iso3 == 'FRA', ]
  if (dim(x1)[2] > 0 &
      is.na(match('GUF', X$iso3)))
    x1$iso3[x1$iso3 == 'FRA'] <- 'GUF'
  x2 <- X[X$iso3 == 'NOR', ]
  if (dim(x2)[2] > 0 &
      is.na(match('SJM', X$iso3)))
    x2$iso3[x2$iso3 == 'NOR'] <- 'SJM'
  x3 <- x4 <- X[1,]
  x3$iso3 <- 'ESH'
  X <- rbind(X, x1, x2, x3)


  #  add missing circles for ASM, PYF, MNP, WLF
  asm <-
    gworld[gworld$id == "WSM", ]
  asm$id <-
    "ASM"
  asm$group <-
    "ASM.1"
  asm$long <- asm$long + 2
  asm$lat <- asm$lat - 0.5
  pyf <-
    gworld[gworld$id == "COK", ]
  pyf$id <-
    "PYF"
  pyf$group <-
    "PYF.1"
  pyf$long <- pyf$long + 10
  pyf$lat <- pyf$lat + 1
  mnp <-
    gworld[gworld$id == "GUM", ]
  mnp$id <-
    "MNP"
  mnp$group <-
    "MNP.1"
  mnp$long <- mnp$long + 0.5
  mnp$lat <- mnp$lat + 2
  wlf <-
    gworld[gworld$id == "WSM", ]
  wlf$id <-
    "WLF"
  wlf$group <-
    "WLF.1"
  wlf$long <- wlf$long - 5
  wlf$lat <- wlf$lat - 0.2

  gworld <- rbind(gworld, asm, pyf, mnp, wlf)


  # Askai Chin hack
  eastAC <- gline[gline$group == 2.12,]
  westAC <- gline[gline$group == 2.12 & gline$order == c(27),]
  westAC$order <- 37
  AC <- rbind (eastAC, westAC)
  AC$hole <- FALSE
  AC$piece <- 1
  AC$group <- 'Askai Chin.1'
  AC$id <- 'Askai Chin'


  # map parts
  dashiso3s <- c("EGY", "ISR", "KOR", "PRK", "PSE", "SDN", "SSD")
  gworldndash <- gworld[!gworld$id %in% dashiso3s, ]
  gworlddash <- gworld[gworld$id %in% dashiso3s, ]
  gworlddash$group2 <- as.character(gworlddash$group)

  SSD <-
    gworlddash[!(gworlddash$long > 25 &
                           gworlddash$lat < 13 &
                           gworlddash$long < 34 & gworlddash$lat > 9) & gworlddash$id == 'SSD', ]
  SSD[24:27, 'group2'] <- 'SSD.1.2'
  SDN <-
      gworlddash[!(gworlddash$long > 25 &
                     gworlddash$lat < 13 &
                     gworlddash$long < 34 &
                     gworlddash$lat > 9) &
        !(gworlddash$long > 33.2 &
            gworlddash$lat < 23 & gworlddash$long < 35 & gworlddash$lat > 21.5) & gworlddash$id == 'SDN', ]
  SDN[14:31, 'group2'] <- 'SDN.1.2'
  SDN[32:33, 'group2'] <- 'SDN.1.3'
  EGY <-
    gworlddash[!(gworlddash$long > 33.2 &
                   gworlddash$lat < 23 & gworlddash$long < 35 & gworlddash$lat > 21.5) & gworlddash$id == 'EGY', ]
  EGY[13:15, 'group2'] <- 'EGY.1.2'
  ISR <-
      gworlddash[!(gworlddash$long > 34.8 &
                     gworlddash$lat < 32.6 &
                     gworlddash$long < 35.4 &
                     gworlddash$lat > 31.3) &
        !(gworlddash$long > 34.5 &
            gworlddash$lat < 31.55 & gworlddash$long < 34.6 & gworlddash$lat > 31.5) & gworlddash$id == 'ISR', ]
  ISR[7:15, 'group2'] <- 'ISR.1.2'
  ISR[16:18, 'group2'] <- 'ISR.1.3'
  PSE <- gworlddash[gworlddash$id == 'PSE', ]
  PSE <- PSE[16:17,]
  KOR <-
    gworlddash[!(gworlddash$long > 127 &
                   gworlddash$lat < 38.5 & gworlddash$long < 127.5 & gworlddash$lat > 38) & gworlddash$id == 'KOR', ]
  KOR <- KOR[1:10,]
  PRK <-
    gworlddash[!(gworlddash$long > 127 &
                   gworlddash$lat < 38.5 & gworlddash$long < 127.5 & gworlddash$lat > 38) & gworlddash$id == 'PRK', ]
  PRK[5:12, 'group2'] <- 'PRK.1.2'

  gworlddash2 <- rbind(SSD, SDN, EGY, ISR, PSE, KOR, PRK)

  # Create solid lines for Jammu Kashmir
  jk1 <- gpoly[gpoly$id == "Jammu and Kashmir", ]
  jk1$group2 <- as.character(jk1$group)
  jk1[1:2, 'group2'] <- 'Jammu and Kashmir.2'
  jk1[8:16, 'group2'] <- 'Jammu and Kashmir.3'
  jk1[21:22, 'group2'] <- 'Jammu and Kashmir.4'
  jk2 <- jk1[jk1$group2 != 'Jammu and Kashmir.1', ]


  # recentring
  if (recentre > 0) {
    duplon <- function(dta) {
      dta2 <- dta
      dta2$long <- dta2$long + 360
      dta2$order <- dta2$order + 10000
      dta2$group <- as.factor(paste(as.character(dta2$group),'b'))
      return(rbind(dta, dta2))
    }
    gworld <- duplon(gworld)
    gworldndash <- duplon(gworldndash)
    gworlddash2$group <- gworlddash2$group2
    gworlddash2 <- duplon(gworlddash2)
    AC <- duplon(AC)
    jk2$group <- jk2$group2
    jk2 <- duplon(jk2)
    gpoly <- duplon(gpoly)
    gline <- duplon(gline)
  }



  pol1 <-
    ggplot2::geom_polygon(data = gworldndash,
                 aes_(group = ~group),
                 colour = line.col,
                 fill = NA)   # map all countries
  lin0 <-
    ggplot2::geom_path(data = gworlddash2, aes_(group = ~group), colour = line.col)
  pol2 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Lakes", ],
      aes_(group = ~group),
      fill = water.col,
      colour = line.col
    )
  pol3 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Jammu and Kashmir", ],
      aes_(group = ~group),
      fill = I("grey75"),
      colour = NA
    )
  pol4 <-
    ggplot2::geom_polygon(
      data = AC,
      aes_(group = ~group),
      fill = I("grey75"),
      colour = NA
    )
  pol5 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Abyei", ],
      aes_(group = ~group),
      fill = I("grey75"),
      colour = line.col,
      linetype = "dotted"
    )
  pol6 <-
    ggplot2::geom_polygon(
      data = gworld[gworld$id == 'ESH',],
      aes_(group = ~group),
      fill = I("grey75"),
      colour = line.col
    )
  lin1 <-
    ggplot2::geom_path(data = gline[gline$id %in% 2, ],
              aes_(group = ~group),
              colour = line.col)
  lin2 <-
    ggplot2::geom_path(
      data = gline[gline$id %in% c(0, 3, 6, 7), ],
      aes_(group = ~group),
      colour = line.col,
      linetype = "dashed"
    ) 	# dashed lines over color of country
  lin3 <-
    ggplot2::geom_path(
      data = gline[gline$id %in% c(1, 4, 5), ],
      aes_(group = ~group),
      colour = line.col,
      linetype = "dashed"
    )
  lin4 <-
    ggplot2::geom_path(data = jk2, aes_(group = ~group), colour = line.col)
  thm1 <- ggplot2::scale_y_continuous('', breaks = NULL)
  thm2 <- ggplot2::scale_x_continuous('', breaks = NULL)
  thm3 <- ggplot2::theme_bw()

  #   disclaimer
  disclaim <- paste(
    "\uA9 World Health Organization", format(Sys.Date(), "%Y"), ". All rights reserved.
  The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of
  the World Health Organization concerning the legal status of any country, territory, city or area or of its authorities,or concerning the delimitation
  of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate borderlines for which there may not yet be full agreement."
  )

  #   merge data
  toplot <-
    merge(gworld,
          X,
          by.x = 'id',
          by.y = 'iso3',
          all.x = TRUE)
  toplot <- toplot[order(toplot$order), ]
  levels(toplot$var) <-
    c(levels(toplot$var), na.label, 'Not applicable')
  toplot[is.na(toplot$var), "var"] <- na.label
  toplot[toplot$id == "ESH", "var"] <- 'Not applicable'

  # plot
  zx <- c(-180, 180)
  if (recentre>0) zx <- zx + recentre
  zy <- c(min(gworld$lat), max(gworld$lat))

  p <-  ggplot2::ggplot(toplot, aes_(x=~long, y=~lat)) +
    ggplot2::geom_polygon(aes_(group = ~group, fill = ~var), colour = NA) +
    pol1 + pol2 + pol3 + pol4 + pol5 + pol6 +
    lin0 + lin1 + lin2 + lin3 + lin4 +
    thm1 + thm2 + thm3 +
    ggplot2::geom_polygon(aes_(group = ~group, fill = ~var), toplot[toplot$id %in% c('SWZ', 'LSO'),]) +
    ggplot2::scale_fill_manual(legend.title, values = col2) +
    ggplot2::coord_cartesian(xlim = zx, ylim = zy, expand = FALSE) +
    ggplot2::labs(title = map.title) +
    ggplot2::theme(
      aspect.ratio = 2.2 / 4,
      plot.title = element_text(size = 16, hjust = 0),
      plot.background = element_rect(fill = water.col),
#      legend.key = element_rect(color = 'grey75'),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.text = element_text(size = 7),
      legend.position = legend.pos,
      legend.justification = c(0.5, 1),
      legend.title = element_text(size = 7, hjust = 0),
      rect = element_blank()
    )

  if (disclaimer == FALSE)
    print(p)
  else
  {
    print(p) +
      ggplot2::labs(caption = disclaim) +
      ggplot2::theme(plot.caption.position = 'plot',
            plot.caption = element_text(size = 6,
                                        hjust = 0.5))
  }
}

#' @export
