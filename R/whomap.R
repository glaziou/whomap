#' Choropleth world maps
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
#' @param hidef Highly detailed map (slow rendering).
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
whomap <- function (X = data.frame(iso3 = NA, var = NA),
                    colours = NULL,
                    low.col = '#BDD7E7',
                    high.col = '#08519C',
                    line.col = 'black',
                    line.width = 0.5,
                    map.title = "",
                    legend.title = "",
                    water.col = 'lightskyblue',
                    na.label = 'No data',
                    na.col = 'grey95',
                    disclaimer = FALSE,
                    legend.pos = c(0.14, 0.26),
                    recentre = 12,
                    hidef = FALSE
                    )
{
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "var") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'var'")

  if (hidef == FALSE)
    .lodef(
      X,
      colours,
      low.col,
      high.col,
      line.col,
      map.title,
      legend.title,
      water.col,
      na.label,
      na.col,
      disclaimer,
      legend.pos,
      recentre
    )
  else
    .hidef(
      X,
      colours,
      low.col,
      high.col,
      line.col,
      line.width,
      map.title,
      legend.title,
      water.col,
      na.label,
      na.col,
      disclaimer,
      legend.pos,
      recentre
    )
}




#' Choropleth world maps
#'
#' `.lodef()` prints a choropleth world map based on shape files
#' from the World Health Organization. It requires ggplot2.
#'
#' @return A ggplot2 plot.
#' @author Philippe Glaziou, adapted from scripts from Tom Hiatt and Hazim Timimi.
#' @import ggplot2
#' @import scales
#' @export
#'
.lodef <- function (X,
                    colours,
                    low.col,
                    high.col,
                    line.col,
                    map.title,
                    legend.title,
                    water.col,
                    na.label,
                    na.col,
                    disclaimer,
                    legend.pos,
                    recentre)
{
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "var") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'var'")

  X <- as.data.frame(X[!is.na(X$var) & X$var != "",])
  if (is.factor(X$var) &
      !is.na(match('', levels(X$var))))
    X <- droplevels(X[!grepl("^\\s*$", X$var), , drop = FALSE])
  if (!is.factor(X$var))
    X$var <- as.factor(X$var)


  # recentre
  stopifnot(is.numeric(recentre))
  if (recentre <= -180 | recentre >= 180)
    stop('recentre must be a number betwen -180 and 180')
  if (recentre < 0)
    recentre <- recentre + 360
  if (recentre == 180)
    recentre <- 0



  # colors
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
  x1 <- X[X$iso3 == 'FRA',]
  if (dim(x1)[2] > 0 &
      is.na(match('GUF', X$iso3)))
    x1$iso3[x1$iso3 == 'FRA'] <- 'GUF'
  x2 <- X[X$iso3 == 'NOR',]
  if (dim(x2)[2] > 0 &
      is.na(match('SJM', X$iso3)))
    x2$iso3[x2$iso3 == 'NOR'] <- 'SJM'
  x3 <- x4 <- X[1, ]
  x3$iso3 <- 'ESH'
  X <- rbind(X, x1, x2, x3)


  # add missing circles for ASM, PYF, MNP, WLF
  asm <-
    gworld[gworld$id == "WSM",]
  asm$id <-
    "ASM"
  asm$group <-
    "ASM.1"
  asm$long <- asm$long + 2
  asm$lat <- asm$lat - 0.5
  pyf <-
    gworld[gworld$id == "COK",]
  pyf$id <-
    "PYF"
  pyf$group <-
    "PYF.1"
  pyf$long <- pyf$long + 10
  pyf$lat <- pyf$lat + 1
  mnp <-
    gworld[gworld$id == "GUM",]
  mnp$id <-
    "MNP"
  mnp$group <-
    "MNP.1"
  mnp$long <- mnp$long + 0.5
  mnp$lat <- mnp$lat + 2
  wlf <-
    gworld[gworld$id == "WSM",]
  wlf$id <-
    "WLF"
  wlf$group <-
    "WLF.1"
  wlf$long <- wlf$long - 5
  wlf$lat <- wlf$lat - 0.2

  gworld <- rbind(gworld, asm, pyf, mnp, wlf)


  # Aksai Chin hack
  eastAC <- gline[gline$group == 2.12, ]
  westAC <- gline[gline$group == 2.12 & gline$order == c(27), ]
  westAC$order <- 37
  AC <- rbind (eastAC, westAC)
  AC$hole <- FALSE
  AC$piece <- 1
  AC$group <- 'Askai Chin.1'
  AC$id <- 'Askai Chin'


  # map parts
  dashiso3s <- c("EGY", "ISR", "KOR", "PRK", "PSE", "SDN", "SSD")
  gworldndash <- gworld[!gworld$id %in% dashiso3s,]
  gworlddash <- gworld[gworld$id %in% dashiso3s,]
  gworlddash$group2 <- as.character(gworlddash$group)

  SSD <-
    gworlddash[!(gworlddash$long > 25 &
                   gworlddash$lat < 13 &
                   gworlddash$long < 34 &
                   gworlddash$lat > 9) & gworlddash$id == 'SSD',]
  SSD[24:27, 'group2'] <- 'SSD.1.2'
  SDN <-
    gworlddash[!(gworlddash$long > 25 &
                   gworlddash$lat < 13 &
                   gworlddash$long < 34 &
                   gworlddash$lat > 9) &
                 !(
                   gworlddash$long > 33.2 &
                     gworlddash$lat < 23 &
                     gworlddash$long < 35 &
                     gworlddash$lat > 21.5
                 ) & gworlddash$id == 'SDN',]
  SDN[14:31, 'group2'] <- 'SDN.1.2'
  SDN[32:33, 'group2'] <- 'SDN.1.3'
  EGY <-
    gworlddash[!(
      gworlddash$long > 33.2 &
        gworlddash$lat < 23 &
        gworlddash$long < 35 &
        gworlddash$lat > 21.5
    ) & gworlddash$id == 'EGY',]
  EGY[13:15, 'group2'] <- 'EGY.1.2'
  ISR <-
    gworlddash[!(
      gworlddash$long > 34.8 &
        gworlddash$lat < 32.6 &
        gworlddash$long < 35.4 &
        gworlddash$lat > 31.3
    ) &
      !(
        gworlddash$long > 34.5 &
          gworlddash$lat < 31.55 &
          gworlddash$long < 34.6 &
          gworlddash$lat > 31.5
      ) & gworlddash$id == 'ISR',]
  ISR[7:15, 'group2'] <- 'ISR.1.2'
  ISR[16:18, 'group2'] <- 'ISR.1.3'
  PSE <- gworlddash[gworlddash$id == 'PSE',]
  PSE <- PSE[16:17, ]
  KOR <-
    gworlddash[!(
      gworlddash$long > 127 &
        gworlddash$lat < 38.5 &
        gworlddash$long < 127.5 &
        gworlddash$lat > 38
    ) & gworlddash$id == 'KOR',]
  KOR <- KOR[1:10, ]
  PRK <-
    gworlddash[!(
      gworlddash$long > 127 &
        gworlddash$lat < 38.5 &
        gworlddash$long < 127.5 &
        gworlddash$lat > 38
    ) & gworlddash$id == 'PRK',]
  PRK[5:12, 'group2'] <- 'PRK.1.2'

  gworlddash2 <- rbind(SSD, SDN, EGY, ISR, PSE, KOR, PRK)

  # Create solid lines for Jammu Kashmir
  jk1 <- gpoly[gpoly$id == "Jammu and Kashmir",]
  jk1$group2 <- as.character(jk1$group)
  jk1[1:2, 'group2'] <- 'Jammu and Kashmir.2'
  jk1[8:16, 'group2'] <- 'Jammu and Kashmir.3'
  jk1[21:22, 'group2'] <- 'Jammu and Kashmir.4'
  jk2 <- jk1[jk1$group2 != 'Jammu and Kashmir.1',]


  # recentring
  if (recentre > 0) {
    duplon <- function(dta) {
      dta2 <- dta
      dta2$long <- dta2$long + 360
      dta2$order <- dta2$order + 10000
      dta2$group <- as.factor(paste(as.character(dta2$group), 'b'))
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
    ggplot2::geom_polygon(
      data = gworldndash,
      aes(group = .data$group),
      colour = line.col,
      fill = NA
    )
  lin0 <-
    ggplot2::geom_path(data = gworlddash2, aes(group = .data$group), colour = line.col)
  pol2 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Lakes",],
      aes(group = .data$group),
      fill = water.col,
      colour = line.col
    )
  pol3 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Jammu and Kashmir",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )
  pol4 <-
    ggplot2::geom_polygon(
      data = AC,
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )
  pol5 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Abyei",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = line.col,
      linetype = "dotted"
    )
  pol6 <-
    ggplot2::geom_polygon(
      data = gworld[gworld$id == 'ESH', ],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = line.col
    )
  lin1 <-
    ggplot2::geom_path(data = gline[gline$id %in% 2,],
                       aes(group = .data$group),
                       colour = line.col)
  lin2 <-
    ggplot2::geom_path(
      data = gline[gline$id %in% c(0, 3, 6, 7),],
      aes(group = .data$group),
      colour = line.col,
      linetype = "dashed"
    )
  lin3 <-
    ggplot2::geom_path(
      data = gline[gline$id %in% c(1, 4, 5),],
      aes(group = .data$group),
      colour = line.col,
      linetype = "dashed"
    )
  lin4 <-
    ggplot2::geom_path(data = jk2, aes(group = .data$group), colour = line.col)
  thm1 <- ggplot2::scale_y_continuous('', breaks = NULL)
  thm2 <- ggplot2::scale_x_continuous('', breaks = NULL)
  thm3 <- ggplot2::theme_bw()

  # disclaimer
  disclaim <- paste(
    "\uA9 World Health Organization",
    format(Sys.Date(), "%Y"),
    ". All rights reserved.
  The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of
  the World Health Organization concerning the legal status of any country, territory, city or area or of its authorities,or concerning the delimitation
  of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate borderlines for which there may not yet be full agreement."
  )

  # merge data
  toplot <-
    merge(gworld,
          X,
          by.x = 'id',
          by.y = 'iso3',
          all.x = TRUE)
  toplot <- toplot[order(toplot$order),]
  levels(toplot$var) <-
    c(levels(toplot$var), na.label, 'Not applicable')
  toplot[is.na(toplot$var), "var"] <- na.label
  toplot[toplot$id == "ESH", "var"] <- 'Not applicable'

  # plot
  zx <- c(-180, 180)
  if (recentre > 0)
    zx <- zx + recentre
  zy <- c(min(gworld$lat), max(gworld$lat))

  p <-
    ggplot2::ggplot(toplot, aes(x = .data$long, y = .data$lat)) +
    ggplot2::geom_polygon(aes(group = .data$group, fill = .data$var), colour = NA) +
    pol1 + pol2 + pol3 + pol4 + pol5 + pol6 +
    lin0 + lin1 + lin2 + lin3 + lin4 +
    thm1 + thm2 + thm3 +
    ggplot2::geom_polygon(aes(group = .data$group, fill = .data$var), toplot[toplot$id %in% c('SWZ', 'LSO'), ]) +
    ggplot2::scale_fill_manual(legend.title, values = col2) +
    ggplot2::coord_cartesian(xlim = zx,
                             ylim = zy,
                             expand = FALSE) +
    ggplot2::labs(title = map.title) +
    ggplot2::theme(
      aspect.ratio = 2.2 / 4,
      plot.title = element_text(size = 16, hjust = 0),
      plot.background = element_rect(fill = water.col),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.text = element_text(size = 7),
      legend.position = legend.pos,
      legend.justification = c(0.5, 1),
      legend.title = element_text(size = 7, hjust = 0),
      rect = element_blank()
    )

  if (disclaimer == FALSE)
    return(p)
  else
  {
    return(p) +
      ggplot2::labs(caption = disclaim) +
      ggplot2::theme(plot.caption.position = 'plot',
                     plot.caption = element_text(size = 6,
                                                 hjust = 0.5))
  }
}








#' Choropleth world maps
#'
#' `.hidef()` prints a choropleth world map based on shape files
#' from the World Health Organization. It requires ggplot2.
#'
#' @return A ggplot2 plot.
#' @author Philippe Glaziou
#' @import ggplot2
#' @import scales
#' @export
#'
.hidef <- function (X,
                    colours,
                    low.col,
                    high.col,
                    line.col,
                    line.width,
                    map.title,
                    legend.title,
                    water.col,
                    na.label,
                    na.col,
                    disclaimer,
                    legend.pos,
                    recentre)
{
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "var") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'var'")

  X <- as.data.frame(X[!is.na(X$var) & X$var != "",])
  if (is.factor(X$var) &
      !is.na(match('', levels(X$var))))
    X <- droplevels(X[!grepl("^\\s*$", X$var), , drop = FALSE])
  if (!is.factor(X$var))
    X$var <- as.factor(X$var)


  # recentre
  stopifnot(is.numeric(recentre))
  if (recentre <= -180 | recentre >= 180)
    stop('recentre must be a number betwen -180 and 180')
  if (recentre < 0)
    recentre <- recentre + 360
  if (recentre == 180)
    recentre <- 0



  # colors
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


  #   add GUF (=FRA), SJM (=NOR), ESH (NA), TWN (=CHN)
  x1 <- X[X$iso3 == 'FRA',]
  if (dim(x1)[2] > 0 &
      is.na(match('GUF', X$iso3)))
    x1$iso3[x1$iso3 == 'FRA'] <- 'GUF'
  x2 <- X[X$iso3 == 'NOR',]
  if (dim(x2)[2] > 0 &
      is.na(match('SJM', X$iso3)))
    x2$iso3[x2$iso3 == 'NOR'] <- 'SJM'
  x3 <- X[X$iso3 == 'CHN',]
  if (dim(x3)[2] > 0 &
      is.na(match('TWN', X$iso3)))
    x3$iso3[x3$iso3 == 'CHN'] <- 'TWN'
  x4 <- X[1, ]
  x4$iso3 <- 'ESH'

  X <- rbind(X, x1, x2, x3, x4)

  # Aksai Chin hack
  eastAC <- gline[gline$group == 2.12, ]
  westAC <- gline[gline$group == 2.12 & gline$order == c(27), ]
  westAC$order <- 37
  AC <- rbind (eastAC, westAC)
  AC$hole <- FALSE
  AC$piece <- 1
  AC$group <- 'Askai Chin.1'
  AC$id <- 'Askai Chin'


  # recentring
  if (recentre > 0) {
    duplon <- function(dta) {
      dta2 <- dta
      dta2$long <- dta2$long + 360
      dta2$order <- dta2$order + 10000
      dta2$group <- as.factor(paste(as.character(dta2$group), 'b'))
      return(rbind(dta, dta2))
    }
    gw <- duplon(gw)
    dispa <- duplon(dispa)
    dispb <- duplon(dispb)
    gpoly <- duplon(gpoly)
    ac <- duplon(AC)
  }

  pol <-
    ggplot2::geom_polygon(
      data = gw,
      aes(group = .data$group),
      colour = line.col,
      size = line.width,
      fill = NA
    )
  d1 <-
    ggplot2::geom_polygon(
      data = dispa,
      aes(group = .data$group),
      colour = line.col,
      size = line.width,
      linetype = 'dashed',
      fill = NA
    )
  d2 <-
    ggplot2::geom_polygon(
      data = dispb[dispb$group != 18.1, ],
      aes(group = .data$group),
      colour = line.col,
      size = line.width,
      linetype = 'dashed',
      fill = NA
    )
  ld1 <-
    ggplot2::geom_path(
      data = dispa,
      aes(group = .data$group),
      colour = line.col,
      size = line.width,
      linetype = 'dotted',
    )
  ld2 <-
    ggplot2::geom_path(
      data = dispb[dispb$group != 18.1, ],
      aes(group = .data$group),
      colour = line.col,
      size = line.width,
      linetype = 'dotted',
    )

  lakes <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id=='Lakes', ],
      aes(group = .data$group),
      fill = water.col,
      colour = line.col,
      size = line.width
    )

  jk <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Jammu and Kashmir",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )

  ak <-
    ggplot2::geom_polygon(
      data = AC,
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )

  ab <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Abyei",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA,
    )

  lin <-
    ggplot2::geom_path(data = gw,
                       aes(group = .data$group),
                       colour = line.col,
                       size = line.width)

  thm1 <- ggplot2::scale_y_continuous('', breaks = NULL)
  thm2 <- ggplot2::scale_x_continuous('', breaks = NULL)
  thm3 <- ggplot2::theme_bw()

  # disclaimer
  disclaim <- paste(
    "\uA9 World Health Organization",
    format(Sys.Date(), "%Y"),
    ". All rights reserved.
  The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of
  the World Health Organization concerning the legal status of any country, territory, city or area or of its authorities,or concerning the delimitation
  of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate borderlines for which there may not yet be full agreement."
  )

  # merge data
  toplot <-
    merge(gw,
          X,
          by.x = 'id',
          by.y = 'iso3',
          all.x = TRUE)
  toplot <- toplot[order(toplot$order),]
  levels(toplot$var) <-
    c(levels(toplot$var), na.label, 'Not applicable')
  toplot[is.na(toplot$var), "var"] <- na.label
  toplot[toplot$id == "ESH", "var"] <- 'Not applicable'

  # plot
  zx <- c(-180, 180)
  if (recentre > 0)
    zx <- zx + recentre
  zy <- c(min(gw$lat), max(gw$lat))

  p <-
    ggplot2::ggplot(toplot, aes(x = .data$long, y = .data$lat)) +
    ggplot2::geom_polygon(aes(group = .data$group, fill = .data$var), colour = NA, size = line.width) +
    lin + lakes + ab + ak + jk + ld1 + ld2 +
    thm1 + thm2 + thm3 +
    ggplot2::geom_polygon(aes(group = .data$group, fill = .data$var), toplot[toplot$id %in% c('SWZ', 'LSO'), ]) +
    ggplot2::scale_fill_manual(legend.title, values = col2) +
    ggplot2::coord_cartesian(xlim = zx,
                             ylim = zy,
                             expand = FALSE) +
    ggplot2::labs(title = map.title) +
    ggplot2::theme(
      aspect.ratio = 2.2 / 4,
      plot.title = element_text(size = 16, hjust = 0),
      plot.background = element_rect(fill = water.col),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.text = element_text(size = 7),
      legend.position = legend.pos,
      legend.justification = c(0.5, 1),
      legend.title = element_text(size = 7, hjust = 0),
      rect = element_blank()
    )

  if (disclaimer == FALSE)
    return(p)
  else
  {
    return(p) +
      ggplot2::labs(caption = disclaim) +
      ggplot2::theme(plot.caption.position = 'plot',
                     plot.caption = element_text(size = 6,
                                                 hjust = 0.5))
  }
}







#' `bubblemap()` prints a world map based on shape files
#' from the World Health Organization. It requires ggplot2.
#'
#' @param X a dataframe. It must contain a variable "iso" (factor)
#' with standard WHO ISO3 country codes and a variable "size" (bubble sizes).
#' @param line.col Colour of country border lines.
#' @param map.title Map title.
#' @param legend.title Legend title.
#' @param water.col Colour of oceans and lakes.
#' @param bubble.col Colour of bubbles.
#' @param bubble.alpha Bubble transparency.
#' @param scale.breaks Bubble scale breaks.
#' @param scale.limits Scale limits.
#' @param scale.labels Scale labels.
#' @param legend.pos A vector of two numbers, positions the legend.
#' @param recentre A longitude value between -180 and 180 set at the middle of the map.
#' @param hidef Highly detailed map (slow rendering).
#' @return A ggplot2 plot.
#' @author Philippe Glaziou, adapted from scripts from Tom Hiatt and Hazim Timimi.
#' @import ggplot2
#' @import scales
#' @export
#' @examples
#' bubblemap(data.frame(iso3=c('BRA','ZAF','RUS','IND','CHN'), size=runif(5)),
#'          scale.breaks=c(.1,.5),
#'          legend.title = 'Silly Example')
#'
bubblemap <- function (X = data.frame(iso3 = NA, size = NA),
                       line.col = 'black',
                       map.title = "",
                       legend.title = "",
                       water.col = 'lightskyblue',
                       bubble.col = 'orange',
                       bubble.alpha = 0.4,
                       scale.breaks = waiver(),
                       scale.limits = NULL,
                       scale.labels = waiver(),
                       line.width = 0.5,
                       legend.pos = c(0.14, 0.26),
                       recentre = 12,
                       hidef = FALSE)
{
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "size") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'size'")


  if (hidef == FALSE)
    .lodefbb(
      X,
      line.col,
      map.title,
      legend.title,
      water.col,
      bubble.col,
      bubble.alpha,
      scale.breaks,
      scale.limits,
      scale.labels,
      legend.pos,
      recentre
    )
  else
    .hidefbb(
      X,
      line.col,
      map.title,
      legend.title,
      water.col,
      bubble.col,
      bubble.alpha,
      scale.breaks,
      scale.limits,
      scale.labels,
      line.width,
      legend.pos,
      recentre
    )
}




#' `.lodefbb()` prints a world map based on shape files
#' from the World Health Organization. It requires ggplot2.
#'
#' @return A ggplot2 plot.
#' @author Philippe Glaziou, adapted from scripts from Tom Hiatt and Hazim Timimi.
#' @import ggplot2
#' @import scales
#' @export
#'
.lodefbb <- function (X,
                     line.col,
                     map.title,
                     legend.title,
                     water.col,
                     bubble.col,
                     bubble.alpha,
                     scale.breaks,
                     scale.limits,
                     scale.labels,
                     legend.pos,
                     recentre)
{
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "size") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'size'")

  #   recentre
  stopifnot(is.numeric(recentre))
  if (recentre <= -180 | recentre >= 180)
    stop('recentre must be a number betwen -180 and 180')
  if (recentre < 0)
    recentre <- recentre + 360
  if (recentre == 180)
    recentre <- 0


  #  add missing circles for ASM, PYF, MNP, WLF
  asm <-
    gworld[gworld$id == "WSM",]
  asm$id <-
    "ASM"
  asm$group <-
    "ASM.1"
  asm$long <- asm$long + 2
  asm$lat <- asm$lat - 0.5
  pyf <-
    gworld[gworld$id == "COK",]
  pyf$id <-
    "PYF"
  pyf$group <-
    "PYF.1"
  pyf$long <- pyf$long + 10
  pyf$lat <- pyf$lat + 1
  mnp <-
    gworld[gworld$id == "GUM",]
  mnp$id <-
    "MNP"
  mnp$group <-
    "MNP.1"
  mnp$long <- mnp$long + 0.5
  mnp$lat <- mnp$lat + 2
  wlf <-
    gworld[gworld$id == "WSM",]
  wlf$id <-
    "WLF"
  wlf$group <-
    "WLF.1"
  wlf$long <- wlf$long - 5
  wlf$lat <- wlf$lat - 0.2

  gworld <- rbind(gworld, asm, pyf, mnp, wlf)


  # Askai Chin hack
  eastAC <- gline[gline$group == 2.12, ]
  westAC <- gline[gline$group == 2.12 & gline$order == c(27), ]
  westAC$order <- 37
  AC <- rbind (eastAC, westAC)
  AC$hole <- FALSE
  AC$piece <- 1
  AC$group <- 'Askai Chin.1'
  AC$id <- 'Askai Chin'


  # map parts
  dashiso3s <- c("EGY", "ISR", "KOR", "PRK", "PSE", "SDN", "SSD")
  gworldndash <- gworld[!gworld$id %in% dashiso3s,]
  gworlddash <- gworld[gworld$id %in% dashiso3s,]
  gworlddash$group2 <- as.character(gworlddash$group)

  SSD <-
    gworlddash[!(gworlddash$long > 25 &
                   gworlddash$lat < 13 &
                   gworlddash$long < 34 &
                   gworlddash$lat > 9) & gworlddash$id == 'SSD',]
  SSD[24:27, 'group2'] <- 'SSD.1.2'
  SDN <-
    gworlddash[!(gworlddash$long > 25 &
                   gworlddash$lat < 13 &
                   gworlddash$long < 34 &
                   gworlddash$lat > 9) &
                 !(
                   gworlddash$long > 33.2 &
                     gworlddash$lat < 23 &
                     gworlddash$long < 35 &
                     gworlddash$lat > 21.5
                 ) & gworlddash$id == 'SDN',]
  SDN[14:31, 'group2'] <- 'SDN.1.2'
  SDN[32:33, 'group2'] <- 'SDN.1.3'
  EGY <-
    gworlddash[!(
      gworlddash$long > 33.2 &
        gworlddash$lat < 23 &
        gworlddash$long < 35 &
        gworlddash$lat > 21.5
    ) & gworlddash$id == 'EGY',]
  EGY[13:15, 'group2'] <- 'EGY.1.2'
  ISR <-
    gworlddash[!(
      gworlddash$long > 34.8 &
        gworlddash$lat < 32.6 &
        gworlddash$long < 35.4 &
        gworlddash$lat > 31.3
    ) &
      !(
        gworlddash$long > 34.5 &
          gworlddash$lat < 31.55 &
          gworlddash$long < 34.6 &
          gworlddash$lat > 31.5
      ) & gworlddash$id == 'ISR',]
  ISR[7:15, 'group2'] <- 'ISR.1.2'
  ISR[16:18, 'group2'] <- 'ISR.1.3'
  PSE <- gworlddash[gworlddash$id == 'PSE',]
  PSE <- PSE[16:17, ]
  KOR <-
    gworlddash[!(
      gworlddash$long > 127 &
        gworlddash$lat < 38.5 &
        gworlddash$long < 127.5 &
        gworlddash$lat > 38
    ) & gworlddash$id == 'KOR',]
  KOR <- KOR[1:10, ]
  PRK <-
    gworlddash[!(
      gworlddash$long > 127 &
        gworlddash$lat < 38.5 &
        gworlddash$long < 127.5 &
        gworlddash$lat > 38
    ) & gworlddash$id == 'PRK',]
  PRK[5:12, 'group2'] <- 'PRK.1.2'

  gworlddash2 <- rbind(SSD, SDN, EGY, ISR, PSE, KOR, PRK)

  # Create solid lines for Jammu Kashmir
  jk1 <- gpoly[gpoly$id == "Jammu and Kashmir",]
  jk1$group2 <- as.character(jk1$group)
  jk1[1:2, 'group2'] <- 'Jammu and Kashmir.2'
  jk1[8:16, 'group2'] <- 'Jammu and Kashmir.3'
  jk1[21:22, 'group2'] <- 'Jammu and Kashmir.4'
  jk2 <- jk1[jk1$group2 != 'Jammu and Kashmir.1',]


  # recentring
  if (recentre > 0) {
    duplon <- function(dta) {
      dta2 <- dta
      dta2$long <- dta2$long + 360
      dta2$order <- dta2$order + 10000
      dta2$group <- as.factor(paste(as.character(dta2$group), 'b'))
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
    ggplot2::geom_polygon(
      data = gworldndash,
      aes(group = .data$group),
      colour = line.col,
      fill = NA
    )
  lin0 <-
    ggplot2::geom_path(data = gworlddash2, aes(group = .data$group), colour = line.col)
  pol2 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Lakes",],
      aes(group = .data$group),
      fill = water.col,
      colour = line.col
    )
  pol3 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Jammu and Kashmir",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )
  pol4 <-
    ggplot2::geom_polygon(
      data = AC,
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )
  pol5 <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Abyei",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = line.col,
      linetype = "dotted"
    )
  pol6 <-
    ggplot2::geom_polygon(
      data = gworld[gworld$id == 'ESH', ],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = line.col
    )
  lin1 <-
    ggplot2::geom_path(data = gline[gline$id %in% 2,],
                       aes(group = .data$group),
                       colour = line.col)
  lin2 <-
    ggplot2::geom_path(
      data = gline[gline$id %in% c(0, 3, 6, 7),],
      aes(group = .data$group),
      colour = line.col,
      linetype = "dashed"
    )
  lin3 <-
    ggplot2::geom_path(
      data = gline[gline$id %in% c(1, 4, 5),],
      aes(group = .data$group),
      colour = line.col,
      linetype = "dashed"
    )
  lin4 <-
    ggplot2::geom_path(data = jk2, aes(group = .data$group), colour = line.col)
  thm1 <- ggplot2::scale_y_continuous('', breaks = NULL)
  thm2 <- ggplot2::scale_x_continuous('', breaks = NULL)
  thm3 <- ggplot2::theme_bw()

  # merge data
  centres <- merge(X, centroid, by.x = 'iso3', by.y = 'id')

  # plot
  zx <- c(-180, 180)
  if (recentre > 0)
    zx <- zx + recentre
  zy <- c(min(gworld$lat), max(gworld$lat))

  p <-
    ggplot2::ggplot(gworld, aes(x = .data$long, y = .data$lat)) +
    ggplot2::geom_polygon(aes(group = .data$group), fill = 'white') +
    pol1 + pol2 + pol3 + pol4 + pol5 + pol6 +
    lin0 + lin1 + lin2 + lin3 + lin4 +
    thm1 + thm2 + thm3 +
    ggplot2::coord_cartesian(xlim = zx,
                             ylim = zy,
                             expand = FALSE) +
    ggplot2::labs(title = map.title) +
    ggplot2::theme(
      aspect.ratio = 2.2 / 4,
      plot.title = element_text(size = 16, hjust = 0),
      plot.background = element_rect(fill = water.col),
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 7),
      legend.position = legend.pos,
      legend.justification = c(0.5, 1),
      legend.title = element_text(size = 7, hjust = 0.4),
      rect = element_blank()
    ) +
    ggplot2::geom_point(
      aes(
        x = .data$long,
        y = .data$lat,
        size = .data$size
      ),
      data = centres,
      shape = 21,
      color = bubble.col,
      fill = bubble.col,
      alpha = bubble.alpha
    ) +
    ggplot2::scale_size_area(
      name = legend.title,
      limits = scale.limits,
      breaks = scale.breaks,
      labels = scale.labels,
      max_size = 25
    )

  return(p)
}







#' `.hidefbb()` prints a world map based on shape files
#' from the World Health Organization. It requires ggplot2.
#'
#' @return A ggplot2 plot.
#' @author Philippe Glaziou
#' @import ggplot2
#' @import scales
#' @export
#'
.hidefbb <- function (X,
                     line.col,
                     map.title,
                     legend.title,
                     water.col,
                     bubble.col,
                     bubble.alpha,
                     scale.breaks,
                     scale.limits,
                     scale.labels,
                     line.width,
                     legend.pos,
                     recentre)
{
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "size") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'size'")

  #   recentre
  stopifnot(is.numeric(recentre))
  if (recentre <= -180 | recentre >= 180)
    stop('recentre must be a number betwen -180 and 180')
  if (recentre < 0)
    recentre <- recentre + 360
  if (recentre == 180)
    recentre <- 0


  # Aksai Chin hack
  eastAC <- gline[gline$group == 2.12, ]
  westAC <- gline[gline$group == 2.12 & gline$order == c(27), ]
  westAC$order <- 37
  AC <- rbind (eastAC, westAC)
  AC$hole <- FALSE
  AC$piece <- 1
  AC$group <- 'Askai Chin.1'
  AC$id <- 'Askai Chin'


  # recentring
  if (recentre > 0) {
    duplon <- function(dta) {
      dta2 <- dta
      dta2$long <- dta2$long + 360
      dta2$order <- dta2$order + 10000
      dta2$group <- as.factor(paste(as.character(dta2$group), 'b'))
      return(rbind(dta, dta2))
    }
    gw <- duplon(gw)
    dispa <- duplon(dispa)
    dispb <- duplon(dispb)
    gpoly <- duplon(gpoly)
    ac <- duplon(AC)
  }

  pol <-
    ggplot2::geom_polygon(
      data = gw,
      aes(group = .data$group),
      colour = line.col,
      size = line.width,
      fill = NA
    )
  ld1 <-
    ggplot2::geom_path(
      data = dispa,
      aes(group = .data$group),
      colour = line.col,
      size = line.width,
      linetype = 'dotted'
    )
  ld2 <-
    ggplot2::geom_path(
      data = dispb[dispb$group != 18.1, ],
      aes(group = .data$group),
      colour = line.col,
      size = line.width,
      linetype = 'dotted'
    )

  lakes <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id=='Lakes', ],
      aes(group = .data$group),
      fill = water.col,
      colour = line.col,
      size = line.width
    )

  esh <-
    ggplot2::geom_polygon(
      data = gw[gw$id == "ESH",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )
  jk <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Jammu and Kashmir",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )
  ak <-
    ggplot2::geom_polygon(
      data = ac,
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA
    )
  ab <-
    ggplot2::geom_polygon(
      data = gpoly[gpoly$id == "Abyei",],
      aes(group = .data$group),
      fill = I("grey75"),
      colour = NA,
    )

  lin <-
    ggplot2::geom_path(data = gw,
                       aes(group = .data$group),
                       colour = line.col,
                       size = line.width)

  thm1 <- ggplot2::scale_y_continuous('', breaks = NULL)
  thm2 <- ggplot2::scale_x_continuous('', breaks = NULL)
  thm3 <- ggplot2::theme_bw()

  # merge data
  centres <- merge(X, centroid, by.x = 'iso3', by.y = 'id')

  # plot
  zx <- c(-180, 180)
  if (recentre > 0)
    zx <- zx + recentre
  zy <- c(min(gw$lat), max(gw$lat))

  p <-
    ggplot2::ggplot(gw, aes(x = .data$long, y = .data$lat)) +
    ggplot2::geom_polygon(aes(group = .data$group), fill = 'white') +
    lin + lakes + esh + jk + ak + ab + ld1 + ld2 +
    thm1 + thm2 + thm3 +
    ggplot2::coord_cartesian(xlim = zx,
                             ylim = zy,
                             expand = FALSE) +
    ggplot2::labs(title = map.title) +
    ggplot2::theme(
      aspect.ratio = 2.2 / 4,
      plot.title = element_text(size = 16, hjust = 0),
      plot.background = element_rect(fill = water.col),
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 7),
      legend.position = legend.pos,
      legend.justification = c(0.5, 1),
      legend.title = element_text(size = 7, hjust = 0.4),
      rect = element_blank()
    ) +
    ggplot2::geom_point(
      aes(
        x = .data$long,
        y = .data$lat,
        size = .data$size
      ),
      data = centres,
      shape = 21,
      color = bubble.col,
      fill = bubble.col,
      alpha = bubble.alpha
    ) +
    ggplot2::scale_size_area(
      name = legend.title,
      limits = scale.limits,
      breaks = scale.breaks,
      labels = scale.labels,
      max_size = 25
    )

  return(p)
}
