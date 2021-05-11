#' @export
whomap <- function (X,
                    colours = NULL,
                    low.col = '#BDD7E7',
                    high.col = '#08519C',
                    line.col = 'black',
                    map.title = "",
                    legend.title = "",
                    background = NA,
                    na.label = 'No data',
                    disclaimer = FALSE,
                    legend.pos = c(0.09, 0.26))
{
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "var") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'var'")

  if(!is.factor(X$var)) X$var <- as.factor(X$var)

  #   colors
  if (!is.null(colours) &
      length(unique(X[["var"]])) != length(colours))
    stop(
      paste(
        "var categories and colors do not match. var has",
        length(unique(X[["var"]])),
        "levels and colours has",
        length(colours),
        "levels"
      )
    )


  if (is.null(colours)) {
    x <- seq(0, 1, length = length(unique(X[["var"]])))
    x1 <- seq_gradient_pal(low.col, high.col)(x)
  } else
    x1 <- colours

  colours2 <- c(x1, 'white', 'grey75')


  #   add GUF (=FRA), COK (No data), ESH (NA)
  x1 <- X[X$iso3 %in% c('FRA')]
  if (dim(x1)[2] > 0 &
      is.na(match('GUF', X$iso3)))
    x1$iso3[x1$iso3 == 'FRA'] <- 'GUF'
  x2 <- x3 <- X[1,]
  x2$iso3 <- 'COK'
  x2$var <- 'No data'
  x3$iso3 <- 'ESH'
  x3$var <- 'Not applicable'
  X <- rbind(X, x1, x2, x3)


  # add missing circles for ASM, PYF, MNP, WLF
  asm <-
    subset(gworld, id == "WSM")
  asm$id <-
    "ASM"
  asm$group <-
    "ASM.1"
  asm$long <- asm$long + 2
  asm$lat <- asm$lat - 0.5
  pyf <-
    subset(gworld, id == "COK")
  pyf$id <-
    "PYF"
  pyf$group <-
    "PYF.1"
  pyf$long <- pyf$long + 10
  pyf$lat <- pyf$lat + 1
  mnp <-
    subset(gworld, id == "GUM")
  mnp$id <-
    "MNP"
  mnp$group <-
    "MNP.1"
  mnp$long <- mnp$long + 0.5
  mnp$lat <- mnp$lat + 2
  wlf <-
    subset(gworld, id == "WSM")
  wlf$id <-
    "WLF"
  wlf$group <-
    "WLF.1"
  wlf$long <- wlf$long - 5
  wlf$lat <- wlf$lat - 0.2

  gworld <- rbind(gworld, asm, pyf, mnp, wlf)

  # Color Svalbard and Jan Mayen the same as Norway
  gworld[gworld$group == "SJM.1", "piece"] <- "2"
  gworld[gworld$group == "SJM.2", "piece"] <- "3"
  gworld[gworld$group == "SJM.3", "piece"] <- "4"

  gworld[gworld$id == "SJM", "id"] <- "NOR"

  levels(gworld$group) <-
    c(levels(gworld$group), "NOR.2", "NOR.3", "NOR.4")
  gworld[gworld$group == "SJM.1", "group"] <- "NOR.2"
  gworld[gworld$group == "SJM.2", "group"] <- "NOR.3"
  gworld[gworld$group == "SJM.3", "group"] <- "NOR.4"


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
  gworldndash <- subset(gworld, !id %in% dashiso3s)
  gworlddash <- subset(gworld, id %in% dashiso3s)
  gworlddash$group2 <- as.character(gworlddash$group)

  SSD <-
    subset(gworlddash, !(long > 25 &
                           lat < 13 &
                           long < 34 & lat > 9) & id == 'SSD')
  SSD[24:27, 'group2'] <- 'SSD.1.2'
  SDN <-
    subset(
      gworlddash,
      !(long > 25 &
          lat < 13 &
          long < 34 &
          lat > 9) &
        !(long > 33.2 &
            lat < 23 & long < 35 & lat > 21.5) & id == 'SDN'
    )
  SDN[14:31, 'group2'] <- 'SDN.1.2'
  SDN[32:33, 'group2'] <- 'SDN.1.3'
  EGY <-
    subset(gworlddash,
           !(long > 33.2 &
               lat < 23 & long < 35 & lat > 21.5) & id == 'EGY')
  EGY[13:15, 'group2'] <- 'EGY.1.2'
  ISR <-
    subset(
      gworlddash,
      !(long > 34.8 &
          lat < 32.6 &
          long < 35.4 &
          lat > 31.3) &
        !(long > 34.5 &
            lat < 31.55 & long < 34.6 & lat > 31.5) & id == 'ISR'
    )
  ISR[7:15, 'group2'] <- 'ISR.1.2'
  ISR[16:18, 'group2'] <- 'ISR.1.3'
  PSE <- subset(gworlddash, id == 'PSE')
  PSE <- PSE[16:17,]
  KOR <-
    subset(gworlddash,
           !(long > 127 &
               lat < 38.5 & long < 127.5 & lat > 38) & id == 'KOR')
  KOR <- KOR[1:10,]
  PRK <-
    subset(gworlddash,
           !(long > 127 &
               lat < 38.5 & long < 127.5 & lat > 38) & id == 'PRK')
  PRK[5:12, 'group2'] <- 'PRK.1.2'

  gworlddash2 <- rbind(SSD, SDN, EGY, ISR, PSE, KOR, PRK)

  # Create solid lines for Jammu Kashmir
  jk1 <- subset(gpoly, id == "Jammu and Kashmir")
  jk1$group2 <- as.character(jk1$group)
  jk1[1:2, 'group2'] <- 'Jammu and Kashmir.2'
  jk1[8:16, 'group2'] <- 'Jammu and Kashmir.3'
  jk1[21:22, 'group2'] <- 'Jammu and Kashmir.4'
  jk2 <- subset(jk1, group2 != 'Jammu and Kashmir.1')

  pol1 <-
    geom_polygon(data = gworldndash,
                 aes(group = group),
                 colour = line.col,
                 fill = NA)   # map all countries
  lin0 <-
    geom_path(data = gworlddash2, aes(group = group2), colour = line.col)
  pol2 <-
    geom_polygon(
      data = subset(gpoly, id == "Lakes"),
      aes(group = group),
      fill = I("white"),
      colour = line.col
    )
  pol3 <-
    geom_polygon(
      data = subset(gpoly, id == "Jammu and Kashmir"),
      aes(group = group),
      fill = I("grey75"),
      colour = NA
    )
  pol3b <-
    geom_polygon(
      data = AC,
      aes(group = group),
      fill = I("grey75"),
      colour = NA
    )
  pol4 <-
    geom_polygon(
      data = subset(gpoly, id == "Abyei"),
      aes(group = group),
      fill = I("grey75"),
      colour = line.col,
      linetype = "dotted"
    )
  pol5 <-
    geom_polygon(
      data = gworld[gworld$id == 'ESH',],
      aes(group = group),
      fill = I("grey75"),
      colour = line.col
    )
  lin1 <-
    geom_path(data = subset(gline, id %in% 2),
              aes(group = group),
              colour = line.col)
  lin2 <-
    geom_path(
      data = subset(gline, id %in% c(0, 3, 6, 7)),
      aes(group = group),
      colour = line.col,
      linetype = "dashed"
    ) 	# dashed lines over color of country
  lin3 <-
    geom_path(
      data = subset(gline, id %in% c(1, 4, 5)),
      aes(group = group),
      colour = line.col,
      linetype = "dashed"
    )
  #   lin4 <- geom_path(data = subset(gline, id %in% c(8)), aes(group = group), colour = "white", linetype = "dotted")   # dotted white lines (8 and 9 are the same!) I'm replacing this with a new line 4...
  lin4 <-
    geom_path(data = jk2, aes(group = group2), colour = line.col)
  thm1 <- scale_y_continuous('', breaks = NULL)
  thm2 <- scale_x_continuous('', breaks = NULL)
  thm3 <- theme_bw()

  #   disclaimer
  disclaim <-
    ifelse(
      disclaimer == FALSE,
      "",
      "\uA9 World Health Organization 2015. All rights reserved.
  The designations employed and the presentation of the material in this publication do not
  imply the expression of any opinion whatsoever on the part of the World Health Organization
  concerning the legal status of any country, territory, city or area or of its authorities,
  or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on
  maps represent approximate borderlines for which there may not yet be full agreement."
    )

  #   merge data
  toplot <- mapdata(X)
  levels(toplot$var) <-
    c(levels(toplot$var), na.label, 'Not applicable')
  toplot[is.na(toplot$var), "var"] <- na.label
  toplot[toplot$id == "ESH", "var"] <- 'Not applicable'

  # plot
  zx <- c(-180, 180)
  zy <- c(min(gworld$lat), max(gworld$lat))

  plot <-  ggplot(toplot, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = var), colour = NA) +
    pol1 + pol2 + pol3 + pol3b + pol4 + pol5 + lin0 + lin1 + lin2 + lin3 + lin4 +
    thm1 + thm2 + thm3 +
    geom_polygon(aes(group = group, fill = var), toplot[toplot$id %in% c('SWZ', 'LSO'),]) +
    scale_fill_manual(legend.title, values = colours2) +
    coord_cartesian(xlim = zx, ylim = zy) +
    labs(title = map.title) +

    theme(
      aspect.ratio = 2.2 / 4,
      plot.title = element_text(size = 16, hjust = 0),
      plot.background = element_rect(fill = background),
      legend.key.size = unit(0.50, "cm"),
      legend.text = element_text(size = 8),
      legend.position = legend.pos,
      legend.justification = c(0.5, 1),
      legend.title = element_text(size = 10, hjust = 0),
      rect = element_blank()
    ) +
    annotate("text",
             -20,
             -54,
             label = disclaim,
             size = 2,
             hjust = 0)

  print(plot)
}
