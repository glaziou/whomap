#' @export
whomap <- function (X, Z = scale_fill_brewer(palette = "YlGnBu", type = "seq"), legendpos = c(0.1, 0.3), col='black', cut=T)
  {
    if (is.data.frame(X) == FALSE)
      stop("X must be a dataframe")
    if (all(c("iso3", "var") %in% names(X)) == FALSE)
      stop("X must have two variables named 'iso3' and 'var'")
    toplot <- mapdata(X)
    pol1 <- geom_polygon(data = gworld, aes(group = group), colour = col,
                         fill = NA)
    pol2 <- geom_polygon(data = subset(gpoly, id == "Lakes"),
                         aes(group = group), fill = I("white"), colour = col)
    pol3 <- geom_polygon(data = subset(gpoly, id == "Jammu and Kashmir"),
                         aes(group = group), fill = I("grey75"), colour = col,
                         linetype = "dashed")
    pol4 <- geom_polygon(data = subset(gpoly, id == "Abyei"),
                         aes(group = group), fill = I("grey75"), colour = col,
                         linetype = "dotted")
    pol5 <- geom_polygon(data = gworld[gworld$id == "ESH", ],
                         aes(group = group), fill = I("grey75"), colour = col)
    lin1 <- geom_path(data = subset(gline, id %in% 2), aes(group = group),
                      colour = "grey50")
    lin2 <- geom_path(data = subset(gline, id %in% c(0, 3, 6,
                                                     7)), aes(group = group), colour = "white", linetype = "dashed")
    lin3 <- geom_path(data = subset(gline, id %in% c(1, 4, 5)),
                      aes(group = group), colour = col, linetype = "dashed")
    lin4 <- geom_path(data = subset(gline, id %in% c(8)), aes(group = group),
                      colour = "white", linetype = "dotted")
    thm1 <- scale_y_continuous("", breaks = NULL)
    thm2 <- scale_x_continuous("", breaks = NULL)
    thm3 <- theme_bw()
    ggplot(toplot, aes(long, lat)) + geom_polygon(data = toplot,
                                                  aes(group = group, fill = var)) + pol1 + pol2 + pol3 +
                                                    pol4 + pol5 + lin1 + lin2 + lin3 + lin4 + thm1 + thm2 +
                                                    thm3 + geom_polygon(aes(group = group, fill = var), toplot[toplot$id %in%
                                                    c("SWZ", "LSO"), ]) + Z + coord_cartesian(xlim = c(ifelse(cut, -152, -180),
                                                                                                       180)) + theme(aspect.ratio = 2.2/4, legend.position = legendpos,
                                                                                                                     panel.border = element_blank())
  }
