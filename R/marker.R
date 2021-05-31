#' Choropleth world maps, add secondary country markers
#'
#' `add_marker()` is used in combination with `whomap()` and adds a secondary
#' marker located at the centroid of listed countries and territories.
#'
#' @param iso3 A vector of ISO3 country codes.
#' @param shape Marker shape.
#' @param col Marker colour.
#' @param size Marker size.
#' @param lab Marker label.
#' @return A ggplot2 function.
#' @author Philippe Glaziou.
#' @import ggplot2
#' @export
#' @examples
#' brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'), var=1:5)
#' whomap(brics, legend.title='BRICS', legend.pos=c(0.14, 0.34)) +
#'    add_marker('BRA', lab='Subnational\ndata')
#'
add_marker <- function(iso3 = NA_character_,
                       shape = 17,
                       col = 'red',
                       size = 3,
                       lab = '') {
  if (!(iso3 %in% centroid$id))
    stop("iso3 should only include valid ISO3 country codes")

  dta1 <- dta2 <- centroid[centroid$id %in% iso3, ]
  dta2$long <- dta2$long + 360
  dta <- rbind(dta1, dta2)
  dta$var2 <- ''

  list(
    ggplot2::geom_point(
      aes(.data$long, .data$lat, shape = .data$var2),
      data = dta,
      col = col,
      size = size
    ),
    ggplot2::scale_shape_manual(values=shape, label=lab),
    ggplot2::guides(shape = guide_legend(element_blank(), order = 0))
  )
}
