#' Title
#'
#' @inheritParams ggplot2::stat_bin
#'
#' @return
#' @export
#' @importFrom ggplot2 layer
#' @examples
stat_nested <- function(mapping = NULL, data = NULL, geom = "polygon",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatNested, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @importFrom ggplot2 coord_munch ggproto Stat GeomPolygon
#' @importFrom grid pathGrob gpar
#' @importFrom scales alpha
#' @references http://qiita.com/kohske/items/9272e29a75d32416ff5e
GeomHolygon <- ggplot2::ggproto(
  "GeomHolygon",
  ggplot2::GeomPolygon,
  extra_params = c("na.rm", "rule"),
  draw_panel = function(data, scales, coordinates, rule) {
    n <- nrow(data)
    if (n == 1)
      return(zeroGrob())

    munched <- coord_munch(coordinates, data, scales)
    munched <- munched[order(munched$group), ]

    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx, ]
print(first_rows)
    ggplot2:::ggname(
      "geom_holygon",
      pathGrob(munched$x, munched$y, default.units = "native",
               id = munched$group, rule = rule,
               gp = gpar(col = first_rows$colour,
                         fill = alpha(first_rows$fill, first_rows$alpha),
                         lwd = first_rows$size * 1, #.pt,
                         lty = first_rows$linetype)))
  }
)




#' @export
#' @importFrom ggplot2 ggproto Stat
StatNested <- ggplot2::ggproto("StatNested", ggplot2::Stat,
                               compute_group = function(data, scales) {
                                 data
                               },

                               required_aes = c("x", "y", "group", "fill")
)


#' Title
#'
#' @inheritParams ggplot2::layer
#' @param na.rm remove NA
#' @param rule winding or evenodd as per \code{\link[graphics]{polypath}}
#' @param ... passed to \code{\link[ggplot2]{layer}}
#' @return
#' @export
#'
#' @examples
geom_holygon <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, rule = "winding", ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomHolygon,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm , rule = rule, ...))
}
