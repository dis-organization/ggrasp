#' @rdname nest-Spatial
#' @export
nest1_.Spatial <- function(data, ...) {
  sptab <-  sptable(data) %>%
    group_by_("object_") %>% nest_(key_col = "Object")

  attrd <- as_data_frame(as.data.frame(data))
  y <- bind_cols(attrd, sptab)
  attr(y, "crs") <- proj4string(data)
  class(y) <- c("sp1nest", class(y))
  y
}

#' Nested Spatial
#'
#' Create nested tables from Spatial classes.
#'
#' For \code{nest_} the tables are doubly nested.
#' For \code{nest1_} the tables are singly nested.
#' @param data sp Spatial object
#' @param ... ignored
#' @examples
#' library(tidyr)
#' data(holy_poly)
#' library(dplyr)
#' spdata <- spbabel::sp(holy_poly, attr_tab = data_frame(mydat = c("a", "b")))
#' ##ggplot(holy_poly, aes(x = x_, y = y_)) + geom_holygon(aes(group = branch_, fill = object_))
#' ggg <- nest(spdata)
#' plot(ggg, col = rainbow(nrow(ggg), alpha = 0.5))
#' ## plot with ggvis
#' ##holy_poly  %>% group_by(object_)  %>% ggvis(~x_, ~y_, fill = ~object_, stroke = ~branch_)  %>% layer_paths()
#'
#' @return nested tibble
#' @export
#'
#' @importFrom spbabel sptable
#' @importFrom dplyr as_data_frame bind_cols group_by_
#' @importFrom sp proj4string
#' @importFrom tidyr nest_
#' @rdname nest-Spatial
nest_.Spatial <- function(data, ...) {
  sptab <-  sptable(data) %>%
    group_by_("branch_", "object_", "island_") %>%
    nest_(key_col = "Branch_") %>%
    group_by_("object_") %>% nest_(key_col = "Object")

  attrd <- as_data_frame(as.data.frame(data))
  y <- bind_cols(attrd, sptab)
  attr(y, "crs") <- proj4string(data)
  class(y) <- c("spnest", class(y))
  y
}

#' Plot spnest
#'
#' Basic ggplot of nested tibble.
#' @param x nested tibble
#'
#' @return
#' @export
#'
#' @examples
#' hp <- bind_rows(lapply(split(coords, coords$branch_), function(x) if (x$island_[1]) x else {x <- x[nrow(x):1, ]; x}))
#' @importFrom ggplot2 ggplot aes geom_polygon
#' @importFrom tidyr unnest
plot.spnest <- function(x, y = "object_", col = "#7F7F7F7F", ..., add = FALSE) {
  allcoords <- unnest(unnest(x[, "Object"]))
  #coords %>% group_by_("object_") %>% ggvis(~x_, ~y_, fill = ~object_, stroke = ~branch_)  %>% layer_paths()
  if (!add) plot0(allcoords$x_, allcoords$y_)
  col <- rep(col, nrow(allcoords))
  for (i in seq(nrow(x))) {
    coords <- unnest(unnest(x[i, "Object"]))
    nacoords <- na_grp(coords, coords$branch_)
    polypath(nacoords$x_, nacoords$y_, col = col[i], ...)
  }
  invisible(NULL)
}

#' @importFrom dplyr bind_rows
na_grp <- function(x, g) {
  head(bind_rows(lapply(split(x, g), function(xa) rbind(xa, NA))), -1)
}
## from https://github.com/rstudio/gggeom
plot0 <- function(x, y,
                      xlim = range(x, na.rm = TRUE),
                      ylim = range(y, na.rm = TRUE), ...) {
  old <- par(mar = c(1.5, 1.5, 0, 0), cex = 0.8)
  on.exit(par(old))

  plot.default(xlim, ylim, type = "n", xlab = "", ylab = "", axes = FALSE)
  axis(1, lwd = 0, lwd.ticks = 1, col = "grey80", col.axis = "grey60", padj = -1)
  axis(2, lwd = 0, lwd.ticks = 1, col = "grey80", col.axis = "grey60", padj = 1)
  grid(lty = "solid", col = "grey80")
}
