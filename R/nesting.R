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
#' spdata <- spbabel::sp(holy_poly, attr_tab = data_frame(mydat = c("a", "b")))
#' ggg <- nest(spdata)
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
#' @importFrom ggplot2 ggplot aes geom_polygon
#' @importFrom tidyr unnest
plot.spnest <- function(x) {
  ggplot(unnest(unnest(x))) + aes(x = x_, y = y_, group = branch_, fill = object_) + geom_polygon()
}

