
#' Title
#'
#' @param dat1
#' @param map1
#'
#' @return
#' @export
#' @examples
#' map1 <- holy_poly
#' dat1 <- tibble(object_ = 1:3, mydat = c("a", "b", "c"))
#' @importFrom dplyr %>% bind_rows distinct mutate select select_
#' @importFrom tibble tibble
grasp <- function(dat1, map1) {
  ## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
  v_atts <- c("x_", "y_")

  map1 <- map1 %>%
    mutate(vertex_  = as.integer(factor(do.call(paste, select_(map1, .dots = v_atts)))))
  sXv <- bind_rows(lapply(split(map1, map1$branch_), function(a) tibble(vertex_ = pairs1(a$vertex_), branch_ = a$branch_[1])))
  sXv$segment_ <- rep(seq(nrow(sXv)/2), each = 2)
  s <- sXv %>% distinct(segment_)
  bXs <- sXv %>% distinct(segment_, .keep_all = TRUE) %>% select(-vertex_)
  sXv$branch_ <- NULL
  b <- map1 %>% distinct(object_, branch_, island_) %>% dplyr::select(object_, branch_, island_)


  b <- map1 %>% dplyr::select(-object_, -island_)

  ## four tables (dat1, map2, map4, map5)
  bXv <- b %>% dplyr::select(branch_, vertex_, order_)
  v <- b %>% distinct(x_, y_, vertex_) %>% dplyr::select(x_, y_, vertex_)

  list(o = dat1, b = b, bXv = bXv, v = v, s = s, sXv = sXv, bXs = bXs)

}

pairs0 <- function (x) {
  data_frame(s0 = head(x, -1), s1 = tail(x, -1))
}

grist <- function(x) {
  br <- vector("list", nrow(x$o))
  for (i in seq(nrow(x$o))) {
    obj <- x$o %>% select(object_) %>% slice(i)  %>% inner_join(x$b, "object_") %>% inner_join(x$bXv, "branch_") %>% inner_join(x$v, "vertex_")
    br[[i]] <- bind_rows(lapply(split(obj, obj$branch_), function(a) bind_cols(pairs0(a$vertex_), data_frame(branch_ = head(a$branch_, -1)))))
  }
  bind_rows(br)
}

seg2struct <- function(x) {
  v <- x$v
  v$i <- seq(nrow(v))
  s <- x$s
  s %>% inner_join(v, c("s0" = "vertex_")) %>%
    transmute(s1, i0 = i) %>%
    inner_join(v, c("s1" = "vertex_")) %>%
    transmute(i0, i1 = i)
}

