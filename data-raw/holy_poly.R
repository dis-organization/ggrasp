library(manifoldr)
#devtools::install_github("mdsumner/manifoldr")

library(manifoldr)
drawing <- DrawingA("data-raw/branched.map", "Drawing")
library(spbabel)
holy_poly <- sptable(drawing)
devtools::use_data(holy_poly, compress = "bzip2")
