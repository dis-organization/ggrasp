# #devtools::install_github("mdsumner/manifoldr")
#
# library(manifoldr)
# drawing <- DrawingA("data-raw/branched.map", "Drawing")
# library(spbabel)
# holy_poly <- sptable(drawing)
# devtools::use_data(holy_poly, compress = "bzip2")


library(rgdal)
drawing <- readOGR("data-raw", "mybranch")
 library(spbabel)
 holy_poly <- sptable(drawing)
 devtools::use_data(holy_poly, compress = "bzip2")
