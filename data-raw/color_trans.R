# ## code to prepare `color_trans` dataset goes here
#
# color_trans <- expand.grid(red = 0:255, green = 0:255, blue = 0:255)
# hex <- as.hexmode(0:255)
# color_hex <- expand.grid(r = hex, g = hex, b = hex)
# color_trans$hex <- sprintf('#%s%s%s', color_hex$r, color_hex$g, color_hex$b)
# hsv <- grDevices::rgb2hsv(color_trans$red, color_trans$green, color_trans$blue)
# color_trans$h <- hsv["h", ]
# color_trans$s <- hsv["s", ]
# color_trans$v <- hsv["v", ]
# lab <- webmorphR:::col2lab(color_trans$hex)
# color_trans$l <- lab$L
# color_trans$a <- lab$a
# color_trans$b <- lab$b
#
# # ended up about 400MB, so not worth it :(
# usethis::use_data(color_trans, overwrite = TRUE)
