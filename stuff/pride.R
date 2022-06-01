# devtools::install_github("debruine/webmorphR")
library(webmorphR)
wm_opts(fill = "none")

# colors via https://twitter.com/dataandme/status/1531946768270860288?s=20&t=J25ZA4UETcz-awixPP5gDA 
pride <- c(
  red = '#E50000',
  orange = '#FF8D00',
  yellow = '#FFEE00',
  green = '#028121',
  blue = '#004CFF',
  purple = '#760088',
  black = '#000000',
  brown = '#613915',
  aqua = '#73D7EE',
  pink = '#FFAFC7',
  white = '#FFFFFF'
)

stripes <- blank(6, width = 500, height = 50, color = pride[1:6]) |>
  plot(nrow = 6, padding = 0)

corner <- blank(1, 200, 200, color = pride["white"]) |>
  pad(50, fill = pride["pink"]) |>
  pad(50, fill = pride["aqua"]) |>
  pad(50, fill = pride["brown"]) |>
  pad(50, fill = pride["black"]) |>
  rotate(degrees = 45, 
         keep_size = FALSE) |>
  crop(width = 0.5, height = 2/3, 
       x_off = 0.5, y_off = 1/6) |>
  resize(height = height(stripes))

flag <- image_func(stripes, "composite", corner$img$img)

plot(flag) # view image

# write to disk
write_stim(flag, names = "prog_pride.png")