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

stripes <- blank(6, width = 1500, height = 500/6, color = pride[1:6]) |>
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
write_stim(flag, "~/Desktop/", names = "wide_pride.png")


## magick -only

library(magick)

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

stripes <- sapply(pride[1:6], 
                  image_blank, 
                  width = 500, height = 50) |> 
  do.call(what = c) |>
  image_append(stack = TRUE)

square_corner <- mapply(image_blank, 
                 width = seq(600, 200, -100),
                 height = seq(600, 200, -100),
                 color = pride[7:11]) |>
  do.call(what = c)

rot_corner <- square_corner[1] |>
  image_composite(square_corner[2], offset = "+50+50") |>
  image_composite(square_corner[3], offset = "+100+100") |>
  image_composite(square_corner[4], offset = "+150+150") |>
  image_composite(square_corner[5], offset = "+200+200") |>
  image_background(color = "none", flatten = FALSE) |>
  image_rotate(45) |>
  image_repage()

# calculate some geometry t crop the corner
info <- image_info(rot_corner)
ga <- geometry_area(width = info$width / 2,
                    height = info$height*2/3,
                    x_off = info$width / 2,
                    y_off = info$height / 6)
pcnt <- geometry_size_percent(100 * 300 / (info$height*2/3))

corner <- image_crop(rot_corner, ga, repage = TRUE) |>
  image_resize(geometry = pcnt)

# superimpose
flag <- image_composite(stripes, corner)

plot(flag)

image_write(flag, "flag.png")



