rot_test <- function(orig_w, orig_h, degrees) {
  info <- blank(1, orig_w, orig_h) |> 
    get_imgs() |> 
    magick::image_rotate(degrees) |> 
    magick::image_info()
  
  rot <- rotated_size(orig_w, orig_h, degrees)
  
  list(
    magick_width = info$width,
    magick_height = info$height,
    calc_width = rot$width,
    calc_height = rot$height
  )
}

library(purrr)

x <- expand.grid(orig_w = c(100, 25),
                 orig_h = c(100, 25),
                 degrees = seq(15, 75, 15))


y <- bind_cols(x, pmap_df(x, rot_test))
  

y$w_round_diff <- y$magick_width - round(y$calc_width)
y$w_floor_diff <- y$magick_width - floor(y$calc_width)
y$w_ceil_diff <- y$magick_width - ceiling(y$calc_width)

# the resulting magick image's width and height are always odd if the original image's woidth or height are odd (except square rotations)

dplyr::arrange(y, orig_w, orig_h, degrees) |> View()
