library(webmorphR)
library(webmorphR.stim)
library(rbenchmark)

# test images: 100 1350x1350 images and 189-point FRL 
stimuli <- load_stim_london()[1:100]

# functions for profile
bm <- benchmark(
  crop = { stim_crop <- crop(stimuli, 0.6, 0.8) },
  pad = { stim_pad <- pad(stimuli) },
  resize = { stim_resize <- resize(stimuli, 0.5) },
  rotate = { stim_rot <- rotate(stimuli, 45) },
  mask = { stim_mask <- mask(stimuli) },
  replications = 1
)
bm


## patch -----
img <- stimuli$`001_03`$img
bm <- benchmark(
  crop_first = {
    crop <- magick::image_crop(img, magick::geometry_area(10, 10, 0, 0))
    pixels <- magick::image_raster(crop)
  },
  crop_second = {
    all_pixels <- magick::image_raster(img)
    selected_pixels <- (
      all_pixels$x >= min(0, 10) &
        all_pixels$x <= max(0, 10) &
        all_pixels$y >= min(0, 10) &
        all_pixels$y <= max(0, 10)
    )
    pixels <- all_pixels[selected_pixels, ]
  },
  replications = 10
)

benchmark(
  patch = {patch(stimuli)},
  replications = 1
)



## benchmark: point rotation
i = 2
degrees = c(45, 30)
radians <- degrees * pi / 180;
xm1 <- 775
xm2 <- 640
ym1 <- 600
ym2 <-  675


benchmark(
  rotate = {
    newtem <- apply(stimuli[[i]]$points, 2, function(pt) {
      crad <- cos(radians[i])
      srad <- sin(radians[i])
      x_offset <- pt[[1]] - xm1
      y_offset <- pt[[2]] - ym1
      xr = x_offset * crad - y_offset * srad + xm2
      yr = x_offset * srad + y_offset * crad + ym2
      
      c(x = xr, y = yr)
    })
  },
  rot2 = {
    pt <- stimuli[[i]]$points
    offset <- pt - c(xm1, ym1)
    crad <- cos(radians[i]) * offset
    srad <- sin(radians[i]) * offset
    xr <- crad[1,] - srad[2,] + xm2
    yr <- srad[1,] + crad[2,] + ym2
    newtem2 <- matrix(c(xr, yr), 2, 
                                  byrow = TRUE, 
                                  dimnames = dimnames(pt))
  },
  replications = 100,
  order = "test",
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)

all(newtem == newtem2)


## benchmark: image_info vs assert_image

benchmark(
  image_info = {
    for (i in 1:length(stimuli)) {
      magick::image_info(stimuli[[i]]$img) -> sink
    }
  },
  assert = {
    for (i in 1:length(stimuli)) {
      magick:::assert_image(stimuli[[i]]$img) -> sink
    }
  },
  call = {
    for (i in 1:length(stimuli)) {
      .Call('_magick_magick_image_dead', PACKAGE = 'magick', stimuli[[i]]$img) -> sink
    }
  },
  replications = 10,
  order = "test",
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)


# benchmark magic::image_crop iteration methods ----

ga <- magick::geometry_area(
  width = 500,
  height = 500,
  x_off = 100,
  y_off = 200
)

benchmark(
  iterate = {
    newimg <- stimuli
    for (i in 1:length(stimuli)) {
      newimg[[i]]$img <- magick::image_crop(
        image = stimuli[[i]]$img,
        geometry = ga,
        gravity = "NorthWest"
      )
    }
  },
  apply = {
    newimg_a <- lapply(1:length(stimuli), function(i) {
      magick::image_crop(
        image = stimuli[[i]]$img,
        geometry = ga,
        gravity = "NorthWest"
      )
    })
  },
  vectorise = {
    newimg_v <- magick::image_crop(
      image = get_imgs(stimuli),
      geometry = ga,
      gravity = "NorthWest"
    )
  },
  replications = 1,
  order = "test",
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)


## benchmark offset application methods ----

pts <- stimuli[[1]]$points
benchmark(
  apply = {
    newpt1 <- apply(pts, 2, function(pt){
      pt - c(100, 200)
    })
  },
  apply2 = {
    f <- function(pt) { pt - c(100, 200) }
    newpt2 <- apply(pts, 2, f)
  },
  apply3 = {
    offset <- c(100, 200)
    newpt3 <- apply(pts, 2, `-`, offset)
  },
  sub = {
    newpt_s <- pts
    newpt_s[1, ] <- pts[1, ] - 100
    newpt_s[2, ] <- pts[2, ] - 200
  },
  sub2 = {
    offset <- c(100, 200) |>
      rep(times = ncol(pts)) |>
      matrix(2)
    newpt_s2 <- pts - offset
  },
  sub3 = {
    newpt_s3 <- pts - c(100, 200)
  },
  replications = 1000,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)

# check same output
all(newpt1 == newpt1)
all(newpt1 == newpt3)
all(newpt1 == newpt_s)
all(newpt1 == newpt_s2)
all(newpt1 == newpt_s3)
