stimuli <- demo_stim("f_multi")

test_that("works", {
  
  ow <- stimuli[[1]]$width
  oh <- stimuli[[1]]$height
  w = 100
  h = 100
  
  # no offsets
  ctems <- crop(stimuli, w, h)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, w)
  expect_equal(ctems[[1]]$height, h)
  expect_equal(info$width, w)
  expect_equal(info$height, h)
  expect_equal(stimuli[[1]]$points - c(ow-w, oh-h)/2, ctems[[1]]$points)

  # with offsets
  ctems <- crop(stimuli, w, h, 50, 75)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, w)
  expect_equal(ctems[[1]]$height, h)
  expect_equal(info$width, w)
  expect_equal(info$height, h)
  orig_pt <- stimuli[[1]]$points[, 1]
  new_pt <- ctems[[1]]$points[, 1]
  expect_equal(orig_pt, new_pt + c(50, 75))

  # percents, no height
  ctems <- crop(stimuli, .5)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, ow/2)
  expect_equal(ctems[[1]]$height, oh)
  expect_equal(info$width, ow/2)
  expect_equal(info$height, oh)
  expect_equal(stimuli[[1]]$points - c(ow/4, 0), ctems[[1]]$points)
})

test_that("different crops", {
  stimuli <- demo_stim()
  ow <- width(stimuli)[[1]]

  w <- c(0.5, .6)
  ctems <- crop(stimuli, w, w)
  expect_equivalent(width(ctems) |> unname(), ow*w)
  expect_equivalent(height(ctems) |> unname(), ow*w)
  expect_equal(stimuli[[1]]$points - c(ow/4, ow/4), ctems[[1]]$points)
  expect_equal(stimuli[[2]]$points  - c(ow*.2, ow*.2), ctems[[2]]$points)
})

test_that("no tem", {
  notem <- demo_stim() |> remove_tem()
  x <- crop(notem, 300, 300)
  comp <- c(f_multi = 300, m_multi = 300)
  expect_equal(width(x), comp)
  expect_equal(height(x), comp)
})

test_that("limits", {
  s <- demo_stim()
  
  # 2-pixel width/height
  x <- crop(s, 2, 2)
  expect_equal(width(x), c(f_multi = 2, m_multi = 2))
  expect_equal(height(x), c(f_multi = 2, m_multi = 2))
  
  # 199% width/height
  x <- crop(s, 1.99999, 1.99999)
  expect_equal(width(x), c(f_multi = 999, m_multi = 999))
  expect_equal(height(x), c(f_multi = 999, m_multi = 999))
  
  # 99% offset
  x <- crop(s, x_off = .99, y_off = .99)
  expect_equal(patch(x$f_multi, 
                     width = 495, height = 495,
                     x_off = 5, y_off = 5)[[1]],
               "#FFFFFFFF")
  
  # 1-pixel offset 
  x <- crop(s, x_off = 1, y_off = 1)
  expect_true(patch(x$f_multi, 500, 500)[[1]] !=
               "#FFFFFFFF")
  
})

# no images ----
test_that("no images", {
  tem <- demo_stim(".tem")
  expect_warning(tem_crop <- crop(tem, 0.5, 0.5))
  diff <- tem$f_multi$points - tem_crop$f_multi$points
  expect_equal(unique(diff[1, ]) |> length(), 1)
  expect_equal(unique(diff[2, ]) |> length(), 1)
})

# transparent fill ----
test_that("transparent fill", {
  stimuli <- demo_stim() |> mask(fill = "red")
  fill <- "#00000066"
  
  x <- crop(stimuli, 1.2, .8, fill = fill)
  fg <- patch(x, .1, 10, .45)
  expect_equal(fg[[1]], "#FF0000FF")
  bg <- patch(x, 10, 1)
  expect_equal(bg[[1]], fill)
  
  x <- crop(stimuli, 1.2, .8, fill = "none")
  fg <- patch(x, .1, 10, .45)
  expect_equal(fg[[1]], "#FF0000FF")
  bg <- patch(x, 10, 1)
  expect_equal(bg[[1]], "transparent")
})