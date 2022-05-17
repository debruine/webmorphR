def_parts <- c("name", "notes", "delin_pts", "align_pts", "width", "height", "points", "lines", "closed", "linecolor", "masks")

# tem_def ----
test_that("tem_def", {
  frl <- tem_def()
  expect_equal(names(frl), def_parts)
  expect_equal(frl$delin_pts, c(0, 1, 96))

  frl1 <- tem_def("frl")
  expect_equal(names(frl1), def_parts)
  expect_equal(frl, frl1)

  fpp83 <- tem_def("fpp83")
  expect_equal(names(fpp83), def_parts)
  expect_equal(fpp83$delin_pts, c(63, 81, 82))

  fpp106 <- tem_def("FPP106")
  expect_equal(names(fpp106), def_parts)
  expect_equal(fpp106$delin_pts, c(0, 1, 95))

  # from file
  file <- system.file("extdata/tem_defs/frl.json", package = "webmorphR")
  frl2 <- tem_def(path = file)
  expect_equal(names(frl2), def_parts)
  expect_equal(frl2$delin_pts, c(0, 1, 96))

  expect_equal(frl, frl2)
})

test_that("online", {
  skip_if_offline()

  # no masks in online version
  frl_online <- tem_def(1)
  expect_equal(names(frl_online), def_parts[1:10])
  expect_equal(frl_online$delin_pts, c(0, 1, 96))
})


# subset_tem ----
test_that("subset_tem", {
  stimuli <- demo_stim()
  
  # error
  expect_error(subset_tem(stimuli, 0:200))
  
  # keep
  nt <- subset_tem(stimuli, 0:20)
  expect_equal(stimuli[[1]]$points[, 1:21], nt[[1]]$points)
  
  # delete
  nt <- subset_tem(stimuli, 0:9, 20:188, keep = FALSE)
  expect_equal(stimuli[[1]]$points[, 11:20], nt[[1]]$points)
  
  # no tem
  stimuli <- demo_stim() |> remove_tem()
  expect_error(x <- subset_tem(stimuli),
               "No images had templates")
})

# features ----
test_that("features", {
  expect_error(features("all", tem_id = "nope"))
  
  expect_null(features())
})

## frl features ----
test_that("frl", {
  stimuli <- demo_stim()[1]
  
  mouth_pts <- features("mouth", tem_id = "frl")
  
  mouth <- stimuli |> subset_tem(mouth_pts)
  expect_equal(mouth[[1]]$points, 
               stimuli[[1]]$points[, mouth_pts+1])
  
  

  features <- c("gmm", "oval", "face", "mouth", "nose", "eyes", "brows",
                "left_eye",  "right_eye", "left_brow",  "right_brow",
                "ears", "undereyes", "teeth", "smile_lines", 
                "cheekbones", "philtrum", "chin", "neck", "halo")
  names(features) <- features
  
  expect_silent(
    new <- lapply(features, function(ft) {
      stimuli |> subset_tem(features(ft)) |> draw_tem()
    }) |> do.call(c, args = _)
  )
  
  # skip("needs visual check")
  # new |> setnames(features) |> label() |> plot()
})

## dlib70 features ----
test_that("dlib70", {
  stimuli <- demo_stim("tem_examples", "dlib70")
  
  mouth_pts <- features("mouth", tem_id = "dlib70")
  
  mouth <- stimuli |> subset_tem(mouth_pts)
  expect_equal(mouth[[1]]$points, 
               stimuli[[1]]$points[, mouth_pts+1])
  
  features <- c("face", "mouth", "nose", "eyes", "brows",
                "left_eye",  "right_eye", "left_brow", "right_brow",
                "teeth", "gmm")
  names(features) <- features
  
  expect_silent({
    new <- lapply(features, function(ft) {
      stimuli |> 
        subset_tem(features(ft, tem_id = "dlib70")) |> 
        draw_tem()
    }) |>
      do.call(c, args = _)
  })
  
  # skip("needs visual check")
  # new |> setnames(features) |> label() |> plot()
})
