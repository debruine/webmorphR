test_that("basic", {
  stimuli <- demo_stim("test")
  expect_silent(default <- mask(stimuli))

  # frl masks ----
  masks <- c("oval", "face", "neck", "ears", "eyes", "brows", "nose", "mouth", "teeth", "left_ear", "right_ear", "left_brow", "right_brow", "left_eye", "right_eye")
  colours <- c("red", "#FF6600", "#FFFF0088", "green", "blue", "purple",
               "red", "#FF6600", "#FFFF0088", "green", "blue", "purple",
               "black", "white", "#FFFFFF66")
  masked <- list()
  for (i in seq_along(masks)) {
    expect_silent(masked[[i]] <- mask(stimuli, masks[i], colours[i]))
  }

  # reverse masks ----
  revmasked <- list()
  for (i in seq_along(masks)) {
    expect_silent(revmasked[[i]] <- mask(stimuli, masks[i], colours[i], TRUE))
  }

  # custom masks ----
  # left eye
  lmask <- "18,19,20,21,22;22,30,29,28,18"
  expect_silent(textmask <- mask(stimuli, lmask, reverse = TRUE))

  # right eye
  rmask <- list(left_eye = list(23:27, c(27, 33:31, 23)))
  expect_silent(listmask <- mask(stimuli, rmask, reverse = TRUE))

  # visual checks ----
  skip("needs visual check")

  plot(default)
  plot(masked[[1]])
  plot(revmasked[[5]])
  plot(textmask[1])
  plot(listmask[1])
})
