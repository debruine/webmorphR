test_that("basic", {
  stimuli <- demo_stim()
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
  # plot(default)
  # plot(masked[[1]])
  # plot(revmasked[[5]])
  # plot(textmask[1])
  # plot(listmask[1])
})

## custom mask from vectors ----
test_that("custom mask from vectors", {
  stimuli <- demo_stim()
  
  stimuli[1] |>
    crop_tem() |>
    resize(2) |>
    draw_tem(pt.shape = "index", pt.size = 10)
  
  list_mask <- list(list(
    c(71:75, 50, 56, 78:82),
    c(82,49),
    c(49:47),
    c(47:46),
    c(46:44),
    c(44, 71)
  ))
  text_mask <- lapply(list_mask, lapply, paste, collapse = ",") |>
    lapply(paste, collapse = ";") |>
    paste(collapse = ":")
  
  t <- mask(stimuli[1], text_mask)
  l <- mask(stimuli[1], list_mask)
  
  expect_equal(c(f_multi = 0), 
               compare(t, l, scale = TRUE))
})

# expand ----
test_that("expand", {
  stimuli <- demo_stim(1)
  top_pt <- stimuli[[1]]$points[, 139+1] |> round()
  
  mask1 <- mask(stimuli, fill = "red", expand = 0)
  mask10 <- mask(stimuli, fill = "red", expand = 10)
  negmask <- mask(stimuli, fill = "red", expand = -10)
  revmask1 <- mask(stimuli, fill = "red", expand = 0, reverse = TRUE)
  revmask10 <- mask(stimuli, fill = "red", expand = 10, reverse = TRUE)
  revnegmask <- mask(stimuli, fill = "red", expand = -10, reverse = TRUE)
  c(mask1, mask10, negmask, revmask1, revmask10, revnegmask) |> 
    crop_tem() |> draw_tem() |> plot(nrow = 2)
  
  # check images are different
  diff <- compare(mask1, mask10)[[1]]
  expect_gt(diff, 0)
  
  negdiff <- compare(mask1, negmask)[[1]]
  expect_gt(negdiff, 0)
  
  revdiff <- compare(revmask1, revmask10)[[1]]
  expect_gt(revdiff, 0)
  
  revnegdiff <- compare(revmask1, revnegmask)[[1]]
  expect_gt(revnegdiff, 0)
  
  patch1 <- patch(mask1, x1 = top_pt["x"], x2 = top_pt["x"], 
                  y1 = top_pt["y"] - 3, y2 = top_pt["y"] - 3)
  patch10 <- patch(mask10, x1 = top_pt["x"], x2 = top_pt["x"], 
                  y1 = top_pt["y"] - 3, y2 = top_pt["y"] - 3)
  expect_equal(patch1[[1]], "#FF0000FF")
  expect_true(patch1 != patch10)
  
  revpatch1 <- patch(revmask1, x1 = top_pt["x"], x2 = top_pt["x"], 
                  y1 = top_pt["y"] - 3, y2 = top_pt["y"] - 3)
  revpatch10 <- patch(revmask10, x1 = top_pt["x"], x2 = top_pt["x"], 
                   y1 = top_pt["y"] - 3, y2 = top_pt["y"] - 3)
  expect_equal(revpatch10[[1]], "#FF0000FF")
  expect_true(revpatch1 != revpatch10)
})
