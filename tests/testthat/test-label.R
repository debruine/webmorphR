stimuli <- demo_stim()

# errors ----
test_that("errors", {
  expect_error(mlabel(stimuli, gravity = "nope"))
  #expect_error(mlabel(stimuli, location = "nope"))
  expect_error(mlabel(stimuli, degrees = "nope"))
  expect_error(mlabel(stimuli, size = "nope"))
  #expect_error(mlabel(stimuli, font = "nope"))
  expect_error(mlabel(stimuli, style = "nope"))
  #expect_error(mlabel(stimuli, weight = "nope"))
  #expect_error(mlabel(stimuli, kerning = "nope"))
  expect_error(mlabel(stimuli, decoration = "nope"))
  expect_error(mlabel(stimuli, color = "nope"))
  expect_error(mlabel(stimuli, strokecolor = "nope"))
  expect_error(mlabel(stimuli, boxcolor = "nope"))
})

# mlabel ----
test_that("mlabel", {  
  expect_silent(default <- mlabel(stimuli))
  
  # don't change size
  stim_info <- stimuli[[1]]$img |> magick::image_info()
  label_info <- default[[1]]$img |> magick::image_info()
  expect_equal(stim_info$height, label_info$height)
  expect_equal(stim_info$width, label_info$width)
  expect_equal(stim_info$density, label_info$density)
  
  expect_silent({
    custom <- mlabel(
      stimuli, 
      text = c("ABCDE", "FGHIJ"), 
      gravity = c("north", "west"),
      location = c("+0+10", "+70+150"),
      degrees = c(0, 270),
      size = c(60, 100),
      font = c("sans", "serif"),
      style = c("normal","italic"),
      weight = c(400, 900),
      kerning = c(-10, 20),
      decoration = c("LineThrough", "Underline"),
      color = c("red", "blue"),
      strokecolor = c("blue", NA),
      boxcolor = c(NA, "black")
    )
  })
})

# gglabel ----
test_that("gglabel", {  
  stimuli <- demo_stim()
  
  default <- gglabel(stimuli)
  
  # don't change size
  stim_info <- stimuli[[1]]$img |> magick::image_info()
  label_info <- default[[1]]$img |> magick::image_info()
  expect_equal(stim_info$height, label_info$height)
  expect_equal(stim_info$width, label_info$width)
  expect_equal(stim_info$density, label_info$density)
  
  
  big_pink_bottom <- gglabel(
    geom = "label",
    stimuli, 
    size = 8, 
    x = 0.05, 
    y = 0.05,
    hjust = 0,
    vjust = 0, 
    fill = "hotpink", 
    color = "dodgerblue2",
    label.padding = ggplot2::unit(4, "mm"),
    label.r = ggplot2::unit(0.5, "lines"),
    label.size = 1,
    alpha = 0.7
  )
  
  watermark <- gglabel(
    stimuli,
    label = "watermark",
    size = 20,
    color = "black",
    angle = -30,
    x = 0.5, 
    y = 0.5,
    alpha = 0.5
  )
  
  expect_silent(
    plot_anno <- gglabel(
      stimuli,
      geom = "vline",
      xintercept = width(stimuli)/2,
      color = "black"
    )
  )
})

# label ----
test_that("label", {  
  stimuli <- demo_stim()
  m_guess <- label(stimuli, text = c("A", "B"))
  gg_guess <- label(stimuli, label = c("A", "B"))
  m_explicit <- mlabel(stimuli, text = c("A", "B"))
  gg_explicit <- gglabel(stimuli, label = c("A", "B"))
  
  same_m <- image_comp(m_guess[1], m_explicit[1])
  same_gg <- image_comp(gg_guess[1], gg_explicit[1])
  diff <- image_comp(gg_guess[1], m_explicit[1])
  
  expect_equal(same_m[[1]], 0)
  expect_equal(same_gg[[1]], 0)
  expect_true(diff[[1]] > 0)
  
  # c(m_guess, m_explicit, gg_guess, gg_explicit) |>
  #   plot(maxwidth = 500)
})
  

