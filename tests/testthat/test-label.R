stimuli <- demo_stim()

# errors ----
test_that("errors", {
  expect_error(label(stimuli, gravity = "nope"))
  #expect_error(label(stimuli, location = "nope"))
  expect_error(label(stimuli, degrees = "nope"))
  expect_error(label(stimuli, size = "nope"))
  #expect_error(label(stimuli, font = "nope"))
  expect_error(label(stimuli, style = "nope"))
  #expect_error(label(stimuli, weight = "nope"))
  #expect_error(label(stimuli, kerning = "nope"))
  expect_error(label(stimuli, decoration = "nope"))
  expect_error(label(stimuli, color = "nope"))
  expect_error(label(stimuli, strokecolor = "nope"))
  expect_error(label(stimuli, boxcolor = "nope"))
})

# label ----
test_that("label", {  
  expect_silent(default <- label(stimuli))
  
  expect_silent({
    custom <- label(
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
  
  skip("needs visual check")
  plot(default)
  plot(custom)
})

# gglabel ----
test_that("gglabel", {  
  stimuli <- demo_stim()
  
  labeled_stim <- gglabel(stimuli)
  big_pink_bottom <- gglabel(
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
    geom = "text",
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
  
  skip("needs visual check")
  plot(labeled_stim)
  plot(big_pink_bottom)
  plot(watermark)
  plot(plot_anno)
})

