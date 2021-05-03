stimuli <- demo_stim()

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
