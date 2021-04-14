test_that("mask_oval", {
  stimuli <- demo_stim("lisa")

  expect_silent(x <- mask_oval(stimuli, each = TRUE))
  expect_silent(y <- mask_oval(stimuli, each = FALSE))

  bounds <- c(
    top = 40,
    right = 20,
    bottom = 20,
    left = 40
  )

  expect_silent(z <- mask_oval(stimuli, bounds, fill = "hotpink"))

  skip("needs visual check")

  x %>% draw_tem() %>% plot()
  y %>% plot()
  z %>% plot()
})
