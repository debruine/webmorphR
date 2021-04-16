test_that("draw_tem", {
  stimuli <- demo_stim("test", "f_multi")
  default <- draw_tem(stimuli)
  red <- draw_tem(stimuli, "red")
  thick <- draw_tem(stimuli, "red", 5)
  thin <- draw_tem(stimuli, "red", 0.5)

  # check size scales with image
  red2 <- stimuli %>% resize(0.5) %>% draw_tem("red")

  skip("needs visual check")
  plot(default)
  plot(red)
  plot(red2)
  plot(thick)
  plot(thin)
})
