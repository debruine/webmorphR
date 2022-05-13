test_that("draw_tem", {
  stimuli <- demo_stim("test", "f_multi")
  default <- draw_tem(stimuli)
  red <- draw_tem(stimuli, "red")
  thick <- draw_tem(stimuli, "red", 5)
  thin <- draw_tem(stimuli, "red", 0.5)

  # check size scales with image
  red2 <- stimuli |> resize(0.5) |> draw_tem("red")
  
  alpha1 <- draw_tem(stimuli, "red", pt.size = 10, pt.alpha = 1)
  pt <- get_point(alpha1, 0)
  ptcol <- patch(alpha1[[1]]$img, 
        x1 = floor(pt$x), 
        x2 = floor(pt$x),
        y1 = floor(pt$y), 
        y2 = floor(pt$y)) |> color_conv(to = "rgb")
  expect_equal(ptcol, c(255, 0, 0))

  # plot(default)
  # plot(red)
  # plot(red2)
  # plot(thick)
  # plot(thin)
})
