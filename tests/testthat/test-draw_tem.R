test_that("draw_tem", {
  stimuli <- demo_stim("f_multi")
  default <- draw_tem(stimuli)
  red <- draw_tem(stimuli, "red")
  
  pt <- get_point(default, 0)
  ptcol <- patch(default, 
                 width = 1.1,
                 height = 1.1,
                 x_off = round(pt$x), 
                 y_off = round(pt$y),
                 color = "rgb")
  expect_gt(ptcol$f_multi[["green"]], 100)
  expect_lt(ptcol$f_multi[["red"]], 30)
  expect_lt(ptcol$f_multi[["blue"]], 30)
  
  ptcol <- patch(red, 
                 width = 1.1,
                 height = 1.1,
                 x_off = round(pt$x), 
                 y_off = round(pt$y),
                 color = "rgb")
  expect_gt(ptcol$f_multi[["red"]], 100)
  expect_lt(ptcol$f_multi[["green"]], 30)
  expect_lt(ptcol$f_multi[["blue"]], 30)
  
  # thicker lines
  thick <- draw_tem(stimuli, pt.alpha = 0, line.size = 20)
  thin <- draw_tem(stimuli, pt.alpha = 0, line.size = 0.5)
  thick_col <- patch(thick, 1, 1, 0, 0, "rgb", mean)
  thin_col <- patch(thin, 1, 1, 0, 0, "rgb", mean)
  expect_gt(thick_col$f_multi[["blue"]], thin_col$f_multi[["blue"]])

  # check size scales with image
  red2 <- stimuli |> resize(0.5) |> draw_tem("red")
  
  alpha1 <- draw_tem(stimuli, "red", pt.size = 10, pt.alpha = 1)
  pt <- get_point(alpha1, 0)
  ptcol <- patch(alpha1, 
                 width = 1.1,
                 height = 1.1,
                 x_off = round(pt$x), 
                 y_off = round(pt$y),
                 color = "rgb")
  expect_equal(ptcol$f_multi, c(red=255, green=0, blue=0, alpha=255))

  # plot(default)
  # plot(red)
  # plot(red2)
  # plot(thick)
  # plot(thin)
})
