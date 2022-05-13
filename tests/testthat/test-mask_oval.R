test_that("mask_oval", {
  stimuli <- demo_stim("lisa", 1:2)

  expect_silent(x <- mask_oval(stimuli, each = TRUE))
  expect_silent(y <- mask_oval(stimuli, each = FALSE))

  bounds <- c(
    top = 40,
    right = 20,
    bottom = 20,
    left = 40
  )

  expect_silent(z <- mask_oval(stimuli, bounds, fill = "red"))
  
  # check mask presence and absence
  for (i in 1:2) {
    in_border <- patch(z[[i]]$img, 
          x1 = width(z)/2, x2 = width(z)/2, 
          y1 = 35, y2 = 35)
    out_border <- patch(z[[i]]$img, 
                        x1 = width(z)/2, x2 = width(z)/2, 
                        y1 = 45, y2 = 45)
    expect_equal("#FF0000FF", in_border)
    expect_true(in_border != out_border)
  }

  # skip("needs visual check")
  # # oval at point borders for each face
  # x |> draw_tem() |> plot(maxwidth = 500) 
  # # same oval on both images, can see both faces
  # y |> plot(maxwidth = 500)
  # # red oval with 40/20/20/40 borders
  # z |> plot(maxwidth = 500) 
})
