#wm_opts(server = "https://webmorph.test")

# avg ----
test_that("avg", {
  skip_on_cran()
  skip_if_offline()

  # can't average more than 100 images
  stim120 <- demo_stim() |> rep(60)
  expect_error(avg(stim120))

  # normalisation
  stimuli <- demo_stim()
  avg <- avg(stimuli)
  twopoint <- avg(stimuli, norm = "twopoint")
  rigid <- avg(stimuli, norm = "rigid")
  notex <- avg(stimuli, texture = FALSE)
  
  tem <- average_tem(stimuli)
  dif <- tem[[1]]$points -  avg[[1]]$points
  expect_lt(max(dif), 1)
  expect_gt(min(dif), -1)
  expect_true(compare(avg, twopoint) > 0)
  expect_true(compare(avg, rigid) > 0)
  expect_true(compare(avg, notex) > 0)

  # format (fix tomcat files for this)
  # png <- avg(stimuli, format = "png")
  # gif <- avg(stimuli, format = "gif")
  # expect_equal(magick::image_info(avg$avg$img)$format, "JPEG")
  # expect_equal(magick::image_info(gif$avg$img)$format, "PNG")
  # expect_equal(magick::image_info(png$avg$img)$format, "GIF")

  ## visual checks ----
  # skip("needs visual check")
  # draw_tem(avg)
  # draw_tem(notex)
  # draw_tem(twopoint)
  # draw_tem(rigid)

  ## long process ----
  skip("long process")
  expect_silent(avg100 <- avg(stim120[1:100]))
})

wm_opts(server = "https://webmorph.org")
