wm_opts(server = "https://webmorph.test")

# avg ----
test_that("avg", {
  skip_on_cran()

  # can't average more than 100 images
  stim120 <- rep(demo_stim(), 60)
  expect_error(avg(stim120))

  # normalisation
  stimuli <- demo_stim()
  avg <- avg(stimuli)
  twopoint <- avg(stimuli, norm = "twopoint")
  rigid <- avg(stimuli, norm = "rigid")
  notex <- avg(stimuli, texture = FALSE)

  # format (fix tomcat files for this)
  # png <- avg(stimuli, format = "png")
  # gif <- avg(stimuli, format = "gif")
  # expect_equal(magick::image_info(avg$avg$img)$format, "JPEG")
  # expect_equal(magick::image_info(gif$avg$img)$format, "PNG")
  # expect_equal(magick::image_info(png$avg$img)$format, "GIF")

  ## visual checks ----
  skip("needs visual check")
  draw_tem(avg)
  draw_tem(notex)
  draw_tem(twopoint)
  draw_tem(rigid)

  ## long run time ----
  skip("long run time")
  expect_silent(avg100 <- avg(stim120[1:100]))
})

# trans ----
test_that("trans", {
  skip_on_cran()

  s <- demo_stim()

  masc <- trans(s, s$f_multi, s$m_multi, shape = 1)
  expect_equal(names(masc), c("f_multi", "m_multi"))

  fem_masc <- trans(s, s$f_multi, s$m_multi,
              shape = c(fem = -0.5, masc = 0.5))

  expect_equal(names(fem_masc), c("f_multi_fem", "f_multi_masc", "m_multi_fem", "m_multi_masc"))

  steps <- seq(0, 1, .2)
  faces <- demo_stim("london", "003|005") %>% resize(0.5)
  cont <- trans(faces$`003_03`, faces$`003_03`, faces$`005_03`, steps, steps, steps)

  ## visual checks ----
  skip("needs visual check")
  draw_tem(masc) %>% label() %>% plot()
  draw_tem(fem_masc) %>% label() %>% plot(nrow = 2)
  plot(cont)

  # texture
  s <- demo_stim()
  tex <- trans(s[1], s[1], s[2], .5, .5, .5)
  notex <- trans(s[1], s[1], s[2], .5, .5, 0)
})

# loop ----
test_that("loop", {
  stimuli <- demo_stim()
  loop <- loop(stimuli, 2)
  nm <- c("f_multi_m_multi_1", "f_multi_m_multi_2",
          "m_multi_f_multi_1", "m_multi_f_multi_2")
  expect_equal(names(loop), nm)

  revloop <- loop(stimuli[2:1], 2)
  nm <- c("m_multi_f_multi_1", "m_multi_f_multi_2",
          "f_multi_m_multi_1", "f_multi_m_multi_2")
  expect_equal(names(revloop), nm)

  ## visual checks ----
  skip("needs visual check")
  stimuli <- demo_stim("composite") %>%
    `[`(c(1,6,7,2,4,9,10,5)) %>%
    resize(300)
  loop <- loop(stimuli, 5)
  animate(loop, 10)
})

wm_opts(server = "https://webmorph.org")
