#wm_opts(server = "https://webmorph.test")

# frl ----
test_that("frl", {
  skip_on_cran()
  
  stimuli <- demo_tems("frl")
  sym_both <- symmetrize(stimuli)
  sym_shape <- symmetrize(stimuli, color = 0)
  sym_color <- symmetrize(stimuli, shape = 0)
  sym_anti <- symmetrize(stimuli, shape = -1.0, color = 0)
  
  # c(stimuli, sym_both, sym_shape, sym_color, sym_anti) |> 
  #   plot(maxwidth = 600, nrow = 2)
  
  o_pts <- stimuli[[1]]$points
  b_pts <- sym_both[[1]]$points
  s_pts <- sym_shape[[1]]$points
  c_pts <- sym_color[[1]]$points
  a_pts <- sym_anti[[1]]$points
  
  expect_equal(floor(o_pts), c_pts)
  expect_equal(b_pts, s_pts)
  expect_false(all(s_pts == c_pts))
  expect_equal(c_pts + (c_pts - s_pts), a_pts)
  
  # alias
  sym_shape2 <- symmetrise(stimuli, color = 0)
  expect_equal(sym_shape2[[1]]$points, s_pts)
  expect_equivalent(compare(sym_shape, sym_shape2), 0)
})

# fpp106 ----
test_that("fpp106", {
  skip_on_cran()
  
  tem_id <- "fpp106"
  
  stimuli <- demo_tems(tem_id)
  sym_both <- symmetrize(stimuli, tem_id = tem_id)
  sym_shape <- symmetrize(stimuli, color = 0, tem_id = tem_id)
  sym_color <- symmetrize(stimuli, shape = 0, tem_id = tem_id)
  sym_anti <- symmetrize(stimuli, shape = -1.0, color = 0, tem_id = tem_id)
  
  # c(stimuli, sym_both, sym_shape, sym_color, sym_anti) |>
  #   draw_tem() |>
  #   plot(maxwidth = 600, nrow = 2)
  
  o_pts <- stimuli[[1]]$points
  b_pts <- sym_both[[1]]$points
  s_pts <- sym_shape[[1]]$points
  c_pts <- sym_color[[1]]$points
  a_pts <- sym_anti[[1]]$points
  
  expect_equal(floor(o_pts), c_pts)
  expect_equal(b_pts, s_pts)
  expect_false(all(s_pts == c_pts))
  expect_equal(c_pts + (c_pts - s_pts), a_pts)
})

# fpp83 ----
test_that("fpp83", {
  skip_on_cran()
  
  tem_id <- "fpp83"
  
  stimuli <- demo_tems(tem_id)
  sym_both <- symmetrize(stimuli, tem_id = tem_id)
  sym_shape <- symmetrize(stimuli, color = 0, tem_id = tem_id)
  sym_color <- symmetrize(stimuli, shape = 0, tem_id = tem_id)
  sym_anti <- symmetrize(stimuli, shape = -1.0, color = 0, tem_id = tem_id)
  
  # c(stimuli, sym_both, sym_shape, sym_color, sym_anti) |>
  #   draw_tem() |>
  #   plot(maxwidth = 600, nrow = 2)
  
  o_pts <- stimuli[[1]]$points
  b_pts <- sym_both[[1]]$points
  s_pts <- sym_shape[[1]]$points
  c_pts <- sym_color[[1]]$points
  a_pts <- sym_anti[[1]]$points
  
  expect_equal(floor(o_pts), c_pts)
  expect_equal(b_pts, s_pts)
  expect_false(all(s_pts == c_pts))
  expect_equal(c_pts + (c_pts - s_pts), a_pts)
})


# dlib70 ----
test_that("dlib70", {
  skip_on_cran()
  skip_if_offline()
  
  tem_id <- "dlib70"
  
  stimuli <- demo_tems(tem_id)
  sym_both <- symmetrize(stimuli, tem_id = tem_id)
  sym_shape <- symmetrize(stimuli, color = 0, tem_id = tem_id)
  sym_color <- symmetrize(stimuli, shape = 0, tem_id = tem_id)
  sym_anti <- symmetrize(stimuli, shape = -1.0, color = 0, tem_id = tem_id)
  
  # c(stimuli, sym_both, sym_shape, sym_color, sym_anti) |>
  #   draw_tem() |>
  #   plot(maxwidth = 600, nrow = 2)
  
  o_pts <- stimuli[[1]]$points
  b_pts <- sym_both[[1]]$points
  s_pts <- sym_shape[[1]]$points
  c_pts <- sym_color[[1]]$points
  a_pts <- sym_anti[[1]]$points
  
  expect_equal(floor(o_pts), c_pts)
  expect_equal(b_pts, s_pts)
  expect_false(all(s_pts == c_pts))
  expect_equal(c_pts + (c_pts - s_pts), a_pts)
})

# dlib7 ----
test_that("dlib7", {
  skip_on_cran()
  
  tem_id <- "dlib7"
  
  stimuli <- demo_tems(tem_id)[1]
  sym_both <- symmetrize(stimuli, tem_id = tem_id)
  sym_shape <- symmetrize(stimuli, color = 0, tem_id = tem_id)
  sym_color <- symmetrize(stimuli, shape = 0, tem_id = tem_id)
  sym_anti <- symmetrize(stimuli, shape = -1.0, color = 0, tem_id = tem_id)
  
  # c(stimuli, sym_both, sym_shape, sym_color, sym_anti) |>
  #   draw_tem() |>
  #   plot(maxwidth = 600, nrow = 2)
  
  o_pts <- stimuli[[1]]$points
  b_pts <- sym_both[[1]]$points
  s_pts <- sym_shape[[1]]$points
  c_pts <- sym_color[[1]]$points
  a_pts <- sym_anti[[1]]$points
  
  expect_equal(floor(o_pts), c_pts)
  expect_equal(b_pts, s_pts)
  expect_false(all(s_pts == c_pts))
  expect_equal(c_pts + (c_pts - s_pts), a_pts)
})


wm_opts(server = "https://webmorph.org")
