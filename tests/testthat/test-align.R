stimuli <- demo_stim()

test_that("works", {
  f2 <- align(stimuli, x1 = 100, y1 = 150, x2 = 200, y2 = 250,
              width = 300, height = 400)

  expect_equal(f2[[1]]$points[, 1], c(x=100, y=150))
  expect_equal(f2[[1]]$points[, 2], c(x=200, y=250))
  expect_equal(f2[[2]]$points[, 1], c(x=100, y=150))
  expect_equal(f2[[2]]$points[, 2], c(x=200, y=250))
  expect_equal(f2[[1]]$width, 300)
  expect_equal(f2[[1]]$height, 400)
  expect_equal(f2[[2]]$width, 300)
  expect_equal(f2[[2]]$height, 400)
})

# 1point ----
test_that("1point", {
  f1 <- align(stimuli, pt1 = 55, pt2 = 55, x1 = 250, y1 = 250)
  expect_equal(f1$f_multi$points[, 56], c(x = 250, y = 250))
})

# ref_img ----
test_that("ref_img", {
  ref0 <- align(stimuli)
  ref1 <- align(stimuli, ref_img = 1)
  ref2 <- align(stimuli, ref_img = 2)
  
  pt_f <- stimuli$f_multi$points[, 1:2]
  pt_m <- stimuli$m_multi$points[, 1:2]
  pt0 <- ref0$f_multi$points[, 1:2]
  pt1 <- ref1$f_multi$points[, 1:2]
  pt2 <- ref2$f_multi$points[, 1:2]
  
  expect_equal(pt_f, pt1)
  expect_equal(pt_m, pt2)
  expect_equal((pt_f + pt_m)/2, pt0)
})

# procrustes_coords ----
test_that("procrustes_coords", {
  data <- demo_stim() |> tems_to_array()

  expect_silent(suppressWarnings(g <- procrustes_coords(data)))
  expect_silent(p1 <- procrustes_coords(data, 1))
  expect_silent(p2 <- procrustes_coords(data, 2))

  expect_true(all(g != p1))
  expect_true(all(g != p2))
  expect_true(all(p1 != p2))
})

# procrustes ----
test_that("procrustes", {
  stimuli <- demo_stim() |> crop(0.9, x_off = c(0.1, 0))
  pr <- align(stimuli, procrustes = TRUE)

  # eye points all around the same place (low SD)
  p <- sapply(pr, function(x) {
    x$points[1:2, 1:2] |> as.vector()
  }) |> t() |>
    as.data.frame() |>
    dplyr::summarise_all(sd)

  expect_true(p[[1]] < 5)
  expect_true(p[[2]] < 5)
  expect_true(p[[3]] < 5)
  expect_true(p[[4]] < 5)
})

# no tem ----
test_that("no tem", {
  notem <- remove_tem(stimuli)
  expect_error(align(notem))

  t <- demo_stim()
  not <- remove_tem(t)
  expect_warning(a <- align(c(t, not)))
  b <- align(t)
  expect_equal(a, b)
})
