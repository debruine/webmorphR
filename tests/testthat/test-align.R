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


test_that("procrustes", {
  data <- demo_stim() %>% tems_to_array()

  expect_silent(g <- procrustes_align(data))
  expect_silent(p0 <- procrustes_align(data, 0))

  expect_equal(g, p0)
})

test_that("procrustes align", {
  stimuli <- demo_stim() %>% crop(0.9, x_off = c(0.1, 0))
  pr <- align(stimuli, procrustes = TRUE)

  # eye points all around the same place (low SD)
  p <- sapply(pr, function(x) {
    x$points[1:2, 1:2] %>% as.vector()
  }) %>% t() %>%
    as.data.frame() %>%
    dplyr::summarise_all(sd)

  expect_true(p[[1]] < 5)
  expect_true(p[[2]] < 5)
  expect_true(p[[3]] < 5)
  expect_true(p[[4]] < 5)
})

test_that("no tem", {
  notem <- remove_tem(stimuli)
  expect_error(align(notem))

  t <- demo_stim()
  not <- remove_tem(t)
  expect_warning(a <- align(c(t, not)))
  b <- align(t)
  expect_equal(a, b)
})
