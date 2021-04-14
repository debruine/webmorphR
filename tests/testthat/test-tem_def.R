def_parts <- c("name", "notes", "delin_pts", "align_pts", "width", "height", "points", "lines", "closed", "linecolor", "masks")

test_that("default", {
  frl <- tem_def()
  expect_equal(names(frl), def_parts)
  expect_equal(frl$delin_pts, c(0, 1, 96))

  frl1 <- tem_def("frl")
  expect_equal(names(frl1), def_parts)
  expect_equal(frl, frl1)

  fpp83 <- tem_def("fpp83")
  expect_equal(names(fpp83), def_parts)
  expect_equal(fpp83$delin_pts, c(63, 81, 82))

  fpp106 <- tem_def("FPP106")
  expect_equal(names(fpp106), def_parts)
  expect_equal(fpp106$delin_pts, c(0, 1, 95))

  # from file
  file <- system.file("extdata/tem_defs/FRL.csv", package = "webmorphR")
  frl2 <- tem_def(path = file)
  expect_equal(names(frl2), def_parts)
  expect_equal(frl2$delin_pts, c(0, 1, 96))

  expect_equal(frl, frl2)
})

test_that("online", {
  skip_if_offline()

  # no masks in online version
  frl_online <- tem_def(1)
  expect_equal(names(frl_online), def_parts[1:10])
  expect_equal(frl_online$delin_pts, c(0, 1, 96))
})
