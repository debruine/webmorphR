path <- system.file("extdata/test", package = "webmorphR")
nm <- c("f_multi", "m_multi")
temnames <- c("tempath", "points", "lines", "closed")
imgnames <- c("img","imgpath", "width", "height")

test_that("error", {
  expect_error(read_stim(), 'argument "path" is missing, with no default', fixed= TRUE)
})

test_that("basic", {
  stim <- read_stim(path)
  expect_equal(class(stim), c("stimlist", "list"))
  expect_equal(length(stim), 2)
  expect_equal(names(stim), nm)
  expect_equal(class(stim[[1]]), c("stim", "list"))
  expect_equal(names(stim[[1]]), c(imgnames, temnames))
  expect_equal(class(stim[[1]]$img), "magick-image")
  expect_equal(stim[[1]]$imgpath, file.path(path, "f_multi.jpg"))
  expect_equal(stim[[1]]$tempath, file.path(path, "f_multi.tem"))
  expect_equal(stim[[1]]$width, 500)
  expect_equal(stim[[1]]$height, 500)
  expect_equal(ncol(stim[[1]]$points), 189)
  expect_equal(nrow(stim[[1]]$points), 2)
  expect_equal(length(stim[[1]]$lines), 44)
  expect_equal(length(stim[[1]]$closed), 44)
})

test_that("img only", {
  stim <- read_stim(path, "\\.jpg$")
  expect_equal(length(stim), 2)
  expect_equal(names(stim), nm)
  expect_equal(names(stim[[1]]), imgnames)
  expect_equal(class(stim[[1]]$img), "magick-image")
  expect_equal(stim[[1]]$imgpath, file.path(path, "f_multi.jpg"))
  expect_equal(stim[[1]]$width, 500)
  expect_equal(stim[[1]]$height, 500)
})

test_that("tem only", {
  stim <- read_stim(path, "\\.tem$")
  expect_equal(length(stim), 2)
  expect_equal(names(stim), nm)
  expect_equal(names(stim[[1]]), temnames)
  expect_equal(stim[[1]]$tempath, file.path(path, "f_multi.tem"))
  expect_equal(ncol(stim[[1]]$points), 189)
  expect_equal(nrow(stim[[1]]$points), 2)
  expect_equal(length(stim[[1]]$lines), 44)
  expect_equal(length(stim[[1]]$closed), 44)
})

test_that("mixed img/tem", {
  stim <- read_stim(path, "(f_multi\\.tem|m_multi\\.jpg)")
  expect_equal(class(stim[[1]]), c("stim", "list"))
  expect_equal(names(stim), c("m_multi", "f_multi"))
  expect_equal(names(stim$f_multi), temnames)
  expect_equal(names(stim$m_multi), imgnames)
})

test_that("multiple files", {
  paths <- paste0(path, "/", c("f_multi", "m_multi"), ".jpg")
  stim <- read_stim(paths)

  expect_equal(names(stim), c("f_multi", "m_multi"))
  expect_equal(names(stim$m_multi), imgnames)
})

test_that("by index", {
  stim1 <- read_stim(path, 1)
  expect_equal(names(stim1), nm[1])
  expect_equal(dim(stim1[[1]]$points), c(2, 189))

  stim2 <- read_stim(path, 2)
  expect_equal(names(stim2), nm[2])

  stim3 <- read_stim(path, 1:2)
  expect_equal(names(stim3), nm[1:2])

  stim4 <- read_stim(path, c(2, 1))
  expect_equal(names(stim4), nm[c(2, 1)])
})

test_that("multiple tems", {
  path <- system.file("extdata/tem_examples", package = "webmorphR")
  expect_silent(s <- read_stim(path))
})
